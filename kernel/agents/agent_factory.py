"""Unified Agent factory.

Agent インスタンス生成を一元化し、共通の実行コンテキスト
（tool gateway / mcp / skills / tiered memory）を注入する。

A2A Hub 互換 / LocalAgentBus 標準経路:
    from_module() / from_app_config() で ResilientAgent を検出・インスタンス化し、
    内部では LocalAgentBus に登録し、LocalA2AHub は互換ラッパーとして扱う。

使用例:
    >>> from kernel.agents.agent_factory import AgentFactory
    >>> agents = AgentFactory.from_app_config(Path("apps/my_app/app_config.json"))
"""

from __future__ import annotations

import importlib
import inspect
import json
import logging
from dataclasses import dataclass, field
from threading import Lock
from typing import TYPE_CHECKING, Any

from kernel.agents.agent_block import AgentBlock
from kernel.agents.local_agent_bus import LocalAgentBus, get_agent_bus
from kernel.agents.resilient_agent import ResilientAgent


if TYPE_CHECKING:
    from pathlib import Path

    from kernel.agent_decorator import RegisteredAgent
    from kernel.protocols.a2a_hub import LocalA2AHub


_logger = logging.getLogger(__name__)

DEFAULT_AGENT_TYPE = "specialist"
AgentInstance = ResilientAgent[Any, Any] | AgentBlock


@dataclass(slots=True)
class TieredMemory:
    """Tiered memory コンテナ."""

    short_term: dict[str, Any] = field(default_factory=dict)
    long_term: dict[str, Any] = field(default_factory=dict)
    ephemeral: dict[str, Any] = field(default_factory=dict)


@dataclass(slots=True)
class AgentSharedContext:
    """Agent 共通実行コンテキスト."""

    tool_gateway: Any = None
    mcp: dict[str, Any] = field(default_factory=dict)
    skills: dict[str, Any] = field(default_factory=dict)
    memory: TieredMemory = field(default_factory=TieredMemory)


@dataclass(slots=True)
class AgentFactorySpec:
    """Agent 生成仕様."""

    agent_class: type[Any] | None = None
    class_name: str | None = None
    module_path: str | None = None
    init_kwargs: dict[str, Any] = field(default_factory=dict)
    agent_type: str | None = None
    shared_context: AgentSharedContext | None = None


class AgentFactoryError(RuntimeError):
    """Agent factory errors."""


_DEFAULT_CONTEXT: AgentSharedContext | None = None
_DEFAULT_CONTEXT_LOCK = Lock()


def get_default_shared_context() -> AgentSharedContext:
    """共有コンテキストのシングルトンを返す."""
    global _DEFAULT_CONTEXT
    if _DEFAULT_CONTEXT is not None:
        return _DEFAULT_CONTEXT

    with _DEFAULT_CONTEXT_LOCK:
        if _DEFAULT_CONTEXT is None:
            _DEFAULT_CONTEXT = AgentSharedContext()
        return _DEFAULT_CONTEXT


def create(spec: AgentFactorySpec) -> Any:
    """仕様に基づいて Agent を生成する."""
    agent_class = _resolve_agent_class(spec)
    shared_context = spec.shared_context or get_default_shared_context()
    resolved_type = _resolve_agent_type(spec.agent_type)

    init_kwargs = dict(spec.init_kwargs)
    context_kwargs = {
        "tool_gateway": shared_context.tool_gateway,
        "mcp": shared_context.mcp,
        "skills": shared_context.skills,
        "memory": shared_context.memory,
        "agent_type": resolved_type,
    }

    try:
        instance = _instantiate(agent_class, init_kwargs=init_kwargs, context_kwargs=context_kwargs)
    except TypeError as exc:
        msg = f"Failed to instantiate agent '{agent_class.__name__}': {exc}"
        raise AgentFactoryError(msg) from exc

    _attach_shared_context(instance, shared_context=shared_context, agent_type=resolved_type)
    return instance


def _resolve_agent_type(agent_type: str | None) -> str:
    if not isinstance(agent_type, str):
        return DEFAULT_AGENT_TYPE
    normalized = agent_type.strip().lower()
    return normalized or DEFAULT_AGENT_TYPE


def _resolve_agent_class(spec: AgentFactorySpec) -> type[Any]:
    if spec.agent_class is not None:
        return spec.agent_class

    if not spec.class_name:
        msg = "class_name is required when agent_class is not provided"
        raise AgentFactoryError(msg)
    if not spec.module_path:
        msg = "module_path is required when agent_class is not provided"
        raise AgentFactoryError(msg)

    module = importlib.import_module(spec.module_path)
    try:
        agent_class = getattr(module, spec.class_name)
    except AttributeError as exc:
        msg = f"Class '{spec.class_name}' not found in module '{spec.module_path}'"
        raise AgentFactoryError(msg) from exc

    if not isinstance(agent_class, type):
        msg = f"Resolved object '{spec.class_name}' is not a class"
        raise AgentFactoryError(msg)
    return agent_class


def _instantiate(
    agent_class: type[Any],
    *,
    init_kwargs: dict[str, Any],
    context_kwargs: dict[str, Any],
) -> Any:
    signature = inspect.signature(agent_class.__init__)
    accepts_var_kwargs = any(param.kind == inspect.Parameter.VAR_KEYWORD for param in signature.parameters.values())

    merged_with_context = dict(init_kwargs)
    merged_with_context.update(context_kwargs)
    first_attempt = _filter_kwargs(merged_with_context, signature=signature, accepts_var_kwargs=accepts_var_kwargs)

    try:
        return agent_class(**first_attempt)
    except TypeError:
        fallback = _filter_kwargs(init_kwargs, signature=signature, accepts_var_kwargs=accepts_var_kwargs)
        if fallback == first_attempt:
            raise
        return agent_class(**fallback)


def _filter_kwargs(
    kwargs: dict[str, Any],
    *,
    signature: inspect.Signature,
    accepts_var_kwargs: bool,
) -> dict[str, Any]:
    if accepts_var_kwargs:
        return dict(kwargs)
    allowed = {name for name in signature.parameters if name != "self"}
    return {key: value for key, value in kwargs.items() if key in allowed}


def _attach_shared_context(
    agent: Any,
    *,
    shared_context: AgentSharedContext,
    agent_type: str,
) -> None:
    agent._agent_type = agent_type
    agent._shared_context = shared_context

    shared_values = {
        "tool_gateway": shared_context.tool_gateway,
        "mcp": shared_context.mcp,
        "skills": shared_context.skills,
        "memory": shared_context.memory,
    }
    for attr, value in shared_values.items():
        try:
            current = getattr(agent, attr, None)
        except Exception:
            current = None
        if current is None:
            try:
                setattr(agent, attr, value)
            except Exception:
                continue


# ============================================================================
# AgentFactory — LocalAgentBus 標準登録（Hub は互換入力）
# ============================================================================


class AgentInstantiationError(Exception):
    """Agent インスタンス化失敗の例外.

    Attributes:
        agent_name: 失敗した Agent 名
        module_path: インポートしようとしたモジュールパス
    """

    def __init__(self, agent_name: str, module_path: str, cause: Exception) -> None:
        self.agent_name = agent_name
        self.module_path = module_path
        super().__init__(f"Failed to instantiate {agent_name} from {module_path}: {cause}")


class AgentFactory:
    """app_config.json から Agent を自動インスタンス化するファクトリ.

    責務:
    - module パスから ResilientAgent サブクラスを検出
    - インスタンス化して LocalAgentBus に自動登録
    - app_config.json の agents[] を一括処理
    """

    @staticmethod
    def from_module(
        module_path: str,
        *,
        class_name: str | None = None,
        llm_client: Any = None,
        init_kwargs: dict[str, Any] | None = None,
        hub: LocalA2AHub | None = None,
        bus: LocalAgentBus | None = None,
    ) -> AgentInstance:
        """モジュールパスから Agent を検出・インスタンス化.

        検出優先順位:
        1. ResilientAgent サブクラス
        2. AgentBlock サブクラス（後方互換）

        インスタンス化フォールバック:
        1. llm_client + init_kwargs で試行
        2. init_kwargs のみで試行
        3. 引数なしで試行

        Args:
            module_path: Python モジュールパス（例: "apps.my_app.agents.my_agent"）
            llm_client: LLM クライアント（None の場合は自動注入）
            init_kwargs: Agent コンストラクタへの追加キーワード引数
            hub: 互換用の登録先 LocalA2AHub
            bus: 正規登録先 LocalAgentBus（None の場合は hub またはグローバル bus）

        Returns:
            インスタンス化された Agent

        Raises:
            AgentInstantiationError: インスタンス化に失敗した場合
        """
        target_bus = _resolve_registration_bus(bus=bus, hub=hub)

        try:
            module = importlib.import_module(module_path)
        except ImportError as e:
            msg = "(unknown)"
            raise AgentInstantiationError(msg, module_path, e) from e

        # モジュール内の Agent サブクラスを検索
        # 優先順位:
        # 1. class_name で指定された正確なクラス
        # 2. ResilientAgent サブクラス
        # 3. AgentBlock サブクラス
        # 4. @agent デコレータ登録クラス
        agent_cls: type[Any] | None = None
        fallback_cls: type[Any] | None = None
        decorated_cls: type[Any] | None = None

        if class_name:
            resolved_obj = getattr(module, class_name, None)
            if resolved_obj is None or not inspect.isclass(resolved_obj):
                msg = f"Class '{class_name}' not found in module '{module_path}'"
                raise AgentInstantiationError(class_name, module_path, AttributeError(msg))
            agent_cls = resolved_obj
        else:
            for _attr_name, obj in inspect.getmembers(module, inspect.isclass):
                if obj.__module__ != module_path:
                    continue
                if issubclass(obj, ResilientAgent) and obj is not ResilientAgent:
                    agent_cls = obj
                    break
                if issubclass(obj, AgentBlock) and obj is not AgentBlock and fallback_cls is None:
                    fallback_cls = obj
                # @agent デコレータで登録されたクラスを検出
                if hasattr(obj, "_agent_registered") and decorated_cls is None:
                    decorated_cls = obj

            if agent_cls is None:
                agent_cls = fallback_cls

        # @agent デコレータ経由の場合: RegisteredAgent から直接インスタンスを取得
        if agent_cls is None and decorated_cls is not None:
            registered_obj = getattr(decorated_cls, "_agent_registered", None)
            if registered_obj is None:
                msg = f"Decorator registration metadata is missing for {module_path}"
                msg = "(decorated)"
                raise AgentInstantiationError(msg, module_path, ValueError(msg))
            registered: RegisteredAgent = registered_obj
            try:
                instance = registered.get_instance()
            except Exception as e:
                raise AgentInstantiationError(registered.name, module_path, e) from e
            if instance is None:
                msg = f"RegisteredAgent.get_instance() returned None for {registered.name}"
                raise AgentInstantiationError(registered.name, module_path, ValueError(msg))
            if not isinstance(instance, (ResilientAgent, AgentBlock)):
                msg = f"RegisteredAgent.get_instance() returned unsupported type: {type(instance).__name__}"
                raise AgentInstantiationError(registered.name, module_path, TypeError(msg))

            # bus に登録（既に登録済みの場合はスキップ）
            agent_name = registered.name
            if target_bus.discover(agent_name) is None:
                target_bus.register(instance)
                _logger.info("Agent auto-registered (decorator): %s from %s", agent_name, module_path)

            return instance

        if agent_cls is None:
            msg = "No ResilientAgent/AgentBlock/@agent subclass found"
            msg = "(none)"
            raise AgentInstantiationError(msg, module_path, ValueError(msg))

        # インスタンス化（複数フォールバック戦略）
        extra = init_kwargs or {}
        instance = None
        cls_name = getattr(agent_cls, "name", "(unknown)")

        for kwargs in [
            {"llm_client": llm_client, **extra},
            {**extra},
            {},
        ]:
            try:
                instance = agent_cls(**kwargs)
                break
            except TypeError:
                continue

        if instance is None:
            last_err = TypeError(f"All instantiation strategies failed for {cls_name}")
            raise AgentInstantiationError(cls_name, module_path, last_err)

        # bus に登録（既に登録済みの場合はスキップ）
        agent_name = getattr(instance, "name", cls_name)
        if target_bus.discover(agent_name) is None:
            target_bus.register(instance)
            _logger.info("Agent auto-registered: %s from %s", agent_name, module_path)

        if not isinstance(instance, (ResilientAgent, AgentBlock)):
            msg = f"Instantiated object is not an agent: {type(instance).__name__}"
            raise AgentInstantiationError(cls_name, module_path, TypeError(msg))
        return instance

    @staticmethod
    def from_app_config(
        config_path: Path,
        *,
        llm_client: Any = None,
        hub: LocalA2AHub | None = None,
        bus: LocalAgentBus | None = None,
        agent_init_overrides: dict[str, dict[str, Any]] | None = None,
    ) -> dict[str, AgentInstance]:
        """app_config.json の agents[] から全 Agent をインスタンス化.

        Args:
            config_path: app_config.json のパス
            llm_client: LLM クライアント
            hub: 互換用の登録先 LocalA2AHub
            bus: 正規登録先 LocalAgentBus

        Returns:
            Agent 名 → ResilientAgent インスタンスのマッピング

        Raises:
            FileNotFoundError: 設定ファイルが存在しない場合
        """
        if not config_path.is_file():
            msg = f"Config file not found: {config_path}"
            raise FileNotFoundError(msg)

        raw = json.loads(config_path.read_text("utf-8"))
        agents_config: list[dict[str, Any]] = raw.get("agents", [])

        result: dict[str, AgentInstance] = {}
        errors: list[str] = []

        for agent_entry in agents_config:
            module_path = agent_entry.get("module")
            class_name = agent_entry.get("class_name")
            agent_name = agent_entry.get("name", "(unknown)")

            if module_path is None:
                _logger.warning("Agent %s has no module path, skipping", agent_name)
                continue

            if not class_name:
                _logger.warning(
                    "Agent %s has no class_name; falling back to module scan compatibility path",
                    agent_name,
                )

            # app_config.json の init_kwargs を取得
            entry_init_kwargs = agent_entry.get("init_kwargs")
            merged_init_kwargs: dict[str, Any] = {}
            if isinstance(entry_init_kwargs, dict):
                merged_init_kwargs.update(entry_init_kwargs)
            override_kwargs = agent_init_overrides.get(agent_name, {}) if agent_init_overrides else {}
            merged_init_kwargs.update(override_kwargs)

            try:
                instance = AgentFactory.from_module(
                    module_path,
                    class_name=class_name,
                    llm_client=llm_client,
                    init_kwargs=merged_init_kwargs,
                    hub=hub,
                    bus=bus,
                )
                resolved_name = getattr(instance, "name", None) or agent_name
                result[resolved_name] = instance
            except AgentInstantiationError as e:
                errors.append(str(e))
                _logger.warning("Skipping agent %s: %s", agent_name, e)

        _logger.info(
            "AgentFactory: %d agents instantiated, %d errors from %s",
            len(result),
            len(errors),
            config_path,
        )
        return result


def _resolve_registration_bus(*, bus: LocalAgentBus | None, hub: LocalA2AHub | None) -> LocalAgentBus:
    """登録先 bus を解決."""
    if bus is not None:
        return bus
    if hub is not None:
        return hub.bus
    return get_agent_bus()
