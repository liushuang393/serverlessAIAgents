"""Unified Agent factory.

Agent インスタンス生成を一元化し、共通の実行コンテキスト
（tool gateway / mcp / skills / tiered memory）を注入する。
"""

from __future__ import annotations

import importlib
import inspect
from dataclasses import dataclass, field
from threading import Lock
from typing import Any


DEFAULT_AGENT_TYPE = "specialist"


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
