"""
コンポーネントファクトリーシステム

このモジュールは、レジストリと連携してコンポーネントの動的生成と管理を行います。
依存性注入、設定管理、ライフサイクル管理を提供します。
"""

import asyncio
from contextlib import asynccontextmanager
from dataclasses import dataclass, field
from typing import Any, Dict, Generic, List, Optional, TypeVar

from ..config import get_settings
from ..utils.logging import get_logger
from .registry import ComponentRegistry, get_registry

logger = get_logger(__name__)

T = TypeVar("T")


@dataclass
class DependencySpec:
    """依存関係の仕様"""

    component_type: str
    component_name: str
    version: Optional[str] = None
    required: bool = True
    config_override: Dict[str, Any] = field(default_factory=dict)


@dataclass
class ComponentSpec:
    """コンポーネント仕様"""

    component_type: str
    component_name: str
    version: Optional[str] = None
    config: Dict[str, Any] = field(default_factory=dict)
    dependencies: List[DependencySpec] = field(default_factory=list)
    singleton: bool = False
    lazy_init: bool = False


@dataclass
class AgentConfig:
    """エージェント設定"""

    name: str
    agent_type: str
    config: Dict[str, Any] = field(default_factory=dict)
    dependencies: List[DependencySpec] = field(default_factory=list)
    singleton: bool = False
    lazy_init: bool = False


class ComponentFactory(Generic[T]):
    """コンポーネントファクトリー基底クラス"""

    def __init__(self, registry: Optional[ComponentRegistry] = None):
        self.registry = registry or get_registry()
        self._singletons: Dict[str, Any] = {}
        self._dependency_cache: Dict[str, Any] = {}

    async def create(
        self,
        component_type: str,
        component_name: str,
        version: Optional[str] = None,
        config: Optional[Dict[str, Any]] = None,
        **kwargs,
    ) -> T:
        """
        コンポーネントを作成する

        Args:
            component_type: コンポーネントタイプ
            component_name: コンポーネント名
            version: バージョン
            config: 設定辞書
            **kwargs: 追加パラメータ

        Returns:
            T: 作成されたコンポーネント
        """
        key = f"{component_type}:{component_name}:{version or 'current'}"

        # シングルトンチェック
        registration = self.registry.get_registration(component_type, component_name)
        if registration and key in self._singletons:
            logger.debug(f"シングルトンインスタンスを返します: {key}")
            return self._singletons[key]  # type: ignore

        # 設定をマージ
        merged_config = await self._merge_config(
            component_type, component_name, config or {}
        )
        merged_config.update(kwargs)

        # 依存関係を解決
        dependencies = await self._resolve_dependencies(component_type, component_name)
        merged_config.update(dependencies)

        # インスタンスを作成
        instance = await self.registry.create_instance(
            component_type=component_type,
            component_name=component_name,
            version=version,
            **merged_config,
        )

        # シングルトンとして保存
        if registration and self._is_singleton(registration):
            self._singletons[key] = instance

        # 初期化後処理
        await self._post_create(instance, component_type, component_name)

        logger.debug(f"コンポーネントを作成しました: {key}")
        return instance  # type: ignore

    async def _merge_config(
        self, component_type: str, component_name: str, user_config: Dict[str, Any]
    ) -> Dict[str, Any]:
        """設定をマージする"""
        # デフォルト設定を取得
        settings = get_settings()
        default_config = self._get_default_config(component_type, settings)

        # レジストリから設定を取得
        registration = self.registry.get_registration(component_type, component_name)
        registry_config = {}
        if registration:
            current_version = registration.current_version
            if current_version in registration.versions:
                registry_config = registration.versions[current_version].config

        # 設定をマージ（優先度: user_config > registry_config > default_config）
        merged = {**default_config, **registry_config, **user_config}

        logger.debug(f"設定をマージしました: {component_type}:{component_name}")
        return merged

    def _get_default_config(self, component_type: str, settings) -> Dict[str, Any]:
        """コンポーネントタイプに応じたデフォルト設定を取得する"""
        config_map = {
            "memory": {
                "max_items": settings.memory_max_items,
                "similarity_threshold": settings.memory_similarity_threshold,
            },
            "tool": {
                "timeout": settings.tool_timeout,
                "max_calls": settings.max_tool_calls,
            },
            "chunker": {
                "chunk_size": settings.chunk_size,
                "overlap": settings.chunk_overlap,
            },
            "llm": {
                "model": settings.default_model,
                "max_tokens": settings.max_tokens,
                "temperature": settings.temperature,
            },
        }

        return config_map.get(component_type, {})

    async def _resolve_dependencies(
        self, component_type: str, component_name: str
    ) -> Dict[str, Any]:
        """依存関係を解決する"""
        dependencies = {}

        # 依存関係の仕様を取得（実際の実装では設定ファイルやアノテーションから取得）
        dependency_specs = self._get_dependency_specs(component_type, component_name)

        for spec in dependency_specs:
            dep_key = f"{spec.component_type}:{spec.component_name}"

            try:
                # キャッシュから取得を試行
                if dep_key in self._dependency_cache:
                    dependency = self._dependency_cache[dep_key]
                else:
                    # 依存関係を作成
                    dependency = await self.create(
                        component_type=spec.component_type,
                        component_name=spec.component_name,
                        version=spec.version,
                        config=spec.config_override,
                    )
                    self._dependency_cache[dep_key] = dependency

                # パラメータ名を推測（実際の実装では型ヒントやアノテーションを使用）
                param_name = self._infer_parameter_name(spec.component_type)
                dependencies[param_name] = dependency

            except Exception as e:
                if spec.required:
                    logger.error(f"必須依存関係の解決に失敗しました: {dep_key} - {e}")
                    raise
                else:
                    logger.warning(f"オプション依存関係の解決に失敗しました: {dep_key} - {e}")

        return dependencies

    def _get_dependency_specs(
        self, component_type: str, component_name: str
    ) -> List[DependencySpec]:
        """依存関係の仕様を取得する（実際の実装では設定ファイルやアノテーションから）"""
        # 基本的な依存関係のマッピング
        dependency_map = {
            "augmented_llm": [
                DependencySpec("llm", "openai", required=True),
                DependencySpec("memory", "vector", required=True),
                DependencySpec("tool", "manager", required=True),
            ],
            "agent_router": [
                DependencySpec("router", "rule_based", required=True),
            ],
            "memory_centric": [
                DependencySpec("memory", "vector", required=True),
                DependencySpec("chunker", "smart", required=True),
                DependencySpec("parser", "multi", required=True),
            ],
        }

        return dependency_map.get(component_name, [])

    def _infer_parameter_name(self, component_type: str) -> str:
        """コンポーネントタイプからパラメータ名を推測する"""
        name_map = {
            "llm": "llm_provider",
            "memory": "memory",
            "tool": "tool_manager",
            "router": "router",
            "chunker": "chunker",
            "parser": "parser",
            "evaluator": "evaluator",
        }

        return name_map.get(component_type, component_type)

    def _is_singleton(self, registration) -> bool:
        """コンポーネントがシングルトンかどうかを判定する"""
        # 実際の実装では設定やアノテーションから判定
        singleton_types = {"memory", "tool"}
        return registration.component_type in singleton_types

    async def _post_create(
        self, instance: Any, component_type: str, component_name: str
    ) -> None:
        """インスタンス作成後の処理"""
        # 初期化処理
        if hasattr(instance, "initialize") and callable(instance.initialize):
            if asyncio.iscoroutinefunction(instance.initialize):
                await instance.initialize()
            else:
                instance.initialize()

        # ヘルスチェック開始
        if hasattr(instance, "start_health_check") and callable(
            instance.start_health_check
        ):
            if asyncio.iscoroutinefunction(instance.start_health_check):
                await instance.start_health_check()
            else:
                instance.start_health_check()

    async def destroy(
        self, component_type: str, component_name: str, version: Optional[str] = None
    ) -> None:
        """コンポーネントを破棄する"""
        key = f"{component_type}:{component_name}:{version or 'current'}"

        if key in self._singletons:
            instance = self._singletons[key]

            # クリーンアップ処理
            if hasattr(instance, "cleanup") and callable(instance.cleanup):
                if asyncio.iscoroutinefunction(instance.cleanup):
                    await instance.cleanup()
                else:
                    instance.cleanup()

            del self._singletons[key]
            logger.debug(f"コンポーネントを破棄しました: {key}")

    def clear_cache(self) -> None:
        """キャッシュをクリアする"""
        self._dependency_cache.clear()
        logger.debug("依存関係キャッシュをクリアしました")


class ArchitectureFactory(ComponentFactory):
    """アーキテクチャパターン専用ファクトリー"""

    async def create_augmented_llm(
        self,
        llm_provider: Optional[str] = None,
        memory_type: Optional[str] = None,
        config: Optional[Dict[str, Any]] = None,
    ):
        """Augmented LLMアーキテクチャを作成する"""
        return await self.create(
            component_type="architecture",
            component_name="augmented_llm",
            config={
                "llm_provider": llm_provider or "openai",
                "memory_type": memory_type or "vector",
                **(config or {}),
            },
        )

    async def create_agent_router(
        self, router_type: Optional[str] = None, config: Optional[Dict[str, Any]] = None
    ):
        """Agent Routerアーキテクチャを作成する"""
        return await self.create(
            component_type="architecture",
            component_name="agent_router",
            config={"router_type": router_type or "rule_based", **(config or {})},
        )

    async def create_parallel_agents(
        self,
        agent_configs: Optional[List[AgentConfig]] = None,
        config: Optional[Dict[str, Any]] = None,
    ):
        """Parallel Agentsアーキテクチャを作成する"""
        return await self.create(
            component_type="architecture",
            component_name="parallel_agents",
            config={"agent_configs": agent_configs or [], **(config or {})},
        )


class ComponentBuilder:
    """コンポーネントビルダー（Fluent Interface）"""

    def __init__(self, factory: Optional[ComponentFactory] = None):
        self.factory = factory or ComponentFactory()
        self._spec = ComponentSpec("", "")

    def component(self, component_type: str, component_name: str) -> "ComponentBuilder":
        """コンポーネントタイプと名前を設定する"""
        self._spec.component_type = component_type
        self._spec.component_name = component_name
        return self

    def version(self, version: str) -> "ComponentBuilder":
        """バージョンを設定する"""
        self._spec.version = version
        return self

    def config(self, **config) -> "ComponentBuilder":
        """設定を追加する"""
        self._spec.config.update(config)
        return self

    def dependency(
        self,
        component_type: str,
        component_name: str,
        version: Optional[str] = None,
        required: bool = True,
        **config_override,
    ) -> "ComponentBuilder":
        """依存関係を追加する"""
        dep_spec = DependencySpec(
            component_type=component_type,
            component_name=component_name,
            version=version,
            required=required,
            config_override=config_override,
        )
        self._spec.dependencies.append(dep_spec)
        return self

    def singleton(self, is_singleton: bool = True) -> "ComponentBuilder":
        """シングルトン設定"""
        self._spec.singleton = is_singleton
        return self

    def lazy(self, is_lazy: bool = True) -> "ComponentBuilder":
        """遅延初期化設定"""
        self._spec.lazy_init = is_lazy
        return self

    async def build(self) -> Any:
        """コンポーネントを構築する"""
        return await self.factory.create(
            component_type=self._spec.component_type,
            component_name=self._spec.component_name,
            version=self._spec.version,
            config=self._spec.config,
        )


# グローバルファクトリーインスタンス
_factory: Optional[ComponentFactory] = None
_architecture_factory: Optional[ArchitectureFactory] = None


def get_factory() -> ComponentFactory:
    """コンポーネントファクトリーのシングルトンインスタンスを取得する"""
    global _factory
    if _factory is None:
        _factory = ComponentFactory()
    return _factory


def get_architecture_factory() -> ArchitectureFactory:
    """アーキテクチャファクトリーのシングルトンインスタンスを取得する"""
    global _architecture_factory
    if _architecture_factory is None:
        _architecture_factory = ArchitectureFactory()
    return _architecture_factory


# 便利関数
async def create_component(
    component_type: str, component_name: str, version: Optional[str] = None, **config
) -> Any:
    """コンポーネントを作成する便利関数"""
    factory = get_factory()
    return await factory.create(component_type, component_name, version, config)


def builder() -> ComponentBuilder:
    """コンポーネントビルダーを作成する便利関数"""
    return ComponentBuilder()


@asynccontextmanager
async def component_context(
    component_type: str, component_name: str, version: Optional[str] = None, **config
):
    """コンポーネントのコンテキストマネージャー"""
    factory = get_factory()
    instance = await factory.create(component_type, component_name, version, config)

    try:
        yield instance
    finally:
        await factory.destroy(component_type, component_name, version)
