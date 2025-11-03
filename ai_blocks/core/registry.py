"""
コンポーネントレジストリとホットスワップ機能

このモジュールは、実行時でのコンポーネント交換を可能にするレジストリシステムを提供します。
バージョン管理、段階的デプロイメント、ロールバック機能を含みます。
"""

import asyncio
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Dict, List, Optional, Type

from pydantic import BaseModel, Field

from ..utils.logging import get_logger

logger = get_logger(__name__)


class ComponentStatus(str, Enum):
    """コンポーネントの状態"""

    ACTIVE = "active"  # アクティブ（使用中）
    INACTIVE = "inactive"  # 非アクティブ（待機中）
    DEPRECATED = "deprecated"  # 非推奨（段階的廃止予定）
    FAILED = "failed"  # 失敗（エラー状態）


class DeploymentStrategy(str, Enum):
    """デプロイメント戦略"""

    IMMEDIATE = "immediate"  # 即座に切り替え
    CANARY = "canary"  # カナリアデプロイメント
    BLUE_GREEN = "blue_green"  # ブルーグリーンデプロイメント
    ROLLING = "rolling"  # ローリングアップデート


@dataclass
class ComponentVersion:
    """コンポーネントバージョン情報"""

    version: str
    component_class: Type
    factory_func: Optional[Callable] = None
    config: Dict[str, Any] = field(default_factory=dict)
    status: ComponentStatus = ComponentStatus.INACTIVE
    created_at: datetime = field(default_factory=datetime.now)
    activated_at: Optional[datetime] = None
    health_check_func: Optional[Callable] = None
    rollback_data: Dict[str, Any] = field(default_factory=dict)


class ComponentRegistration(BaseModel):
    """コンポーネント登録情報"""

    component_type: str = Field(..., description="コンポーネントタイプ")
    component_name: str = Field(..., description="コンポーネント名")
    current_version: str = Field(..., description="現在のバージョン")
    versions: Dict[str, ComponentVersion] = Field(
        default_factory=dict, description="利用可能なバージョン"
    )
    deployment_strategy: DeploymentStrategy = Field(
        default=DeploymentStrategy.IMMEDIATE, description="デプロイメント戦略"
    )
    canary_percentage: float = Field(default=0.0, description="カナリアデプロイメントの割合")

    class Config:
        arbitrary_types_allowed = True


class HealthCheckResult(BaseModel):
    """ヘルスチェック結果"""

    healthy: bool = Field(..., description="健全性")
    message: str = Field(..., description="メッセージ")
    metrics: Dict[str, Any] = Field(default_factory=dict, description="メトリクス")
    timestamp: datetime = Field(default_factory=datetime.now, description="チェック時刻")


class ComponentRegistry:
    """コンポーネントレジストリ（シングルトン）"""

    _instance: Optional["ComponentRegistry"] = None
    _lock = asyncio.Lock()

    def __new__(cls) -> "ComponentRegistry":
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._initialized = False
        return cls._instance

    def __init__(self):
        if hasattr(self, "_initialized") and self._initialized:
            return

        self._registrations: Dict[str, ComponentRegistration] = {}
        self._instances: Dict[str, Any] = {}
        self._health_check_tasks: Dict[str, asyncio.Task] = {}
        self._deployment_locks: Dict[str, asyncio.Lock] = {}
        self._metrics: Dict[str, Dict[str, Any]] = {}
        self._initialized: bool = True

        logger.info("コンポーネントレジストリを初期化しました")

    def register_component(
        self,
        component_type: str,
        component_name: str,
        component_class: Type,
        version: str = "1.0.0",
        factory_func: Optional[Callable] = None,
        config: Optional[Dict[str, Any]] = None,
        health_check_func: Optional[Callable] = None,
        deployment_strategy: DeploymentStrategy = DeploymentStrategy.IMMEDIATE,
    ) -> None:
        """
        コンポーネントを登録する

        Args:
            component_type: コンポーネントタイプ（例：'memory', 'tool'）
            component_name: コンポーネント名（例：'vector_memory', 'openai_tool'）
            component_class: コンポーネントクラス
            version: バージョン
            factory_func: ファクトリー関数（Noneの場合はクラスを直接使用）
            config: 設定辞書
            health_check_func: ヘルスチェック関数
            deployment_strategy: デプロイメント戦略
        """
        key = f"{component_type}:{component_name}"

        # 新しいバージョン情報を作成
        version_info = ComponentVersion(
            version=version,
            component_class=component_class,
            factory_func=factory_func,
            config=config or {},
            health_check_func=health_check_func,
        )

        if key not in self._registrations:
            # 新規登録
            registration = ComponentRegistration(
                component_type=component_type,
                component_name=component_name,
                current_version=version,
                deployment_strategy=deployment_strategy,
            )
            registration.versions[version] = version_info
            self._registrations[key] = registration
            self._deployment_locks[key] = asyncio.Lock()

            logger.info(f"新しいコンポーネントを登録しました: {key} v{version}")
        else:
            # バージョン追加
            self._registrations[key].versions[version] = version_info
            logger.info(f"コンポーネントの新バージョンを追加しました: {key} v{version}")

    async def create_instance(
        self,
        component_type: str,
        component_name: str,
        version: Optional[str] = None,
        **kwargs,
    ) -> Any:
        """
        コンポーネントのインスタンスを作成する

        Args:
            component_type: コンポーネントタイプ
            component_name: コンポーネント名
            version: バージョン（Noneの場合は現在のバージョン）
            **kwargs: 追加の初期化パラメータ

        Returns:
            Any: コンポーネントインスタンス
        """
        key = f"{component_type}:{component_name}"

        if key not in self._registrations:
            raise ValueError(f"コンポーネントが登録されていません: {key}")

        registration = self._registrations[key]
        target_version = version or registration.current_version

        if target_version not in registration.versions:
            raise ValueError(f"バージョンが見つかりません: {key} v{target_version}")

        version_info = registration.versions[target_version]

        # ファクトリー関数またはクラスを使用してインスタンスを作成
        if version_info.factory_func:
            instance = await self._call_factory(
                version_info.factory_func, version_info.config, kwargs
            )
        else:
            merged_config = {**version_info.config, **kwargs}
            instance = version_info.component_class(**merged_config)

        # インスタンスを記録
        instance_key = f"{key}:{target_version}:{uuid.uuid4().hex[:8]}"
        self._instances[instance_key] = instance

        logger.debug(f"コンポーネントインスタンスを作成しました: {instance_key}")
        return instance

    async def _call_factory(
        self, factory_func: Callable, config: Dict[str, Any], kwargs: Dict[str, Any]
    ) -> Any:
        """ファクトリー関数を呼び出す"""
        merged_config = {**config, **kwargs}

        if asyncio.iscoroutinefunction(factory_func):
            return await factory_func(**merged_config)
        else:
            return factory_func(**merged_config)

    async def deploy_version(
        self,
        component_type: str,
        component_name: str,
        target_version: str,
        strategy: Optional[DeploymentStrategy] = None,
        canary_percentage: float = 0.1,
    ) -> bool:
        """
        新しいバージョンをデプロイする

        Args:
            component_type: コンポーネントタイプ
            component_name: コンポーネント名
            target_version: デプロイするバージョン
            strategy: デプロイメント戦略
            canary_percentage: カナリアデプロイメントの割合

        Returns:
            bool: デプロイメント成功
        """
        key = f"{component_type}:{component_name}"

        async with self._deployment_locks.get(key, asyncio.Lock()):
            if key not in self._registrations:
                raise ValueError(f"コンポーネントが登録されていません: {key}")

            registration = self._registrations[key]

            if target_version not in registration.versions:
                raise ValueError(f"バージョンが見つかりません: {key} v{target_version}")

            current_version = registration.current_version
            deployment_strategy = strategy or registration.deployment_strategy

            logger.info(
                f"デプロイメント開始: {key} v{current_version} -> v{target_version} ({deployment_strategy})"
            )

            try:
                if deployment_strategy == DeploymentStrategy.IMMEDIATE:
                    success = await self._deploy_immediate(registration, target_version)
                elif deployment_strategy == DeploymentStrategy.CANARY:
                    success = await self._deploy_canary(
                        registration, target_version, canary_percentage
                    )
                elif deployment_strategy == DeploymentStrategy.BLUE_GREEN:
                    success = await self._deploy_blue_green(
                        registration, target_version
                    )
                elif deployment_strategy == DeploymentStrategy.ROLLING:
                    success = await self._deploy_rolling(registration, target_version)
                else:
                    raise ValueError(f"サポートされていないデプロイメント戦略: {deployment_strategy}")

                if success:
                    logger.info(f"デプロイメント成功: {key} v{target_version}")
                else:
                    logger.error(f"デプロイメント失敗: {key} v{target_version}")

                return success

            except Exception as e:
                logger.error(f"デプロイメント中にエラーが発生しました: {key} v{target_version} - {e}")
                await self._rollback(registration, current_version)
                return False

    async def _deploy_immediate(
        self, registration: ComponentRegistration, target_version: str
    ) -> bool:
        """即座デプロイメント"""
        old_version = registration.current_version

        # ヘルスチェック
        if not await self._health_check(registration, target_version):
            return False

        # バージョン切り替え
        registration.versions[old_version].status = ComponentStatus.DEPRECATED
        registration.versions[target_version].status = ComponentStatus.ACTIVE
        registration.versions[target_version].activated_at = datetime.now()
        registration.current_version = target_version

        return True

    async def _deploy_canary(
        self,
        registration: ComponentRegistration,
        target_version: str,
        percentage: float,
    ) -> bool:
        """カナリアデプロイメント"""
        # ヘルスチェック
        if not await self._health_check(registration, target_version):
            return False

        # カナリア設定
        registration.canary_percentage = percentage
        registration.versions[target_version].status = ComponentStatus.ACTIVE

        # 段階的に割合を増加
        for step_percentage in [percentage, 0.25, 0.5, 0.75, 1.0]:
            registration.canary_percentage = step_percentage
            await asyncio.sleep(1)  # 実際の環境では適切な間隔を設定

            if not await self._health_check(registration, target_version):
                return False

        # 完全切り替え
        old_version = registration.current_version
        registration.versions[old_version].status = ComponentStatus.DEPRECATED
        registration.current_version = target_version
        registration.canary_percentage = 0.0

        return True

    async def _deploy_blue_green(
        self, registration: ComponentRegistration, target_version: str
    ) -> bool:
        """ブルーグリーンデプロイメント"""
        # ヘルスチェック
        if not await self._health_check(registration, target_version):
            return False

        # 瞬時切り替え（実際の環境ではロードバランサーの設定変更等）
        old_version = registration.current_version
        registration.versions[old_version].status = ComponentStatus.DEPRECATED
        registration.versions[target_version].status = ComponentStatus.ACTIVE
        registration.versions[target_version].activated_at = datetime.now()
        registration.current_version = target_version

        return True

    async def _deploy_rolling(
        self, registration: ComponentRegistration, target_version: str
    ) -> bool:
        """ローリングアップデート"""
        # ヘルスチェック
        if not await self._health_check(registration, target_version):
            return False

        # 段階的な切り替え（実際の環境では複数インスタンスを順次更新）
        old_version = registration.current_version

        for step in range(1, 6):  # 5段階で更新
            await asyncio.sleep(0.5)  # 実際の環境では適切な間隔を設定

            if not await self._health_check(registration, target_version):
                return False

            logger.debug(f"ローリングアップデート進行中: {step}/5")

        # 完全切り替え
        registration.versions[old_version].status = ComponentStatus.DEPRECATED
        registration.versions[target_version].status = ComponentStatus.ACTIVE
        registration.versions[target_version].activated_at = datetime.now()
        registration.current_version = target_version

        return True

    async def _health_check(
        self, registration: ComponentRegistration, version: str
    ) -> bool:
        """ヘルスチェックを実行する"""
        version_info = registration.versions[version]

        if not version_info.health_check_func:
            return True  # ヘルスチェック関数がない場合は成功とみなす

        try:
            if asyncio.iscoroutinefunction(version_info.health_check_func):
                result = await version_info.health_check_func()
            else:
                result = version_info.health_check_func()

            if isinstance(result, HealthCheckResult):
                return result.healthy
            else:
                return bool(result)

        except Exception as e:
            logger.error(f"ヘルスチェック中にエラーが発生しました: {e}")
            return False

    async def _rollback(
        self, registration: ComponentRegistration, target_version: str
    ) -> None:
        """ロールバックを実行する"""
        logger.warning(f"ロールバック実行中: {registration.component_name} -> v{target_version}")

        # 現在のバージョンを失敗状態に設定
        current_version_info = registration.versions[registration.current_version]
        current_version_info.status = ComponentStatus.FAILED

        # ターゲットバージョンをアクティブに設定
        registration.versions[target_version].status = ComponentStatus.ACTIVE
        registration.current_version = target_version

        logger.info(f"ロールバック完了: {registration.component_name} v{target_version}")

    def get_registration(
        self, component_type: str, component_name: str
    ) -> Optional[ComponentRegistration]:
        """コンポーネント登録情報を取得する"""
        key = f"{component_type}:{component_name}"
        return self._registrations.get(key)

    def list_components(self) -> List[ComponentRegistration]:
        """登録されているコンポーネント一覧を取得する"""
        return list(self._registrations.values())

    def get_metrics(self) -> Dict[str, Any]:
        """レジストリのメトリクスを取得する"""
        return {
            "total_components": len(self._registrations),
            "total_instances": len(self._instances),
            "active_health_checks": len(self._health_check_tasks),
            "registrations": {
                key: {
                    "current_version": reg.current_version,
                    "total_versions": len(reg.versions),
                    "deployment_strategy": reg.deployment_strategy,
                    "canary_percentage": reg.canary_percentage,
                }
                for key, reg in self._registrations.items()
            },
        }


# グローバルレジストリインスタンス
_registry: Optional[ComponentRegistry] = None


def get_registry() -> ComponentRegistry:
    """コンポーネントレジストリのシングルトンインスタンスを取得する"""
    global _registry
    if _registry is None:
        _registry = ComponentRegistry()
    return _registry


# デコレータ関数
def register_component(
    component_type: str,
    component_name: str,
    version: str = "1.0.0",
    health_check_func: Optional[Callable] = None,
    deployment_strategy: DeploymentStrategy = DeploymentStrategy.IMMEDIATE,
):
    """
    コンポーネント登録デコレータ

    Args:
        component_type: コンポーネントタイプ
        component_name: コンポーネント名
        version: バージョン
        health_check_func: ヘルスチェック関数
        deployment_strategy: デプロイメント戦略
    """

    def decorator(cls):
        registry = get_registry()
        registry.register_component(
            component_type=component_type,
            component_name=component_name,
            component_class=cls,
            version=version,
            health_check_func=health_check_func,
            deployment_strategy=deployment_strategy,
        )
        return cls

    return decorator
