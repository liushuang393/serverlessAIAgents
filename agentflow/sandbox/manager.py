"""サンドボックスマネージャー.

Daytonaのサンドボックス管理設計を参考に、複数サンドボックスの
一元管理と自動クリーンアップ機能を提供。

設計原則:
- 一元管理: 全サンドボックスを統一的に管理
- 自動クリーンアップ: 非アクティブなサンドボックスの自動削除
- リソース監視: 総リソース使用量の追跡

使用例:
    >>> from agentflow.sandbox.manager import SandboxManager
    >>>
    >>> manager = SandboxManager()
    >>>
    >>> # サンドボックス作成
    >>> sandbox = await manager.create(provider="docker")
    >>> await sandbox.start()
    >>> result = await sandbox.execute("print('Hello')")
    >>>
    >>> # 一覧取得
    >>> sandboxes = manager.list()
    >>>
    >>> # クリーンアップ
    >>> await manager.cleanup_inactive(max_idle_seconds=3600)
"""

from __future__ import annotations

import asyncio
import logging
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any

from agentflow.sandbox.base import SandboxConfig, SandboxState
from agentflow.sandbox.lifecycle import ManagedSandbox


if TYPE_CHECKING:
    import builtins


logger = logging.getLogger(__name__)


class SandboxManager:
    """サンドボックスマネージャー.

    複数のサンドボックスを一元管理し、ライフサイクル操作を提供。

    主な機能:
    - サンドボックスの作成・取得・削除
    - 一覧取得とフィルタリング
    - 自動クリーンアップ
    - リソース使用状況の集計

    Attributes:
        sandboxes: 管理中のサンドボックス

    Example:
        >>> manager = SandboxManager()
        >>> sandbox = await manager.create(provider="docker")
        >>> await sandbox.start()
        >>> # ...使用...
        >>> await manager.cleanup_inactive()
    """

    _instance: SandboxManager | None = None

    def __new__(cls) -> SandboxManager:
        """シングルトンパターン."""
        if cls._instance is None:
            cls._instance = super().__new__(cls)
            cls._instance._initialized = False
        return cls._instance

    def __init__(self) -> None:
        """初期化."""
        if getattr(self, "_initialized", False):
            return

        self._sandboxes: dict[str, ManagedSandbox] = {}
        self._cleanup_task: asyncio.Task[None] | None = None
        self._logger = logging.getLogger(__name__)
        self._initialized = True

    @property
    def sandboxes(self) -> dict[str, ManagedSandbox]:
        """管理中のサンドボックス."""
        return dict(self._sandboxes)

    async def create(
        self,
        provider: str = "docker",
        config: SandboxConfig | None = None,
        sandbox_id: str | None = None,
    ) -> ManagedSandbox:
        """サンドボックスを作成.

        Args:
            provider: プロバイダ名
            config: 設定
            sandbox_id: サンドボックスID

        Returns:
            ManagedSandbox インスタンス
        """
        sandbox = await ManagedSandbox.create(provider, config, sandbox_id)
        self._sandboxes[sandbox.sandbox_id] = sandbox
        self._logger.info(f"サンドボックス作成: {sandbox.sandbox_id}")
        return sandbox

    def get(self, sandbox_id: str) -> ManagedSandbox | None:
        """サンドボックスを取得.

        Args:
            sandbox_id: サンドボックスID

        Returns:
            ManagedSandbox または None
        """
        return self._sandboxes.get(sandbox_id)

    def list(
        self,
        state: SandboxState | None = None,
        provider: str | None = None,
    ) -> list[ManagedSandbox]:
        """サンドボックス一覧を取得.

        Args:
            state: フィルタする状態
            provider: フィルタするプロバイダ

        Returns:
            サンドボックスリスト
        """
        result = list(self._sandboxes.values())

        if state is not None:
            result = [s for s in result if s.state == state]

        return result

    async def delete(self, sandbox_id: str) -> bool:
        """サンドボックスを削除.

        Args:
            sandbox_id: サンドボックスID

        Returns:
            削除成功かどうか
        """
        sandbox = self._sandboxes.get(sandbox_id)
        if not sandbox:
            return False

        await sandbox.delete()
        del self._sandboxes[sandbox_id]
        self._logger.info(f"サンドボックス削除: {sandbox_id}")
        return True

    async def cleanup_inactive(
        self,
        max_idle_seconds: int = 3600,
        include_stopped: bool = True,
    ) -> builtins.list[str]:
        """非アクティブなサンドボックスをクリーンアップ.

        Args:
            max_idle_seconds: 最大アイドル時間（秒）
            include_stopped: 停止中も対象にする

        Returns:
            削除されたサンドボックスIDリスト
        """
        now = datetime.now(UTC)
        deleted_ids: list[str] = []

        for sandbox_id, sandbox in list(self._sandboxes.items()):
            idle_seconds = (now - sandbox.last_activity_at).total_seconds()

            should_delete = False
            if sandbox.state == SandboxState.DELETED:
                should_delete = True
            elif sandbox.state == SandboxState.ARCHIVED or (sandbox.state == SandboxState.STOPPED and include_stopped):
                should_delete = idle_seconds > max_idle_seconds

            if should_delete:
                try:
                    await sandbox.delete()
                except Exception as e:
                    self._logger.exception(f"クリーンアップ失敗 {sandbox_id}: {e}")
                    continue

                del self._sandboxes[sandbox_id]
                deleted_ids.append(sandbox_id)

        if deleted_ids:
            self._logger.info(f"クリーンアップ完了: {len(deleted_ids)} 個削除")

        return deleted_ids

    async def start_auto_cleanup(
        self,
        interval_seconds: int = 300,
        max_idle_seconds: int = 3600,
    ) -> None:
        """自動クリーンアップを開始.

        Args:
            interval_seconds: クリーンアップ間隔（秒）
            max_idle_seconds: 最大アイドル時間（秒）
        """
        if self._cleanup_task and not self._cleanup_task.done():
            return

        async def cleanup_loop() -> None:
            while True:
                await asyncio.sleep(interval_seconds)
                await self.cleanup_inactive(max_idle_seconds)

        self._cleanup_task = asyncio.create_task(cleanup_loop())
        self._logger.info("自動クリーンアップを開始しました")

    def stop_auto_cleanup(self) -> None:
        """自動クリーンアップを停止."""
        if self._cleanup_task and not self._cleanup_task.done():
            self._cleanup_task.cancel()
            self._cleanup_task = None
            self._logger.info("自動クリーンアップを停止しました")

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        states: dict[str, int] = {state.value: 0 for state in SandboxState}
        total_executions = 0
        total_execution_ms = 0.0

        for sandbox in self._sandboxes.values():
            states[sandbox.state.value] += 1
            stats = sandbox.stats
            total_executions += stats["execution_count"]
            total_execution_ms += stats["total_execution_ms"]

        return {
            "total_sandboxes": len(self._sandboxes),
            "states": states,
            "total_executions": total_executions,
            "total_execution_ms": total_execution_ms,
            "auto_cleanup_active": (self._cleanup_task is not None and not self._cleanup_task.done()),
        }

    async def shutdown(self) -> None:
        """全サンドボックスをシャットダウン."""
        self.stop_auto_cleanup()

        for sandbox_id in list(self._sandboxes.keys()):
            try:
                await self.delete(sandbox_id)
            except Exception as e:
                self._logger.exception(f"シャットダウン中のエラー {sandbox_id}: {e}")

        self._logger.info("マネージャーをシャットダウンしました")


# グローバルマネージャーインスタンス
_manager: SandboxManager | None = None


def get_sandbox_manager() -> SandboxManager:
    """グローバルサンドボックスマネージャーを取得.

    Returns:
        SandboxManager インスタンス
    """
    global _manager
    if _manager is None:
        _manager = SandboxManager()
    return _manager
