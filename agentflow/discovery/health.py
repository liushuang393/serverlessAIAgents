"""ヘルスチェック機能.

Agent の生存確認とステータス更新を行うヘルスチェッカー。
"""

import asyncio
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from agentflow.discovery.registry import InMemoryAgentRegistry


class HealthChecker:
    """Agent ヘルスチェッカー.

    定期的に Agent のヘルスチェックを実行し、ステータスを更新する。

    Attributes:
        registry: Agent レジストリ
        interval: チェック間隔（秒）
        _task: バックグラウンドタスク
        _running: 実行中フラグ
    """

    def __init__(
        self,
        registry: "InMemoryAgentRegistry",
        interval: int = 10,
    ) -> None:
        """初期化.

        Args:
            registry: Agent レジストリ
            interval: チェック間隔（秒）
        """
        self.registry = registry
        self.interval = interval
        self._task: asyncio.Task | None = None
        self._running = False

    async def start(self) -> None:
        """ヘルスチェックを開始."""
        if self._running:
            return

        self._running = True
        self._task = asyncio.create_task(self._run_loop())

    async def stop(self) -> None:
        """ヘルスチェックを停止."""
        self._running = False
        if self._task:
            self._task.cancel()
            try:
                await self._task
            except asyncio.CancelledError:
                pass
            self._task = None

    async def _run_loop(self) -> None:
        """ヘルスチェックループ."""
        while self._running:
            try:
                await self._check()
                await asyncio.sleep(self.interval)
            except asyncio.CancelledError:
                break
            except Exception:
                # エラーログを記録して継続
                await asyncio.sleep(self.interval)

    async def _check(self) -> None:
        """ヘルスチェックを実行."""
        # タイムアウトした Agent をクリーンアップ
        await self.registry.cleanup_stale()

    async def __aenter__(self) -> "HealthChecker":
        """コンテキストマネージャーエントリー."""
        await self.start()
        return self

    async def __aexit__(self, *args: object) -> None:
        """コンテキストマネージャーエグジット."""
        await self.stop()

