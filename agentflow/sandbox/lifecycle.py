"""サンドボックスライフサイクル管理.

Daytonaのライフサイクル設計を参考に、サンドボックスの状態管理と
自動クリーンアップ機能を提供。

設計原則:
- 状態遷移管理: 明確な状態マシンによる安全な遷移
- 自動タイムアウト: 非アクティブ時の自動停止・アーカイブ
- リソース監視: CPU/Memory使用状況の追跡
- イベント通知: 状態変更時のコールバック

使用例:
    >>> from agentflow.sandbox.lifecycle import ManagedSandbox
    >>>
    >>> # 基本使用
    >>> sandbox = await ManagedSandbox.create(provider="docker")
    >>> await sandbox.start()
    >>> result = await sandbox.execute("print('Hello')")
    >>> await sandbox.stop()
    >>>
    >>> # コンテキストマネージャー使用
    >>> async with ManagedSandbox.create(provider="docker") as sandbox:
    ...     result = await sandbox.execute("print('Hello')")
"""

from __future__ import annotations

import asyncio
import logging
import uuid
from dataclasses import dataclass, field
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any

from agentflow.sandbox.base import (
    ExecutionResult,
    ResourceUsage,
    SandboxConfig,
    SandboxProvider,
    SandboxState,
)


if TYPE_CHECKING:
    from collections.abc import Callable


logger = logging.getLogger(__name__)


@dataclass
class SandboxEvent:
    """サンドボックスイベント.

    状態変更や実行完了時に発行されるイベント。

    Attributes:
        event_type: イベントタイプ
        sandbox_id: サンドボックスID
        timestamp: タイムスタンプ
        data: イベントデータ
    """

    event_type: str
    sandbox_id: str
    timestamp: datetime = field(default_factory=lambda: datetime.now(UTC))
    data: dict[str, Any] = field(default_factory=dict)


# イベントタイプ定数
class EventType:
    """サンドボックスイベントタイプ."""

    STATE_CHANGED = "sandbox.state_changed"
    EXECUTION_STARTED = "sandbox.execution_started"
    EXECUTION_COMPLETED = "sandbox.execution_completed"
    RESOURCE_WARNING = "sandbox.resource_warning"
    AUTO_STOP = "sandbox.auto_stop"
    AUTO_ARCHIVE = "sandbox.auto_archive"


class ManagedSandbox:
    """ライフサイクル管理付きサンドボックス.

    Daytonaのサンドボックス設計を参考に、完全なライフサイクル管理を提供。

    主な機能:
    - 状態管理: CREATED → STARTED → STOPPED → ARCHIVED → DELETED
    - 自動タイムアウト: 非アクティブ時の自動停止
    - リソース監視: 使用状況の追跡と警告
    - イベント通知: 状態変更時のコールバック

    Attributes:
        sandbox_id: サンドボックスID
        state: 現在の状態
        config: 設定
        created_at: 作成時刻
        last_activity_at: 最後のアクティビティ時刻

    Example:
        >>> sandbox = await ManagedSandbox.create(provider="docker")
        >>> await sandbox.start()
        >>> print(sandbox.state)  # SandboxState.STARTED
        >>> result = await sandbox.execute("print(1 + 1)")
        >>> await sandbox.stop()
    """

    def __init__(
        self,
        provider: SandboxProvider,
        config: SandboxConfig | None = None,
        sandbox_id: str | None = None,
    ) -> None:
        """初期化.

        Args:
            provider: サンドボックスプロバイダ
            config: 設定
            sandbox_id: サンドボックスID（Noneで自動生成）
        """
        self._provider = provider
        self._config = config or SandboxConfig()
        self.sandbox_id = sandbox_id or f"sb-{uuid.uuid4().hex[:12]}"
        self._state = SandboxState.CREATED
        self.created_at = datetime.now(UTC)
        self.last_activity_at = self.created_at
        self._started_at: datetime | None = None
        self._stopped_at: datetime | None = None
        self._execution_count = 0
        self._total_execution_ms = 0.0
        self._last_resource_usage: ResourceUsage | None = None
        self._event_callbacks: list[Callable[[SandboxEvent], Any]] = []
        self._auto_stop_task: asyncio.Task[None] | None = None
        self._logger = logging.getLogger(f"{__name__}.{self.sandbox_id}")

    @property
    def state(self) -> SandboxState:
        """現在の状態."""
        return self._state

    @property
    def config(self) -> SandboxConfig:
        """設定."""
        return self._config

    @property
    def is_running(self) -> bool:
        """実行中かどうか."""
        return self._state == SandboxState.STARTED

    @property
    def uptime_seconds(self) -> float:
        """稼働時間（秒）."""
        if not self._started_at:
            return 0.0
        end_time = self._stopped_at or datetime.now(UTC)
        return (end_time - self._started_at).total_seconds()

    @property
    def stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "sandbox_id": self.sandbox_id,
            "state": self._state.value,
            "created_at": self.created_at.isoformat(),
            "started_at": self._started_at.isoformat() if self._started_at else None,
            "stopped_at": self._stopped_at.isoformat() if self._stopped_at else None,
            "last_activity_at": self.last_activity_at.isoformat(),
            "uptime_seconds": self.uptime_seconds,
            "execution_count": self._execution_count,
            "total_execution_ms": self._total_execution_ms,
            "avg_execution_ms": (
                self._total_execution_ms / self._execution_count
                if self._execution_count > 0
                else 0.0
            ),
        }

    @classmethod
    async def create(
        cls,
        provider: str = "docker",
        config: SandboxConfig | None = None,
        sandbox_id: str | None = None,
    ) -> ManagedSandbox:
        """ファクトリメソッド: サンドボックスを作成.

        Args:
            provider: プロバイダ名（docker/e2b/microsandbox）
            config: 設定
            sandbox_id: サンドボックスID

        Returns:
            ManagedSandbox インスタンス
        """
        from agentflow.sandbox import get_sandbox

        sandbox_provider = get_sandbox(provider, config)
        return cls(sandbox_provider, config, sandbox_id)

    def on_event(self, callback: Callable[[SandboxEvent], Any]) -> Callable[[], None]:
        """イベントコールバックを登録.

        Args:
            callback: コールバック関数

        Returns:
            登録解除関数
        """
        self._event_callbacks.append(callback)

        def unsubscribe() -> None:
            if callback in self._event_callbacks:
                self._event_callbacks.remove(callback)

        return unsubscribe

    def _emit_event(self, event_type: str, data: dict[str, Any] | None = None) -> None:
        """イベントを発行."""
        event = SandboxEvent(
            event_type=event_type,
            sandbox_id=self.sandbox_id,
            data=data or {},
        )
        for callback in self._event_callbacks:
            try:
                callback(event)
            except Exception as e:
                self._logger.exception(f"イベントコールバックでエラー: {e}")

    async def _transition_state(self, new_state: SandboxState) -> None:
        """状態を遷移.

        Args:
            new_state: 新しい状態

        Raises:
            ValueError: 無効な状態遷移
        """
        if not SandboxState.can_transition(self._state, new_state):
            msg = f"無効な状態遷移: {self._state.value} → {new_state.value}"
            raise ValueError(msg)

        old_state = self._state
        self._state = new_state
        self._logger.info(f"状態遷移: {old_state.value} → {new_state.value}")

        self._emit_event(
            EventType.STATE_CHANGED,
            {"old_state": old_state.value, "new_state": new_state.value},
        )

    async def start(self) -> None:
        """サンドボックスを起動.

        Raises:
            ValueError: 起動できない状態
        """
        await self._transition_state(SandboxState.STARTED)
        self._started_at = datetime.now(UTC)
        self._stopped_at = None
        self.last_activity_at = self._started_at
        self._logger.info("サンドボックスを起動しました")

        # 自動停止タスクを開始
        if self._config.auto_stop_seconds > 0:
            self._start_auto_stop_timer()

    async def stop(self) -> None:
        """サンドボックスを停止.

        Raises:
            ValueError: 停止できない状態
        """
        self._cancel_auto_stop_timer()
        await self._transition_state(SandboxState.STOPPED)
        self._stopped_at = datetime.now(UTC)
        self._logger.info("サンドボックスを停止しました")

    async def archive(self) -> None:
        """サンドボックスをアーカイブ.

        Raises:
            ValueError: アーカイブできない状態
        """
        await self._transition_state(SandboxState.ARCHIVED)
        self._logger.info("サンドボックスをアーカイブしました")

    async def delete(self) -> None:
        """サンドボックスを削除.

        Raises:
            ValueError: 削除できない状態
        """
        self._cancel_auto_stop_timer()
        await self._transition_state(SandboxState.DELETED)
        await self._provider.close()
        self._logger.info("サンドボックスを削除しました")

    async def execute(
        self,
        code: str,
        *,
        timeout: float | None = None,
        packages: list[str] | None = None,
        env: dict[str, str] | None = None,
        files: dict[str, bytes] | None = None,
    ) -> ExecutionResult:
        """コードを実行.

        Args:
            code: Python コード
            timeout: タイムアウト秒
            packages: インストールするパッケージ
            env: 環境変数
            files: ファイル

        Returns:
            ExecutionResult

        Raises:
            RuntimeError: 実行できない状態
        """
        if not self.is_running:
            msg = f"サンドボックスは実行中ではありません: {self._state.value}"
            raise RuntimeError(msg)

        self._emit_event(EventType.EXECUTION_STARTED, {"code_length": len(code)})

        result = await self._provider.execute(
            code,
            timeout=timeout,
            packages=packages,
            env=env,
            files=files,
        )

        self._execution_count += 1
        self._total_execution_ms += result.duration_ms
        self.last_activity_at = datetime.now(UTC)

        self._emit_event(
            EventType.EXECUTION_COMPLETED,
            {
                "success": result.success,
                "duration_ms": result.duration_ms,
                "exit_code": result.exit_code,
            },
        )

        # 自動停止タイマーをリセット
        if self._config.auto_stop_seconds > 0:
            self._reset_auto_stop_timer()

        return result

    def _start_auto_stop_timer(self) -> None:
        """自動停止タイマーを開始."""
        self._cancel_auto_stop_timer()

        async def auto_stop_task() -> None:
            await asyncio.sleep(self._config.auto_stop_seconds)
            if self.is_running:
                self._logger.info("自動停止を実行")
                self._emit_event(EventType.AUTO_STOP, {})
                await self.stop()

        self._auto_stop_task = asyncio.create_task(auto_stop_task())

    def _reset_auto_stop_timer(self) -> None:
        """自動停止タイマーをリセット."""
        if self._config.auto_stop_seconds > 0:
            self._start_auto_stop_timer()

    def _cancel_auto_stop_timer(self) -> None:
        """自動停止タイマーをキャンセル."""
        if self._auto_stop_task and not self._auto_stop_task.done():
            self._auto_stop_task.cancel()
            self._auto_stop_task = None

    async def __aenter__(self) -> ManagedSandbox:
        """async with サポート."""
        await self.start()
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """async with サポート."""
        if self.is_running:
            await self.stop()
        if self._state != SandboxState.DELETED:
            await self.delete()
