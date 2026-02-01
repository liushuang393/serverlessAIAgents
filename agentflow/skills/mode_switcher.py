# -*- coding: utf-8 -*-
"""実行モード切替機構.

isolated/real_machine モードの動的切替を安全に管理。

設計原則:
- isolated → real_machine への切替は人工確認必須
- real_machine → isolated への切替は即時可能（安全方向）
- モード切替履歴を監査ログに記録
- 切替時に全スキルの権限を再評価

Example:
    >>> switcher = ModeSwitcher(gateway)
    >>> await switcher.switch_to_real_machine(reason="デプロイ作業")
    >>> # 作業完了後
    >>> await switcher.switch_to_isolated()
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any, Callable, Awaitable

from agentflow.skills.os.config import ExecutionMode


class SwitchDirection(str, Enum):
    """切替方向."""

    TO_ISOLATED = "to_isolated"          # 安全方向（即時可）
    TO_REAL_MACHINE = "to_real_machine"  # 危険方向（要確認）


@dataclass
class ModeTransition:
    """モード遷移記録."""

    from_mode: ExecutionMode
    to_mode: ExecutionMode
    reason: str
    requested_by: str
    approved_by: str | None
    timestamp: datetime = field(default_factory=datetime.now)
    success: bool = True

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "from_mode": self.from_mode.value,
            "to_mode": self.to_mode.value,
            "reason": self.reason,
            "requested_by": self.requested_by,
            "approved_by": self.approved_by,
            "timestamp": self.timestamp.isoformat(),
            "success": self.success,
        }


class ModeSwitchError(Exception):
    """モード切替エラー."""

    def __init__(self, message: str, code: str = "mode_switch_error") -> None:
        """初期化."""
        super().__init__(message)
        self.code = code


class ModeSwitchDenied(ModeSwitchError):
    """モード切替拒否."""

    def __init__(self, reason: str) -> None:
        super().__init__(f"モード切替が拒否されました: {reason}", "switch_denied")


class ModeSwitcher:
    """実行モード切替機構.

    ゲートウェイの実行モードを安全に切り替える。
    real_machine への切替は人工確認が必須。
    """

    def __init__(
        self,
        gateway: Any,  # SkillGateway（循環インポート回避）
        *,
        confirmation_handler: Callable[[str, dict], Awaitable[bool]] | None = None,
        auto_revert_seconds: int | None = None,
    ) -> None:
        """初期化.

        Args:
            gateway: SkillGateway インスタンス
            confirmation_handler: 人工確認ハンドラ
            auto_revert_seconds: 自動復帰秒数（None=無効）
        """
        self._gateway = gateway
        self._confirmation_handler = confirmation_handler
        self._auto_revert_seconds = auto_revert_seconds
        self._logger = logging.getLogger(__name__)
        self._history: list[ModeTransition] = []
        self._revert_task: Any = None

    @property
    def current_mode(self) -> ExecutionMode:
        """現在の実行モード."""
        return ExecutionMode(self._gateway._config.execution_mode)

    @property
    def history(self) -> list[ModeTransition]:
        """切替履歴."""
        return list(self._history)

    async def switch_to_isolated(
        self,
        *,
        reason: str = "安全モードへ復帰",
        requested_by: str = "system",
    ) -> ModeTransition:
        """isolated モードへ切替（即時可能）.

        Args:
            reason: 切替理由
            requested_by: 要求者

        Returns:
            遷移記録
        """
        current = self.current_mode
        if current == ExecutionMode.ISOLATED:
            self._logger.info("既に isolated モードです")
            return ModeTransition(
                from_mode=current,
                to_mode=ExecutionMode.ISOLATED,
                reason=reason,
                requested_by=requested_by,
                approved_by=None,
                success=True,
            )

        # 自動復帰タスクをキャンセル
        await self._cancel_auto_revert()

        # 即時切替（安全方向）
        self._gateway._config.execution_mode = ExecutionMode.ISOLATED.value

        transition = ModeTransition(
            from_mode=current,
            to_mode=ExecutionMode.ISOLATED,
            reason=reason,
            requested_by=requested_by,
            approved_by=None,  # 安全方向は承認不要
        )
        self._history.append(transition)

        self._logger.info(
            "AUDIT: モード切替 %s → %s (理由: %s)",
            current.value, ExecutionMode.ISOLATED.value, reason,
        )

        return transition

    async def switch_to_real_machine(
        self,
        *,
        reason: str,
        requested_by: str = "user",
        skip_confirmation: bool = False,
    ) -> ModeTransition:
        """real_machine モードへ切替（人工確認必須）.

        Args:
            reason: 切替理由（必須）
            requested_by: 要求者
            skip_confirmation: 確認スキップ（危険、テスト用）

        Returns:
            遷移記録

        Raises:
            ModeSwitchDenied: 切替が拒否された場合
        """
        current = self.current_mode
        if current == ExecutionMode.REAL_MACHINE:
            self._logger.info("既に real_machine モードです")
            return ModeTransition(
                from_mode=current,
                to_mode=ExecutionMode.REAL_MACHINE,
                reason=reason,
                requested_by=requested_by,
                approved_by=None,
                success=True,
            )

        # 人工確認
        approved_by: str | None = None
        if not skip_confirmation:
            if self._confirmation_handler is None:
                raise ModeSwitchDenied(
                    "real_machine モードへの切替には人工確認ハンドラが必要です"
                )

            confirmed = await self._confirmation_handler(
                f"real_machine モードへの切替を許可しますか？\n理由: {reason}",
                {"reason": reason, "requested_by": requested_by},
            )
            if not confirmed:
                transition = ModeTransition(
                    from_mode=current,
                    to_mode=ExecutionMode.REAL_MACHINE,
                    reason=reason,
                    requested_by=requested_by,
                    approved_by=None,
                    success=False,
                )
                self._history.append(transition)
                raise ModeSwitchDenied("ユーザーが切替を拒否しました")

            approved_by = "user"

        # 切替実行
        self._gateway._config.execution_mode = ExecutionMode.REAL_MACHINE.value

        transition = ModeTransition(
            from_mode=current,
            to_mode=ExecutionMode.REAL_MACHINE,
            reason=reason,
            requested_by=requested_by,
            approved_by=approved_by or "skip_confirmation",
        )
        self._history.append(transition)

        self._logger.warning(
            "AUDIT: モード切替 %s → %s (理由: %s, 承認者: %s)",
            current.value, ExecutionMode.REAL_MACHINE.value, reason, approved_by,
        )

        # 自動復帰タイマー設定
        if self._auto_revert_seconds:
            await self._schedule_auto_revert()

        return transition

    async def _schedule_auto_revert(self) -> None:
        """自動復帰をスケジュール."""
        import asyncio

        await self._cancel_auto_revert()

        async def _revert() -> None:
            await asyncio.sleep(self._auto_revert_seconds)  # type: ignore
            self._logger.warning("自動復帰: real_machine → isolated")
            await self.switch_to_isolated(
                reason=f"自動復帰（{self._auto_revert_seconds}秒経過）",
                requested_by="auto_revert",
            )

        self._revert_task = asyncio.create_task(_revert())

    async def _cancel_auto_revert(self) -> None:
        """自動復帰タスクをキャンセル."""
        if self._revert_task and not self._revert_task.done():
            self._revert_task.cancel()
            self._revert_task = None

