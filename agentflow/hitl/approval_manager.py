"""ApprovalManager - 承認管理モジュール.

人間の承認ワークフローを管理するためのコンポーネント。
承認リクエストの追跡、通知、エスカレーションを担当。
"""

from __future__ import annotations

import asyncio
import logging
from collections.abc import Awaitable, Callable
from datetime import datetime, timedelta
from typing import Any

from agentflow.hitl.types import (
    ApprovalRequest,
    ApprovalResponse,
    ApprovalStatus,
    HITLConfig,
)


logger = logging.getLogger(__name__)

# 承認コールバック型定義
ApprovalCallback = Callable[[ApprovalRequest], Awaitable[ApprovalResponse | None]]


class ApprovalManager:
    """承認管理クラス.

    承認リクエストのライフサイクルを管理:
    - リクエストの登録と追跡
    - 通知の送信
    - タイムアウト処理
    - エスカレーション
    """

    def __init__(
        self,
        config: HITLConfig | None = None,
        notification_callback: ApprovalCallback | None = None,
    ) -> None:
        """初期化.

        Args:
            config: HITL 設定
            notification_callback: 通知送信コールバック
        """
        self.config = config or HITLConfig()
        self._pending: dict[str, ApprovalRequest] = {}
        self._responses: dict[str, ApprovalResponse] = {}
        self._waiting: dict[str, asyncio.Event] = {}
        self._notification_callback = notification_callback
        self._escalation_callbacks: list[ApprovalCallback] = []

    async def request_approval(
        self,
        request: ApprovalRequest,
        *,
        timeout_seconds: int | None = None,
        notify: bool = True,
    ) -> ApprovalResponse:
        """承認をリクエスト.

        Args:
            request: 承認リクエスト
            timeout_seconds: タイムアウト秒数
            notify: 通知を送信するか

        Returns:
            承認レスポンス
        """
        effective_timeout = timeout_seconds or self.config.default_timeout_seconds
        request.expires_at = datetime.utcnow() + timedelta(seconds=effective_timeout)

        # 保留中リクエストに追加
        self._pending[request.id] = request
        self._waiting[request.id] = asyncio.Event()

        logger.info(
            f"Approval requested: {request.id} (action={request.action}, "
            f"priority={request.priority})"
        )

        # 通知を送信
        if notify and self._notification_callback:
            try:
                await self._notification_callback(request)
            except Exception as e:
                logger.exception(f"通知送信に失敗: {e}")

        # 承認待ち
        try:
            await asyncio.wait_for(
                self._waiting[request.id].wait(),
                timeout=effective_timeout,
            )
            response = self._responses.get(request.id)
            if response:
                return response
            # 応答がない場合は期限切れとして扱う
            return self._create_expired_response(request)
        except TimeoutError:
            logger.warning(f"Approval request timed out: {request.id}")
            return self._create_expired_response(request)
        finally:
            self._cleanup_request(request.id)

    async def submit_response(
        self,
        request_id: str,
        response: ApprovalResponse,
    ) -> bool:
        """承認レスポンスを送信.

        Args:
            request_id: リクエストID
            response: 承認レスポンス

        Returns:
            成功した場合 True
        """
        if request_id not in self._pending:
            logger.warning(f"Unknown request ID: {request_id}")
            return False

        self._responses[request_id] = response
        event = self._waiting.get(request_id)
        if event:
            event.set()

        logger.info(
            f"Approval response submitted: {request_id} "
            f"(status={response.status.value}, approved={response.approved})"
        )
        return True

    async def approve(
        self,
        request_id: str,
        approver: str | None = None,
        comment: str | None = None,
        modifications: dict[str, Any] | None = None,
    ) -> bool:
        """リクエストを承認."""
        return await self.submit_response(
            request_id,
            ApprovalResponse(
                request_id=request_id,
                status=ApprovalStatus.APPROVED,
                approved=True,
                approver=approver,
                comment=comment,
                modifications=modifications or {},
            ),
        )

    async def reject(
        self,
        request_id: str,
        rejector: str | None = None,
        reason: str | None = None,
    ) -> bool:
        """リクエストを拒否."""
        return await self.submit_response(
            request_id,
            ApprovalResponse(
                request_id=request_id,
                status=ApprovalStatus.REJECTED,
                approved=False,
                approver=rejector,
                rejection_reason=reason,
            ),
        )

    def get_pending_requests(self) -> list[ApprovalRequest]:
        """保留中のリクエスト一覧を取得."""
        return list(self._pending.values())

    def get_request(self, request_id: str) -> ApprovalRequest | None:
        """指定IDのリクエストを取得."""
        return self._pending.get(request_id)

    def _cleanup_request(self, request_id: str) -> None:
        """リクエストをクリーンアップ."""
        self._pending.pop(request_id, None)
        self._waiting.pop(request_id, None)
        # レスポンスは履歴として一定期間保持可能

    def _create_expired_response(self, request: ApprovalRequest) -> ApprovalResponse:
        """期限切れレスポンスを作成."""
        return ApprovalResponse(
            request_id=request.id,
            status=ApprovalStatus.EXPIRED,
            approved=False,
            rejection_reason="Request expired due to timeout",
        )

    def register_escalation_callback(self, callback: ApprovalCallback) -> None:
        """エスカレーションコールバックを登録."""
        self._escalation_callbacks.append(callback)

    async def escalate(self, request_id: str) -> None:
        """リクエストをエスカレート."""
        request = self._pending.get(request_id)
        if not request:
            logger.warning(f"Cannot escalate unknown request: {request_id}")
            return

        request.priority = "critical"
        logger.info(f"Request escalated: {request_id}")

        for callback in self._escalation_callbacks:
            try:
                await callback(request)
            except Exception as e:
                logger.exception(f"Escalation callback failed: {e}")
