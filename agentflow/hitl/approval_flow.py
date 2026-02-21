"""ApprovalFlow - AG-UI統合承認フローモジュール.

AG-UIプロトコルと連携した承認ワークフローを提供。
リアルタイムイベントストリーミングで承認UIと通信。
"""

from __future__ import annotations

import asyncio
import logging
import time
import uuid
from collections.abc import AsyncIterator, Awaitable, Callable
from typing import Any

from agentflow.hitl.types import (
    ApprovalRequest,
    ApprovalResponse,
    ApprovalStatus,
    HITLConfig,
)
from agentflow.protocols.agui_events import (
    AGUIEventType,
    ApprovalRequiredEvent,
    ApprovalSubmittedEvent,
    ApprovalTimeoutEvent,
)


logger = logging.getLogger(__name__)

# イベントエミッタ型定義
EventEmitter = Callable[[dict[str, Any]], Awaitable[None]]


class RiskLevel:
    """リスクレベル定数."""

    LOW = "low"
    NORMAL = "normal"
    HIGH = "high"
    CRITICAL = "critical"


class ApprovalFlowConfig(HITLConfig):
    """ApprovalFlow固有の設定.

    HITLConfigを拡張して、AG-UI統合に必要な設定を追加。
    """

    auto_approve_risk_levels: list[str] = [RiskLevel.LOW]
    """自動承認するリスクレベル."""

    escalation_risk_levels: list[str] = [RiskLevel.CRITICAL]
    """エスカレーションが必要なリスクレベル."""

    notification_on_high_risk: bool = True
    """高リスク時に通知を送信するか."""


class ApprovalFlow:
    """AG-UI統合承認フロー.

    AG-UIイベントプロトコルを使用した承認ワークフローを管理。
    リアルタイムでフロントエンドと通信し、承認UIを制御。

    使用例:
        >>> flow = ApprovalFlow(flow_id="my-flow")
        >>> async for event in flow.request_approval(
        ...     action="delete_user",
        ...     reason="ユーザー削除は不可逆操作",
        ...     risk_level=RiskLevel.HIGH,
        ... ):
        ...     yield event  # SSEストリームに送信
        >>> response = flow.get_response(request_id)
    """

    def __init__(
        self,
        flow_id: str | None = None,
        config: ApprovalFlowConfig | None = None,
        event_emitter: EventEmitter | None = None,
    ) -> None:
        """初期化.

        Args:
            flow_id: フローID（AG-UIイベント用）
            config: 承認フロー設定
            event_emitter: 外部イベント送信コールバック（オプション）
        """
        self._flow_id = flow_id or str(uuid.uuid4())
        self._config = config or ApprovalFlowConfig()
        self._event_emitter = event_emitter
        self._pending: dict[str, ApprovalRequest] = {}
        self._responses: dict[str, ApprovalResponse] = {}
        self._waiting: dict[str, asyncio.Event] = {}
        self._notification_callbacks: list[Callable[[ApprovalRequest], Awaitable[None]]] = []

    @property
    def flow_id(self) -> str:
        """フローID."""
        return self._flow_id

    def register_notification_callback(
        self,
        callback: Callable[[ApprovalRequest], Awaitable[None]],
    ) -> None:
        """通知コールバックを登録.

        Args:
            callback: 承認リクエスト時に呼ばれるコールバック
        """
        self._notification_callbacks.append(callback)

    async def request_approval(
        self,
        action: str,
        reason: str,
        *,
        risk_level: str = RiskLevel.NORMAL,
        context: dict[str, Any] | None = None,
        options: list[dict[str, Any]] | None = None,
        requester: str | None = None,
        timeout_seconds: int | None = None,
    ) -> AsyncIterator[dict[str, Any]]:
        """承認をリクエストし、AG-UIイベントを生成.

        Args:
            action: 承認対象のアクション名
            reason: 承認が必要な理由
            risk_level: リスクレベル
            context: 追加コンテキスト
            options: 選択肢（オプション）
            requester: リクエスター（Agent名等）
            timeout_seconds: タイムアウト秒数

        Yields:
            AG-UIイベント辞書
        """
        # 自動承認チェック
        if risk_level in self._config.auto_approve_risk_levels:
            logger.info(f"自動承認: action={action}, risk_level={risk_level}")
            # 自動承認イベントを生成
            auto_response = ApprovalResponse(
                request_id=str(uuid.uuid4()),
                status=ApprovalStatus.APPROVED,
                approved=True,
                approver="system",
                comment="自動承認（低リスク）",
            )
            yield self._create_submitted_event(auto_response, action).to_dict()
            return

        # 承認リクエスト作成
        request = ApprovalRequest(
            action=action,
            reason=reason,
            context=context or {},
            requester=requester,
            priority=self._risk_to_priority(risk_level),
            timeout_seconds=timeout_seconds or self._config.default_timeout_seconds,
        )

        # 内部状態に登録
        self._pending[request.id] = request
        self._waiting[request.id] = asyncio.Event()

        logger.info(f"承認リクエスト発行: id={request.id}, action={action}, risk_level={risk_level}")

        # 承認要求イベントを生成
        yield self._create_required_event(request, risk_level, options).to_dict()

        # 通知コールバックを実行
        await self._send_notifications(request)

        # 外部イベントエミッタに送信
        if self._event_emitter:
            event = self._create_required_event(request, risk_level, options)
            await self._event_emitter(event.to_dict())

        # 承認待ち
        effective_timeout = timeout_seconds or self._config.default_timeout_seconds
        try:
            await asyncio.wait_for(
                self._waiting[request.id].wait(),
                timeout=effective_timeout,
            )
            response = self._responses.get(request.id)
            if response:
                yield self._create_submitted_event(response, action).to_dict()
        except TimeoutError:
            logger.warning(f"承認タイムアウト: {request.id}")
            yield self._create_timeout_event(request).to_dict()
            # タイムアウトレスポンスを設定
            self._responses[request.id] = ApprovalResponse(
                request_id=request.id,
                status=ApprovalStatus.EXPIRED,
                approved=False,
                rejection_reason="承認タイムアウト",
            )
        finally:
            self._cleanup_request(request.id)

    async def submit_response(
        self,
        request_id: str,
        approved: bool,
        approver: str | None = None,
        comment: str | None = None,
        modifications: dict[str, Any] | None = None,
    ) -> bool:
        """承認レスポンスを送信.

        Args:
            request_id: リクエストID
            approved: 承認されたか
            approver: 承認者
            comment: コメント
            modifications: 修正内容

        Returns:
            成功した場合 True
        """
        if request_id not in self._pending:
            logger.warning(f"不明なリクエストID: {request_id}")
            return False

        status = ApprovalStatus.APPROVED if approved else ApprovalStatus.REJECTED
        response = ApprovalResponse(
            request_id=request_id,
            status=status,
            approved=approved,
            approver=approver,
            comment=comment,
            modifications=modifications or {},
        )

        self._responses[request_id] = response
        event = self._waiting.get(request_id)
        if event:
            event.set()

        logger.info(f"承認レスポンス受信: {request_id}, approved={approved}, approver={approver}")
        return True

    def get_response(self, request_id: str) -> ApprovalResponse | None:
        """承認レスポンスを取得.

        Args:
            request_id: リクエストID

        Returns:
            承認レスポンス（存在する場合）
        """
        return self._responses.get(request_id)

    def get_pending_requests(self) -> list[ApprovalRequest]:
        """保留中のリクエスト一覧を取得."""
        return list(self._pending.values())

    def _create_required_event(
        self,
        request: ApprovalRequest,
        risk_level: str,
        options: list[dict[str, Any]] | None,
    ) -> ApprovalRequiredEvent:
        """承認要求イベントを作成."""
        return ApprovalRequiredEvent(
            event_type=AGUIEventType.APPROVAL_REQUIRED,
            timestamp=time.time(),
            flow_id=self._flow_id,
            request_id=request.id,
            action=request.action,
            reason=request.reason,
            risk_level=risk_level,
            context=request.context,
            options=options or [],
            timeout_seconds=request.timeout_seconds or self._config.default_timeout_seconds,
            requester=request.requester,
        )

    def _create_submitted_event(
        self,
        response: ApprovalResponse,
        action: str,
    ) -> ApprovalSubmittedEvent:
        """承認送信イベントを作成."""
        return ApprovalSubmittedEvent(
            event_type=AGUIEventType.APPROVAL_SUBMITTED,
            timestamp=time.time(),
            flow_id=self._flow_id,
            request_id=response.request_id,
            approved=response.approved,
            approver=response.approver,
            comment=response.comment,
            modifications=response.modifications,
        )

    def _create_timeout_event(self, request: ApprovalRequest) -> ApprovalTimeoutEvent:
        """タイムアウトイベントを作成."""
        return ApprovalTimeoutEvent(
            event_type=AGUIEventType.APPROVAL_TIMEOUT,
            timestamp=time.time(),
            flow_id=self._flow_id,
            request_id=request.id,
            action=request.action,
        )

    def _risk_to_priority(self, risk_level: str) -> str:
        """リスクレベルを優先度に変換."""
        mapping = {
            RiskLevel.LOW: "low",
            RiskLevel.NORMAL: "normal",
            RiskLevel.HIGH: "high",
            RiskLevel.CRITICAL: "critical",
        }
        return mapping.get(risk_level, "normal")

    async def _send_notifications(self, request: ApprovalRequest) -> None:
        """通知を送信."""
        for callback in self._notification_callbacks:
            try:
                await callback(request)
            except Exception as e:
                logger.error(f"通知送信エラー: {e}", exc_info=True)

    def _cleanup_request(self, request_id: str) -> None:
        """リクエストをクリーンアップ."""
        self._pending.pop(request_id, None)
        self._waiting.pop(request_id, None)
