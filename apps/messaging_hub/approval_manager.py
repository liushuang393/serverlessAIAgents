"""承認管理モジュール.

高リスク操作の承認フローを管理する。
リスクレベルに基づいた承認要求、自動承認ルール、履歴管理を提供。

使用例:
    >>> manager = ApprovalManager()
    >>> request_id = await manager.request_approval(
    ...     skill_name="write_file",
    ...     params={"path": "/tmp/test.txt"},
    ...     risk_level=RiskLevel.HIGH,
    ...     user_id="user123",
    ... )
    >>> await manager.approve(request_id, approver_id="admin")
"""

from __future__ import annotations

import asyncio
import logging
import time
import uuid
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow.skills.gateway import RiskLevel


if TYPE_CHECKING:
    from collections.abc import Awaitable, Callable


class ApprovalStatus(str, Enum):
    """承認状態."""

    PENDING = "pending"
    APPROVED = "approved"
    REJECTED = "rejected"
    EXPIRED = "expired"
    AUTO_APPROVED = "auto_approved"


@dataclass
class ApprovalRequest:
    """承認リクエスト.

    Attributes:
        id: リクエストID
        skill_name: スキル名
        risk_level: リスクレベル
        params: スキルパラメータ
        user_id: 要求ユーザーID
        status: 承認状態
        created_at: 作成日時
        expires_at: 有効期限
        decided_at: 決定日時
        decided_by: 決定者ID
        rejection_reason: 拒否理由
        metadata: 追加メタデータ
    """

    id: str
    skill_name: str
    risk_level: RiskLevel
    params: dict[str, Any]
    user_id: str
    status: ApprovalStatus = ApprovalStatus.PENDING
    created_at: datetime = field(default_factory=datetime.now)
    expires_at: datetime | None = None
    decided_at: datetime | None = None
    decided_by: str | None = None
    rejection_reason: str | None = None
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "skill_name": self.skill_name,
            "risk_level": self.risk_level.value,
            "params": self.params,
            "user_id": self.user_id,
            "status": self.status.value,
            "created_at": self.created_at.isoformat(),
            "expires_at": self.expires_at.isoformat() if self.expires_at else None,
            "decided_at": self.decided_at.isoformat() if self.decided_at else None,
            "decided_by": self.decided_by,
            "rejection_reason": self.rejection_reason,
            "metadata": self.metadata,
        }

    def is_expired(self) -> bool:
        """有効期限切れか確認."""
        if self.expires_at is None:
            return False
        return datetime.now() > self.expires_at


@dataclass
class AutoApprovalRule:
    """自動承認ルール.

    Attributes:
        id: ルールID
        skill_name: 対象スキル名（None=全スキル）
        max_risk_level: 最大リスクレベル
        allowed_users: 許可ユーザーリスト（空=全ユーザー）
        enabled: 有効フラグ
    """

    id: str
    skill_name: str | None = None
    max_risk_level: RiskLevel = RiskLevel.LOW
    allowed_users: list[str] = field(default_factory=list)
    enabled: bool = True

    def matches(self, skill_name: str, risk_level: RiskLevel, user_id: str) -> bool:
        """ルールにマッチするか確認."""
        if not self.enabled:
            return False

        # スキル名チェック
        if self.skill_name is not None and self.skill_name != skill_name:
            return False

        # リスクレベルチェック（順序: LOW < MEDIUM < HIGH < CRITICAL）
        risk_order = {
            RiskLevel.LOW: 0,
            RiskLevel.MEDIUM: 1,
            RiskLevel.HIGH: 2,
            RiskLevel.CRITICAL: 3,
        }
        if risk_order.get(risk_level, 99) > risk_order.get(self.max_risk_level, 0):
            return False

        # ユーザーチェック
        return not (self.allowed_users and user_id not in self.allowed_users)


class ApprovalManager:
    """承認管理クラス.

    高リスク操作の承認フローを管理。
    WebSocket経由でリアルタイム通知を行う。
    """

    def __init__(
        self,
        default_expiry_minutes: int = 30,
        websocket_hub: Any | None = None,
    ) -> None:
        """初期化.

        Args:
            default_expiry_minutes: デフォルト有効期限（分）
            websocket_hub: WebSocketHub（リアルタイム通知用）
        """
        self._requests: dict[str, ApprovalRequest] = {}
        self._history: list[ApprovalRequest] = []
        self._auto_rules: list[AutoApprovalRule] = []
        self._default_expiry = timedelta(minutes=default_expiry_minutes)
        self._hub = websocket_hub
        self._logger = logging.getLogger(__name__)
        self._callbacks: list[Callable[[ApprovalRequest], Awaitable[None]]] = []

        # デフォルトの自動承認ルール
        self._setup_default_rules()

    def _setup_default_rules(self) -> None:
        """デフォルトの自動承認ルールを設定."""
        # LOWリスク操作は自動承認
        self._auto_rules.append(
            AutoApprovalRule(
                id="default_low_risk",
                skill_name=None,
                max_risk_level=RiskLevel.LOW,
                allowed_users=[],
                enabled=True,
            )
        )

    def add_auto_rule(self, rule: AutoApprovalRule) -> None:
        """自動承認ルールを追加."""
        self._auto_rules.append(rule)
        self._logger.info("自動承認ルール追加: %s", rule.id)

    def remove_auto_rule(self, rule_id: str) -> bool:
        """自動承認ルールを削除."""
        for i, rule in enumerate(self._auto_rules):
            if rule.id == rule_id:
                self._auto_rules.pop(i)
                self._logger.info("自動承認ルール削除: %s", rule_id)
                return True
        return False

    def list_auto_rules(self) -> list[AutoApprovalRule]:
        """自動承認ルール一覧."""
        return self._auto_rules.copy()

    def on_request(self, callback: Callable[[ApprovalRequest], Awaitable[None]]) -> None:
        """承認リクエスト通知コールバックを登録."""
        self._callbacks.append(callback)

    async def request_approval(
        self,
        skill_name: str,
        params: dict[str, Any],
        risk_level: RiskLevel,
        user_id: str,
        metadata: dict[str, Any] | None = None,
        expiry_minutes: int | None = None,
    ) -> tuple[str, bool]:
        """承認をリクエスト.

        Args:
            skill_name: スキル名
            params: パラメータ
            risk_level: リスクレベル
            user_id: 要求ユーザーID
            metadata: 追加メタデータ
            expiry_minutes: 有効期限（分）

        Returns:
            (リクエストID, 自動承認されたか)
        """
        request_id = str(uuid.uuid4())
        expiry = timedelta(minutes=expiry_minutes) if expiry_minutes else self._default_expiry

        # 自動承認チェック
        for rule in self._auto_rules:
            if rule.matches(skill_name, risk_level, user_id):
                self._logger.info(
                    "自動承認: skill=%s, rule=%s, user=%s",
                    skill_name,
                    rule.id,
                    user_id,
                )
                request = ApprovalRequest(
                    id=request_id,
                    skill_name=skill_name,
                    risk_level=risk_level,
                    params=params,
                    user_id=user_id,
                    status=ApprovalStatus.AUTO_APPROVED,
                    expires_at=datetime.now() + expiry,
                    decided_at=datetime.now(),
                    decided_by="auto_rule:" + rule.id,
                    metadata=metadata or {},
                )
                self._history.append(request)
                return request_id, True

        # 承認リクエストを作成
        request = ApprovalRequest(
            id=request_id,
            skill_name=skill_name,
            risk_level=risk_level,
            params=params,
            user_id=user_id,
            status=ApprovalStatus.PENDING,
            expires_at=datetime.now() + expiry,
            metadata=metadata or {},
        )

        self._requests[request_id] = request
        self._logger.info(
            "承認リクエスト作成: id=%s, skill=%s, risk=%s, user=%s",
            request_id,
            skill_name,
            risk_level.value,
            user_id,
        )

        # コールバック通知
        for callback in self._callbacks:
            try:
                await callback(request)
            except Exception as e:
                self._logger.exception("コールバックエラー: %s", e)

        # WebSocket通知
        if self._hub:
            await self._hub.broadcast(
                {
                    "type": "approval_request",
                    "data": request.to_dict(),
                }
            )

        return request_id, False

    async def approve(
        self,
        request_id: str,
        approver_id: str,
    ) -> bool:
        """リクエストを承認.

        Args:
            request_id: リクエストID
            approver_id: 承認者ID

        Returns:
            承認成功したか
        """
        request = self._requests.get(request_id)
        if request is None:
            self._logger.warning("承認リクエストが見つかりません: %s", request_id)
            return False

        if request.status != ApprovalStatus.PENDING:
            self._logger.warning("リクエストは既に処理済み: %s, status=%s", request_id, request.status.value)
            return False

        if request.is_expired():
            request.status = ApprovalStatus.EXPIRED
            self._move_to_history(request_id)
            self._logger.warning("リクエストは有効期限切れ: %s", request_id)
            return False

        request.status = ApprovalStatus.APPROVED
        request.decided_at = datetime.now()
        request.decided_by = approver_id
        self._move_to_history(request_id)

        self._logger.info("承認完了: id=%s, approver=%s", request_id, approver_id)

        # WebSocket通知
        if self._hub:
            await self._hub.broadcast(
                {
                    "type": "approval_decided",
                    "data": request.to_dict(),
                }
            )

        return True

    async def reject(
        self,
        request_id: str,
        rejecter_id: str,
        reason: str = "",
    ) -> bool:
        """リクエストを拒否.

        Args:
            request_id: リクエストID
            rejecter_id: 拒否者ID
            reason: 拒否理由

        Returns:
            拒否成功したか
        """
        request = self._requests.get(request_id)
        if request is None:
            self._logger.warning("承認リクエストが見つかりません: %s", request_id)
            return False

        if request.status != ApprovalStatus.PENDING:
            self._logger.warning("リクエストは既に処理済み: %s, status=%s", request_id, request.status.value)
            return False

        request.status = ApprovalStatus.REJECTED
        request.decided_at = datetime.now()
        request.decided_by = rejecter_id
        request.rejection_reason = reason
        self._move_to_history(request_id)

        self._logger.info("拒否完了: id=%s, rejecter=%s, reason=%s", request_id, rejecter_id, reason)

        # WebSocket通知
        if self._hub:
            await self._hub.broadcast(
                {
                    "type": "approval_decided",
                    "data": request.to_dict(),
                }
            )

        return True

    def _move_to_history(self, request_id: str) -> None:
        """リクエストを履歴に移動."""
        request = self._requests.pop(request_id, None)
        if request:
            self._history.append(request)

    def get_request(self, request_id: str) -> ApprovalRequest | None:
        """リクエストを取得."""
        return self._requests.get(request_id)

    def list_pending(self) -> list[ApprovalRequest]:
        """保留中のリクエスト一覧."""
        # 有効期限切れをチェック
        expired_ids = []
        for req_id, req in self._requests.items():
            if req.is_expired():
                req.status = ApprovalStatus.EXPIRED
                expired_ids.append(req_id)

        for req_id in expired_ids:
            self._move_to_history(req_id)

        return [r for r in self._requests.values() if r.status == ApprovalStatus.PENDING]

    def list_history(
        self,
        limit: int = 100,
        offset: int = 0,
        status_filter: ApprovalStatus | None = None,
    ) -> list[ApprovalRequest]:
        """履歴を取得.

        Args:
            limit: 取得件数
            offset: オフセット
            status_filter: ステータスフィルター

        Returns:
            履歴リスト（新しい順）
        """
        filtered = self._history
        if status_filter:
            filtered = [r for r in filtered if r.status == status_filter]

        # 新しい順にソート
        filtered = sorted(filtered, key=lambda r: r.created_at, reverse=True)
        return filtered[offset : offset + limit]

    def get_statistics(self) -> dict[str, Any]:
        """統計情報を取得."""
        pending_count = len(self.list_pending())
        history = self._history

        approved_count = sum(1 for r in history if r.status == ApprovalStatus.APPROVED)
        rejected_count = sum(1 for r in history if r.status == ApprovalStatus.REJECTED)
        expired_count = sum(1 for r in history if r.status == ApprovalStatus.EXPIRED)
        auto_approved_count = sum(1 for r in history if r.status == ApprovalStatus.AUTO_APPROVED)

        # リスクレベル別カウント
        risk_counts = {level.value: 0 for level in RiskLevel}
        for req in history:
            risk_counts[req.risk_level.value] += 1

        return {
            "pending": pending_count,
            "approved": approved_count,
            "rejected": rejected_count,
            "expired": expired_count,
            "auto_approved": auto_approved_count,
            "total_processed": len(history),
            "by_risk_level": risk_counts,
        }

    async def wait_for_approval(
        self,
        request_id: str,
        timeout_seconds: float = 300,
        poll_interval: float = 1.0,
    ) -> ApprovalStatus:
        """承認を待機.

        Args:
            request_id: リクエストID
            timeout_seconds: タイムアウト秒数
            poll_interval: ポーリング間隔

        Returns:
            最終ステータス
        """
        start_time = time.time()

        while time.time() - start_time < timeout_seconds:
            # 履歴をチェック（処理済みの場合）
            for req in self._history:
                if req.id == request_id:
                    return req.status

            # 保留中をチェック
            request = self._requests.get(request_id)
            if request is None:
                # 見つからない場合は履歴を再チェック
                for req in self._history:
                    if req.id == request_id:
                        return req.status
                return ApprovalStatus.EXPIRED

            if request.status != ApprovalStatus.PENDING:
                return request.status

            if request.is_expired():
                request.status = ApprovalStatus.EXPIRED
                self._move_to_history(request_id)
                return ApprovalStatus.EXPIRED

            await asyncio.sleep(poll_interval)

        return ApprovalStatus.EXPIRED
