# -*- coding: utf-8 -*-
"""HITL 型定義モジュール.

Human-in-the-Loop パターンで使用するデータモデルを定義。
"""

from __future__ import annotations

import uuid
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class InterruptType(str, Enum):
    """割り込みタイプ."""

    APPROVAL = "approval"  # 承認待ち
    INPUT = "input"  # ユーザー入力待ち
    REVIEW = "review"  # レビュー待ち
    CONFIRMATION = "confirmation"  # 確認待ち
    ESCALATION = "escalation"  # エスカレーション


class ApprovalStatus(str, Enum):
    """承認ステータス."""

    PENDING = "pending"  # 保留中
    APPROVED = "approved"  # 承認済み
    REJECTED = "rejected"  # 拒否
    EXPIRED = "expired"  # 期限切れ
    CANCELLED = "cancelled"  # キャンセル


class CommandType(str, Enum):
    """コマンドタイプ（再開時の指示）."""

    RESUME = "resume"  # 通常再開
    APPROVE = "approve"  # 承認して再開
    REJECT = "reject"  # 拒否して再開
    UPDATE = "update"  # 状態を更新して再開
    CANCEL = "cancel"  # ワークフローをキャンセル
    RETRY = "retry"  # リトライ


class ApprovalRequest(BaseModel):
    """承認リクエスト.

    Agent が人間の承認を要求する際に使用。
    """

    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    action: str = Field(..., description="承認対象のアクション名")
    resource_id: str | None = Field(None, description="対象リソースID")
    resource_type: str | None = Field(None, description="対象リソースタイプ")
    reason: str = Field(..., description="承認が必要な理由")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")
    requester: str | None = Field(None, description="リクエスター（Agent名等）")
    priority: str = Field("normal", description="優先度: low/normal/high/critical")
    timeout_seconds: int | None = Field(None, description="タイムアウト秒数")
    expires_at: datetime | None = Field(None, description="有効期限")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")
    created_at: datetime = Field(default_factory=datetime.utcnow)


class ApprovalResponse(BaseModel):
    """承認レスポンス.

    人間からの承認/拒否の応答。
    """

    request_id: str = Field(..., description="対象リクエストID")
    status: ApprovalStatus = Field(..., description="承認ステータス")
    approved: bool = Field(False, description="承認されたか")
    approver: str | None = Field(None, description="承認者")
    rejection_reason: str | None = Field(None, description="拒否理由")
    modifications: dict[str, Any] = Field(default_factory=dict, description="修正内容")
    comment: str | None = Field(None, description="コメント")
    responded_at: datetime = Field(default_factory=datetime.utcnow)


class Command(BaseModel):
    """再開コマンド.

    中断されたワークフローを再開する際の指示。
    """

    type: CommandType = Field(..., description="コマンドタイプ")
    value: Any = Field(None, description="コマンド値（承認結果、更新データ等）")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")
    issuer: str | None = Field(None, description="コマンド発行者")
    issued_at: datetime = Field(default_factory=datetime.utcnow)

    @classmethod
    def approve(cls, value: Any = None, approver: str | None = None) -> "Command":
        """承認コマンドを作成."""
        return cls(type=CommandType.APPROVE, value=value, issuer=approver)

    @classmethod
    def reject(cls, reason: str | None = None, rejector: str | None = None) -> "Command":
        """拒否コマンドを作成."""
        return cls(type=CommandType.REJECT, value=reason, issuer=rejector)

    @classmethod
    def update(cls, updates: dict[str, Any], updater: str | None = None) -> "Command":
        """更新コマンドを作成."""
        return cls(type=CommandType.UPDATE, value=updates, issuer=updater)


class InterruptPayload(BaseModel):
    """割り込みペイロード.

    interrupt() が発生した時点の状態を保持。
    """

    id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    interrupt_type: InterruptType = Field(..., description="割り込みタイプ")
    request: ApprovalRequest | None = Field(None, description="承認リクエスト")
    prompt: str | None = Field(None, description="ユーザーへのプロンプト")
    options: list[str] | None = Field(None, description="選択肢（INPUT タイプ用）")
    node_id: str | None = Field(None, description="中断されたノードID")
    flow_id: str | None = Field(None, description="フローID")
    state: dict[str, Any] = Field(default_factory=dict, description="中断時の状態")
    created_at: datetime = Field(default_factory=datetime.utcnow)


class HITLConfig(BaseModel):
    """HITL 設定."""

    enabled: bool = Field(True, description="HITL を有効にするか")
    default_timeout_seconds: int = Field(3600, description="デフォルトタイムアウト（1時間）")
    auto_approve_low_risk: bool = Field(False, description="低リスク操作を自動承認")
    require_approval_for: list[str] = Field(
        default_factory=lambda: ["delete", "update", "create", "execute"],
        description="承認が必要なアクションパターン",
    )
    notification_channels: list[str] = Field(
        default_factory=list,
        description="通知チャンネル（slack, email, webhook）",
    )
    escalation_timeout_seconds: int = Field(
        1800, description="エスカレーションまでの時間（30分）"
    )
    max_pending_requests: int = Field(100, description="最大保留リクエスト数")

