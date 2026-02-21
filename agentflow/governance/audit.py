"""ガバナンス監査モデルとロガー.

監査イベントの構造化と永続化インターフェースを提供する。
"""

from __future__ import annotations

import logging
import uuid
from abc import ABC, abstractmethod
from datetime import UTC, datetime
from typing import override

from pydantic import BaseModel, Field

from agentflow.security.policy_engine import AuthDecision, AuthMode


class AuditEvent(BaseModel):
    """ガバナンス監査イベント.

    ツール実行の意思決定を追跡するための監査ログ。
    """

    event_id: str = Field(default_factory=lambda: str(uuid.uuid4()))
    timestamp: datetime = Field(default_factory=lambda: datetime.now(UTC))
    tool_name: str = Field(..., description="対象ツール名")
    tool_call_id: str | None = Field(default=None, description="ツール呼び出しID")
    decision: str = Field(..., description="ガバナンス判定")
    reason: str = Field(..., description="判定理由")
    auth_decision: AuthDecision | None = Field(default=None, description="認可判定")
    auth_mode: AuthMode | None = Field(default=None, description="認可モード")
    auth_reason: str | None = Field(default=None, description="認可理由")
    requires_approval: bool = Field(default=False, description="承認必須フラグ")
    audit_required: bool = Field(default=False, description="監査必須フラグ")
    operation_type: str = Field(default="", description="操作タイプ")
    risk_level: str = Field(default="", description="リスクレベル")
    run_id: str | None = Field(default=None, description="実行ID")
    trace_id: str | None = Field(default=None, description="トレースID")
    thread_id: str | None = Field(default=None, description="スレッドID")
    flow_id: str | None = Field(default=None, description="フローID")
    user_id: str | None = Field(default=None, description="ユーザーID")
    metadata: dict[str, object] = Field(default_factory=dict, description="追加メタデータ")


class AuditLogger(ABC):
    """監査ロガーインターフェース.

    実装は監査イベントを永続化する責務を持つ。
    """

    @abstractmethod
    def log_event(self, event: AuditEvent) -> None:
        """監査イベントを記録."""


class LoggingAuditLogger(AuditLogger):
    """ロギングベースの監査ロガー."""

    def __init__(self, logger: logging.Logger | None = None) -> None:
        """初期化.

        Args:
            logger: ロガーインスタンス
        """

        self._logger: logging.Logger = logger or logging.getLogger(__name__)

    @override
    def log_event(self, event: AuditEvent) -> None:
        """監査イベントをログに出力."""

        self._logger.warning("AUDIT: %s", event.model_dump())


__all__ = [
    "AuditEvent",
    "AuditLogger",
    "LoggingAuditLogger",
]
