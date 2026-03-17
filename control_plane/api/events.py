"""AgentFlow 標準イベントスキーマ.

全アプリケーションで使用するイベント型を定義する。
独自イベント型の定義は禁止 - 必ずここで定義された型を使用すること。

既存の2系統を統一:
- control_plane.api.sse_emitter.SSEEventType  (API層: flat 名前)
- kernel.protocols.agui_events.AGUIEventType (AG-UI層: dot 名前)

本モジュールが正規定義元（canonical source）となる。
"""

from __future__ import annotations

from datetime import UTC, datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class SSEEventType(str, Enum):
    """SSE イベントタイプ（標準定義）.

    全アプリケーションはこの列挙型のみを使用すること。
    独自のイベントタイプ文字列を直接使用してはならない。
    """

    # --- フロー制御 ---
    FLOW_START = "flow.start"
    FLOW_COMPLETE = "flow.complete"
    FLOW_ERROR = "flow.error"
    FLOW_CANCEL = "flow.cancel"

    # --- ノード制御 ---
    NODE_START = "node.start"
    NODE_COMPLETE = "node.complete"
    NODE_ERROR = "node.error"

    # --- 進捗・ログ ---
    PROGRESS = "progress"
    LOG = "log"

    # --- データ配信 ---
    DATA = "data"
    PARTIAL = "partial"
    CHUNK = "chunk"

    # --- 接続 ---
    CONNECTION_ESTABLISHED = "connection.established"

    # --- 結果 ---
    RESULT = "result"

    # --- Agent 関連 ---
    AGENT_START = "agent.start"
    AGENT_COMPLETE = "agent.complete"
    TOOL_CALL = "tool.call"
    TOOL_RESULT = "tool.result"

    # --- HITL（Human-in-the-Loop） ---
    APPROVAL_REQUIRED = "approval.required"
    APPROVAL_RESPONSE = "approval.response"
    APPROVAL_TIMEOUT = "approval.timeout"
    CLARIFICATION_REQUIRED = "clarification.required"
    CLARIFICATION_RECEIVED = "clarification.received"

    # --- レビュー ---
    REVIEW_VERDICT = "review.verdict"
    EARLY_RETURN = "early_return"


class AgentEvent(BaseModel):
    """AgentFlow 標準イベントモデル.

    全アプリケーションはこのモデルに準拠してイベントを emit すること。
    SSEEmitter / AGUIEventEmitter の両方から利用可能な統一スキーマ。
    """

    type: SSEEventType
    agent_id: str | None = None
    data: dict[str, Any] = Field(default_factory=dict)
    timestamp: datetime = Field(
        default_factory=lambda: datetime.now(UTC),
    )
    run_id: str | None = None
    node_name: str | None = None

    def to_sse_data(self) -> dict[str, Any]:
        """SSE送信用の dict に変換.

        Returns:
            SSE ペイロードとして送信可能な辞書
        """
        result: dict[str, Any] = {
            "type": self.type.value,
            "data": self.data,
            "timestamp": self.timestamp.isoformat(),
        }
        if self.agent_id is not None:
            result["agent_id"] = self.agent_id
        if self.run_id is not None:
            result["run_id"] = self.run_id
        if self.node_name is not None:
            result["node_name"] = self.node_name
        return result


__all__ = [
    "AgentEvent",
    "SSEEventType",
]
