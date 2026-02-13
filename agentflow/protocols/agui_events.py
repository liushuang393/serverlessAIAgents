"""AG-UI イベント定義.

このモジュールは AgentFlow のフローイベントを AG-UI プロトコルイベントに変換するための
データモデルを提供します。

SSE シリアライズ:
    全ての AGUIEvent は to_sse() メソッドで SSE 形式に変換できます。
    これにより、アプリケーション層でフィールドを手動で抽出する必要がなくなります。
"""

import json
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


class AGUIEventType(str, Enum):
    """AG-UI イベントタイプ.

    AG-UI プロトコルで定義されているイベントタイプ。
    """

    # フロー実行イベント
    FLOW_START = "flow.start"
    FLOW_COMPLETE = "flow.complete"
    FLOW_ERROR = "flow.error"
    FLOW_CANCEL = "flow.cancel"

    # ノード実行イベント
    NODE_START = "node.start"
    NODE_COMPLETE = "node.complete"
    NODE_ERROR = "node.error"

    # プログレスイベント
    PROGRESS = "progress"

    # ログイベント
    LOG = "log"

    # クラリフィケーションイベント（ユーザー補足要求）
    CLARIFICATION_REQUIRED = "clarification.required"
    CLARIFICATION_RECEIVED = "clarification.received"

    # 承認イベント（HITL）
    APPROVAL_REQUIRED = "approval_required"
    APPROVAL_SUBMITTED = "approval_submitted"
    APPROVAL_TIMEOUT = "approval_timeout"


class AGUIEvent(BaseModel):
    """AG-UI イベントベースクラス.

    全ての AG-UI イベントの基底クラス。

    使用例:
        event = FlowCompleteEvent(...)
        sse_string = event.to_sse()  # "data: {...}\\n\\n"
    """

    event_type: AGUIEventType = Field(..., description="イベントタイプ")
    timestamp: float = Field(..., description="イベント発生時刻 (Unix タイムスタンプ)")
    flow_id: str = Field(..., description="フロー実行 ID")
    data: dict[str, Any] = Field(default_factory=dict, description="イベントデータ")

    def to_dict(self) -> dict[str, Any]:
        """イベントを辞書形式に変換.

        Pydantic の model_dump() を使用し、event_type を文字列値に変換。
        None 値のフィールドは除外される。

        Returns:
            SSE 送信用の辞書
        """
        result = self.model_dump(exclude_none=True)
        # event_type を文字列値に変換
        if "event_type" in result:
            result["event_type"] = self.event_type.value
        return result

    def to_sse(self, ensure_ascii: bool = False) -> str:
        """SSE 形式の文字列に変換.

        フレームワークが提供する標準シリアライズ。
        アプリケーション層で個別フィールドを処理する必要がない。

        Args:
            ensure_ascii: ASCII エスケープするか（日本語等を \\uXXXX に変換）

        Returns:
            "data: {JSON}\\n\\n" 形式の文字列
        """
        return f"data: {json.dumps(self.to_dict(), ensure_ascii=ensure_ascii)}\n\n"


class FlowStartEvent(AGUIEvent):
    """フロー開始イベント.

    フローの実行が開始されたときに発行されます。
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.FLOW_START, description="イベントタイプ"
    )


class FlowCompleteEvent(AGUIEvent):
    """フロー完了イベント.

    フローの実行が正常に完了したときに発行されます。

    結果の取得方法:
    - result_id が設定されている場合: /api/results/{result_id} で取得
    - result が設定されている場合: イベント内に完整結果を含む
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.FLOW_COMPLETE, description="イベントタイプ"
    )
    result_id: str | None = Field(default=None, description="結果ID（ストア参照用）")
    result: dict[str, Any] | None = Field(
        default=None, description="完整結果（小サイズ時直接含む）"
    )
    include_result: bool = Field(default=False, description="結果をイベントに含むか")


class FlowErrorEvent(AGUIEvent):
    """フローエラーイベント.

    フローの実行中にエラーが発生したときに発行されます。
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.FLOW_ERROR, description="イベントタイプ"
    )
    error_message: str = Field(..., description="エラーメッセージ")
    error_type: str = Field(..., description="エラータイプ")


class FlowCancelEvent(AGUIEvent):
    """フローキャンセルイベント.

    フローの実行がキャンセルされたときに発行されます。
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.FLOW_CANCEL, description="イベントタイプ"
    )


class NodeStartEvent(AGUIEvent):
    """ノード開始イベント.

    ノードの実行が開始されたときに発行されます。
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.NODE_START, description="イベントタイプ"
    )
    node_id: str = Field(..., description="ノード ID")
    node_name: str = Field(..., description="ノード名")


class NodeCompleteEvent(AGUIEvent):
    """ノード完了イベント.

    ノードの実行が正常に完了したときに発行されます。
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.NODE_COMPLETE, description="イベントタイプ"
    )
    node_id: str = Field(..., description="ノード ID")
    node_name: str = Field(..., description="ノード名")


class NodeErrorEvent(AGUIEvent):
    """ノードエラーイベント.

    ノードの実行中にエラーが発生したときに発行されます。
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.NODE_ERROR, description="イベントタイプ"
    )
    node_id: str = Field(..., description="ノード ID")
    node_name: str = Field(..., description="ノード名")
    error_message: str = Field(..., description="エラーメッセージ")
    error_type: str = Field(..., description="エラータイプ")


class ProgressEvent(AGUIEvent):
    """プログレスイベント.

    フローの進行状況を報告するために発行されます。
    """

    event_type: AGUIEventType = Field(default=AGUIEventType.PROGRESS, description="イベントタイプ")
    current: int = Field(..., description="現在の進行数")
    total: int = Field(..., description="合計数")
    percentage: float = Field(..., description="進行率 (0-100)")


class LogEvent(AGUIEvent):
    """ログイベント.

    フロー実行中のログメッセージを配信するために発行されます。
    """

    event_type: AGUIEventType = Field(default=AGUIEventType.LOG, description="イベントタイプ")
    level: str = Field(..., description="ログレベル (DEBUG, INFO, WARNING, ERROR)")
    message: str = Field(..., description="ログメッセージ")
    source: str | None = Field(default=None, description="ログソース")


class ClarificationQuestion(BaseModel):
    """補足質問."""

    id: str = Field(..., description="質問ID")
    text: str = Field(..., description="質問テキスト")
    type: str = Field(default="text", description="入力タイプ (text, number, select)")
    required: bool = Field(default=True, description="必須フラグ")
    options: list[str] = Field(default_factory=list, description="選択肢（selectタイプ用）")
    placeholder: str = Field(default="", description="プレースホルダー")


class ClarificationRequiredEvent(AGUIEvent):
    """補足要求イベント.

    Agent が情報不足を検出し、ユーザーに補足を求める際に発行されます。
    フロントエンドはこのイベントを受信したら補足入力フォームを表示します。
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.CLARIFICATION_REQUIRED, description="イベントタイプ"
    )
    original_question: str = Field(..., description="元の質問")
    questions: list[ClarificationQuestion] = Field(..., description="補足質問リスト")
    message: str = Field(default="", description="ユーザーへのメッセージ")
    timeout_seconds: int = Field(default=300, description="入力タイムアウト秒")


class ClarificationReceivedEvent(AGUIEvent):
    """補足受信イベント.

    ユーザーが補足情報を提供した際に発行されます。
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.CLARIFICATION_RECEIVED, description="イベントタイプ"
    )
    answers: dict[str, Any] = Field(..., description="ユーザー回答")


class ApprovalRequiredEvent(AGUIEvent):
    """承認要求イベント."""

    event_type: AGUIEventType = Field(
        default=AGUIEventType.APPROVAL_REQUIRED,
        description="イベントタイプ",
    )
    request_id: str = Field(..., description="承認要求ID")
    action: str = Field(..., description="対象アクション")
    reason: str = Field(..., description="承認理由")
    risk_level: str = Field(default="normal", description="リスクレベル")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")
    options: list[dict[str, Any]] = Field(default_factory=list, description="選択肢")
    timeout_seconds: int = Field(default=300, description="承認タイムアウト秒")
    requester: str | None = Field(default=None, description="リクエスター")


class ApprovalSubmittedEvent(AGUIEvent):
    """承認結果イベント."""

    event_type: AGUIEventType = Field(
        default=AGUIEventType.APPROVAL_SUBMITTED,
        description="イベントタイプ",
    )
    request_id: str = Field(..., description="承認要求ID")
    approved: bool = Field(..., description="承認されたか")
    approver: str | None = Field(default=None, description="承認者")
    comment: str | None = Field(default=None, description="コメント")
    modifications: dict[str, Any] = Field(default_factory=dict, description="修正内容")


class ApprovalTimeoutEvent(AGUIEvent):
    """承認タイムアウトイベント."""

    event_type: AGUIEventType = Field(
        default=AGUIEventType.APPROVAL_TIMEOUT,
        description="イベントタイプ",
    )
    request_id: str = Field(..., description="承認要求ID")
    action: str = Field(..., description="対象アクション")


LEGACY_EVENT_TYPE_MAP: dict[AGUIEventType, str] = {
    AGUIEventType.FLOW_START: "flow_start",
    AGUIEventType.FLOW_COMPLETE: "flow_complete",
    AGUIEventType.FLOW_ERROR: "flow_error",
    AGUIEventType.FLOW_CANCEL: "flow_cancel",
    AGUIEventType.NODE_START: "node_start",
    AGUIEventType.NODE_COMPLETE: "node_complete",
    AGUIEventType.NODE_ERROR: "node_error",
    AGUIEventType.PROGRESS: "progress",
    AGUIEventType.LOG: "log",
    AGUIEventType.CLARIFICATION_REQUIRED: "clarification_required",
    AGUIEventType.CLARIFICATION_RECEIVED: "clarification_received",
    AGUIEventType.APPROVAL_REQUIRED: "approval_required",
    AGUIEventType.APPROVAL_SUBMITTED: "approval_submitted",
    AGUIEventType.APPROVAL_TIMEOUT: "approval_timeout",
}


def to_legacy_dict(event: AGUIEvent) -> dict[str, Any]:
    """Convert AG-UI event to legacy-compatible dict."""
    payload = event.to_dict()
    payload["type"] = LEGACY_EVENT_TYPE_MAP.get(event.event_type, event.event_type.value)
    if isinstance(event, (FlowErrorEvent, NodeErrorEvent)) and "message" not in payload:
        payload["message"] = event.error_message
    data = payload.get("data")
    if isinstance(data, dict) and "success" in data:
        payload["success"] = data["success"]
    return payload


__all__ = [
    "LEGACY_EVENT_TYPE_MAP",
    "AGUIEvent",
    "AGUIEventType",
    "ApprovalRequiredEvent",
    "ApprovalSubmittedEvent",
    "ApprovalTimeoutEvent",
    "ClarificationQuestion",
    "ClarificationReceivedEvent",
    "ClarificationRequiredEvent",
    "FlowCancelEvent",
    "FlowCompleteEvent",
    "FlowErrorEvent",
    "FlowStartEvent",
    "LogEvent",
    "NodeCompleteEvent",
    "NodeErrorEvent",
    "NodeStartEvent",
    "ProgressEvent",
    "to_legacy_dict",
]
