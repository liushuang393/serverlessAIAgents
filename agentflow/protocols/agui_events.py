"""AG-UI イベント定義.

このモジュールは AgentFlow のフローイベントを AG-UI プロトコルイベントに変換するための
データモデルを提供します。
"""

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


class AGUIEvent(BaseModel):
    """AG-UI イベントベースクラス.

    全ての AG-UI イベントの基底クラス。
    """

    event_type: AGUIEventType = Field(..., description="イベントタイプ")
    timestamp: float = Field(..., description="イベント発生時刻 (Unix タイムスタンプ)")
    flow_id: str = Field(..., description="フロー実行 ID")
    data: dict[str, Any] = Field(default_factory=dict, description="イベントデータ")


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
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.FLOW_COMPLETE, description="イベントタイプ"
    )


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

    event_type: AGUIEventType = Field(
        default=AGUIEventType.PROGRESS, description="イベントタイプ"
    )
    current: int = Field(..., description="現在の進行数")
    total: int = Field(..., description="合計数")
    percentage: float = Field(..., description="進行率 (0-100)")


class LogEvent(AGUIEvent):
    """ログイベント.

    フロー実行中のログメッセージを配信するために発行されます。
    """

    event_type: AGUIEventType = Field(
        default=AGUIEventType.LOG, description="イベントタイプ"
    )
    level: str = Field(..., description="ログレベル (DEBUG, INFO, WARNING, ERROR)")
    message: str = Field(..., description="ログメッセージ")
    source: str | None = Field(default=None, description="ログソース")

