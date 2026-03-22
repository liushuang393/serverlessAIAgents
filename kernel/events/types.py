"""Event Type 定数 - Kernel イベント契約.

Kernel 内で発行されるイベントのタイプ名を定数として定義。
全レイヤーがこの定数を参照することで、文字列のタイポや不整合を防止する。

仕様:
    flow.start / node.start / progress / tool.call /
    tool.result / review / flow.complete
"""

from __future__ import annotations

from enum import Enum


class EventType(str, Enum):
    """Kernel イベントタイプ定数.

    全イベント名はドット区切りで名前空間を示す。
    """

    # --- Flow ライフサイクル ---
    FLOW_START = "flow.start"
    FLOW_COMPLETE = "flow.complete"

    # --- Node ライフサイクル ---
    NODE_START = "node.start"
    NODE_COMPLETE = "node.complete"

    # --- 進捗 ---
    PROGRESS = "progress"

    # --- Tool ---
    TOOL_CALL = "tool.call"
    TOOL_RESULT = "tool.result"

    # --- Review (PEV) ---
    REVIEW = "review"

    # --- エラー ---
    FLOW_ERROR = "flow.error"
    NODE_ERROR = "node.error"
    TOOL_ERROR = "tool.error"


__all__ = ["EventType"]
