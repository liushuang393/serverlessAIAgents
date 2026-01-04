# -*- coding: utf-8 -*-
"""Flow進捗追跡.

フロー実行の進捗を自動追跡し、AG-UIプロトコルイベントを発行。

設計原則:
- 自動化：進捗パーセンテージを自動計算
- プロトコル互換：AG-UI標準イベントを発行
"""

from __future__ import annotations

import logging
import time
from typing import TYPE_CHECKING, Any

if TYPE_CHECKING:
    from agentflow.flow.graph import FlowGraph
    from agentflow.flow.nodes import FlowNode


class ProgressTracker:
    """進捗追跡器.

    フロー実行の進捗を追跡し、ノード開始/完了イベントを発行。

    Example:
        >>> tracker = ProgressTracker(graph, "flow-123")
        >>> event = tracker.on_node_start(node)
        >>> event = tracker.on_node_complete(node, result)
        >>> tracker.progress  # 0.5 = 50%
    """

    def __init__(self, graph: FlowGraph, flow_id: str) -> None:
        """初期化.

        Args:
            graph: フローグラフ
            flow_id: フローインスタンスID
        """
        self._logger = logging.getLogger("agentflow.flow.progress")
        self._graph = graph
        self._flow_id = flow_id
        self._completed: int = 0
        self._current_node: str | None = None

    @property
    def progress(self) -> float:
        """現在の進捗（0.0 ~ 1.0）."""
        total = self._graph.node_count
        if total == 0:
            return 1.0
        return self._completed / total

    @property
    def progress_percent(self) -> int:
        """現在の進捗パーセンテージ（0 ~ 100）."""
        return int(self.progress * 100)

    def on_node_start(self, node: FlowNode) -> dict[str, Any]:
        """ノード開始イベント.

        Args:
            node: 実行を開始するノード

        Returns:
            AG-UI NodeStartEvent形式の辞書
        """
        self._current_node = node.id
        return {
            "event_type": "node.start",
            "type": "node_start",  # 後方互換
            "timestamp": time.time(),
            "flow_id": self._flow_id,
            "node_id": node.id,
            "node_name": node.name,
            "data": {
                "label": node.label,
                "icon": node.icon,
                "progress": self.progress_percent,
            },
        }

    def on_node_complete(
        self, node: FlowNode, result: dict[str, Any], *, success: bool = True
    ) -> dict[str, Any]:
        """ノード完了イベント.

        Args:
            node: 完了したノード
            result: ノード実行結果
            success: 実行が成功したか（失敗時はカウントしない）

        Returns:
            AG-UI NodeCompleteEvent形式の辞書

        Note:
            success=True の場合のみ進捗カウンターを増加。
            失敗したノードは on_node_error で処理され、
            このメソッドは success=True でのみ呼ばれるべき。
        """
        # 成功時のみカウント（失敗ノードは進捗に含めない）
        if success:
            self._completed += 1
        self._current_node = None
        return {
            "event_type": "node.complete",
            "type": "node_complete",  # 後方互換
            "timestamp": time.time(),
            "flow_id": self._flow_id,
            "node_id": node.id,
            "node_name": node.name,
            "data": {
                "progress": self.progress_percent,
                "success": success,
            },
        }

    def on_node_error(
        self, node: FlowNode, error: str, error_type: str = "AgentError"
    ) -> dict[str, Any]:
        """ノードエラーイベント.

        Args:
            node: エラーが発生したノード
            error: エラーメッセージ
            error_type: エラータイプ名

        Returns:
            AG-UI NodeErrorEvent形式の辞書
        """
        return {
            "event_type": "node.error",
            "type": "node_error",  # 後方互換
            "timestamp": time.time(),
            "flow_id": self._flow_id,
            "node_id": node.id,
            "node_name": node.name,
            "error_message": error,
            "error_type": error_type,
            "message": error,  # フロントエンドの fallback 用
            "data": {
                "progress": self.progress_percent,
            },
        }

    def on_flow_start(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """フロー開始イベント."""
        return {
            "event_type": "flow.start",
            "type": "flow_start",  # 後方互換
            "timestamp": time.time(),
            "flow_id": self._flow_id,
            "data": {
                "total_nodes": self._graph.node_count,
                "inputs_preview": str(inputs)[:100],
            },
        }

    def on_flow_complete(self, result: dict[str, Any]) -> dict[str, Any]:
        """フロー完了イベント."""
        return {
            "event_type": "flow.complete",
            "type": "flow_complete",  # 後方互換
            "timestamp": time.time(),
            "flow_id": self._flow_id,
            "data": {
                "progress": 100,
            },
            "result": result,
        }

    def on_flow_error(self, error: Exception) -> dict[str, Any]:
        """フローエラーイベント."""
        return {
            "event_type": "flow.error",
            "type": "flow_error",  # 後方互換
            "timestamp": time.time(),
            "flow_id": self._flow_id,
            "error_message": str(error),
            "error_type": type(error).__name__,
            "message": str(error),  # フロントエンドの fallback 用
            "data": {
                "node_id": self._current_node,
                "progress": self.progress_percent,
            },
        }

    def reset(self) -> None:
        """進捗をリセット（REVISEロールバック時用）."""
        self._completed = 0
        self._current_node = None


__all__ = ["ProgressTracker"]

