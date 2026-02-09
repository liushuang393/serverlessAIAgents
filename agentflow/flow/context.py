"""Flow実行コンテキスト.

フロー実行中のすべての状態を管理：入力、結果、メモリ、リビジョンカウント等。

設計原則:
- 単一データソース：すべての状態を一元管理
- シリアライズ可能：デバッグと永続化に便利
- 明確なライフサイクル：リセットとクリーンアップをサポート
"""

from __future__ import annotations

import copy
import logging
import uuid
from typing import Any


class FlowContext:
    """フロー実行コンテキスト.

    Example:
        >>> ctx = FlowContext("my-flow")
        >>> ctx.set_inputs({"question": "..."})
        >>> ctx.set_result("agent1", {"answer": "..."})
        >>> ctx.get_result("agent1")
        {'answer': '...'}
    """

    def __init__(self, flow_id: str | None = None) -> None:
        """初期化.

        Args:
            flow_id: フローID、提供されない場合は自動生成
        """
        self._logger = logging.getLogger("agentflow.flow.context")
        self.flow_id = flow_id or f"flow-{uuid.uuid4().hex[:8]}"

        # データストレージ
        self._inputs: dict[str, Any] = {}
        self._results: dict[str, dict[str, Any]] = {}
        self._memory: dict[str, Any] = {}

        # 状態
        self._revision_count: int = 0
        self._current_node: str | None = None
        self._completed_nodes: list[str] = []

    # ========================================
    # 入力管理
    # ========================================

    def set_inputs(self, inputs: dict[str, Any]) -> None:
        """入力を設定."""
        self._inputs = copy.deepcopy(inputs)

    def get_inputs(self) -> dict[str, Any]:
        """入力のコピーを取得."""
        return copy.deepcopy(self._inputs)

    @property
    def inputs(self) -> dict[str, Any]:
        """元の入力（読み取り専用）."""
        return self._inputs

    # ========================================
    # 結果管理
    # ========================================

    def set_result(self, node_id: str, result: dict[str, Any]) -> None:
        """ノード結果を保存."""
        self._results[node_id] = copy.deepcopy(result)
        if node_id not in self._completed_nodes:
            self._completed_nodes.append(node_id)

    def get_result(self, node_id: str, default: dict[str, Any] | None = None) -> dict[str, Any]:
        """ノード結果を取得."""
        result = self._results.get(node_id)
        return copy.deepcopy(result) if result else (default or {})

    def has_result(self, node_id: str) -> bool:
        """結果があるかチェック."""
        return node_id in self._results

    def get_all_results(self) -> dict[str, dict[str, Any]]:
        """すべての結果を取得."""
        return copy.deepcopy(self._results)

    # ========================================
    # メモリシステム
    # ========================================

    def set(self, key: str, value: Any) -> None:
        """カスタムデータを保存."""
        self._memory[key] = value

    def get(self, key: str, default: Any = None) -> Any:
        """カスタムデータを取得."""
        return self._memory.get(key, default)

    def remove(self, key: str) -> None:
        """カスタムデータを削除."""
        self._memory.pop(key, None)

    # ========================================
    # リビジョン管理
    # ========================================

    @property
    def revision_count(self) -> int:
        """現在のリビジョン回数."""
        return self._revision_count

    def increment_revision(self) -> int:
        """リビジョンカウントを増加."""
        self._revision_count += 1
        return self._revision_count

    def clear_results_from(self, node_id: str) -> None:
        """指定ノードから開始する結果をクリア（REVISEロールバック用）."""
        if node_id not in self._completed_nodes:
            return
        idx = self._completed_nodes.index(node_id)
        for nid in self._completed_nodes[idx:]:
            self._results.pop(nid, None)
        self._completed_nodes = self._completed_nodes[:idx]
        self._logger.info(f"{node_id}から開始する結果をクリアしました")

    # ========================================
    # 状態
    # ========================================

    @property
    def current_node(self) -> str | None:
        """現在のノード."""
        return self._current_node

    def set_current_node(self, node_id: str | None) -> None:
        """現在のノードを設定."""
        self._current_node = node_id

    @property
    def completed_nodes(self) -> list[str]:
        """完了したノードリスト."""
        return self._completed_nodes.copy()

    def clear(self) -> None:
        """すべての状態をクリア."""
        self._inputs.clear()
        self._results.clear()
        self._memory.clear()
        self._revision_count = 0
        self._current_node = None
        self._completed_nodes.clear()

    def to_dict(self) -> dict[str, Any]:
        """シリアライズ（デバッグ用）."""
        return {
            "flow_id": self.flow_id,
            "inputs": self._inputs,
            "results": self._results,
            "memory": self._memory,
            "revision_count": self._revision_count,
            "completed_nodes": self._completed_nodes,
        }

    def __repr__(self) -> str:
        return f"FlowContext({self.flow_id!r}, completed={len(self._completed_nodes)})"


__all__ = ["FlowContext"]

