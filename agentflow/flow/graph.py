"""Flowグラフ構造.

ノードの順序付きリストとノード間の関係を管理。

設計原則:
- シンプルな線形グラフ：ほとんどのシナリオは順次実行
- ジャンプサポート：指定ノードへのロールバック（REVISE）
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from agentflow.flow.nodes import FlowNode


class FlowGraph:
    """フローグラフ.

    ノードリストを管理し、ノード検索とトラバーサル機能を提供。

    Example:
        >>> graph = FlowGraph()
        >>> graph.add_node(agent_node)
        >>> graph.add_node(review_node)
        >>> for node in graph:
        ...     print(node.id)
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger("agentflow.flow.graph")
        self._nodes: list[FlowNode] = []
        self._node_map: dict[str, FlowNode] = {}

    def add_node(self, node: FlowNode) -> None:
        """ノードを追加.

        Args:
            node: 追加するノード

        Raises:
            ValueError: ノードIDが重複している場合
        """
        if node.id in self._node_map:
            msg = f"ノードIDが重複しています: {node.id}"
            raise ValueError(msg)
        self._nodes.append(node)
        self._node_map[node.id] = node
        self._logger.debug(f"ノードを追加: {node.id} ({node.node_type.name})")

    def get_node(self, node_id: str) -> FlowNode | None:
        """IDに基づいてノードを取得."""
        return self._node_map.get(node_id)

    def get_node_index(self, node_id: str) -> int:
        """ノードのリスト内インデックスを取得."""
        for i, node in enumerate(self._nodes):
            if node.id == node_id:
                return i
        return -1

    def get_next_node(self, current_id: str) -> FlowNode | None:
        """次のノードを取得.

        Args:
            current_id: 現在のノードID

        Returns:
            次のノード、現在が最後の場合はNoneを返す
        """
        idx = self.get_node_index(current_id)
        if idx < 0 or idx >= len(self._nodes) - 1:
            return None
        return self._nodes[idx + 1]

    def get_nodes_from(self, node_id: str) -> list[FlowNode]:
        """指定ノードから開始するすべての後続ノードを取得.

        REVISEロールバックシナリオで使用。

        Args:
            node_id: 開始ノードID

        Returns:
            そのノードから開始するノードリスト（そのノードを含む）
        """
        idx = self.get_node_index(node_id)
        if idx < 0:
            return []
        return self._nodes[idx:]

    @property
    def nodes(self) -> list[FlowNode]:
        """すべてのノードリスト."""
        return self._nodes.copy()

    @property
    def node_count(self) -> int:
        """ノード数."""
        return len(self._nodes)

    def __iter__(self):
        """すべてのノードをイテレート."""
        return iter(self._nodes)

    def __len__(self) -> int:
        """ノード数."""
        return len(self._nodes)

    def __repr__(self) -> str:
        node_ids = [n.id for n in self._nodes]
        return f"FlowGraph({node_ids})"


__all__ = ["FlowGraph"]

