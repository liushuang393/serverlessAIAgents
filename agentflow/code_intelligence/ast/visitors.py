"""AST Visitor パターン.

AST の走査と変換のための Visitor パターンを提供します。
"""

from __future__ import annotations

from abc import ABC
from typing import Any, TypeVar

from agentflow.code_intelligence.ast.unified_ast import ASTNode, ASTNodeType


T = TypeVar("T")


class ASTVisitor[T](ABC):
    """AST Visitor 基底クラス.

    AST を走査し、各ノードを訪問します。

    Example:
        >>> class PrintVisitor(ASTVisitor[None]):
        ...     def visit_function(self, node: ASTNode) -> None:
        ...         print(f"Function: {node.name}")
        ...
        >>> visitor = PrintVisitor()
        >>> visitor.visit(ast.root)
    """

    def visit(self, node: ASTNode) -> T:
        """ノードを訪問.

        Args:
            node: 訪問するノード

        Returns:
            訪問結果
        """
        # ノードタイプに応じたメソッドを呼び出し
        method_name = f"visit_{node.type.value}"
        visitor = getattr(self, method_name, self.generic_visit)
        return visitor(node)

    def generic_visit(self, node: ASTNode) -> T:
        """汎用訪問メソッド.

        特定のメソッドが定義されていない場合に呼び出されます。

        Args:
            node: 訪問するノード

        Returns:
            訪問結果
        """
        for child in node.children:
            self.visit(child)
        return None  # type: ignore

    # 各ノードタイプ用のメソッド（サブクラスでオーバーライド）

    def visit_program(self, node: ASTNode) -> T:
        """プログラムノードを訪問."""
        return self.generic_visit(node)

    def visit_module(self, node: ASTNode) -> T:
        """モジュールノードを訪問."""
        return self.generic_visit(node)

    def visit_class(self, node: ASTNode) -> T:
        """クラスノードを訪問."""
        return self.generic_visit(node)

    def visit_function(self, node: ASTNode) -> T:
        """関数ノードを訪問."""
        return self.generic_visit(node)

    def visit_method(self, node: ASTNode) -> T:
        """メソッドノードを訪問."""
        return self.generic_visit(node)

    def visit_variable(self, node: ASTNode) -> T:
        """変数ノードを訪問."""
        return self.generic_visit(node)

    def visit_statement(self, node: ASTNode) -> T:
        """文ノードを訪問."""
        return self.generic_visit(node)

    def visit_expression(self, node: ASTNode) -> T:
        """式ノードを訪問."""
        return self.generic_visit(node)

    def visit_block(self, node: ASTNode) -> T:
        """ブロックノードを訪問."""
        return self.generic_visit(node)

    def visit_if(self, node: ASTNode) -> T:
        """IF ノードを訪問."""
        return self.generic_visit(node)

    def visit_for(self, node: ASTNode) -> T:
        """FOR ノードを訪問."""
        return self.generic_visit(node)

    def visit_while(self, node: ASTNode) -> T:
        """WHILE ノードを訪問."""
        return self.generic_visit(node)


class ASTTransformVisitor(ASTVisitor[ASTNode | None]):
    """AST 変換 Visitor.

    AST を走査し、変換を適用します。

    Example:
        >>> class RenameVisitor(ASTTransformVisitor):
        ...     def visit_function(self, node: ASTNode) -> ASTNode:
        ...         node.name = f"transformed_{node.name}"
        ...         return node
    """

    def transform(self, node: ASTNode) -> ASTNode | None:
        """AST を変換.

        Args:
            node: 変換するルートノード

        Returns:
            変換後のノード
        """
        return self.visit(node)

    def generic_visit(self, node: ASTNode) -> ASTNode:
        """汎用訪問（子ノードも変換）.

        Args:
            node: 訪問するノード

        Returns:
            変換後のノード
        """
        new_children = []
        for child in node.children:
            new_child = self.visit(child)
            if new_child is not None:
                new_children.append(new_child)

        node.children = new_children
        return node


class NodeCollector(ASTVisitor[None]):
    """ノード収集 Visitor.

    指定タイプのノードを収集します。

    Example:
        >>> collector = NodeCollector([ASTNodeType.FUNCTION])
        >>> collector.visit(ast.root)
        >>> functions = collector.nodes
    """

    def __init__(self, node_types: list[ASTNodeType]) -> None:
        """初期化.

        Args:
            node_types: 収集するノードタイプ
        """
        self._node_types = set(node_types)
        self.nodes: list[ASTNode] = []

    def generic_visit(self, node: ASTNode) -> None:
        """汎用訪問."""
        if node.type in self._node_types:
            self.nodes.append(node)
        for child in node.children:
            self.visit(child)


class NodeCounter(ASTVisitor[int]):
    """ノードカウント Visitor.

    ノード数をカウントします。

    Example:
        >>> counter = NodeCounter()
        >>> count = counter.visit(ast.root)
    """

    def generic_visit(self, node: ASTNode) -> int:
        """汎用訪問."""
        count = 1  # 自身をカウント
        for child in node.children:
            count += self.visit(child)
        return count


class DepthCalculator(ASTVisitor[int]):
    """深さ計算 Visitor.

    AST の最大深さを計算します。
    """

    def generic_visit(self, node: ASTNode) -> int:
        """汎用訪問."""
        if not node.children:
            return 1

        max_child_depth = 0
        for child in node.children:
            child_depth = self.visit(child)
            max_child_depth = max(max_child_depth, child_depth)

        return 1 + max_child_depth


class ASTSerializer(ASTVisitor[dict[str, Any]]):
    """AST シリアライズ Visitor.

    AST を辞書にシリアライズします。
    """

    def generic_visit(self, node: ASTNode) -> dict[str, Any]:
        """汎用訪問."""
        return {
            "type": node.type.value,
            "name": node.name,
            "attributes": node.attributes,
            "children": [self.visit(child) for child in node.children],
            "location": {
                "line": node.line,
                "column": node.column,
                "end_line": node.end_line,
                "end_column": node.end_column,
            },
        }


__all__ = [
    "ASTSerializer",
    "ASTTransformVisitor",
    "ASTVisitor",
    "DepthCalculator",
    "NodeCollector",
    "NodeCounter",
]
