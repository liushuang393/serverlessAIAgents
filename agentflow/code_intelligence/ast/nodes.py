"""AST ノード型定義.

特定の用途向けのノードファクトリ関数を提供します。
"""

from __future__ import annotations

from typing import Any

from agentflow.code_intelligence.ast.unified_ast import (
    ASTNode,
    ASTNodeType,
    TypeInfo,
)


def ProgramNode(
    name: str,
    *,
    language: str = "",
    **kwargs: Any,
) -> ASTNode:
    """プログラムノードを作成.

    Args:
        name: プログラム名
        language: 言語

    Returns:
        プログラムノード
    """
    return ASTNode(
        type=ASTNodeType.PROGRAM,
        name=name,
        attributes={"language": language, **kwargs},
    )


def FunctionNode(
    name: str,
    *,
    return_type: TypeInfo | None = None,
    parameters: list[dict[str, Any]] | None = None,
    is_async: bool = False,
    visibility: str = "public",
    **kwargs: Any,
) -> ASTNode:
    """関数ノードを作成.

    Args:
        name: 関数名
        return_type: 戻り値の型
        parameters: パラメータリスト
        is_async: 非同期関数かどうか
        visibility: 可視性

    Returns:
        関数ノード
    """
    return ASTNode(
        type=ASTNodeType.FUNCTION,
        name=name,
        attributes={
            "return_type": return_type.to_dict() if return_type else None,
            "parameters": parameters or [],
            "is_async": is_async,
            "visibility": visibility,
            **kwargs,
        },
    )


def MethodNode(
    name: str,
    *,
    return_type: TypeInfo | None = None,
    parameters: list[dict[str, Any]] | None = None,
    is_async: bool = False,
    is_static: bool = False,
    visibility: str = "public",
    **kwargs: Any,
) -> ASTNode:
    """メソッドノードを作成.

    Args:
        name: メソッド名
        return_type: 戻り値の型
        parameters: パラメータリスト
        is_async: 非同期メソッドかどうか
        is_static: 静的メソッドかどうか
        visibility: 可視性

    Returns:
        メソッドノード
    """
    return ASTNode(
        type=ASTNodeType.METHOD,
        name=name,
        attributes={
            "return_type": return_type.to_dict() if return_type else None,
            "parameters": parameters or [],
            "is_async": is_async,
            "is_static": is_static,
            "visibility": visibility,
            **kwargs,
        },
    )


def ClassNode(
    name: str,
    *,
    base_classes: list[str] | None = None,
    interfaces: list[str] | None = None,
    is_abstract: bool = False,
    visibility: str = "public",
    **kwargs: Any,
) -> ASTNode:
    """クラスノードを作成.

    Args:
        name: クラス名
        base_classes: 基底クラスリスト
        interfaces: 実装インターフェースリスト
        is_abstract: 抽象クラスかどうか
        visibility: 可視性

    Returns:
        クラスノード
    """
    return ASTNode(
        type=ASTNodeType.CLASS,
        name=name,
        attributes={
            "base_classes": base_classes or [],
            "interfaces": interfaces or [],
            "is_abstract": is_abstract,
            "visibility": visibility,
            **kwargs,
        },
    )


def VariableNode(
    name: str,
    *,
    var_type: TypeInfo | None = None,
    is_mutable: bool = True,
    initial_value: str | None = None,
    visibility: str = "private",
    **kwargs: Any,
) -> ASTNode:
    """変数ノードを作成.

    Args:
        name: 変数名
        var_type: 変数の型
        is_mutable: 可変かどうか
        initial_value: 初期値
        visibility: 可視性

    Returns:
        変数ノード
    """
    return ASTNode(
        type=ASTNodeType.VARIABLE,
        name=name,
        attributes={
            "var_type": var_type.to_dict() if var_type else None,
            "is_mutable": is_mutable,
            "initial_value": initial_value,
            "visibility": visibility,
            **kwargs,
        },
    )


def StatementNode(
    statement_type: str,
    *,
    content: str = "",
    **kwargs: Any,
) -> ASTNode:
    """文ノードを作成.

    Args:
        statement_type: 文タイプ
        content: 文内容

    Returns:
        文ノード
    """
    return ASTNode(
        type=ASTNodeType.STATEMENT,
        name=statement_type,
        attributes={
            "content": content,
            **kwargs,
        },
    )


def ExpressionNode(
    expression_type: str,
    *,
    value: str = "",
    operator: str | None = None,
    **kwargs: Any,
) -> ASTNode:
    """式ノードを作成.

    Args:
        expression_type: 式タイプ
        value: 値
        operator: 演算子

    Returns:
        式ノード
    """
    return ASTNode(
        type=ASTNodeType.EXPRESSION,
        name=expression_type,
        attributes={
            "value": value,
            "operator": operator,
            **kwargs,
        },
    )


def IfNode(
    condition: ASTNode,
    then_block: ASTNode,
    else_block: ASTNode | None = None,
    **kwargs: Any,
) -> ASTNode:
    """IF ノードを作成.

    Args:
        condition: 条件式
        then_block: Then ブロック
        else_block: Else ブロック

    Returns:
        IF ノード
    """
    node = ASTNode(
        type=ASTNodeType.IF,
        name="if",
        attributes=kwargs,
    )
    node.add_child(condition)
    node.add_child(then_block)
    if else_block:
        node.add_child(else_block)
    return node


def ForNode(
    init: ASTNode | None,
    condition: ASTNode | None,
    update: ASTNode | None,
    body: ASTNode,
    **kwargs: Any,
) -> ASTNode:
    """FOR ノードを作成.

    Args:
        init: 初期化式
        condition: 条件式
        update: 更新式
        body: ループ本体

    Returns:
        FOR ノード
    """
    node = ASTNode(
        type=ASTNodeType.FOR,
        name="for",
        attributes=kwargs,
    )
    if init:
        node.add_child(init)
    if condition:
        node.add_child(condition)
    if update:
        node.add_child(update)
    node.add_child(body)
    return node


def BlockNode(statements: list[ASTNode] | None = None, **kwargs: Any) -> ASTNode:
    """ブロックノードを作成.

    Args:
        statements: 文リスト

    Returns:
        ブロックノード
    """
    node = ASTNode(
        type=ASTNodeType.BLOCK,
        name="block",
        attributes=kwargs,
    )
    for stmt in statements or []:
        node.add_child(stmt)
    return node


__all__ = [
    "BlockNode",
    "ClassNode",
    "ExpressionNode",
    "ForNode",
    "FunctionNode",
    "IfNode",
    "MethodNode",
    "ProgramNode",
    "StatementNode",
    "VariableNode",
]
