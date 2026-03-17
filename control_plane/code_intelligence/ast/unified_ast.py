"""UnifiedAST - 統一抽象構文木.

言語非依存の抽象構文木モデルを提供します。

設計原則:
- 言語非依存: どの言語からも変換可能
- 拡張可能: 新しいノードタイプを追加可能
- シリアライズ可能: JSON/YAML に変換可能

使用例:
    >>> ast = UnifiedAST(source_language="cobol")
    >>> ast.root = ProgramNode(name="MAIN-PROGRAM")
    >>> ast.add_symbol("WS-COUNTER", SymbolInfo(...))
"""

from __future__ import annotations

import uuid
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from collections.abc import Iterator


class ASTNodeType(str, Enum):
    """ASTノードタイプ."""

    # プログラム構造
    PROGRAM = "program"
    MODULE = "module"
    PACKAGE = "package"

    # 宣言
    CLASS = "class"
    INTERFACE = "interface"
    FUNCTION = "function"
    METHOD = "method"
    CONSTRUCTOR = "constructor"
    VARIABLE = "variable"
    CONSTANT = "constant"
    PARAMETER = "parameter"

    # 文
    STATEMENT = "statement"
    BLOCK = "block"
    IF = "if"
    SWITCH = "switch"
    CASE = "case"
    FOR = "for"
    WHILE = "while"
    DO_WHILE = "do_while"
    RETURN = "return"
    BREAK = "break"
    CONTINUE = "continue"
    TRY = "try"
    CATCH = "catch"
    FINALLY = "finally"
    THROW = "throw"

    # 式
    EXPRESSION = "expression"
    LITERAL = "literal"
    IDENTIFIER = "identifier"
    BINARY_OP = "binary_op"
    UNARY_OP = "unary_op"
    CALL = "call"
    INDEX = "index"
    MEMBER_ACCESS = "member_access"
    ASSIGNMENT = "assignment"

    # 型
    TYPE = "type"
    TYPE_PARAMETER = "type_parameter"

    # その他
    COMMENT = "comment"
    ANNOTATION = "annotation"
    IMPORT = "import"
    EXPORT = "export"

    # COBOL 固有
    DIVISION = "division"
    SECTION = "section"
    PARAGRAPH = "paragraph"
    PERFORM = "perform"
    MOVE = "move"
    COMPUTE = "compute"
    PIC = "pic"


@dataclass
class TypeInfo:
    """型情報.

    Attributes:
        name: 型名
        kind: 型種別（primitive, class, array, generic）
        is_nullable: Nullable かどうか
        type_params: ジェネリック型パラメータ
        element_type: 配列要素の型
    """

    name: str
    kind: str = "primitive"
    is_nullable: bool = False
    type_params: list[str] = field(default_factory=list)
    element_type: TypeInfo | None = None
    source_type: str = ""  # 元の言語の型

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "kind": self.kind,
            "is_nullable": self.is_nullable,
            "type_params": self.type_params,
            "element_type": self.element_type.to_dict() if self.element_type else None,
            "source_type": self.source_type,
        }


@dataclass
class SymbolInfo:
    """シンボル情報.

    Attributes:
        name: シンボル名
        kind: シンボル種別（variable, function, class, etc.）
        type_info: 型情報
        scope: スコープ（global, local, class, etc.）
        is_mutable: 可変かどうか
        is_exported: エクスポートされているか
        line: 定義行
        column: 定義列
    """

    name: str
    kind: str
    type_info: TypeInfo | None = None
    scope: str = "local"
    is_mutable: bool = True
    is_exported: bool = False
    line: int = 0
    column: int = 0
    location: str = ""
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "name": self.name,
            "kind": self.kind,
            "type_info": self.type_info.to_dict() if self.type_info else None,
            "scope": self.scope,
            "is_mutable": self.is_mutable,
            "is_exported": self.is_exported,
            "line": self.line,
            "column": self.column,
            "location": self.location,
            "metadata": self.metadata,
        }


@dataclass
class ImportInfo:
    """インポート情報.

    Attributes:
        module: モジュールパス
        names: インポートする名前（空リストで全体）
        alias: エイリアス
        is_default: デフォルトインポートか
    """

    module: str
    names: list[str] = field(default_factory=list)
    alias: str | None = None
    is_default: bool = False
    is_static: bool = False

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "module": self.module,
            "names": self.names,
            "alias": self.alias,
            "is_default": self.is_default,
            "is_static": self.is_static,
        }


@dataclass
class ASTNode:
    """ASTノード.

    統一ASTの基本ノード。

    Attributes:
        id: ノードID
        type: ノードタイプ
        name: ノード名
        children: 子ノード
        attributes: 属性
        line: 開始行
        column: 開始列
        end_line: 終了行
        end_column: 終了列
    """

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    type: ASTNodeType = ASTNodeType.STATEMENT
    name: str = ""
    children: list[ASTNode] = field(default_factory=list)
    attributes: dict[str, Any] = field(default_factory=dict)
    line: int = 0
    column: int = 0
    end_line: int = 0
    end_column: int = 0
    parent: ASTNode | None = field(default=None, repr=False)
    # 互換フィールド: 旧実装の引数名を受け入れる
    node_type: ASTNodeType | None = None
    metadata: dict[str, Any] = field(default_factory=dict)
    start_line: int = 0
    start_column: int = 0

    def __post_init__(self) -> None:
        """旧フィールドとの互換性を維持."""
        if self.node_type is not None:
            self.type = self.node_type
        if self.start_line and self.line == 0:
            self.line = self.start_line
        if self.start_column and self.column == 0:
            self.column = self.start_column
        if self.metadata:
            self.attributes = {**self.attributes, **self.metadata}

    def add_child(self, child: ASTNode) -> None:
        """子ノードを追加.

        Args:
            child: 追加する子ノード
        """
        child.parent = self
        self.children.append(child)

    def find_by_type(self, node_type: ASTNodeType) -> Iterator[ASTNode]:
        """指定タイプのノードを検索.

        Args:
            node_type: 検索するノードタイプ

        Yields:
            マッチしたノード
        """
        if self.type == node_type:
            yield self
        for child in self.children:
            yield from child.find_by_type(node_type)

    def find_by_name(self, name: str) -> Iterator[ASTNode]:
        """指定名前のノードを検索.

        Args:
            name: 検索する名前

        Yields:
            マッチしたノード
        """
        if self.name == name:
            yield self
        for child in self.children:
            yield from child.find_by_name(name)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "type": self.type.value,
            "name": self.name,
            "children": [c.to_dict() for c in self.children],
            "attributes": self.attributes,
            "line": self.line,
            "column": self.column,
            "end_line": self.end_line,
            "end_column": self.end_column,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> ASTNode:
        """辞書から作成."""
        node = cls(
            id=data.get("id", str(uuid.uuid4())),
            type=ASTNodeType(data["type"]),
            name=data.get("name", ""),
            attributes=data.get("attributes", {}),
            line=data.get("line", 0),
            column=data.get("column", 0),
            end_line=data.get("end_line", 0),
            end_column=data.get("end_column", 0),
        )
        for child_data in data.get("children", []):
            child = cls.from_dict(child_data)
            node.add_child(child)
        return node


@dataclass
class UnifiedAST:
    """統一抽象構文木.

    言語非依存の AST 表現。

    Attributes:
        id: AST ID
        source_language: ソース言語
        source_file: ソースファイルパス
        root: ルートノード
        symbols: シンボルテーブル
        imports: インポート情報
        metadata: メタデータ
    """

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    source_language: str = ""
    source_file: str = ""
    root: ASTNode | None = None
    symbols: dict[str, SymbolInfo] = field(default_factory=dict)
    imports: list[ImportInfo] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=lambda: datetime.now(UTC))

    def add_symbol(self, name: str, symbol: SymbolInfo) -> None:
        """シンボルを追加.

        Args:
            name: シンボル名
            symbol: シンボル情報
        """
        self.symbols[name] = symbol

    def get_symbol(self, name: str) -> SymbolInfo | None:
        """シンボルを取得.

        Args:
            name: シンボル名

        Returns:
            シンボル情報（存在しない場合は None）
        """
        return self.symbols.get(name)

    def add_import(self, import_info: ImportInfo) -> None:
        """インポートを追加.

        Args:
            import_info: インポート情報
        """
        self.imports.append(import_info)

    def find_nodes(self, node_type: ASTNodeType) -> list[ASTNode]:
        """指定タイプのノードを検索.

        Args:
            node_type: 検索するノードタイプ

        Returns:
            マッチしたノードのリスト
        """
        if not self.root:
            return []
        return list(self.root.find_by_type(node_type))

    def get_functions(self) -> list[ASTNode]:
        """関数ノードを取得.

        Returns:
            関数ノードのリスト
        """
        return self.find_nodes(ASTNodeType.FUNCTION) + self.find_nodes(ASTNodeType.METHOD)

    def get_classes(self) -> list[ASTNode]:
        """クラスノードを取得.

        Returns:
            クラスノードのリスト
        """
        return self.find_nodes(ASTNodeType.CLASS)

    def get_variables(self) -> list[ASTNode]:
        """変数ノードを取得.

        Returns:
            変数ノードのリスト
        """
        return self.find_nodes(ASTNodeType.VARIABLE)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "source_language": self.source_language,
            "source_file": self.source_file,
            "root": self.root.to_dict() if self.root else None,
            "symbols": {k: v.to_dict() for k, v in self.symbols.items()},
            "imports": [i.to_dict() for i in self.imports],
            "metadata": self.metadata,
            "created_at": self.created_at.isoformat(),
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> UnifiedAST:
        """辞書から作成."""
        ast = cls(
            id=data.get("id", str(uuid.uuid4())),
            source_language=data.get("source_language", ""),
            source_file=data.get("source_file", ""),
            metadata=data.get("metadata", {}),
        )

        if data.get("root"):
            ast.root = ASTNode.from_dict(data["root"])

        for name, symbol_data in data.get("symbols", {}).items():
            type_info = None
            if symbol_data.get("type_info"):
                type_info = TypeInfo(**symbol_data["type_info"])
            symbol = SymbolInfo(
                name=symbol_data["name"],
                kind=symbol_data["kind"],
                type_info=type_info,
                scope=symbol_data.get("scope", "local"),
                is_mutable=symbol_data.get("is_mutable", True),
                is_exported=symbol_data.get("is_exported", False),
                line=symbol_data.get("line", 0),
                column=symbol_data.get("column", 0),
                metadata=symbol_data.get("metadata", {}),
            )
            ast.symbols[name] = symbol

        for import_data in data.get("imports", []):
            ast.imports.append(ImportInfo(**import_data))

        return ast


__all__ = [
    "ASTNode",
    "ASTNodeType",
    "ImportInfo",
    "SymbolInfo",
    "TypeInfo",
    "UnifiedAST",
]
