"""CodeParser - 統一パーサーインターフェース.

多言語解析のための統一インターフェースを定義します。

使用例:
    >>> class PythonParser(CodeParser):
    ...     def parse(self, source_code: str, context: ParseContext) -> ParseResult:
    ...         # Python 固有の解析
    ...         pass
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from pathlib import Path

    from agentflow.code_intelligence.ast.unified_ast import UnifiedAST


class ParseError(Exception):
    """解析エラー.

    Attributes:
        message: エラーメッセージ
        line: エラー発生行
        column: エラー発生列
        source_snippet: エラー周辺のソースコード
    """

    def __init__(
        self,
        message: str,
        line: int = 0,
        column: int = 0,
        source_snippet: str = "",
    ) -> None:
        """初期化.

        Args:
            message: エラーメッセージ
            line: エラー発生行
            column: エラー発生列
            source_snippet: エラー周辺のソースコード
        """
        self.message = message
        self.line = line
        self.column = column
        self.source_snippet = source_snippet
        super().__init__(f"{message} at line {line}, column {column}")


@dataclass
class ParseContext:
    """解析コンテキスト.

    解析時のオプションと状態を保持します。

    Attributes:
        file_path: ファイルパス
        encoding: エンコーディング
        include_comments: コメントを含めるか
        include_whitespace: 空白を含めるか
        strict_mode: 厳格モード
        language_version: 言語バージョン
        options: 追加オプション
    """

    file_path: str = ""
    encoding: str = "utf-8"
    include_comments: bool = True
    include_whitespace: bool = False
    strict_mode: bool = False
    language_version: str = ""
    options: dict[str, Any] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class ParseResult:
    """解析結果.

    Attributes:
        success: 成功したか
        ast: 統一AST
        errors: エラーリスト
        warnings: 警告リスト
        metadata: メタデータ
    """

    success: bool
    ast: UnifiedAST | None = None
    errors: list[ParseError] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "success": self.success,
            "ast": self.ast.to_dict() if self.ast else None,
            "errors": [
                {
                    "message": e.message,
                    "line": e.line,
                    "column": e.column,
                    "source_snippet": e.source_snippet,
                }
                for e in self.errors
            ],
            "warnings": self.warnings,
            "metadata": self.metadata,
        }


class CodeParser(ABC):
    """コードパーサー基底クラス.

    統一されたパーサーインターフェースを定義します。
    各言語のパーサーはこのクラスを継承して実装します。

    Example:
        >>> parser = get_parser("python")
        >>> result = parser.parse("def foo(): pass", ParseContext())
        >>> if result.success:
        ...     print(result.ast)
    """

    @property
    @abstractmethod
    def language(self) -> str:
        """対応言語名.

        Returns:
            言語名（例: "python", "java", "cobol"）
        """

    @property
    def file_extensions(self) -> list[str]:
        """対応ファイル拡張子.

        Returns:
            拡張子リスト（例: [".py", ".pyw"]）
        """
        return []

    @property
    def language_version(self) -> str:
        """言語バージョン.

        Returns:
            バージョン文字列（例: "3.11"）
        """
        return ""

    @abstractmethod
    def parse(self, source_code: str, context: ParseContext) -> ParseResult:
        """ソースコードを解析.

        Args:
            source_code: ソースコード文字列
            context: 解析コンテキスト

        Returns:
            解析結果
        """

    def parse_file(self, file_path: Path, context: ParseContext | None = None) -> ParseResult:
        """ファイルを解析.

        Args:
            file_path: ファイルパス
            context: 解析コンテキスト（省略時はデフォルト）

        Returns:
            解析結果
        """
        if context is None:
            context = ParseContext()

        context.file_path = str(file_path)

        try:
            encoding = context.encoding or "utf-8"
            source_code = file_path.read_text(encoding=encoding)
            return self.parse(source_code, context)

        except FileNotFoundError:
            return ParseResult(
                success=False,
                errors=[ParseError(f"File not found: {file_path}")],
            )
        except UnicodeDecodeError as e:
            return ParseResult(
                success=False,
                errors=[ParseError(f"Encoding error: {e}")],
            )

    def validate_syntax(self, source_code: str) -> tuple[bool, list[str]]:
        """構文を検証.

        Args:
            source_code: ソースコード

        Returns:
            (有効かどうか, エラーメッセージリスト)
        """
        result = self.parse(source_code, ParseContext(strict_mode=True))
        error_messages = [e.message for e in result.errors]
        return result.success, error_messages

    def can_parse(self, file_path: Path) -> bool:
        """ファイルを解析できるか判定.

        Args:
            file_path: ファイルパス

        Returns:
            解析可能な場合 True
        """
        suffix = file_path.suffix.lower()
        return suffix in self.file_extensions


class TreeSitterParser(CodeParser):
    """Tree-sitter ベースのパーサー基底クラス.

    Tree-sitter を使用した高速・正確な解析を提供します。

    サブクラスで言語固有の tree-sitter ライブラリを指定します。
    """

    def __init__(self) -> None:
        """初期化."""
        self._parser = None
        self._language = None

    def _ensure_parser(self) -> None:
        """パーサーを初期化（遅延初期化）."""
        if self._parser is None:
            self._init_tree_sitter()

    def _init_tree_sitter(self) -> None:
        """Tree-sitter を初期化.

        サブクラスでオーバーライドして言語固有の初期化を行います。
        """
        # Tree-sitter の初期化コード
        # 実際の実装では tree_sitter ライブラリを使用


__all__ = [
    "CodeParser",
    "ParseContext",
    "ParseError",
    "ParseResult",
    "TreeSitterParser",
]
