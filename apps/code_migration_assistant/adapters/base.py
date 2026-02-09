"""Language Adapter Base Classes.

言語アダプターの抽象基底クラス。ソース言語/ターゲット言語の共通インターフェースを定義する。
"""

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import Any


@dataclass
class ExecutionResult:
    """コード実行結果."""

    success: bool
    outputs: dict[str, Any] = field(default_factory=dict)
    stdout: str = ""
    stderr: str = ""
    return_code: int = 0
    execution_time_ms: float = 0.0
    error: str | None = None


@dataclass
class AST:
    """抽象構文木."""

    program_id: str
    divisions: dict[str, Any] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)
    variables: list[dict[str, Any]] = field(default_factory=list)
    procedures: list[dict[str, Any]] = field(default_factory=list)


class SourceLanguageAdapter(ABC):
    """ソース言語アダプターのインターフェース.

    ソース言語の解析と実行方法を定義する。
    """

    @property
    @abstractmethod
    def language_name(self) -> str:
        """言語名称."""

    @abstractmethod
    def parse(self, source_code: str) -> AST:
        """ソースコードをASTへ解析する.

        Args:
            source_code: ソースコード

        Returns:
            抽象構文木
        """

    @abstractmethod
    def extract_variables(self, ast: AST) -> list[dict[str, Any]]:
        """変数定義を抽出する.

        Args:
            ast: 抽象構文木

        Returns:
            変数リスト
        """

    @abstractmethod
    def identify_external_calls(self, ast: AST) -> list[dict[str, Any]]:
        """外部呼び出しを識別する（ファイル、DB、APIなど）.

        Args:
            ast: 抽象構文木

        Returns:
            外部呼び出しリスト
        """

    def execute(self, source_code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """ソースコードを実行する（任意、差分テスト用）.

        Args:
            source_code: ソースコード
            inputs: 入力パラメータ

        Returns:
            実行結果
        """
        return ExecutionResult(
            success=False,
            error="Execution not supported for this language",
        )


class TargetLanguageAdapter(ABC):
    """ターゲット言語アダプターのインターフェース.

    ターゲット言語の生成と実行方法を定義する。
    """

    @property
    @abstractmethod
    def language_name(self) -> str:
        """言語名称."""

    @abstractmethod
    def generate_skeleton(self, ast: AST, class_name: str) -> str:
        """コードスケルトンを生成する（クラス定義・フィールド宣言）.

        Args:
            ast: ソースコードのAST
            class_name: クラス名

        Returns:
            コードスケルトン
        """

    @abstractmethod
    def generate_test_skeleton(self, class_name: str, test_cases: list[dict]) -> str:
        """テストコードのスケルトンを生成する.

        Args:
            class_name: 対象クラス名
            test_cases: テストケース

        Returns:
            テストコードのスケルトン
        """

    @abstractmethod
    def compile(self, code: str) -> tuple[bool, list[str]]:
        """コードをコンパイルする.

        Args:
            code: ソースコード

        Returns:
            （成功, エラーリスト）
        """

    @abstractmethod
    def execute(self, code: str, inputs: dict[str, Any]) -> ExecutionResult:
        """コードを実行する.

        Args:
            code: ソースコード
            inputs: 入力パラメータ

        Returns:
            実行結果
        """

    def get_type_mapping(self, source_type: str, pic_clause: str = "") -> str:
        """型マッピングを取得する.

        Args:
            source_type: ソース型
            pic_clause: PIC句（COBOL固有）

        Returns:
            ターゲット言語の型
        """
        # デフォルト実装（子クラスで上書き可能）
        mapping = {
            "numeric": "int",
            "decimal": "BigDecimal",
            "string": "String",
        }
        return mapping.get(source_type, "Object")
