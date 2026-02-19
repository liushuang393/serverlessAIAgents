"""CodeTransformer - 統一変換インターフェース.

AST 間の変換を行う統一インターフェースを定義します。

使用例:
    >>> transformer = get_transformer("cobol", "java")
    >>> result = transformer.transform(cobol_ast, context)
    >>> print(result.target_code)
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.code_intelligence.ast.unified_ast import UnifiedAST


class TransformError(Exception):
    """変換エラー.

    Attributes:
        message: エラーメッセージ
        source_node: エラー発生ノード
        phase: エラー発生フェーズ
    """

    def __init__(
        self,
        message: str,
        source_node: str = "",
        phase: str = "",
    ) -> None:
        self.message = message
        self.source_node = source_node
        self.phase = phase
        super().__init__(f"{phase}: {message}" if phase else message)


@dataclass
class TransformContext:
    """変換コンテキスト.

    Attributes:
        source_language: ソース言語
        target_language: ターゲット言語
        target_version: ターゲット言語バージョン
        naming_convention: 命名規則
        preserve_comments: コメントを保持するか
        generate_docs: ドキュメントを生成するか
        options: 追加オプション
    """

    source_language: str = ""
    target_language: str = ""
    target_version: str = ""
    naming_convention: str = "camelCase"
    preserve_comments: bool = True
    generate_docs: bool = True
    indent_style: str = "space"
    indent_size: int = 4
    options: dict[str, Any] = field(default_factory=dict)


@dataclass
class TransformResult:
    """変換結果.

    Attributes:
        success: 成功したか
        target_ast: ターゲットAST
        target_code: 生成されたコード
        errors: エラーリスト
        warnings: 警告リスト
        mappings: ソース-ターゲット マッピング
        metadata: メタデータ
    """

    success: bool
    target_ast: UnifiedAST | None = None
    target_code: str = ""
    errors: list[TransformError] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    mappings: dict[str, str] = field(default_factory=dict)
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "success": self.success,
            "target_ast": self.target_ast.to_dict() if self.target_ast else None,
            "target_code": self.target_code,
            "errors": [
                {"message": e.message, "source_node": e.source_node, "phase": e.phase}
                for e in self.errors
            ],
            "warnings": self.warnings,
            "mappings": self.mappings,
            "metadata": self.metadata,
        }


class CodeTransformer(ABC):
    """コード変換基底クラス.

    AST から AST への変換、およびコード生成を行います。

    Example:
        >>> transformer = get_transformer("cobol", "java")
        >>> result = transformer.transform(cobol_ast, context)
    """

    @property
    @abstractmethod
    def source_language(self) -> str:
        """ソース言語."""

    @property
    @abstractmethod
    def target_language(self) -> str:
        """ターゲット言語."""

    @abstractmethod
    def transform(
        self,
        source_ast: UnifiedAST,
        context: TransformContext,
    ) -> TransformResult:
        """AST を変換.

        Args:
            source_ast: ソースAST
            context: 変換コンテキスト

        Returns:
            変換結果
        """

    def generate_code(
        self,
        ast: UnifiedAST,
        context: TransformContext,
    ) -> str:
        """AST からコードを生成.

        Args:
            ast: 対象AST
            context: 変換コンテキスト

        Returns:
            生成されたコード
        """
        # デフォルト実装（サブクラスでオーバーライド）
        return ""

    def get_type_mapping(self, source_type: str) -> str:
        """型マッピングを取得.

        Args:
            source_type: ソース型

        Returns:
            ターゲット型
        """
        # デフォルトマッピング
        return source_type


class LLMAssistedTransformer(CodeTransformer):
    """LLM 支援変換.

    複雑な変換を LLM の支援で行います。
    """

    def __init__(self, llm_client: Any = None) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント
        """
        from agentflow.providers import get_llm

        self._llm = llm_client or get_llm()

    async def transform_with_llm(
        self,
        source_code: str,
        context: TransformContext,
    ) -> str:
        """LLM でコード変換.

        Args:
            source_code: ソースコード
            context: 変換コンテキスト

        Returns:
            変換後のコード
        """
        prompt = f"""Convert the following {context.source_language} code to {context.target_language}.

Source code:
```{context.source_language}
{source_code}
```

Requirements:
- Preserve the original logic
- Use {context.naming_convention} naming convention
- Add appropriate comments
- Follow {context.target_language} best practices

Output only the converted code without explanations."""

        response = await self._llm.generate(prompt)
        return response.content if hasattr(response, "content") else str(response)


__all__ = [
    "CodeTransformer",
    "LLMAssistedTransformer",
    "TransformContext",
    "TransformError",
    "TransformResult",
]
