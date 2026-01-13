# -*- coding: utf-8 -*-
"""ICodeGenerator - コード生成器インターフェース.

Workflow から実行可能なコードを生成するインターフェース。

このインターフェースは安定しており、変更は慎重に行う必要があります。
"""

from __future__ import annotations

from io import BytesIO
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable

if TYPE_CHECKING:
    from agentflow.core.interfaces.types import (
        CodeGenOptions,
        CodeOutputType,
        FilePreview,
        GeneratedCode,
        WorkflowDefinition,
    )


@runtime_checkable
class ICodeGenerator(Protocol):
    """コード生成器インターフェース.

    Workflow 定義から実行可能なコード（前端/後端/全栈）を生成します。

    実装要件:
    - generate(): コードを生成してメモリに保持
    - preview(): 生成されるファイルを事前にプレビュー
    - export_zip(): ZIP ファイルとしてエクスポート

    使用例:
        >>> generator = CodeGenerator()
        >>> code = await generator.generate(
        ...     workflow=my_workflow,
        ...     output_type=CodeOutputType.FULLSTACK,
        ...     options=CodeGenOptions(app_name="my-app"),
        ... )
        >>> print(code.files.keys())
        ['app.py', 'requirements.txt', 'README.md', ...]
    """

    async def generate(
        self,
        workflow: "WorkflowDefinition",
        output_type: "CodeOutputType",
        options: "CodeGenOptions | None" = None,
    ) -> "GeneratedCode":
        """コードを生成.

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ（frontend/backend/fullstack）
            options: 生成オプション

        Returns:
            生成されたコード

        Raises:
            ValueError: 無効なワークフローまたはオプション
        """
        ...

    async def preview(
        self,
        workflow: "WorkflowDefinition",
        output_type: "CodeOutputType",
    ) -> dict[str, "FilePreview"]:
        """生成されるファイルをプレビュー.

        実際のコード生成を行わずに、生成されるファイルの
        一覧と内容のプレビューを返します。

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ

        Returns:
            ファイルパス -> FilePreview のマップ
        """
        ...

    async def export_zip(
        self,
        workflow: "WorkflowDefinition",
        output_type: "CodeOutputType",
        options: "CodeGenOptions | None" = None,
    ) -> BytesIO:
        """ZIP ファイルとしてエクスポート.

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ
            options: 生成オプション

        Returns:
            ZIP ファイルの BytesIO
        """
        ...

    def get_supported_frameworks(
        self,
        output_type: "CodeOutputType",
    ) -> list[str]:
        """サポートされているフレームワークを取得.

        Args:
            output_type: 出力タイプ

        Returns:
            フレームワーク名のリスト
        """
        ...


__all__ = ["ICodeGenerator"]
