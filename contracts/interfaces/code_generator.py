"""ICodeGenerator - コード生成器インターフェース.

contracts 層の正規配置。
Workflow から実行可能なコードを生成するインターフェース。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Protocol, runtime_checkable


if TYPE_CHECKING:
    from io import BytesIO

    from contracts.core import (
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
    """

    async def generate(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> GeneratedCode:
        """コードを生成."""
        ...

    async def preview(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
    ) -> dict[str, FilePreview]:
        """生成されるファイルをプレビュー."""
        ...

    async def export_zip(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> BytesIO:
        """ZIP ファイルとしてエクスポート."""
        ...

    def get_supported_frameworks(
        self,
        output_type: CodeOutputType,
    ) -> list[str]:
        """サポートされているフレームワークを取得."""
        ...


__all__ = ["ICodeGenerator"]
