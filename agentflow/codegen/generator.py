"""CodeGenerator - コード生成器実装.

ICodeGenerator インターフェースを実装し、Workflow から
実行可能なコードを生成します。
"""

from __future__ import annotations

import logging
import zipfile
from io import BytesIO

from agentflow.codegen.builders.backend import BackendBuilder
from agentflow.codegen.builders.frontend import FrontendBuilder
from agentflow.codegen.builders.fullstack import FullstackBuilder
from agentflow.core.interfaces import (
    CodeGenOptions,
    CodeOutputType,
    FilePreview,
    GeneratedCode,
    ICodeGenerator,
    WorkflowDefinition,
)


logger = logging.getLogger(__name__)


class CodeGenerator(ICodeGenerator):
    """コード生成器.

    Workflow 定義から実行可能なコードを生成します。

    サポートする出力タイプ:
    - FRONTEND: React アプリケーション
    - BACKEND: FastAPI アプリケーション
    - FULLSTACK: React + FastAPI の完全アプリ
    """

    def __init__(self) -> None:
        """初期化."""
        self._builders = {
            CodeOutputType.FRONTEND: FrontendBuilder(),
            CodeOutputType.BACKEND: BackendBuilder(),
            CodeOutputType.FULLSTACK: FullstackBuilder(),
        }

    async def generate(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> GeneratedCode:
        """コードを生成.

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ
            options: 生成オプション

        Returns:
            生成されたコード
        """
        if options is None:
            options = CodeGenOptions(app_name=workflow.name.lower().replace(" ", "-") or "agentflow-app")
        elif not options.app_name:
            options.app_name = workflow.name.lower().replace(" ", "-") or "agentflow-app"

        builder = self._builders.get(output_type)
        if builder is None:
            msg = f"Unsupported output type: {output_type}"
            raise ValueError(msg)

        logger.info(f"Generating {output_type.value} code for workflow: {workflow.name}")

        files = await builder.build(workflow, options)

        # エントリーポイントとコマンドを決定
        entry_point, build_cmd, start_cmd = self._get_commands(output_type, options)

        return GeneratedCode(
            files=files,
            entry_point=entry_point,
            build_command=build_cmd,
            start_command=start_cmd,
            output_type=output_type,
        )

    async def preview(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
    ) -> dict[str, FilePreview]:
        """生成されるファイルをプレビュー.

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ

        Returns:
            ファイルプレビューのマップ
        """
        # 実際に生成してプレビュー形式に変換
        code = await self.generate(workflow, output_type)

        previews: dict[str, FilePreview] = {}
        for path, content in code.files.items():
            lines = content.split("\n")
            preview_lines = 20
            content_preview = "\n".join(lines[:preview_lines])
            if len(lines) > preview_lines:
                content_preview += f"\n... ({len(lines) - preview_lines} more lines)"

            previews[path] = FilePreview(
                path=path,
                content_preview=content_preview,
                size=len(content.encode("utf-8")),
                lines=len(lines),
            )

        return previews

    async def export_zip(
        self,
        workflow: WorkflowDefinition,
        output_type: CodeOutputType,
        options: CodeGenOptions | None = None,
    ) -> BytesIO:
        """ZIP ファイルとしてエクスポート.

        Args:
            workflow: ワークフロー定義
            output_type: 出力タイプ
            options: 生成オプション

        Returns:
            ZIP ファイルの BytesIO
        """
        code = await self.generate(workflow, output_type, options)

        zip_buffer = BytesIO()
        with zipfile.ZipFile(zip_buffer, "w", zipfile.ZIP_DEFLATED) as zf:
            for path, content in code.files.items():
                zf.writestr(path, content)

        zip_buffer.seek(0)
        return zip_buffer

    def get_supported_frameworks(
        self,
        output_type: CodeOutputType,
    ) -> list[str]:
        """サポートされているフレームワークを取得.

        Args:
            output_type: 出力タイプ

        Returns:
            フレームワーク名のリスト
        """
        frameworks = {
            CodeOutputType.FRONTEND: ["react", "vue", "nextjs"],
            CodeOutputType.BACKEND: ["fastapi", "flask", "express"],
            CodeOutputType.FULLSTACK: ["react-fastapi", "nextjs-fastapi"],
        }
        return frameworks.get(output_type, [])

    def _get_commands(
        self,
        output_type: CodeOutputType,
        options: CodeGenOptions,
    ) -> tuple[str, str | None, str | None]:
        """コマンドを取得.

        Returns:
            (entry_point, build_command, start_command)
        """
        if output_type == CodeOutputType.FRONTEND:
            return (
                "src/App.tsx",
                "npm run build",
                "npm run dev",
            )
        if output_type == CodeOutputType.BACKEND:
            return (
                "app.py",
                None,
                "uvicorn app:app --host 0.0.0.0 --port 8000",
            )
        # FULLSTACK
        return (
            "backend/app.py",
            "cd frontend && npm run build",
            "uvicorn backend.app:app --host 0.0.0.0 --port 8000",
        )


__all__ = ["CodeGenerator"]
