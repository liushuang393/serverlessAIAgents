"""codegen ルーター.

エンドポイント:
    POST /generate  - ワークフロー定義からコードを生成
    GET  /frameworks - サポートフレームワーク一覧
"""

from __future__ import annotations

import logging
import uuid
from typing import Any

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from apps.dev_studio.wizard.specs import normalize_spec_kind, workflow_from_spec


_logger = logging.getLogger(__name__)

router = APIRouter(tags=["codegen"])


class GenerateRequest(BaseModel):
    """コード生成リクエスト."""

    workflow_name: str = Field("", description="ワークフロー名")
    workflow_description: str = Field("", description="ワークフロー説明")
    output_type: str = Field("fullstack", description="出力タイプ: frontend / backend / fullstack")
    app_name: str = Field("", description="アプリ名（省略時はワークフロー名から生成）")
    framework: str = Field("", description="フレームワーク（省略時は fastapi）")
    spec_kind: str | None = Field(default=None, description="agent / system")
    spec: dict[str, Any] | None = Field(default=None, description="builder 生成の spec")
    backend_port: int | None = Field(default=None, description="生成コード用 backend port")
    frontend_port: int | None = Field(default=None, description="生成コード用 frontend port")


class GenerateResponse(BaseModel):
    """コード生成レスポンス."""

    success: bool
    files: dict[str, str] = Field(default_factory=dict)
    entry_point: str = ""
    build_command: str | None = None
    start_command: str | None = None
    output_type: str = ""
    metadata: dict[str, Any] = Field(default_factory=dict)


@router.post("/generate", response_model=GenerateResponse)
async def generate_code(request: GenerateRequest) -> GenerateResponse:
    """ワークフロー定義からコードを生成.

    Args:
        request: 生成リクエスト

    Returns:
        生成されたコード（ファイル辞書）
    """
    try:
        from kernel.core.interfaces import CodeGenOptions, CodeOutputType, WorkflowDefinition

        # output_type を変換
        type_map = {
            "frontend": CodeOutputType.FRONTEND,
            "backend": CodeOutputType.BACKEND,
            "fullstack": CodeOutputType.FULLSTACK,
        }
        output_type = type_map.get(request.output_type.lower(), CodeOutputType.FULLSTACK)

        # ワークフロー定義を構築（contracts.core.types.WorkflowDefinition 形式）
        spec_kind = normalize_spec_kind(request.spec_kind)
        if request.spec is not None:
            workflow = workflow_from_spec(spec_kind, request.spec)
            workflow_name = workflow.name
        else:
            workflow_name = request.workflow_name.strip()
            if not workflow_name:
                raise ValueError("workflow_name または spec は必須です")
            workflow = WorkflowDefinition(
                id=str(uuid.uuid4()),
                name=workflow_name,
                description=request.workflow_description,
            )
            if request.spec_kind:
                workflow.metadata["spec_kind"] = spec_kind

        app_name = request.app_name or workflow.name.lower().replace(" ", "-")

        # オプションを設定
        options = CodeGenOptions(
            app_name=app_name,
            framework=request.framework or "fastapi",
            backend_port=request.backend_port or CodeGenOptions().backend_port,
            frontend_port=request.frontend_port or CodeGenOptions().frontend_port,
        )

        # コードを生成
        from apps.dev_studio.codegen.generator import CodeGenerator

        generator = CodeGenerator()
        result = await generator.generate(workflow, output_type, options)

        return GenerateResponse(
            success=True,
            files=result.files,
            entry_point=result.entry_point,
            build_command=result.build_command,
            start_command=result.start_command,
            output_type=output_type.value,
            metadata={
                "file_count": len(result.files),
                "spec_kind": workflow.metadata.get("spec_kind", spec_kind),
                "workflow_name": workflow.name,
            },
        )

    except Exception as e:
        _logger.exception(f"コード生成エラー: {e}")
        raise HTTPException(status_code=500, detail=f"生成失敗: {e}") from e


@router.get("/frameworks")
async def list_frameworks() -> dict[str, Any]:
    """サポートフレームワーク一覧を返す."""
    from apps.dev_studio.codegen.generator import CodeGenerator
    from kernel.core.interfaces import CodeOutputType

    generator = CodeGenerator()
    return {
        "frameworks": {
            "frontend": generator.get_supported_frameworks(CodeOutputType.FRONTEND),
            "backend": generator.get_supported_frameworks(CodeOutputType.BACKEND),
            "fullstack": generator.get_supported_frameworks(CodeOutputType.FULLSTACK),
        }
    }


__all__ = ["router"]
