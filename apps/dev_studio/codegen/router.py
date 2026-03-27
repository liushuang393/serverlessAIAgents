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


_logger = logging.getLogger(__name__)

router = APIRouter(tags=["codegen"])


class GenerateRequest(BaseModel):
    """コード生成リクエスト."""

    workflow_name: str = Field(..., description="ワークフロー名")
    workflow_description: str = Field("", description="ワークフロー説明")
    output_type: str = Field("fullstack", description="出力タイプ: frontend / backend / fullstack")
    app_name: str = Field("", description="アプリ名（省略時はワークフロー名から生成）")
    framework: str = Field("", description="フレームワーク（省略時は fastapi）")


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
        app_name = request.app_name or request.workflow_name.lower().replace(" ", "-")
        workflow = WorkflowDefinition(
            id=str(uuid.uuid4()),
            name=request.workflow_name,
            description=request.workflow_description,
        )

        # オプションを設定
        options = CodeGenOptions(app_name=app_name, framework=request.framework or "fastapi")

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
            metadata={"file_count": len(result.files)},
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
