"""wizard ルーター.

エンドポイント:
    POST /create         - 自然言語記述から Agent を生成
    POST /create-system  - 自然言語記述から複数 Agent System を生成
    POST /validate       - Agent / System 仕様を検証
"""

from __future__ import annotations

import logging
from typing import Any, Literal

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from apps.dev_studio.wizard.models import AgentSpec, WizardConfig
from apps.dev_studio.wizard.specs import (
    build_agent_spec,
    build_system_spec,
    list_builder_templates,
    normalize_spec_kind,
    validate_builder_spec,
)


_logger = logging.getLogger(__name__)

router = APIRouter(tags=["wizard"])


class CreateAgentRequest(BaseModel):
    """Agent 生成リクエスト."""

    description: str = Field(..., description="Agent の自然言語記述（日本語可）")
    name: str | None = Field(None, description="Agent 名（省略時は自動生成）")
    auto_test: bool = Field(True, description="生成後に自動テストを実行するか")
    auto_publish: bool = Field(False, description="検証後に自動発布するか")
    min_confidence: float = Field(0.7, ge=0.0, le=1.0, description="最小信頼度")


class CreateAgentResponse(BaseModel):
    """Agent 生成レスポンス."""

    success: bool
    agent_spec: dict[str, Any] | None = None
    generated_code: str | None = None
    test_results: dict[str, Any] | None = None
    validation: dict[str, Any] | None = None
    published: bool = False
    publish_id: str | None = None
    error: str | None = None
    duration_ms: float = 0.0


class ValidateSpecRequest(BaseModel):
    """Agent / System 仕様検証リクエスト."""

    spec_kind: Literal["agent", "system"] = Field(default="agent", description="仕様種別")
    spec: dict[str, Any] | None = Field(default=None, description="builder 用の生 spec")

    name: str = Field("", description="Agent 名")
    description: str = Field("", description="Agent の説明")
    capabilities: list[str] = Field(default_factory=list, description="能力リスト")
    engine_type: str = Field("simple", description="Engine タイプ")
    system_prompt: str = Field("", description="システムプロンプト")


class CreateSystemRequest(BaseModel):
    """System 生成リクエスト."""

    description: str = Field(..., description="System の自然言語記述")
    name: str | None = Field(None, description="System 名")
    template_id: str | None = Field(None, description="テンプレート ID")


class CreateSystemResponse(BaseModel):
    """System 生成レスポンス."""

    success: bool
    system_spec: dict[str, Any] | None = None
    validation: dict[str, Any] | None = None
    generated_code: str | None = None
    error: str | None = None


@router.post("/create", response_model=CreateAgentResponse)
async def create_agent(request: CreateAgentRequest) -> CreateAgentResponse:
    """自然言語記述から Agent を自動生成.

    Args:
        request: 生成リクエスト

    Returns:
        生成結果（AgentSpec・コード・テスト結果）
    """
    try:
        from apps.dev_studio.wizard.agent_wizard import AgentWizard

        config = WizardConfig(
            auto_test=request.auto_test,
            auto_publish=request.auto_publish,
            min_confidence=request.min_confidence,
        )
        wizard = AgentWizard(config=config)

        result = await wizard.create_from_description(
            request.description,
            name=request.name,
            auto_test=request.auto_test,
            auto_publish=request.auto_publish,
        )

        return CreateAgentResponse(
            success=result.success,
            agent_spec=result.agent_spec.to_dict() if result.agent_spec else None,
            generated_code=result.generated_code,
            test_results=result.test_results.to_dict() if result.test_results else None,
            validation=result.validation.to_dict() if result.validation else None,
            published=result.published,
            publish_id=result.publish_id,
            error=result.error,
            duration_ms=result.duration_ms,
        )

    except Exception as e:
        _logger.warning("AgentWizard fallback を使用します: %s", e)
        fallback_spec = build_agent_spec(
            description=request.description,
            name=request.name,
        )
        validation = validate_builder_spec("agent", fallback_spec.to_dict()).to_dict()
        return CreateAgentResponse(
            success=True,
            agent_spec=fallback_spec.to_dict(),
            validation=validation,
            error=None,
            duration_ms=0.0,
        )


@router.post("/validate")
async def validate_spec(request: ValidateSpecRequest) -> dict[str, Any]:
    """Agent 仕様を検証.

    Args:
        request: 検証対象の仕様

    Returns:
        検証結果
    """
    spec_kind = normalize_spec_kind(request.spec_kind)
    spec = request.spec
    if spec is None:
        spec = AgentSpec.from_dict(
            {
                "name": request.name,
                "description": request.description,
                "capabilities": request.capabilities,
                "system_prompt": request.system_prompt,
                "engine_type": request.engine_type,
            }
        ).to_dict()

    return validate_builder_spec(spec_kind, spec).to_dict()


@router.post("/create-system", response_model=CreateSystemResponse)
async def create_system(request: CreateSystemRequest) -> CreateSystemResponse:
    """自然言語記述から System spec を生成する."""
    try:
        system_spec = build_system_spec(
            description=request.description,
            name=request.name,
            template_id=request.template_id,
        )
        validation = validate_builder_spec("system", system_spec.to_dict()).to_dict()
        return CreateSystemResponse(
            success=True,
            system_spec=system_spec.to_dict(),
            validation=validation,
        )
    except Exception as e:
        _logger.exception("System 生成エラー: %s", e)
        raise HTTPException(status_code=500, detail=f"System 生成失敗: {e}") from e


@router.get("/templates")
async def list_templates() -> dict[str, Any]:
    """利用可能な Agent テンプレート一覧を返す."""
    templates = list_builder_templates()
    return {"templates": templates, "total": len(templates)}


__all__ = ["router"]
