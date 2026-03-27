"""wizard ルーター.

エンドポイント:
    POST /create   - 自然言語記述から Agent を生成
    POST /validate - Agent 仕様を検証
    GET  /status   - 最後の生成ジョブのステータスを取得
"""

from __future__ import annotations

import logging
from typing import Any

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

from apps.dev_studio.wizard.models import WizardConfig


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
    """Agent 仕様検証リクエスト."""

    name: str = Field(..., description="Agent 名")
    description: str = Field(..., description="Agent の説明")
    capabilities: list[str] = Field(default_factory=list, description="能力リスト")
    engine_type: str = Field("simple", description="Engine タイプ")
    system_prompt: str = Field("", description="システムプロンプト")


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
        _logger.exception(f"Agent 生成エラー: {e}")
        raise HTTPException(status_code=500, detail=f"Agent 生成失敗: {e}") from e


@router.post("/validate")
async def validate_spec(request: ValidateSpecRequest) -> dict[str, Any]:
    """Agent 仕様を検証.

    Args:
        request: 検証対象の仕様

    Returns:
        検証結果
    """
    errors: list[str] = []
    warnings: list[str] = []

    if not request.name:
        errors.append("Agent 名は必須です")
    elif not request.name[0].isupper():
        warnings.append("Agent 名は PascalCase を推奨します")

    if not request.description:
        warnings.append("説明文を追加することを推奨します")

    if not request.capabilities:
        warnings.append("能力リストが空です")

    valid_engines = {"simple", "pipeline", "gate", "rag"}
    if request.engine_type not in valid_engines:
        errors.append(f"不正な engine_type: {request.engine_type}。有効値: {valid_engines}")

    score = max(0.0, 1.0 - len(errors) * 0.3 - len(warnings) * 0.1)

    return {
        "valid": len(errors) == 0,
        "score": score,
        "errors": errors,
        "warnings": warnings,
    }


@router.get("/templates")
async def list_templates() -> dict[str, Any]:
    """利用可能な Agent テンプレート一覧を返す."""
    return {
        "templates": [
            {"id": "simple", "name": "Simple Agent", "description": "基本的な単一タスク Agent"},
            {"id": "pipeline", "name": "Pipeline Agent", "description": "複数ステップを順次処理する Agent"},
            {"id": "rag", "name": "RAG Agent", "description": "知識ベース検索と生成を組み合わせた Agent"},
            {"id": "gate", "name": "Gate Agent", "description": "条件分岐ロジックを持つ Agent"},
        ]
    }


__all__ = ["router"]
