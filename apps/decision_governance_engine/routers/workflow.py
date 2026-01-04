# -*- coding: utf-8 -*-
"""ワークフロー設定APIルーター.

エンドポイント:
    - GET /api/health: ヘルスチェック
    - GET /api/agents: Agent定義取得（非推奨）
    - GET /api/agents/definitions: Agent定義取得（YAML駆動）
    - GET /api/flows/{flow_id}/definition: Flow定義取得
    - GET /api/results/{result_id}: 結果取得
    - GET /api/workflow/config: Studio UI用設定
"""

import logging
from pathlib import Path
from typing import Any

import yaml
from fastapi import APIRouter, HTTPException
from pydantic import BaseModel

from agentflow.core.flow_definition import FlowDefinitionRegistry
from agentflow.core.result_store import ResultStoreManager

from apps.decision_governance_engine.flow_config import get_flow_definition
from apps.decision_governance_engine.routers.decision import get_engine
from apps.decision_governance_engine.services.agent_registry import AgentRegistry

logger = logging.getLogger("decision_api.workflow")

router = APIRouter(tags=["ワークフロー"])


# ========================================
# スキーマ定義
# ========================================

class AgentDefinition(BaseModel):
    """Agent定義スキーマ."""
    id: str
    name: str
    label: str


class HealthResponse(BaseModel):
    """ヘルスチェックレスポンス."""
    status: str = "ok"
    version: str = "2.0.0"


# ========================================
# エンドポイント
# ========================================

@router.get("/api/health", response_model=HealthResponse)
async def health_check() -> HealthResponse:
    """ヘルスチェック."""
    return HealthResponse()


@router.get("/api/agents", response_model=list[AgentDefinition])
async def get_agents() -> list[AgentDefinition]:
    """Agent定義を取得（非推奨: /api/agents/definitionsを使用）."""
    engine = get_engine()
    return [AgentDefinition(**d) for d in engine.get_agent_definitions()]


@router.get("/api/agents/definitions")
async def get_agent_definitions() -> dict[str, Any]:
    """Agent定義を取得（YAML駆動、フロントエンド同期用）.

    agent_definitions.yaml から読み込んだ統一定義を返す。
    フロントエンドはこの定義を使用してUIを構築すべき。

    Returns:
        flow_id, name, version, agents を含む辞書
    """
    # 静的読み込み（AgentRegistry初期化不要）
    definitions = AgentRegistry.load_definitions_static()
    if not definitions.get("agents"):
        # フォールバック: 旧方式
        definition = get_flow_definition()
        return definition.to_frontend_dict()
    return definitions


@router.get("/api/flows/{flow_id}/definition")
async def get_flow_definition_api(flow_id: str) -> dict[str, Any]:
    """Flow定義を取得（前端同期用）.

    FlowDefinitionRegistry またはYAMLから読み込む。
    """
    # まずレジストリから取得を試みる
    registry = FlowDefinitionRegistry.get_instance()
    definition = registry.get(flow_id)

    if definition:
        return definition.to_frontend_dict()

    # フォールバック: 旧方式
    definition = get_flow_definition()
    if definition.flow_id != flow_id:
        raise HTTPException(status_code=404, detail=f"Flow not found: {flow_id}")
    return definition.to_dict()


@router.get("/api/results/{result_id}")
async def get_result_api(result_id: str) -> dict[str, Any]:
    """フロー実行結果を取得."""
    result = await ResultStoreManager.get(result_id)
    if not result:
        raise HTTPException(status_code=404, detail=f"Result not found: {result_id}")
    return result.model_dump()


@router.get("/api/workflow/config")
async def get_workflow_config() -> dict[str, Any]:
    """Studio UI用のワークフロー設定を取得."""
    config_path = Path(__file__).parent.parent / "agent.yaml"
    with open(config_path) as f:
        config = yaml.safe_load(f)

    return {
        "name": config.get("name"),
        "version": config.get("version"),
        "description": config.get("description"),
        "agents": config.get("agents", {}),
        "workflow": config.get("workflow", {}),
        "studio": config.get("studio", {}),
    }

