"""拡張 Agent API ルーター.

/api/agents/internal-kb/query   - 社内KB検索
/api/agents/external-kb/query   - 対客KB検索
/api/agents/maintenance/analyze  - メンテナンス分析
/api/agents/analytics/query     - データ分析
/api/agents/enhanced-faq/query  - 統合FAQ
"""

from __future__ import annotations

import json
import logging
import os
from enum import Enum
from typing import Any

from apps.faq_system.backend.agents import (
    AnalyticsAgent,
    AnalyticsConfig,
    EnhancedFAQAgent,
    EnhancedFAQConfig,
    ExternalKBAgent,
    ExternalKBConfig,
    InternalKBAgent,
    InternalKBConfig,
    MaintenanceAgent,
    MaintenanceConfig,
)
from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.backend.auth.models import UserInfo
from apps.faq_system.routers.dependencies import resolve_default_collection
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)

router = APIRouter(prefix="/api/agents", tags=["拡張Agent"])


# ---------------------------------------------------------------------------
# Agent ファクトリ
# ---------------------------------------------------------------------------


class AgentType(str, Enum):
    """Agent 種別."""

    INTERNAL_KB = "internal_kb"
    EXTERNAL_KB = "external_kb"
    MAINTENANCE = "maintenance"
    ANALYTICS = "analytics"
    ENHANCED_FAQ = "enhanced_faq"


_agent_cache: dict[str, Any] = {}


def _create_agent(agent_type: AgentType) -> Any:
    """Agent インスタンスを生成."""
    match agent_type:
        case AgentType.INTERNAL_KB:
            return InternalKBAgent(
                InternalKBConfig(
                    collection=resolve_default_collection(),
                )
            )
        case AgentType.EXTERNAL_KB:
            return ExternalKBAgent(ExternalKBConfig())
        case AgentType.MAINTENANCE:
            return MaintenanceAgent(MaintenanceConfig())
        case AgentType.ANALYTICS:
            schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
            return AnalyticsAgent(AnalyticsConfig(), db_schema=schema)
        case AgentType.ENHANCED_FAQ:
            schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
            return EnhancedFAQAgent(
                EnhancedFAQConfig(
                    rag_collection=resolve_default_collection(),
                    sql_schema=schema,
                )
            )


def get_agent(agent_type: AgentType) -> Any:
    """Agent 取得（遅延初期化 + シングルトン）."""
    if agent_type.value not in _agent_cache:
        _agent_cache[agent_type.value] = _create_agent(agent_type)
    return _agent_cache[agent_type.value]


# ---------------------------------------------------------------------------
# リクエストモデル
# ---------------------------------------------------------------------------


class AgentQueryRequest(BaseModel):
    """汎用Agent クエリリクエスト."""

    question: str = Field(..., description="質問")
    context: dict[str, Any] = Field(default_factory=dict, description="コンテキスト")
    options: dict[str, Any] = Field(default_factory=dict, description="オプション")


class MaintenanceRequest(BaseModel):
    """メンテナンス分析リクエスト."""

    action: str = Field(
        "full",
        description="アクション (diff / impact / generate / full)",
    )
    old_doc: str = Field("", description="旧ドキュメント")
    new_doc: str = Field("", description="新ドキュメント")
    question: str = Field("", description="質問")
    options: dict[str, Any] = Field(default_factory=dict, description="オプション")


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------


def _build_user_context(user: UserInfo) -> dict[str, Any]:
    """UserInfo → Agent コンテキスト辞書."""
    return {
        "user_id": user.user_id,
        "username": user.username,
        "role": user.role,
        "department": user.department,
    }


async def _run_agent(
    agent_type: AgentType,
    input_data: dict[str, Any],
) -> dict[str, Any]:
    """Agent 実行の共通ラッパー."""
    try:
        agent = get_agent(agent_type)
        return await agent.run(input_data)
    except TimeoutError as exc:
        raise HTTPException(status_code=504, detail=str(exc)) from exc
    except Exception as exc:
        logger.exception("Agent %s error: %s", agent_type.value, exc)
        raise HTTPException(status_code=500, detail=str(exc)) from exc


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.post("/internal-kb/query")
async def internal_kb_query(
    request: AgentQueryRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """社内KB検索 (認証必須)."""
    return await _run_agent(
        AgentType.INTERNAL_KB,
        {
            "question": request.question,
            "context": {**request.context, "user": _build_user_context(user)},
        },
    )


@router.post("/external-kb/query")
async def external_kb_query(
    request: AgentQueryRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """対客KB検索 (認証必須)."""
    return await _run_agent(
        AgentType.EXTERNAL_KB,
        {"question": request.question, "context": request.context},
    )


@router.post("/maintenance/analyze")
async def maintenance_analyze(
    request: MaintenanceRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """メンテナンス分析 (認証必須)."""
    return await _run_agent(
        AgentType.MAINTENANCE,
        {
            "action": request.action,
            "old_doc": request.old_doc,
            "new_doc": request.new_doc,
            "question": request.question,
            "options": request.options,
        },
    )


@router.post("/analytics/query")
async def analytics_query(
    request: AgentQueryRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """データ分析 (認証必須)."""
    return await _run_agent(
        AgentType.ANALYTICS,
        {
            "question": request.question,
            "context": {**request.context, "user": _build_user_context(user)},
        },
    )


@router.post("/enhanced-faq/query")
async def enhanced_faq_query(
    request: AgentQueryRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """統合FAQ検索 (認証必須)."""
    return await _run_agent(
        AgentType.ENHANCED_FAQ,
        {"question": request.question, "context": request.context},
    )
