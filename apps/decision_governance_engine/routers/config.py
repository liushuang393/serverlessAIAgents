# -*- coding: utf-8 -*-
"""設定管理APIルーター.

エンドポイント:
    - GET /api/config/rag: RAG設定一覧取得
    - PUT /api/config/rag/{agent_id}: Agent RAG設定更新
    - POST /api/config/rag/{agent_id}/test: RAG接続テスト
    - GET /api/config/agents: 全Agent設定取得
"""

import logging
from typing import Any

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

logger = logging.getLogger("decision_api.config")

router = APIRouter(prefix="/api/config", tags=["設定管理"])


# ========================================
# スキーマ定義
# ========================================

class RAGSourceConfig(BaseModel):
    """RAGソース設定."""
    name: str = Field(..., description="ソース名（例: industry_practices）")
    enabled: bool = Field(True, description="有効/無効")
    directory: str | None = Field(None, description="ドキュメントディレクトリ")
    url: str | None = Field(None, description="外部RAG URL")


class AgentRAGConfig(BaseModel):
    """Agent RAG設定."""
    agent_id: str = Field(..., description="Agent ID")
    agent_name: str = Field(..., description="Agent名")
    use_rag: bool = Field(False, description="RAG使用有無")
    rag_sources: list[RAGSourceConfig] = Field(default_factory=list, description="RAGソース一覧")
    top_k: int = Field(5, ge=1, le=20, description="検索結果上位K件")
    min_similarity: float = Field(0.3, ge=0.0, le=1.0, description="最小類似度")


class RAGConfigUpdate(BaseModel):
    """RAG設定更新リクエスト."""
    use_rag: bool | None = None
    rag_sources: list[RAGSourceConfig] | None = None
    top_k: int | None = Field(None, ge=1, le=20)
    min_similarity: float | None = Field(None, ge=0.0, le=1.0)


class RAGTestResult(BaseModel):
    """RAGテスト結果."""
    success: bool
    message: str
    document_count: int = 0
    sample_query_result: str | None = None


# ========================================
# グローバル設定ストレージ（本番ではDB使用）
# ========================================

_rag_configs: dict[str, AgentRAGConfig] = {}


def _init_default_configs() -> None:
    """デフォルトRAG設定を初期化."""
    if _rag_configs:
        return

    defaults = [
        AgentRAGConfig(
            agent_id="shu",
            agent_name="術（実行計画）",
            use_rag=True,
            rag_sources=[
                RAGSourceConfig(name="industry_practices", enabled=True),
                RAGSourceConfig(name="case_studies", enabled=True),
            ],
            top_k=3,
            min_similarity=0.4,
        ),
        AgentRAGConfig(
            agent_id="qi",
            agent_name="器（技術実装）",
            use_rag=True,
            rag_sources=[
                RAGSourceConfig(name="technical_docs", enabled=True),
                RAGSourceConfig(name="compliance", enabled=True),
            ],
            top_k=3,
            min_similarity=0.4,
        ),
        AgentRAGConfig(
            agent_id="dao",
            agent_name="道（問題本質）",
            use_rag=False,
            rag_sources=[],
        ),
        AgentRAGConfig(
            agent_id="fa",
            agent_name="法（選択肢分析）",
            use_rag=False,
            rag_sources=[],
        ),
        AgentRAGConfig(
            agent_id="review",
            agent_name="検証",
            use_rag=False,
            rag_sources=[],
        ),
    ]

    for cfg in defaults:
        _rag_configs[cfg.agent_id] = cfg


# ========================================
# エンドポイント
# ========================================

@router.get("/rag", response_model=list[AgentRAGConfig])
async def get_all_rag_configs() -> list[AgentRAGConfig]:
    """全AgentのRAG設定を取得."""
    _init_default_configs()
    return list(_rag_configs.values())


@router.get("/rag/{agent_id}", response_model=AgentRAGConfig)
async def get_agent_rag_config(agent_id: str) -> AgentRAGConfig:
    """特定AgentのRAG設定を取得."""
    _init_default_configs()
    if agent_id not in _rag_configs:
        raise HTTPException(status_code=404, detail=f"Agent not found: {agent_id}")
    return _rag_configs[agent_id]


@router.put("/rag/{agent_id}", response_model=AgentRAGConfig)
async def update_agent_rag_config(agent_id: str, update: RAGConfigUpdate) -> AgentRAGConfig:
    """AgentのRAG設定を更新."""
    _init_default_configs()
    if agent_id not in _rag_configs:
        raise HTTPException(status_code=404, detail=f"Agent not found: {agent_id}")

    cfg = _rag_configs[agent_id]
    if update.use_rag is not None:
        cfg.use_rag = update.use_rag
    if update.rag_sources is not None:
        cfg.rag_sources = update.rag_sources
    if update.top_k is not None:
        cfg.top_k = update.top_k
    if update.min_similarity is not None:
        cfg.min_similarity = update.min_similarity

    logger.info(f"RAG config updated for {agent_id}: use_rag={cfg.use_rag}")
    return cfg


@router.post("/rag/{agent_id}/test", response_model=RAGTestResult)
async def test_agent_rag(agent_id: str) -> RAGTestResult:
    """AgentのRAG接続をテスト."""
    _init_default_configs()
    if agent_id not in _rag_configs:
        raise HTTPException(status_code=404, detail=f"Agent not found: {agent_id}")

    cfg = _rag_configs[agent_id]
    if not cfg.use_rag:
        return RAGTestResult(
            success=False,
            message=f"{cfg.agent_name}はRAGを使用しない設定です",
        )

    try:
        from apps.decision_governance_engine.routers.decision import get_engine
        engine = get_engine()

        agent = None
        if agent_id == "shu":
            agent = engine._shu
        elif agent_id == "qi":
            agent = engine._qi

        if agent and hasattr(agent, "_rag") and agent._rag:
            status = agent._rag.get_status()
            doc_count = status.get("memory", {}).get("total_entries", 0)

            # テストクエリ実行
            test_result = await agent._rag.query("テスト検索")

            return RAGTestResult(
                success=True,
                message="RAG接続成功",
                document_count=doc_count,
                sample_query_result=test_result.answer[:200] if test_result.answer else None,
            )
        else:
            return RAGTestResult(
                success=False,
                message="RAGが初期化されていません",
            )

    except Exception as e:
        logger.error(f"RAG test failed for {agent_id}: {e}")
        return RAGTestResult(
            success=False,
            message=f"RAGテスト失敗: {str(e)}",
        )


@router.get("/agents")
async def get_all_agent_configs() -> dict[str, Any]:
    """全Agent設定を取得（agent.yamlから）."""
    import yaml
    from pathlib import Path

    config_path = Path(__file__).parent.parent / "agent.yaml"
    with open(config_path) as f:
        config = yaml.safe_load(f)

    return {
        "agents": config.get("agents", {}),
        "rag_configs": {k: v.model_dump() for k, v in _rag_configs.items()},
    }


# ========================================
# RAG設定取得関数（他モジュールから参照）
# ========================================

def get_rag_config(agent_id: str) -> AgentRAGConfig | None:
    """特定AgentのRAG設定を取得（他モジュール向け）."""
    _init_default_configs()
    return _rag_configs.get(agent_id)
