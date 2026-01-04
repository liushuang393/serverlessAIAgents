# -*- coding: utf-8 -*-
"""知識ベース管理APIルーター.

通用工厂パターンで複数Agent（術/器）の知識管理を統一。

使用方法:
    # 個別Router作成
    shu_router = create_knowledge_router("shu", "術", "実行計画")
    qi_router = create_knowledge_router("qi", "器", "技術実装")
    
    # または統合Router
    from routers.knowledge import router as knowledge_router
"""

import logging
from typing import Any, Callable

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field

logger = logging.getLogger("decision_api.knowledge")


# ========================================
# スキーマ定義
# ========================================

class KnowledgeDocument(BaseModel):
    """知識ドキュメント追加リクエスト."""
    content: str = Field(..., min_length=10, max_length=5000, description="ドキュメント内容")
    topic: str = Field(default="default", description="トピック分類")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


class KnowledgeListResponse(BaseModel):
    """知識一覧レスポンス."""
    agent: str
    agent_name: str
    documents: list[dict[str, Any]]
    total: int


class KnowledgeAddResponse(BaseModel):
    """知識追加レスポンス."""
    status: str = "success"
    doc_id: str
    agent: str
    message: str


class KnowledgeDeleteResponse(BaseModel):
    """知識削除レスポンス."""
    status: str = "success"
    doc_id: str
    agent: str
    message: str


# ========================================
# Agent取得用依存関係（遅延インポート）
# ========================================

def _get_agent(agent_type: str) -> Any:
    """AgentTypeに応じたAgentインスタンスを取得."""
    from apps.decision_governance_engine.routers.decision import get_engine
    engine = get_engine()

    if agent_type == "shu":
        return engine._shu
    elif agent_type == "qi":
        return engine._qi
    else:
        raise HTTPException(status_code=400, detail=f"Unknown agent type: {agent_type}")


# ========================================
# 通用路由工厂
# ========================================

def create_knowledge_router(
    agent_type: str,
    agent_name: str,
    agent_label: str,
) -> APIRouter:
    """知識管理ルーターを生成する通用工厂.

    Args:
        agent_type: Agent識別子（例: "shu", "qi"）
        agent_name: Agent日本語名（例: "術", "器"）
        agent_label: Agent説明（例: "実行計画", "技術実装"）

    Returns:
        設定済みAPIRouter
    """
    router = APIRouter(
        prefix=f"/api/knowledge/{agent_type}",
        tags=[f"知識管理・{agent_name}"],
    )

    @router.get("", response_model=KnowledgeListResponse)
    async def list_documents() -> KnowledgeListResponse:
        """知識ドキュメント一覧を取得."""
        agent = _get_agent(agent_type)

        if not hasattr(agent, "_rag") or agent._rag is None:
            return KnowledgeListResponse(
                agent=agent_type,
                agent_name=f"{agent_name}（{agent_label}）",
                documents=[],
                total=0,
            )

        status = agent._rag.get_status()
        docs = status.get("memory", {}).get("entries", [])

        return KnowledgeListResponse(
            agent=agent_type,
            agent_name=f"{agent_name}（{agent_label}）",
            documents=docs,
            total=len(docs),
        )

    @router.post("", response_model=KnowledgeAddResponse)
    async def add_document(doc: KnowledgeDocument) -> KnowledgeAddResponse:
        """知識ドキュメントを追加."""
        agent = _get_agent(agent_type)

        if not hasattr(agent, "_rag") or agent._rag is None:
            await agent.initialize_rag()

        doc_id = await agent._rag.add_document(
            content=doc.content,
            topic=doc.topic,
            metadata=doc.metadata,
        )

        logger.info(f"Knowledge added to {agent_type}: {doc_id}")

        return KnowledgeAddResponse(
            doc_id=doc_id,
            agent=agent_type,
            message=f"ドキュメントを{agent_name}の知識ベースに追加しました。",
        )

    @router.delete("/{doc_id}", response_model=KnowledgeDeleteResponse)
    async def delete_document(doc_id: str) -> KnowledgeDeleteResponse:
        """知識ドキュメントを削除."""
        agent = _get_agent(agent_type)

        if not hasattr(agent, "_rag") or agent._rag is None:
            raise HTTPException(status_code=404, detail="Knowledge base not initialized")

        await agent._rag._memory.forget(doc_id)
        logger.info(f"Knowledge deleted from {agent_type}: {doc_id}")

        return KnowledgeDeleteResponse(
            doc_id=doc_id,
            agent=agent_type,
            message="ドキュメントを削除しました。",
        )

    return router


# ========================================
# 統合Router（即座に使用可能）
# ========================================

router = APIRouter()
router.include_router(create_knowledge_router("shu", "術", "実行計画"))
router.include_router(create_knowledge_router("qi", "器", "技術実装"))

