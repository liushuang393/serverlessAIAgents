"""ナレッジベースAPI ルート.

知識ベース管理とRAGクエリ機能。
"""

from __future__ import annotations

from typing import Any

from fastapi import APIRouter

from agentflow.studio.models import (  # noqa: TC001 — FastAPIがランタイムで型評価に必要
    ChatRequest,
    KnowledgeAddRequest,
    RAGQueryRequest,
)


def create_knowledge_router() -> APIRouter:
    """ナレッジベースAPIルーターを作成.

    Returns:
        FastAPI APIRouter
    """
    router = APIRouter(prefix="/api/knowledge", tags=["knowledge"])

    @router.post("/add")
    async def add_knowledge(request: KnowledgeAddRequest) -> dict[str, Any]:
        """知識ベースにドキュメントを追加."""
        from agentflow.services import RAGService

        service = RAGService()

        result = await service.execute(
            action="add_document",
            content=request.content,
            source=request.topic,
            metadata={
                "topic": request.topic,
                **request.metadata,
            },
        )
        ids = result.data.get("ids")
        doc_id = ids[0] if isinstance(ids, list) and ids else ""

        return {
            "status": "success",
            "document_id": doc_id,
            "topic": request.topic,
        }

    @router.post("/query")
    async def query_knowledge(request: RAGQueryRequest) -> dict[str, Any]:
        """知識ベースをクエリ."""
        from agentflow.services import RAGService

        service = RAGService()

        results = await service.execute(
            action="query",
            question=request.question,
            filters={"topic": request.topic} if request.topic else None,
        )

        return {
            "status": "success",
            "answer": results.data.get("answer", ""),
            "sources": results.data.get("documents", []),
        }

    @router.post("/chat")
    async def chat(request: ChatRequest) -> dict[str, Any]:
        """RAG対応チャット."""
        from agentflow.services import RAGService

        service = RAGService()

        results = await service.execute(action="query", question=request.message)

        return {
            "status": "success",
            "session_id": request.session_id,
            "response": results.data.get("answer", ""),
            "sources": results.data.get("documents", []),
        }

    @router.get("/topics")
    async def list_topics() -> list[str]:
        """利用可能なトピック一覧."""
        # 現行 RAGService はトピック管理 API を公開していないため空配列を返す。
        return []

    @router.delete("/{topic}")
    async def delete_topic(topic: str) -> dict[str, str]:
        """トピックを削除."""
        # 現行 RAGService はトピック単位削除 API を公開していない。
        return {"status": "deleted", "topic": topic}

    return router
