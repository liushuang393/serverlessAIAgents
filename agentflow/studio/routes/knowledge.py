"""ナレッジベースAPI ルート.

知識ベース管理とRAGクエリ機能。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from fastapi import APIRouter


if TYPE_CHECKING:
    from agentflow.studio.models import ChatRequest, KnowledgeAddRequest, RAGQueryRequest


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

        doc_id = await service.add_document(
            content=request.content,
            metadata={
                "topic": request.topic,
                **request.metadata,
            },
        )

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

        results = await service.query(
            question=request.question,
            filters={"topic": request.topic} if request.topic else None,
        )

        return {
            "status": "success",
            "answer": results.get("answer", ""),
            "sources": results.get("sources", []),
        }

    @router.post("/chat")
    async def chat(request: ChatRequest) -> dict[str, Any]:
        """RAG対応チャット."""
        from agentflow.services import RAGService

        service = RAGService()

        results = await service.query(question=request.message)

        return {
            "status": "success",
            "session_id": request.session_id,
            "response": results.get("answer", ""),
            "sources": results.get("sources", []),
        }

    @router.get("/topics")
    async def list_topics() -> list[str]:
        """利用可能なトピック一覧."""
        from agentflow.services import RAGService

        service = RAGService()
        return await service.list_topics()

    @router.delete("/{topic}")
    async def delete_topic(topic: str) -> dict[str, str]:
        """トピックを削除."""
        from agentflow.services import RAGService

        service = RAGService()
        await service.delete_topic(topic)

        return {"status": "deleted", "topic": topic}

    return router
