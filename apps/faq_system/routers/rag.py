"""RAG 関連ルーター.

/api/rag/query, /api/rag/add
"""

from __future__ import annotations

import io
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.backend.config import KnowledgeBaseType, kb_registry
from apps.faq_system.backend.rag.parsers import FileParser
from apps.faq_system.routers.dependencies import get_rag_service
from fastapi import APIRouter, Depends, File, Query, UploadFile
from pydantic import BaseModel, Field

from agentflow.services import RAGConfig, RAGService


if TYPE_CHECKING:
    from apps.faq_system.backend.auth.models import UserInfo
else:
    UserInfo = Any


router = APIRouter(tags=["RAG"])


# ---------------------------------------------------------------------------
# リクエストモデル
# ---------------------------------------------------------------------------


class RAGQueryRequest(BaseModel):
    """RAGクエリリクエスト."""

    question: str = Field(..., description="質問")
    kb_type: KnowledgeBaseType = Field(
        default=KnowledgeBaseType.INTERNAL,
        description="KB種別 (internal / external / confidential)",
    )
    collection: str | None = Field(None, description="明示コレクション名（指定時は優先）")
    top_k: int = Field(5, description="取得件数")


class AddDocumentRequest(BaseModel):
    """ドキュメント追加リクエスト."""

    kb_type: KnowledgeBaseType = Field(
        default=KnowledgeBaseType.INTERNAL,
        description="KB種別 (internal / external / confidential)",
    )
    content: str = Field(..., description="ドキュメント内容")
    collection: str | None = Field(None, description="明示コレクション名（指定時は優先）")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.post("/api/rag/query")
async def rag_query(
    request: RAGQueryRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """RAG クエリ API (認証必須)."""
    collection = kb_registry.resolve_collection(
        kb_type=request.kb_type,
        explicit_collection=request.collection,
    )
    service = RAGService(
        RAGConfig(
            collection=collection,
            top_k=request.top_k,
        )
    )
    result = await service.execute(action="query", question=request.question)
    return result.data


@router.post("/api/rag/add")
async def rag_add_document(
    request: AddDocumentRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ドキュメント追加 API (認証必須)."""
    collection = kb_registry.resolve_collection(
        kb_type=request.kb_type,
        explicit_collection=request.collection,
    )
    service = get_rag_service(collection=collection)
    result = await service.execute(
        action="add_document",
        content=request.content,
        metadata=request.metadata,
    )
    return result.data


@router.post("/api/rag/upload")
async def rag_upload_file(
    file: UploadFile = File(...),
    kb_type: KnowledgeBaseType = Query(KnowledgeBaseType.INTERNAL),
    collection: str | None = Query(None),
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ファイルアップロードによるドキュメント追加 API (認証必須)."""
    filename = file.filename or "uploaded_file"
    ext = filename.split(".")[-1].lower() if "." in filename else ""

    content = ""
    file_bytes = await file.read()

    try:
        if ext == "pdf":
            content = FileParser.parse_pdf(io.BytesIO(file_bytes))
        elif ext in ["docx", "doc"]:
            content = FileParser.parse_docx(io.BytesIO(file_bytes))
        elif ext == "csv":
            content = FileParser.parse_csv(io.StringIO(file_bytes.decode("utf-8")))
        else:
            # Default to text
            content = file_bytes.decode("utf-8")
    except Exception as exc:
        return {"success": False, "message": f"ファイルの解析に失敗しました: {exc!s}"}

    if not content.strip():
        return {"success": False, "message": "ファイルからテキストを抽出できませんでした。"}

    target_collection = kb_registry.resolve_collection(
        kb_type=kb_type,
        explicit_collection=collection,
    )
    service = get_rag_service(collection=target_collection)
    result = await service.execute(
        action="add_document",
        content=content,
        metadata={"source": filename, "type": ext},
    )
    return result.data
