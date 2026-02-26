"""RAG 関連ルーター.

/api/rag/query, /api/rag/add
"""

from __future__ import annotations

import io
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.backend.config import KnowledgeBaseType, kb_registry
from apps.faq_system.backend.rag.parsers import FileParser
from apps.faq_system.routers.dependencies import (
    get_rag_ingestion_orchestrator,
    get_rag_service,
    is_rag_enabled,
)
from fastapi import APIRouter, Depends, File, HTTPException, Query, UploadFile
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


class IngestRequest(BaseModel):
    """手動 ingest リクエスト."""

    source_ids: list[str] = Field(default_factory=list, description="対象 source_id 一覧")
    dry_run: bool = Field(default=False, description="実処理を行わず計画のみ返す")


def _raise_service_error(
    *,
    message: str,
    error_code: str | None,
) -> None:
    status_code = 400 if error_code and error_code.startswith("invalid_") else 500
    raise HTTPException(
        status_code=status_code,
        detail={
            "message": message,
            "error_code": error_code or "service_error",
        },
    )


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.post("/api/rag/query")
async def rag_query(
    request: RAGQueryRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """RAG クエリ API (認証必須)."""
    if not is_rag_enabled():
        raise HTTPException(
            status_code=400,
            detail={"message": "RAG is disabled by app config", "error_code": "rag_disabled"},
        )

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
    if not result.success:
        _raise_service_error(
            message=result.error_message or "RAG query failed",
            error_code=result.error_code,
        )
    return result.data


@router.post("/api/rag/add")
async def rag_add_document(
    request: AddDocumentRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ドキュメント追加 API (認証必須)."""
    if not is_rag_enabled():
        raise HTTPException(
            status_code=400,
            detail={"message": "RAG is disabled by app config", "error_code": "rag_disabled"},
        )

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
    if not result.success:
        _raise_service_error(
            message=result.error_message or "RAG add document failed",
            error_code=result.error_code,
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
    if not is_rag_enabled():
        raise HTTPException(
            status_code=400,
            detail={"message": "RAG is disabled by app config", "error_code": "rag_disabled"},
        )

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
        raise HTTPException(
            status_code=400,
            detail={"message": f"ファイルの解析に失敗しました: {exc!s}", "error_code": "invalid_file"},
        ) from exc

    if not content.strip():
        raise HTTPException(
            status_code=400,
            detail={
                "message": "ファイルからテキストを抽出できませんでした。",
                "error_code": "empty_file_content",
            },
        )

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
    if not result.success:
        _raise_service_error(
            message=result.error_message or "RAG upload failed",
            error_code=result.error_code,
        )
    return result.data


@router.post("/api/rag/ingest")
async def rag_ingest(
    request: IngestRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """RAG data source ingest を手動実行."""
    orchestrator = get_rag_ingestion_orchestrator()
    return await orchestrator.ingest(
        source_ids=request.source_ids or None,
        dry_run=request.dry_run,
    )


@router.get("/api/rag/ingest/runs")
async def rag_ingest_runs(
    limit: int = Query(20, ge=1, le=100),
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """直近 ingest 実行履歴を返す."""
    orchestrator = get_rag_ingestion_orchestrator()
    runs = orchestrator.list_runs(limit=limit)
    return {"total": len(runs), "runs": runs}
