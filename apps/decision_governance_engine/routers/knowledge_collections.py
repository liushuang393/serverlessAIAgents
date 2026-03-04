"""Decision Governance Engine — コレクション・ドキュメント管理ルーター.

CollectionManager / DocumentManager を使用した
知識ベースのコレクション CRUD・ドキュメント管理 API を提供する。

ロールベースアクセス制御:
    デモユーザーにロールを割り当て、コレクション一覧取得時に
    フォールバック RBAC マッピングでフィルタリングする。
"""

from __future__ import annotations

import logging
from typing import Any

from apps.decision_governance_engine.routers.auth import UserInfo, require_auth
from fastapi import APIRouter, Depends, File, HTTPException, Query, UploadFile
from pydantic import BaseModel, Field

from agentflow.knowledge.collection_manager import CollectionManager
from agentflow.knowledge.document_manager import DocumentManager
from agentflow.knowledge.rag_access_control import RAGAccessControl


logger = logging.getLogger("decision_api.knowledge_collections")

# DGE デモユーザーのロールマッピング
# 本番環境では JWT / auth_service からロールを取得する
_DEMO_USER_ROLES: dict[str, str] = {
    "admin": "admin",
    "tanaka": "manager",
    "suzuki": "employee",
    "yamamoto": "employee",
}


def _get_user_role(user: UserInfo) -> str:
    """デモユーザーのロールを解決."""
    return _DEMO_USER_ROLES.get(user.username, "employee")

router = APIRouter(tags=["知識ベース・コレクション管理"])


# ---------------------------------------------------------------------------
# マネージャーインスタンス（api.py の lifespan で初期化）
# ---------------------------------------------------------------------------

_collection_manager: CollectionManager | None = None
_document_manager: DocumentManager | None = None


def init_managers(
    collection_manager: CollectionManager,
    document_manager: DocumentManager,
) -> None:
    """マネージャーインスタンスを設定（起動時に呼ばれる）."""
    global _collection_manager, _document_manager
    _collection_manager = collection_manager
    _document_manager = document_manager


def _get_col_mgr() -> CollectionManager:
    if _collection_manager is None:
        raise HTTPException(status_code=503, detail="CollectionManager not initialized")
    return _collection_manager


def _get_doc_mgr() -> DocumentManager:
    if _document_manager is None:
        raise HTTPException(status_code=503, detail="DocumentManager not initialized")
    return _document_manager


# ---------------------------------------------------------------------------
# リクエスト / レスポンスモデル
# ---------------------------------------------------------------------------


class CreateCollectionRequest(BaseModel):
    """コレクション作成リクエスト."""

    collection_name: str = Field(..., description="コレクション名（一意）")
    display_name: str = Field("", description="表示名")
    description: str = Field("", description="説明")
    chunk_strategy: str = Field("recursive", description="チャンキング戦略")
    chunk_size: int = Field(1000, ge=100, le=10000, description="チャンクサイズ")
    chunk_overlap: int = Field(200, ge=0, le=2000, description="チャンクオーバーラップ")
    embedding_model: str | None = Field(None, description="エンベディングモデル")
    retrieval_method: str = Field("semantic", description="検索手法")
    reranker: str | None = Field(None, description="リランカー")
    top_k: int = Field(5, ge=1, le=100, description="上位 K 件")
    min_similarity: float = Field(0.3, ge=0.0, le=1.0, description="最小類似度")
    vector_db_type: str | None = Field(None, description="ベクトル DB 種別")
    vector_db_url: str | None = Field(None, description="ベクトル DB URL")


class UpdateCollectionRequest(BaseModel):
    """コレクション設定更新リクエスト."""

    display_name: str | None = None
    description: str | None = None
    chunk_strategy: str | None = None
    chunk_size: int | None = Field(None, ge=100, le=10000)
    chunk_overlap: int | None = Field(None, ge=0, le=2000)
    embedding_model: str | None = None
    retrieval_method: str | None = None
    reranker: str | None = None
    top_k: int | None = Field(None, ge=1, le=100)
    min_similarity: float | None = Field(None, ge=0.0, le=1.0)
    vector_db_type: str | None = None
    vector_db_url: str | None = None


class TestQueryRequest(BaseModel):
    """テストクエリリクエスト."""

    query: str = Field(..., description="テスト検索クエリ")
    top_k: int = Field(5, ge=1, le=50, description="取得件数")


class ChunkPreviewRequest(BaseModel):
    """チャンクプレビューリクエスト."""

    chunk_strategy: str | None = Field(None, description="チャンキング戦略")
    chunk_size: int | None = Field(None, ge=100, le=10000, description="チャンクサイズ")
    chunk_overlap: int | None = Field(None, ge=0, le=2000, description="オーバーラップ")


# ---------------------------------------------------------------------------
# コレクション CRUD エンドポイント
# ---------------------------------------------------------------------------


@router.post("/api/knowledge/collections")
async def create_collection(
    request: CreateCollectionRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """コレクション作成（認証必須）."""
    mgr = _get_col_mgr()

    try:
        model = await mgr.create_collection(
            collection_name=request.collection_name,
            app_name="decision_governance_engine",
            display_name=request.display_name or request.collection_name,
            description=request.description,
            chunk_strategy=request.chunk_strategy,
            chunk_size=request.chunk_size,
            chunk_overlap=request.chunk_overlap,
            embedding_model=request.embedding_model,
            retrieval_method=request.retrieval_method,
            reranker=request.reranker,
            top_k=request.top_k,
            min_similarity=request.min_similarity,
            vector_db_type=request.vector_db_type,
            vector_db_url=request.vector_db_url,
        )
    except ValueError as exc:
        raise HTTPException(status_code=409, detail=str(exc)) from exc

    return {"collection": model.to_dict()}


@router.get("/api/knowledge/collections")
async def list_collections(
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """コレクション一覧（認証必須）.

    全認証済みユーザーが全コレクションを閲覧可能。
    KB タイプベースのフィルタリングは test-query 時に適用される。
    """
    mgr = _get_col_mgr()
    collections = await mgr.list_collections(app_name="decision_governance_engine")
    return {
        "total": len(collections),
        "collections": [c.to_dict() for c in collections],
    }


@router.get("/api/knowledge/collections/{name}")
async def get_collection(
    name: str,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """コレクション詳細（認証必須）."""
    mgr = _get_col_mgr()
    model = await mgr.get_collection(name)
    if model is None:
        raise HTTPException(status_code=404, detail=f"Collection '{name}' not found")
    return {"collection": model.to_dict()}


@router.patch("/api/knowledge/collections/{name}")
async def update_collection(
    name: str,
    request: UpdateCollectionRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """コレクション設定更新（認証必須）."""
    mgr = _get_col_mgr()
    updates = request.model_dump(exclude_none=True)
    if not updates:
        raise HTTPException(status_code=400, detail="No fields to update")

    try:
        model = await mgr.update_collection(name, updates)
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc

    return {"collection": model.to_dict()}


@router.delete("/api/knowledge/collections/{name}")
async def delete_collection(
    name: str,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """コレクション削除（認証必須）."""
    mgr = _get_col_mgr()
    try:
        await mgr.delete_collection(name)
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return {"deleted": True, "collection_name": name}


# ---------------------------------------------------------------------------
# コレクション統計・テストクエリ
# ---------------------------------------------------------------------------


@router.get("/api/knowledge/collections/{name}/stats")
async def get_collection_stats(
    name: str,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """コレクション統計（認証必須）."""
    mgr = _get_col_mgr()
    try:
        stats = await mgr.get_collection_stats(name)
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc
    return stats


@router.post("/api/knowledge/collections/{name}/test-query")
async def test_query(
    name: str,
    request: TestQueryRequest,
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """テスト検索クエリ（認証必須・ロールベースアクセス制御）.

    ユーザーのロールに基づき、コレクション名に含まれる KB タイプ
    (internal/external/confidential) へのアクセスを制御する。
    """
    mgr = _get_col_mgr()
    role = _get_user_role(user)

    # コレクション名に KB タイプキーワードが含まれる場合、ロールベースで検証
    col_lower = name.lower()
    kb_types = ["internal", "external", "confidential"]
    for kb_type in kb_types:
        if kb_type in col_lower and not RAGAccessControl.check_kb_type_access(role, kb_type):
            raise HTTPException(
                status_code=403,
                detail={
                    "message": f"ロール '{role}' は '{kb_type}' コレクションへのアクセス権限がありません。",
                    "error_code": "kb_access_denied",
                },
            )

    try:
        rag_config = await mgr.build_rag_config(name)
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc

    from agentflow.services import RAGService

    rag_config.top_k = request.top_k
    service = RAGService(rag_config)
    result = await service.execute(action="query", question=request.query)

    if not result.success:
        raise HTTPException(
            status_code=500,
            detail={"message": result.error_message or "Query failed", "error_code": result.error_code},
        )
    return result.data


# ---------------------------------------------------------------------------
# ドキュメント管理エンドポイント
# ---------------------------------------------------------------------------


@router.post("/api/knowledge/collections/{name}/documents")
async def upload_document(
    name: str,
    file: UploadFile = File(...),
    user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ドキュメントアップロード（認証必須）."""
    doc_mgr = _get_doc_mgr()

    file_bytes = await file.read()
    filename = file.filename or "uploaded_file"

    try:
        record = await doc_mgr.upload_document(
            collection_name=name,
            file_content=file_bytes,
            filename=filename,
            user_id=user.user_id,
        )
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc

    return {"document": record.to_dict()}


@router.get("/api/knowledge/collections/{name}/documents")
async def list_documents(
    name: str,
    status: str | None = Query(None, description="ステータスフィルタ"),
    limit: int = Query(100, ge=1, le=500),
    offset: int = Query(0, ge=0),
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ドキュメント一覧（認証必須）."""
    doc_mgr = _get_doc_mgr()
    docs = await doc_mgr.list_documents(
        collection_name=name,
        status=status,
        limit=limit,
        offset=offset,
    )
    return {
        "total": len(docs),
        "documents": [d.to_dict() for d in docs],
    }


@router.get("/api/knowledge/collections/{name}/documents/{doc_id}")
async def get_document(
    name: str,
    doc_id: str,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ドキュメント詳細（認証必須）."""
    doc_mgr = _get_doc_mgr()
    doc = await doc_mgr.get_document(doc_id)
    if doc is None or doc.collection_name != name:
        raise HTTPException(status_code=404, detail=f"Document '{doc_id}' not found")
    return {"document": doc.to_dict()}


@router.post("/api/knowledge/collections/{name}/documents/{doc_id}/preview-chunks")
async def preview_chunks(
    name: str,
    doc_id: str,
    request: ChunkPreviewRequest | None = None,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """チャンクプレビュー（認証必須）."""
    doc_mgr = _get_doc_mgr()
    req = request or ChunkPreviewRequest()

    try:
        chunks = await doc_mgr.preview_chunks(
            document_id=doc_id,
            chunk_strategy=req.chunk_strategy,
            chunk_size=req.chunk_size,
            chunk_overlap=req.chunk_overlap,
        )
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc

    return {"total": len(chunks), "chunks": chunks}


@router.post("/api/knowledge/collections/{name}/documents/{doc_id}/index")
async def index_document(
    name: str,
    doc_id: str,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ドキュメントをインデックスに登録（認証必須）."""
    doc_mgr = _get_doc_mgr()

    try:
        record = await doc_mgr.index_document(doc_id)
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc

    return {"document": record.to_dict()}


@router.post("/api/knowledge/collections/{name}/documents/{doc_id}/reindex")
async def reindex_document(
    name: str,
    doc_id: str,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ドキュメント再インデックス（認証必須）."""
    doc_mgr = _get_doc_mgr()

    try:
        record = await doc_mgr.reindex_document(doc_id)
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc

    return {"document": record.to_dict()}


@router.delete("/api/knowledge/collections/{name}/documents/{doc_id}")
async def delete_document(
    name: str,
    doc_id: str,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ドキュメント削除（認証必須）."""
    doc_mgr = _get_doc_mgr()

    try:
        await doc_mgr.delete_document(doc_id)
    except ValueError as exc:
        raise HTTPException(status_code=404, detail=str(exc)) from exc

    return {"deleted": True, "document_id": doc_id}


@router.post("/api/knowledge/collections/{name}/reindex")
async def reindex_collection(
    name: str,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """コレクション全体の再インデックス（認証必須）."""
    doc_mgr = _get_doc_mgr()
    result = await doc_mgr.reindex_collection(name)
    return result
