"""RAG 関連ルーター.

/api/rag/query, /api/rag/add

ロールベースアクセス制御:
    KB タイプ (internal / external / confidential) へのアクセスを
    ユーザーのロールに基づいてフィルタリングする。
    - admin: internal, external, confidential
    - manager: internal, external
    - employee: internal, external
    - guest: external のみ
"""

from __future__ import annotations

import json
import logging
from typing import TYPE_CHECKING, Any

from fastapi import APIRouter, Depends, File, HTTPException, Query, UploadFile
from fastapi.responses import StreamingResponse
from pydantic import BaseModel, Field

from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.backend.config import KnowledgeBaseType, kb_registry
from apps.faq_system.backend.rag.parsers import FileParser
from apps.faq_system.routers.dependencies import (
    get_rag_ingestion_orchestrator,
    get_rag_service,
    is_rag_enabled,
)
from shared.rag.rag_access_control import RAGAccessControl
from shared.services import RAGConfig, RAGService


if TYPE_CHECKING:
    from apps.faq_system.backend.auth.models import UserInfo
else:
    UserInfo = Any

logger = logging.getLogger("faq.rag")


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
    async_mode: bool = Field(default=False, description="非同期実行（queued -> running）")


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


def _check_kb_access(user: Any, kb_type: KnowledgeBaseType) -> None:
    """ユーザーのロールが KB タイプへのアクセスを許可されているか検証.

    権限不足の場合は HTTP 403 を発生させる。
    """
    role = getattr(user, "role", "employee")
    if not RAGAccessControl.check_kb_type_access(role, kb_type.value):
        logger.warning(
            "KB access denied: user=%s role=%s kb_type=%s",
            getattr(user, "username", "unknown"),
            role,
            kb_type.value,
        )
        raise HTTPException(
            status_code=403,
            detail={
                "message": f"ロール '{role}' は '{kb_type.value}' KB へのアクセス権限がありません。",
                "error_code": "kb_access_denied",
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
    """RAG クエリ API (認証必須・ロールベースアクセス制御)."""
    if not is_rag_enabled():
        raise HTTPException(
            status_code=400,
            detail={"message": "RAG is disabled by app config", "error_code": "rag_disabled"},
        )

    _check_kb_access(_user, request.kb_type)

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
    """ドキュメント追加 API (認証必須・ロールベースアクセス制御)."""
    if not is_rag_enabled():
        raise HTTPException(
            status_code=400,
            detail={"message": "RAG is disabled by app config", "error_code": "rag_disabled"},
        )

    _check_kb_access(_user, request.kb_type)

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
    """ファイルアップロードによるドキュメント追加 API (認証必須・ロールベースアクセス制御)."""
    if not is_rag_enabled():
        raise HTTPException(
            status_code=400,
            detail={"message": "RAG is disabled by app config", "error_code": "rag_disabled"},
        )

    _check_kb_access(_user, kb_type)

    filename = file.filename or "uploaded_file"
    ext = filename.split(".")[-1].lower() if "." in filename else ""

    file_bytes = await file.read()

    try:
        parse_result = FileParser.parse_auto(file_bytes, filename)
        content = parse_result.content
    except ImportError as exc:
        raise HTTPException(
            status_code=400,
            detail={"message": f"パースライブラリが不足: {exc!s}", "error_code": "missing_dependency"},
        ) from exc
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

    # パーサーのメタデータも保存
    doc_metadata: dict[str, Any] = {"source": filename, "type": ext}
    doc_metadata.update(parse_result.metadata)

    result = await service.execute(
        action="add_document",
        content=content,
        metadata=doc_metadata,
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
    if request.async_mode:
        queued = await orchestrator.enqueue_ingest(
            source_ids=request.source_ids or None,
            dry_run=request.dry_run,
        )
        return {
            "accepted": True,
            "run": queued,
        }
    return await orchestrator.ingest(source_ids=request.source_ids or None, dry_run=request.dry_run)


@router.get("/api/rag/ingest/capabilities")
async def rag_ingest_capabilities(
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ingest source type の capability 一覧を返す."""
    orchestrator = get_rag_ingestion_orchestrator()
    return orchestrator.list_source_capabilities()


@router.get("/api/rag/ingest/runs")
async def rag_ingest_runs(
    limit: int = Query(20, ge=1, le=100),
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """直近 ingest 実行履歴を返す."""
    orchestrator = get_rag_ingestion_orchestrator()
    runs = await orchestrator.list_runs(limit=limit)
    return {"total": len(runs), "runs": runs}


@router.get("/api/rag/ingest/runs/{run_id}")
async def rag_ingest_run_detail(
    run_id: str,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ingest run 詳細を返す."""
    orchestrator = get_rag_ingestion_orchestrator()
    run = await orchestrator.get_run(run_id)
    if run is None:
        raise HTTPException(
            status_code=404,
            detail={"message": f"ingest run not found: {run_id}", "error_code": "ingest_run_not_found"},
        )
    return run


@router.get("/api/rag/ingest/runs/{run_id}/events", response_model=None)
async def rag_ingest_run_events(
    run_id: str,
    stream: bool = Query(False, description="true: SSE stream / false: snapshot"),
    limit: int = Query(200, ge=1, le=1000),
    _user: UserInfo = Depends(require_auth),
) -> Any:
    """ingest run イベントを返す（snapshot or SSE）。"""
    orchestrator = get_rag_ingestion_orchestrator()
    if not stream:
        events = await orchestrator.list_run_events(run_id, limit=limit)
        return {"run_id": run_id, "total": len(events), "events": events}

    async def event_generator() -> Any:
        connected = {"event_type": "connected", "run_id": run_id}
        yield f"event: connected\\ndata: {json.dumps(connected)}\\n\\n"
        backlog = await orchestrator.list_run_events(run_id, limit=min(limit, 200))
        for event in backlog:
            payload = json.dumps(event, ensure_ascii=False)
            yield f"event: {event.get('event_type', 'message')}\\ndata: {payload}\\n\\n"
        async for event in orchestrator.subscribe_run_events(run_id):
            payload = json.dumps(event, ensure_ascii=False)
            yield f"event: {event.get('event_type', 'message')}\\ndata: {payload}\\n\\n"

    return StreamingResponse(
        event_generator(),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",
        },
    )


# ---------------------------------------------------------------------------
# ナレッジベース ディレクトリロード API
# ---------------------------------------------------------------------------

_SUPPORTED_EXTENSIONS = {
    ".pdf",
    ".docx",
    ".doc",
    ".xlsx",
    ".xls",
    ".csv",
    ".md",
    ".txt",
    ".json",
    ".jsonl",
    ".html",
    ".htm",
}


class KBLoadDirectoryRequest(BaseModel):
    """ナレッジベースディレクトリロードリクエスト."""

    directory: str = Field(..., description="ロード対象ディレクトリパス")
    collection: str | None = Field(None, description="対象コレクション名（省略時はデフォルト）")
    kb_type: KnowledgeBaseType = Field(
        default=KnowledgeBaseType.INTERNAL,
        description="KB種別",
    )
    recursive: bool = Field(default=True, description="サブディレクトリを再帰スキャンするか")
    glob_pattern: str = Field(default="*", description="ファイル glob パターン")
    auto_group: bool = Field(default=True, description="同ディレクトリのファイルをグループ化するか")
    dry_run: bool = Field(default=False, description="スキャンのみ（実処理しない）")


@router.post("/api/kb/load-directory")
async def kb_load_directory(
    request: KBLoadDirectoryRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """ローカルディレクトリから知識ベースを一括ロード.

    指定ディレクトリ内のサポート対象ファイルをスキャン → パース → チャンク → インデックスする。
    同ディレクトリのファイルは自動的に document_group_id でグループ化される。
    """
    if not is_rag_enabled():
        raise HTTPException(
            status_code=400,
            detail={"message": "RAG is disabled by app config", "error_code": "rag_disabled"},
        )

    _check_kb_access(_user, request.kb_type)

    import hashlib
    import uuid
    from pathlib import Path as _Path

    dir_path = _Path(request.directory)
    if not dir_path.exists():
        raise HTTPException(
            status_code=400,
            detail={
                "message": f"ディレクトリが見つかりません: {request.directory}",
                "error_code": "directory_not_found",
            },
        )
    if not dir_path.is_dir():
        raise HTTPException(
            status_code=400,
            detail={"message": f"ディレクトリではありません: {request.directory}", "error_code": "not_a_directory"},
        )

    # ファイルをスキャン
    if request.recursive:
        files = list(dir_path.rglob(request.glob_pattern))
    else:
        files = list(dir_path.glob(request.glob_pattern))

    target_files = [f for f in files if f.is_file() and f.suffix.lower() in _SUPPORTED_EXTENSIONS]

    if request.dry_run:
        return {
            "status": "dry_run",
            "directory": str(dir_path),
            "total_files": len(target_files),
            "files": [
                {
                    "path": str(f),
                    "filename": f.name,
                    "size": f.stat().st_size,
                    "extension": f.suffix.lower(),
                }
                for f in target_files
            ],
            "supported_extensions": sorted(_SUPPORTED_EXTENSIONS),
        }

    if not target_files:
        return {
            "status": "no_files",
            "directory": str(dir_path),
            "total_files": 0,
            "message": "対象ファイルが見つかりませんでした",
            "supported_extensions": sorted(_SUPPORTED_EXTENSIONS),
        }

    # グループ ID 生成（同ディレクトリのファイル群を紐付け）
    group_id = uuid.uuid4().hex[:12] if request.auto_group else None

    target_collection = kb_registry.resolve_collection(
        kb_type=request.kb_type,
        explicit_collection=request.collection,
    )
    service = get_rag_service(collection=target_collection)

    results: list[dict[str, Any]] = []
    success_count = 0
    error_count = 0
    skip_count = 0
    seen_hashes: set[str] = set()

    for file_path in target_files:
        file_result: dict[str, Any] = {
            "filename": file_path.name,
            "path": str(file_path),
            "size": file_path.stat().st_size,
        }
        try:
            raw_bytes = file_path.read_bytes()

            # 重複検出
            content_hash = hashlib.sha256(raw_bytes).hexdigest()
            if content_hash in seen_hashes:
                file_result["status"] = "skipped"
                file_result["reason"] = "duplicate"
                skip_count += 1
                results.append(file_result)
                continue
            seen_hashes.add(content_hash)

            # パース
            parse_result = FileParser.parse_auto(raw_bytes, file_path.name)
            content = parse_result.content
            if not content.strip():
                file_result["status"] = "skipped"
                file_result["reason"] = "empty_content"
                skip_count += 1
                results.append(file_result)
                continue

            # メタデータ構築
            doc_metadata: dict[str, Any] = {
                "source": file_path.name,
                "source_path": str(file_path),
                "type": file_path.suffix.lower().lstrip("."),
                "content_hash": content_hash,
            }
            if group_id:
                doc_metadata["document_group_id"] = group_id
            doc_metadata.update(parse_result.metadata)

            # インデックス登録
            add_result = await service.execute(
                action="add_document",
                content=content,
                source=file_path.name,
                metadata=doc_metadata,
            )

            if add_result.success:
                file_result["status"] = "success"
                file_result["chunks"] = add_result.data.get("count", 0) if add_result.data else 0
                success_count += 1
            else:
                file_result["status"] = "error"
                file_result["error"] = add_result.error_message or "インデックス登録失敗"
                error_count += 1

        except Exception as exc:
            file_result["status"] = "error"
            file_result["error"] = str(exc)
            error_count += 1
            logger.warning("KB ディレクトリロード - ファイル処理失敗: %s: %s", file_path, exc)

        results.append(file_result)

    return {
        "status": "success" if error_count == 0 else ("partial" if success_count > 0 else "failed"),
        "directory": str(dir_path),
        "collection": target_collection,
        "document_group_id": group_id,
        "total_files": len(target_files),
        "success": success_count,
        "errors": error_count,
        "skipped": skip_count,
        "results": results,
    }


@router.get("/api/kb/supported-formats")
async def kb_supported_formats(
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """サポートされるドキュメント形式一覧."""
    formats = [
        {"extension": ".pdf", "description": "PDF ドキュメント", "parser": "pdfplumber/pypdf"},
        {"extension": ".docx", "description": "Microsoft Word", "parser": "python-docx"},
        {"extension": ".xlsx", "description": "Microsoft Excel", "parser": "openpyxl"},
        {"extension": ".csv", "description": "CSV データ", "parser": "csv"},
        {"extension": ".md", "description": "Markdown", "parser": "markdown"},
        {"extension": ".txt", "description": "プレーンテキスト", "parser": "text"},
        {"extension": ".json", "description": "JSON データ", "parser": "json"},
        {"extension": ".jsonl", "description": "JSON Lines", "parser": "json"},
        {"extension": ".html", "description": "HTML ページ", "parser": "html"},
    ]
    return {
        "formats": formats,
        "extensions": sorted(_SUPPORTED_EXTENSIONS),
    }
