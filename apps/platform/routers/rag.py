"""RAG Router — RAG 概要 API エンドポイント.

GET  /api/studios/framework/rag/overview   — RAG 機能概要
GET  /api/studios/framework/rag/strategies — チャンキング戦略一覧
GET  /api/studios/framework/rag/rerankers  — リランカー一覧
GET  /api/studios/framework/rag/retrieval-methods — 検索方式一覧
GET  /api/studios/framework/rag/patterns — 推奨 RAG パターン一覧
GET  /api/studios/framework/rag/apps       — RAG 使用 App 一覧
GET  /api/studios/framework/rag/apps/configs — 全 App の RAG 設定一覧
GET  /api/studios/framework/rag/apps/{app_name}/config — App 単位 RAG 設定
PATCH /api/studios/framework/rag/apps/{app_name}/config — App 単位 RAG 設定更新（イベント発火付き）
GET  /api/studios/framework/rag/events     — SSE: RAG 設定変更イベント（?app=APP_NAME）
GET  /api/studios/framework/rag/stats      — RAG 統計
"""

from __future__ import annotations

import json
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

import httpx
from apps.platform.schemas.rag_schemas import RAGConfigPatchRequest  # noqa: TC002
from apps.platform.services.rag_config_store import RagConfigStore, get_rag_config_store
from apps.platform.services.rag_overview import RAGOverviewService  # noqa: TC002
from fastapi import APIRouter, HTTPException, Query
from fastapi.responses import StreamingResponse


router = APIRouter(prefix="/api/studios/framework/rag", tags=["rag"])

# モジュールレベルのシングルトン（main.py で初期化）
_overview: RAGOverviewService | None = None


def init_rag_services(overview: RAGOverviewService) -> None:
    """サービスインスタンスを設定.

    Args:
        overview: RAG 概要サービス
    """
    global _overview
    _overview = overview


def _get_overview() -> RAGOverviewService:
    """RAGOverviewService を取得（未初期化時はエラー）."""
    if _overview is None:
        msg = "RAGOverviewService が未初期化です"
        raise RuntimeError(msg)
    return _overview


def _try_get_store() -> RagConfigStore | None:
    """RagConfigStore を取得（未初期化でも None を返す）."""
    try:
        return get_rag_config_store()
    except RuntimeError:
        return None


def _build_contracts_rag_payload(rag: dict[str, Any]) -> dict[str, Any]:
    """App RAG 表示形式から contracts.rag 形式へ正規化."""
    enabled = bool(rag.get("enabled", False))
    collection = rag.get("vector_collection")
    collections = [collection] if isinstance(collection, str) and collection.strip() else []
    return {
        "enabled": enabled,
        "pattern": rag.get("pattern"),
        "provider": rag.get("vector_provider") if enabled else None,
        "collections": collections if enabled else [],
        "data_sources": rag.get("data_sources", []),
        "chunk_strategy": rag.get("chunk_strategy", "recursive"),
        "chunk_size": rag.get("chunk_size", 800),
        "chunk_overlap": rag.get("chunk_overlap", 120),
        "retrieval_method": rag.get("retrieval_method", "hybrid"),
        "embedding_model": rag.get("embedding_model"),
        "rerank_model": rag.get("reranker"),
        "default_top_k": rag.get("top_k", 5),
        "score_threshold": rag.get("score_threshold"),
        "indexing_schedule": rag.get("indexing_schedule"),
    }


def _resolve_backend_url(config_path: str) -> str | None:
    path = Path(config_path)
    if not path.is_file():
        return None
    try:
        payload = json.loads(path.read_text(encoding="utf-8"))
    except (OSError, json.JSONDecodeError):
        return None

    runtime = payload.get("runtime")
    runtime_data = runtime if isinstance(runtime, dict) else {}
    urls = runtime_data.get("urls")
    runtime_urls = urls if isinstance(urls, dict) else {}
    backend_url = runtime_urls.get("backend")
    if isinstance(backend_url, str) and backend_url.strip():
        return backend_url.strip()

    ports = payload.get("ports")
    ports_data = ports if isinstance(ports, dict) else {}
    api_port = ports_data.get("api")
    if isinstance(api_port, int) and api_port > 0:
        return f"http://127.0.0.1:{api_port}"
    return None


# ------------------------------------------------------------------
# エンドポイント
# ------------------------------------------------------------------


@router.get("/overview")
async def get_rag_overview() -> dict[str, Any]:
    """RAG 機能概要."""
    return _get_overview().get_overview()


@router.get("/strategies")
async def list_strategies() -> dict[str, Any]:
    """チャンキング戦略一覧."""
    overview = _get_overview()
    strategies = overview.list_strategies()
    return {"strategies": strategies, "total": len(strategies)}


@router.get("/rerankers")
async def list_rerankers() -> dict[str, Any]:
    """リランカー一覧."""
    overview = _get_overview()
    rerankers = overview.list_rerankers()
    return {"rerankers": rerankers, "total": len(rerankers)}


@router.get("/retrieval-methods")
async def list_retrieval_methods() -> dict[str, Any]:
    """検索方式一覧."""
    overview = _get_overview()
    methods = overview.list_retrieval_methods()
    return {"methods": methods, "total": len(methods)}


@router.get("/patterns")
async def list_patterns() -> dict[str, Any]:
    """推奨 RAG パターン一覧."""
    overview = _get_overview()
    patterns = overview.list_patterns()
    return {"patterns": patterns, "total": len(patterns)}


@router.get("/apps")
async def list_rag_apps() -> dict[str, Any]:
    """RAG を使用している App の一覧."""
    overview = _get_overview()
    apps = overview.apps_using_rag()
    return {"apps": apps, "total": len(apps)}


@router.get("/apps/configs")
async def list_rag_app_configs() -> dict[str, Any]:
    """全 App の RAG 設定一覧."""
    overview = _get_overview()
    apps = overview.list_app_configs()
    return {"apps": apps, "total": len(apps)}


@router.get("/apps/{app_name}/config")
async def get_rag_app_config(app_name: str) -> dict[str, Any]:
    """App 単位 RAG 設定を取得."""
    overview = _get_overview()
    try:
        return overview.get_app_config(app_name)
    except KeyError:
        raise HTTPException(
            status_code=404,
            detail={
                "message": f"App not found: {app_name}",
                "error_code": "APP_NOT_FOUND",
            },
        )


@router.patch("/apps/{app_name}/config")
async def patch_rag_app_config(
    app_name: str,
    patch: RAGConfigPatchRequest,
) -> dict[str, Any]:
    """App 単位 RAG 設定を更新し、実行中の App にイベントを発火."""
    overview = _get_overview()
    try:
        result = overview.update_app_config(
            app_name,
            patch.model_dump(exclude_none=True),
        )
    except KeyError:
        raise HTTPException(
            status_code=404,
            detail={
                "message": f"App not found: {app_name}",
                "error_code": "APP_NOT_FOUND",
            },
        )
    except ValueError as exc:
        raise HTTPException(
            status_code=400,
            detail={
                "message": str(exc),
                "error_code": "RAG_CONFIG_INVALID",
            },
        )

    # RagConfigStore にイベントを発火（ConfigWatcher 経由でホットリロード）
    store = _try_get_store()
    contracts_rag = _build_contracts_rag_payload(result.get("rag", {}))
    now = datetime.now(tz=UTC)
    updated_at = now.isoformat()
    config_version = str(int(now.timestamp() * 1000))
    hot_apply = {"mode": "hot", "applied": False, "subscriber_count": 0}
    if store is not None:
        new_rag_config: dict[str, Any] = result.get("rag", {})
        subscriber_count = await store.fire_config_change(
            app_name,
            contracts_rag=contracts_rag,
            rag_config=new_rag_config,
            config_version=config_version,
            updated_at=updated_at,
        )
        hot_apply = {"mode": "hot", "applied": subscriber_count > 0, "subscriber_count": subscriber_count}

    result["contracts_rag"] = contracts_rag
    result["config_version"] = config_version
    result["updated_at"] = updated_at
    result["hot_apply"] = hot_apply

    return result


@router.get("/events")
async def rag_config_events(
    app: str = Query(..., description="購読対象のアプリ名"),
) -> StreamingResponse:
    """RAG 設定変更 SSE イベントストリーム.

    実行中の App（ConfigWatcher）が設定変更を受け取るための SSE エンドポイント。
    接続を維持し、PATCH /apps/{app}/config が呼ばれるたびにイベントを送信する。

    Args:
        app: 購読対象アプリ名（例: faq_system）

    Returns:
        Server-Sent Events ストリーム（text/event-stream）
    """
    store = _try_get_store()
    if store is None:
        raise HTTPException(
            status_code=503,
            detail={
                "message": "RagConfigStore が未初期化です",
                "error_code": "STORE_NOT_INITIALIZED",
            },
        )

    async def event_generator() -> Any:
        """SSE イベントを生成するジェネレーター."""
        # 接続確立イベントを送信
        connected_event = {
            "event_type": "connected",
            "app_name": app,
        }
        yield f"event: connected\ndata: {json.dumps(connected_event)}\n\n"

        # 設定変更イベントを購読してストリーム送信
        async for event in store.subscribe(app):
            event_type = event.get("event_type", "message")
            data = json.dumps(event)
            yield f"event: {event_type}\ndata: {data}\n\n"

    return StreamingResponse(
        event_generator(),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",
        },
    )


@router.get("/stats")
async def get_rag_stats() -> dict[str, Any]:
    """RAG 統計情報."""
    return _get_overview().stats()


@router.get("/apps/{app_name}/ingest/runs")
async def list_app_ingest_runs(
    app_name: str,
    limit: int = Query(20, ge=1, le=100),
) -> dict[str, Any]:
    """指定 App の ingest run 一覧を取得（FAQ API proxy）."""
    overview = _get_overview()
    try:
        app_config = overview.get_app_config(app_name)
    except KeyError:
        raise HTTPException(
            status_code=404,
            detail={"message": f"App not found: {app_name}", "error_code": "APP_NOT_FOUND"},
        )

    backend_url = _resolve_backend_url(str(app_config.get("config_path", "")))
    if backend_url is None:
        raise HTTPException(
            status_code=400,
            detail={
                "message": f"runtime.urls.backend or ports.api is not configured for app: {app_name}",
                "error_code": "RUNTIME_BACKEND_URL_NOT_FOUND",
            },
        )

    endpoint = f"{backend_url.rstrip('/')}/api/rag/ingest/runs"
    try:
        async with httpx.AsyncClient(timeout=10.0) as client:
            response = await client.get(endpoint, params={"limit": limit})
    except httpx.HTTPError as exc:
        raise HTTPException(
            status_code=502,
            detail={"message": f"Failed to proxy ingest runs: {exc}", "error_code": "INGEST_PROXY_FAILED"},
        ) from exc

    if response.status_code >= 400:
        raise HTTPException(
            status_code=502,
            detail={
                "message": f"FAQ ingest API returned {response.status_code}",
                "error_code": "INGEST_PROXY_BAD_STATUS",
            },
        )

    payload = response.json()
    return payload if isinstance(payload, dict) else {"total": 0, "runs": []}
