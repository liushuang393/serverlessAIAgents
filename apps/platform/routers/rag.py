"""RAG Router — RAG 概要 API エンドポイント.

GET  /api/studios/framework/rag/overview   — RAG 機能概要
GET  /api/studios/framework/rag/strategies — チャンキング戦略一覧
GET  /api/studios/framework/rag/rerankers  — リランカー一覧
GET  /api/studios/framework/rag/retrieval-methods — 検索方式一覧
GET  /api/studios/framework/rag/patterns — 推奨 RAG パターン一覧
GET  /api/studios/framework/rag/apps       — RAG 使用 App 一覧
GET  /api/studios/framework/rag/apps/configs — 全 App の RAG 設定一覧
GET  /api/studios/framework/rag/apps/{app_name}/config — App 単位 RAG 設定
PATCH /api/studios/framework/rag/apps/{app_name}/config — App 単位 RAG 設定更新
GET  /api/studios/framework/rag/stats      — RAG 統計
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from fastapi import APIRouter, HTTPException


if TYPE_CHECKING:
    from apps.platform.schemas.rag_schemas import RAGConfigPatchRequest
    from apps.platform.services.rag_overview import RAGOverviewService


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
    """App 単位 RAG 設定を更新."""
    overview = _get_overview()
    try:
        return overview.update_app_config(
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


@router.get("/stats")
async def get_rag_stats() -> dict[str, Any]:
    """RAG 統計情報."""
    return _get_overview().stats()
