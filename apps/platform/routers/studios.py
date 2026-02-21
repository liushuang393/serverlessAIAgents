"""Studios Router — 3 製品主線 API.

GET  /api/studios                               - Studio 一覧
GET  /api/studios/{studio}/templates            - テンプレート一覧
POST /api/studios/{studio}/runs                 - 実行登録
GET  /api/studios/{studio}/runs/{run_id}/artifacts - 成果物一覧
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Literal

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from apps.platform.services.studio_service import StudioService


router = APIRouter(prefix="/api/studios", tags=["studios"])

_studio_service: StudioService | None = None


class StudioRunRequest(BaseModel):
    """Studio 実行リクエスト."""

    template_id: str | None = Field(default=None, description="テンプレートID")
    app_name: str | None = Field(default=None, description="対象 app 名")
    data_sources: list[str] = Field(default_factory=list, description="データソース")
    permission_scopes: list[str] = Field(default_factory=list, description="権限スコープ")
    risk_level: Literal["low", "medium", "high"] = Field(
        default="medium",
        description="リスクレベル",
    )
    security_mode: Literal["read_only", "approval_required", "autonomous"] | None = Field(
        default=None,
        description="セキュリティモード",
    )
    input: dict[str, Any] = Field(default_factory=dict, description="任意入力")


def init_studio_services(service: StudioService) -> None:
    """サービス初期化."""
    global _studio_service
    _studio_service = service


def _get_studio_service() -> StudioService:
    if _studio_service is None:
        msg = "StudioService が未初期化です"
        raise RuntimeError(msg)
    return _studio_service


@router.get("")
async def list_studios() -> dict[str, Any]:
    """3 製品主線の一覧を返す."""
    service = _get_studio_service()
    studios = service.list_studios()
    return {"studios": studios, "total": len(studios)}


@router.get("/{studio}/templates")
async def list_templates(studio: str) -> dict[str, Any]:
    """Studio ごとのテンプレート一覧を返す."""
    service = _get_studio_service()
    try:
        templates = service.list_templates(studio)
    except ValueError as exc:
        raise HTTPException(
            status_code=404,
            detail={"message": str(exc), "error_code": "STUDIO_NOT_FOUND"},
        )
    return {"studio": studio, "templates": templates, "total": len(templates)}


@router.post("/{studio}/runs")
async def create_run(studio: str, request: StudioRunRequest) -> dict[str, Any]:
    """Studio 実行を登録し、run_id を返す."""
    service = _get_studio_service()
    try:
        result = service.create_run(studio, request.model_dump())
    except ValueError as exc:
        raise HTTPException(
            status_code=404,
            detail={"message": str(exc), "error_code": "STUDIO_NOT_FOUND"},
        )
    return result


@router.get("/{studio}/runs/{run_id}/artifacts")
async def list_run_artifacts(studio: str, run_id: str) -> dict[str, Any]:
    """Studio 実行の成果物を返す."""
    service = _get_studio_service()
    try:
        return service.get_run_artifacts(studio, run_id)
    except ValueError as exc:
        raise HTTPException(
            status_code=404,
            detail={"message": str(exc), "error_code": "STUDIO_NOT_FOUND"},
        )
    except KeyError as exc:
        raise HTTPException(
            status_code=404,
            detail={"message": str(exc), "error_code": "RUN_NOT_FOUND"},
        )
