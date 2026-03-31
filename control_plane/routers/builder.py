"""Builder drafts router."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any, Literal

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from control_plane.services.builder_draft_store import BuilderDraftStore


router = APIRouter(prefix="/api/studios/framework/builder", tags=["builder"])

_draft_store: BuilderDraftStore | None = None


class BuilderDraftPayload(BaseModel):
    """Builder draft payload."""

    name: str = Field(default="", description="表示名")
    template_id: str = Field(default="", description="テンプレート ID")
    goal: str = Field(default="", description="達成したい目標")
    spec_kind: Literal["agent", "system"] = Field(default="agent", description="仕様種別")
    spec: dict[str, Any] = Field(default_factory=dict, description="builder spec")
    generated_files: dict[str, str] = Field(default_factory=dict, description="生成済みファイル")
    status: Literal["draft", "validated", "generated", "materialized", "failed"] = Field(
        default="draft",
        description="進行状態",
    )


def init_builder_services(draft_store: BuilderDraftStore) -> None:
    """サービス初期化."""
    global _draft_store
    _draft_store = draft_store


def _get_store() -> BuilderDraftStore:
    if _draft_store is None:
        msg = "BuilderDraftStore が未初期化です"
        raise RuntimeError(msg)
    return _draft_store


@router.get("/drafts")
async def list_drafts() -> dict[str, Any]:
    store = _get_store()
    drafts = store.list_drafts()
    return {"drafts": drafts, "total": len(drafts)}


@router.post("/drafts")
async def create_draft(payload: BuilderDraftPayload) -> dict[str, Any]:
    draft = _get_store().create_draft(payload.model_dump())
    return {"draft": draft}


@router.get("/drafts/{draft_id}")
async def get_draft(draft_id: str) -> dict[str, Any]:
    draft = _get_store().get_draft(draft_id)
    if draft is None:
        raise HTTPException(
            status_code=404,
            detail={"message": f"Draft not found: {draft_id}", "error_code": "DRAFT_NOT_FOUND"},
        )
    return {"draft": draft}


@router.patch("/drafts/{draft_id}")
async def update_draft(draft_id: str, payload: BuilderDraftPayload) -> dict[str, Any]:
    try:
        draft = _get_store().update_draft(draft_id, payload.model_dump())
    except KeyError as exc:
        raise HTTPException(
            status_code=404,
            detail={"message": str(exc), "error_code": "DRAFT_NOT_FOUND"},
        ) from exc
    return {"draft": draft}


@router.delete("/drafts/{draft_id}")
async def delete_draft(draft_id: str) -> dict[str, Any]:
    try:
        _get_store().delete_draft(draft_id)
    except KeyError as exc:
        raise HTTPException(
            status_code=404,
            detail={"message": str(exc), "error_code": "DRAFT_NOT_FOUND"},
        ) from exc
    return {"success": True, "draft_id": draft_id}


__all__ = ["init_builder_services", "router"]
