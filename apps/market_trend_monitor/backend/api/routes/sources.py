# -*- coding: utf-8 -*-
"""情報源台帳API."""

from typing import Any

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field

from apps.market_trend_monitor.backend.api.state import source_registry_service
from apps.market_trend_monitor.backend.models import SourceRegistrySchema, SourceType

router = APIRouter(tags=["情報源"])


class SourceCreateRequest(BaseModel):
    """情報源登録リクエスト."""

    name: str
    source_type: str
    base_url: str
    reliability_score: float = Field(default=0.5, ge=0.0, le=1.0)
    enabled: bool = True
    terms_url: str | None = None
    metadata: dict[str, Any] = Field(default_factory=dict)


class SourceUpdateRequest(BaseModel):
    """情報源更新リクエスト."""

    enabled: bool | None = None
    reliability_score: float | None = Field(default=None, ge=0.0, le=1.0)
    terms_url: str | None = None
    metadata: dict[str, Any] | None = None


@router.get("/api/sources")
async def list_sources(enabled_only: bool = Query(default=False)) -> dict:
    """情報源一覧を取得."""
    sources = await source_registry_service.list_sources(enabled_only=enabled_only)
    return {
        "sources": [SourceRegistrySchema(**s.to_dict()).model_dump() for s in sources],
        "total": len(sources),
    }


@router.post("/api/sources")
async def create_source(request: SourceCreateRequest) -> dict:
    """情報源を登録."""
    try:
        source_type = SourceType(request.source_type)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail="Invalid source_type") from exc

    entry = await source_registry_service.register_source(
        name=request.name,
        source_type=source_type,
        base_url=request.base_url,
        reliability_score=request.reliability_score,
        enabled=request.enabled,
        terms_url=request.terms_url,
        metadata=request.metadata,
    )
    return SourceRegistrySchema(**entry.to_dict()).model_dump()


@router.put("/api/sources/{source_id}")
async def update_source(source_id: str, request: SourceUpdateRequest) -> dict:
    """情報源を更新."""
    entry = await source_registry_service.update_source(
        source_id,
        enabled=request.enabled,
        reliability_score=request.reliability_score,
        terms_url=request.terms_url,
        metadata=request.metadata,
    )
    if not entry:
        raise HTTPException(status_code=404, detail="Source not found")
    return SourceRegistrySchema(**entry.to_dict()).model_dump()
