# -*- coding: utf-8 -*-
"""証拠台帳API."""

import uuid
from datetime import datetime
from typing import Any

from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field

from apps.market_trend_monitor.backend.api.state import evidence_service
from apps.market_trend_monitor.backend.models import Article, ClaimLevel, SourceType

router = APIRouter(tags=["証拠台帳"])


class EvidenceCreateRequest(BaseModel):
    """証拠登録リクエスト."""

    title: str
    url: str
    source_type: str = Field(default="news")
    content: str = Field(default="")
    keywords: list[str] = Field(default_factory=list)
    metadata: dict[str, Any] = Field(default_factory=dict)


class ClaimCreateRequest(BaseModel):
    """主張作成リクエスト."""

    statement: str
    evidence_ids: list[str] = Field(default_factory=list)


@router.get("/api/evidence")
async def list_evidence(
    source_type: str | None = None,
    min_reliability: float = Query(default=0.0, ge=0.0, le=1.0),
) -> dict:
    """証拠一覧を取得."""
    if source_type:
        try:
            source_enum = SourceType(source_type)
        except ValueError as exc:
            raise HTTPException(status_code=400, detail="Invalid source_type") from exc
    else:
        source_enum = None
    evidences = await evidence_service.list_evidences(
        source_type=source_enum,
        min_reliability=min_reliability,
    )
    return {"evidences": [e.to_dict() for e in evidences], "total": len(evidences)}


@router.get("/api/evidence/{evidence_id}")
async def get_evidence(evidence_id: str) -> dict:
    """証拠詳細を取得."""
    evidence = await evidence_service.get_evidence(evidence_id)
    if not evidence:
        raise HTTPException(status_code=404, detail="Evidence not found")
    return evidence.to_dict()


@router.post("/api/evidence")
async def create_evidence(request: EvidenceCreateRequest) -> dict:
    """証拠を登録."""
    try:
        source_type = SourceType(request.source_type)
    except ValueError as exc:
        raise HTTPException(status_code=400, detail="Invalid source_type") from exc

    article = Article(
        id=str(uuid.uuid4()),
        title=request.title,
        url=request.url,
        source=source_type,
        published_at=datetime.now(),
        content=request.content,
        keywords=request.keywords,
        collected_at=datetime.now(),
        metadata=request.metadata,
    )
    evidence = await evidence_service.register_evidence_from_article(article)
    return evidence.to_dict()


@router.get("/api/claims")
async def list_claims(
    level: str | None = None,
    min_confidence: float = Query(default=0.0, ge=0.0, le=1.0),
) -> dict:
    """主張一覧を取得."""
    if level:
        try:
            level_enum = ClaimLevel(level)
        except ValueError as exc:
            raise HTTPException(status_code=400, detail="Invalid level") from exc
    else:
        level_enum = None
    claims = await evidence_service.list_claims(level=level_enum, min_confidence=min_confidence)
    return {"claims": [c.to_dict() for c in claims], "total": len(claims)}


@router.post("/api/claims")
async def create_claim(request: ClaimCreateRequest) -> dict:
    """主張を作成."""
    claim = await evidence_service.create_claim(
        statement=request.statement,
        evidence_ids=request.evidence_ids,
    )
    return claim.to_dict()


@router.get("/api/claims/{claim_id}/chain")
async def get_claim_chain(claim_id: str) -> dict:
    """主張の証拠チェーンを取得."""
    evidences = await evidence_service.get_evidence_chain(claim_id)
    return {"evidences": [e.to_dict() for e in evidences], "total": len(evidences)}
