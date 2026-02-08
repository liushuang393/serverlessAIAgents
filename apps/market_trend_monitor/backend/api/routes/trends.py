# -*- coding: utf-8 -*-
"""トレンド/レポート/記事API."""

from datetime import datetime

from fastapi import APIRouter, HTTPException, Query

from apps.market_trend_monitor.backend.api.state import store

router = APIRouter(tags=["トレンド"])


def _to_frontend_report(report: dict, trends: list[dict]) -> dict:
    """フロントエンド互換のレポート形式に変換."""
    sections = report.get("sections", [])
    summary = sections[0]["content"] if sections else ""
    generated_at = report.get("generated_at") or datetime.now().isoformat()

    converted_sections: list[dict] = []
    for section in sections:
        charts = section.get("charts", [])
        converted_sections.append(
            {
                "title": section.get("title", ""),
                "content": section.get("content", ""),
                "chart_data": charts[0] if charts else None,
            }
        )

    return {
        "id": report.get("id", ""),
        "title": report.get("title", ""),
        "summary": summary,
        "sections": converted_sections,
        "trends": trends,
        "period_start": generated_at,
        "period_end": generated_at,
        "created_at": generated_at,
        "metadata": report.get("metadata", {}),
    }


@router.get("/api/trends")
async def list_trends(limit: int | None = Query(default=None, ge=1, le=500)) -> dict:
    """トレンド一覧を取得."""
    trends = await store.list_trends(limit=limit)
    return {"trends": trends, "total": len(trends)}


@router.get("/api/trends/{trend_id}")
async def get_trend(trend_id: str) -> dict:
    """トレンド詳細を取得."""
    trend = await store.get_trend(trend_id)
    if not trend:
        raise HTTPException(status_code=404, detail="Trend not found")
    return trend


@router.get("/api/articles")
async def list_articles(limit: int | None = Query(default=None, ge=1, le=500)) -> dict:
    """記事一覧を取得."""
    articles = await store.list_articles(limit=limit)
    return {"articles": articles, "total": len(articles)}


@router.get("/api/reports")
async def list_reports(limit: int | None = Query(default=None, ge=1, le=200)) -> dict:
    """レポート一覧を取得."""
    reports = await store.list_reports(limit=limit)
    trends = await store.list_trends()
    converted = [_to_frontend_report(report, trends) for report in reports]
    return {"reports": converted, "total": len(converted)}


@router.get("/api/reports/{report_id}")
async def get_report(report_id: str) -> dict:
    """レポート詳細を取得."""
    report = await store.get_report(report_id)
    if not report:
        raise HTTPException(status_code=404, detail="Report not found")
    trends = await store.list_trends()
    return _to_frontend_report(report, trends)
