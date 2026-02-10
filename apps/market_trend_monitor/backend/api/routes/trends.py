"""トレンド/レポート/記事API."""

import io
import logging
from datetime import datetime

from apps.market_trend_monitor.backend.api.state import store
from apps.market_trend_monitor.backend.services.report_export_service import ReportExportService
from fastapi import APIRouter, HTTPException, Query
from fastapi.responses import StreamingResponse


router = APIRouter(prefix="/api", tags=["トレンド"])
logger = logging.getLogger(__name__)
report_export_service = ReportExportService()


def _to_frontend_report(report: dict, fallback_trends: list[dict] | None = None) -> dict:
    """フロントエンド互換のレポート形式に変換."""
    sections = report.get("sections", [])
    summary = sections[0]["content"] if sections else ""
    generated_at = report.get("generated_at") or report.get("created_at") or datetime.now().isoformat()
    trend_snapshot = report.get("trend_snapshot", [])

    trends: list[dict]
    if isinstance(trend_snapshot, list):
        trends = trend_snapshot
    else:
        trends = fallback_trends or []

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


@router.get("/trends")
async def list_trends(limit: int | None = Query(default=None, ge=1, le=500)) -> dict:
    """トレンド一覧を取得."""
    trends = await store.list_trends(limit=limit)
    return {"trends": trends, "total": len(trends)}


@router.get("/trends/{trend_id}")
async def get_trend(trend_id: str) -> dict:
    """トレンド詳細を取得."""
    trend = await store.get_trend(trend_id)
    if not trend:
        raise HTTPException(status_code=404, detail="Trend not found")
    return trend


@router.get("/articles")
async def list_articles(limit: int | None = Query(default=None, ge=1, le=500)) -> dict:
    """記事一覧を取得."""
    articles = await store.list_articles(limit=limit)
    return {"articles": articles, "total": len(articles)}


@router.get("/reports")
async def list_reports(limit: int | None = Query(default=None, ge=1, le=200)) -> dict:
    """レポート一覧を取得."""
    reports = await store.list_reports(limit=limit)
    fallback_trends = await store.list_trends()
    converted = [_to_frontend_report(report, fallback_trends) for report in reports]
    return {"reports": converted, "total": len(converted)}


@router.get("/reports/{report_id}")
async def get_report(report_id: str) -> dict:
    """レポート詳細を取得."""
    report = await store.get_report(report_id)
    if not report:
        raise HTTPException(status_code=404, detail="Report not found")
    fallback_trends = await store.list_trends()
    return _to_frontend_report(report, fallback_trends)


def _sanitize_filename(value: str) -> str:
    """ファイル名に使えない文字を除去."""
    safe = "".join(ch if ch.isalnum() or ch in {"_", "-", "."} else "_" for ch in value)
    return safe.strip("_") or "report"


@router.get("/reports/{report_id}/export/pdf")
async def export_report_pdf(report_id: str) -> StreamingResponse:
    """レポートを PDF 形式でエクスポート."""
    report = await store.get_report(report_id)
    if not report:
        raise HTTPException(status_code=404, detail="Report not found")

    fallback_trends = await store.list_trends()
    export_payload = _to_frontend_report(report, fallback_trends)

    try:
        pdf_bytes = report_export_service.export_pdf(export_payload)
    except RuntimeError as exc:
        detail = str(exc)
        status_code = 503 if "未インストール" in detail else 500
        raise HTTPException(status_code=status_code, detail=detail) from exc
    except Exception as exc:
        logger.error("PDF出力で予期せぬ失敗: report_id=%s, error=%s", report_id, exc, exc_info=True)
        raise HTTPException(status_code=500, detail="PDF export failed") from exc

    filename = _sanitize_filename(f"market_trend_report_{report_id}.pdf")
    return StreamingResponse(
        io.BytesIO(pdf_bytes),
        media_type="application/pdf",
        headers={"Content-Disposition": f'attachment; filename="{filename}"'},
    )


@router.get("/reports/{report_id}/export/pptx")
async def export_report_pptx(report_id: str) -> StreamingResponse:
    """レポートを PPTX 形式でエクスポート."""
    report = await store.get_report(report_id)
    if not report:
        raise HTTPException(status_code=404, detail="Report not found")

    fallback_trends = await store.list_trends()
    export_payload = _to_frontend_report(report, fallback_trends)

    try:
        pptx_bytes = report_export_service.export_pptx(export_payload)
    except RuntimeError as exc:
        detail = str(exc)
        status_code = 503 if "未インストール" in detail else 500
        raise HTTPException(status_code=status_code, detail=detail) from exc
    except Exception as exc:
        logger.error(
            "PPTX出力で予期せぬ失敗: report_id=%s, error=%s",
            report_id,
            exc,
            exc_info=True,
        )
        raise HTTPException(status_code=500, detail="PPTX export failed") from exc

    filename = _sanitize_filename(f"market_trend_report_{report_id}.pptx")
    return StreamingResponse(
        io.BytesIO(pptx_bytes),
        media_type="application/vnd.openxmlformats-officedocument.presentationml.presentation",
        headers={"Content-Disposition": f'attachment; filename="{filename}"'},
    )
