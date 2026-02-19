"""競合追跡・成熟度・レポートAPI."""

import logging
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.agents import CollectorAgent
from apps.market_trend_monitor.backend.api.state import store
from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.db import init_db
from apps.market_trend_monitor.backend.db.models import AppSettingModel
from apps.market_trend_monitor.backend.db.session import async_session
from apps.market_trend_monitor.backend.models import Article, SourceType
from fastapi import APIRouter, HTTPException, Query
from pydantic import BaseModel, Field


router = APIRouter(prefix="/api", tags=["戦略機能"])
logger = logging.getLogger(__name__)
COMPETITOR_CONFIG_KEY = "competitor_watchlist_config_v1"

# サービスインスタンスは遅延初期化
_competitor_agent = None
_maturity_service = None
_stakeholder_service = None


def _get_competitor_agent():
    global _competitor_agent
    if _competitor_agent is None:
        from apps.market_trend_monitor.backend.agents.competitor_tracking_agent import (
            CompetitorTrackingAgent,
        )

        _competitor_agent = CompetitorTrackingAgent()
    return _competitor_agent


def _normalize_alias_payload(raw_aliases: Any) -> dict[str, list[str]]:
    """永続化payloadを alias辞書へ正規化."""
    normalized: dict[str, list[str]] = {}
    if not isinstance(raw_aliases, dict):
        return normalized

    for raw_name, raw_values in raw_aliases.items():
        name = str(raw_name).strip()
        if not name:
            continue
        values: list[str] = []
        if isinstance(raw_values, list):
            values = [str(item).strip() for item in raw_values if str(item).strip()]
        normalized[name] = values
    return normalized


async def _load_competitor_config() -> tuple[list[str], dict[str, list[str]]]:
    """DBから競合設定をロード."""
    await init_db()
    async with async_session() as session:
        row = await session.get(AppSettingModel, COMPETITOR_CONFIG_KEY)
        if row is None or not isinstance(row.value, dict):
            return [], {}

        payload = row.value
        competitors_raw = payload.get("competitors", [])
        competitors = (
            [str(item).strip() for item in competitors_raw if str(item).strip()]
            if isinstance(competitors_raw, list)
            else []
        )
        aliases = _normalize_alias_payload(payload.get("competitor_aliases", {}))
        return competitors, aliases


async def _save_competitor_config(
    competitors: list[str],
    competitor_aliases: dict[str, list[str]],
) -> None:
    """競合設定をDBへ保存."""
    await init_db()
    payload = {
        "competitors": list(competitors),
        "competitor_aliases": {key: list(values) for key, values in competitor_aliases.items()},
    }
    async with async_session() as session:
        row = await session.get(AppSettingModel, COMPETITOR_CONFIG_KEY)
        if row is None:
            row = AppSettingModel(key=COMPETITOR_CONFIG_KEY, value=payload)
            session.add(row)
        else:
            row.value = payload
            row.updated_at = datetime.now()
        await session.commit()


async def _ensure_persisted_competitor_config(agent: Any) -> None:
    """DB設定をエージェントへ反映."""
    competitors, competitor_aliases = await _load_competitor_config()
    if competitors:
        agent.set_competitors(competitors)
    if competitor_aliases:
        agent.set_competitor_aliases(competitor_aliases)


def _parse_datetime(raw: str | None) -> datetime:
    """ISO文字列を安全にdatetimeへ変換."""
    if not raw:
        return datetime.now()
    try:
        return datetime.fromisoformat(raw.replace("Z", "+00:00"))
    except ValueError:
        return datetime.now()


def _parse_source(raw: str | None) -> SourceType:
    """文字列をSourceTypeに変換."""
    if not raw:
        return SourceType.NEWS
    try:
        return SourceType(str(raw).lower())
    except ValueError:
        return SourceType.NEWS


def _to_article(payload: dict[str, Any]) -> Article:
    """辞書データをArticleに変換."""
    raw_keywords = payload.get("keywords", [])
    keywords = list(raw_keywords) if isinstance(raw_keywords, list) else []
    raw_metadata = payload.get("metadata", {})
    metadata = dict(raw_metadata) if isinstance(raw_metadata, dict) else {}
    return Article(
        id=str(payload.get("id", "")),
        title=str(payload.get("title", "")),
        url=str(payload.get("url", "")),
        source=_parse_source(payload.get("source")),
        published_at=_parse_datetime(str(payload.get("published_at", ""))),
        content=str(payload.get("content", "")),
        keywords=keywords,
        collected_at=_parse_datetime(str(payload.get("collected_at", ""))),
        metadata=metadata,
    )


async def _load_articles_for_competitor_tracking(limit: int) -> list[Article]:
    """競合分析用にストア記事をロード."""
    raw_articles = await store.list_articles(limit=limit)
    articles: list[Article] = []
    for item in raw_articles:
        if not isinstance(item, dict):
            continue
        articles.append(_to_article(item))
    return articles


def _build_competitor_focus_keywords(
    competitors: list[str],
    competitor_aliases: dict[str, list[str]],
    max_keywords: int,
) -> list[str]:
    """競合名ベースの収集キーワードを生成."""
    queries: list[str] = []
    seen: set[str] = set()
    suffixes = [
        "COBOL to Java migration",
        "mainframe modernization",
        "legacy modernization",
        "COBOL Java リライト",
    ]

    for competitor in competitors:
        aliases = competitor_aliases.get(competitor, [])
        candidates = [competitor, *aliases[:2]]
        for candidate in candidates:
            name = candidate.strip()
            if not name:
                continue
            for suffix in suffixes:
                query = f"{name} {suffix}".strip()
                key = query.casefold()
                if key in seen:
                    continue
                seen.add(key)
                queries.append(query)
                if len(queries) >= max_keywords:
                    return queries
    return queries


async def _collect_articles_for_watchlist_focus(
    keywords: list[str],
    sources: list[str],
) -> int:
    """競合フォーカスで追加収集を実行し、ストアへ反映."""
    if not keywords:
        return 0

    collector_agent = CollectorAgent()
    await collector_agent.initialize()
    try:
        collector_result = await collector_agent.run({"keywords": keywords, "sources": sources})
    finally:
        await collector_agent.cleanup()

    if not isinstance(collector_result, dict):
        return 0
    articles = collector_result.get("articles", [])
    count = len(articles) if isinstance(articles, list) else 0
    if count > 0:
        await store.update_from_flow({"collector": collector_result})
    return count


def _get_maturity_service():
    global _maturity_service
    if _maturity_service is None:
        from apps.market_trend_monitor.backend.services.maturity_assessment_service import (
            MaturityAssessmentService,
        )

        _maturity_service = MaturityAssessmentService()
    return _maturity_service


def _get_stakeholder_service():
    global _stakeholder_service
    if _stakeholder_service is None:
        from apps.market_trend_monitor.backend.services.stakeholder_report_service import (
            StakeholderReportService,
        )

        _stakeholder_service = StakeholderReportService()
    return _stakeholder_service


# ============================================================
# 競合追跡エンドポイント
# ============================================================


@router.get("/competitors")
async def list_competitors() -> dict:
    """競合プロファイル一覧を取得."""
    agent = _get_competitor_agent()
    await _ensure_persisted_competitor_config(agent)
    profiles = agent.list_profiles()
    auto_discovered = False
    source_articles = 0

    # 画面初回表示時に空なら自動発見を試行
    if not profiles:
        articles = await _load_articles_for_competitor_tracking(limit=400)
        source_articles = len(articles)
        if articles:
            profiles = await agent.track_competitors(articles, include_unmatched=False)
            auto_discovered = True

    detected_count = sum(
        1 for profile in profiles if int(profile.metadata.get("article_count", 0)) > 0
    )
    watchlist = agent.get_competitors()
    detected_competitors = [
        profile.name for profile in profiles if int(profile.metadata.get("article_count", 0)) > 0
    ]
    undetected_watchlist = [name for name in watchlist if name not in set(detected_competitors)]
    return {
        "competitors": [p.to_dict() for p in profiles],
        "total": len(profiles),
        "detected_count": detected_count,
        "watchlist_count": len(watchlist),
        "detected_competitors": detected_competitors,
        "undetected_watchlist": undetected_watchlist,
        "auto_discovered": auto_discovered,
        "source_articles": source_articles,
        "scope": "stored_articles_watchlist_match",
    }


class CompetitorConfigRequest(BaseModel):
    """競合設定リクエスト."""

    competitors: list[str] = Field(default_factory=list)
    competitor_aliases: dict[str, list[str]] = Field(default_factory=dict)


class PositioningRequest(BaseModel):
    """ポジショニング比較リクエスト."""

    our_strengths: list[str] = Field(min_length=1)


@router.post("/competitors/positioning")
async def compare_positioning(request: PositioningRequest) -> dict:
    """市場ポジショニング比較."""
    agent = _get_competitor_agent()
    result = await agent.compare_positioning(request.our_strengths)
    return result


class CompetitorDiscoverRequest(BaseModel):
    """競合自動発見リクエスト."""

    include_unmatched: bool = Field(default=False)
    limit: int = Field(default=400, ge=1, le=2000)
    refresh_with_watchlist: bool = Field(default=True)
    max_focus_keywords: int = Field(default=24, ge=1, le=80)


@router.get("/competitors/config")
async def get_competitor_config() -> dict:
    """競合追跡設定を取得."""
    agent = _get_competitor_agent()
    await _ensure_persisted_competitor_config(agent)
    competitors = agent.get_competitors()
    competitor_aliases = agent.get_competitor_aliases()
    return {
        "competitors": competitors,
        "competitor_aliases": competitor_aliases,
        "watchlist_count": len(competitors),
    }


@router.put("/competitors/config")
async def update_competitor_config(request: CompetitorConfigRequest) -> dict:
    """競合追跡設定を更新."""
    agent = _get_competitor_agent()
    competitors = agent.set_competitors(request.competitors)
    agent.set_competitor_aliases(request.competitor_aliases)
    competitor_aliases = agent.get_competitor_aliases()
    await _save_competitor_config(competitors, competitor_aliases)
    return {
        "status": "success",
        "competitors": competitors,
        "competitor_aliases": competitor_aliases,
        "watchlist_count": len(competitors),
    }


@router.post("/competitors/discover")
async def discover_competitors(request: CompetitorDiscoverRequest) -> dict:
    """最新記事から競合を自動発見."""
    agent = _get_competitor_agent()
    await _ensure_persisted_competitor_config(agent)
    watchlist = agent.get_competitors()
    competitor_aliases = agent.get_competitor_aliases()
    focused_collected_articles = 0
    focus_collection_error: str | None = None

    if request.refresh_with_watchlist and watchlist:
        focus_keywords = _build_competitor_focus_keywords(
            competitors=watchlist,
            competitor_aliases=competitor_aliases,
            max_keywords=request.max_focus_keywords,
        )
        focus_sources = list(config.collector.sources)
        if "official_site" not in focus_sources:
            focus_sources.append("official_site")
        try:
            focused_collected_articles = await _collect_articles_for_watchlist_focus(
                keywords=focus_keywords,
                sources=focus_sources,
            )
        except Exception as exc:
            focus_collection_error = str(exc)
            logger.warning("競合フォーカス収集に失敗。既存記事で継続します: %s", exc)

    articles = await _load_articles_for_competitor_tracking(limit=request.limit)
    if not articles:
        message = "分析対象記事がありません。先にデータ収集を実行してください。"
        if focus_collection_error:
            message = f"{message} 追加収集は失敗しましたが既存データで継続しました。"
        return {
            "status": "empty",
            "message": message,
            "competitors": [],
            "total": 0,
            "detected_count": 0,
            "source_articles": 0,
            "watchlist_count": len(watchlist),
            "detected_competitors": [],
            "undetected_watchlist": watchlist,
            "focused_collected_articles": focused_collected_articles,
            "focus_collection_error": focus_collection_error,
            "scope": "stored_articles_watchlist_match",
        }

    profiles = await agent.track_competitors(
        articles,
        include_unmatched=request.include_unmatched,
    )
    detected_competitors = [
        profile.name for profile in profiles if int(profile.metadata.get("article_count", 0)) > 0
    ]
    undetected_watchlist = [name for name in watchlist if name not in set(detected_competitors)]
    detected_count = sum(
        1 for profile in profiles if int(profile.metadata.get("article_count", 0)) > 0
    )
    status = "success" if detected_count > 0 else "not_found"
    if detected_count > 0:
        detected_preview = ", ".join(detected_competitors[:5])
        suffix = " ..." if len(detected_competitors) > 5 else ""
        message = (
            f"在已采集的 {len(articles)} 条文章中，命中 {detected_count}/{len(watchlist)} 个watchlist竞合"
            f"（{detected_preview}{suffix}）。"
        )
    else:
        message = (
            f"在已采集的 {len(articles)} 条文章中未命中watchlist竞合。"
            "请补充别名词典或先执行竞合导向的数据收集。"
        )
    if focused_collected_articles > 0:
        message = f"{message} 本次按竞合关键词新增抓取 {focused_collected_articles} 条。"
    if focus_collection_error:
        message = f"{message} 追加収集で一部失敗がありました（既存記事で分析を継続）。"

    return {
        "status": status,
        "message": message,
        "competitors": [p.to_dict() for p in profiles],
        "total": len(profiles),
        "detected_count": detected_count,
        "watchlist_count": len(watchlist),
        "detected_competitors": detected_competitors,
        "undetected_watchlist": undetected_watchlist,
        "source_articles": len(articles),
        "focused_collected_articles": focused_collected_articles,
        "focus_collection_error": focus_collection_error,
        "scope": "stored_articles_watchlist_match",
    }


@router.get("/competitors/{name}")
async def get_competitor(name: str) -> dict:
    """競合プロファイルを取得."""
    agent = _get_competitor_agent()
    profile = agent.get_profile(name)
    if not profile:
        raise HTTPException(status_code=404, detail="Competitor not found")
    return profile.to_dict()


# ============================================================
# 成熟度エンドポイント
# ============================================================


@router.get("/maturity")
async def list_maturity() -> dict:
    """技術成熟度ランドスケープを取得."""
    service = _get_maturity_service()
    assessments = service.list_assessments()
    return {
        "assessments": [a.to_dict() for a in assessments],
        "total": len(assessments),
    }


@router.get("/maturity/{technology}")
async def get_maturity(technology: str) -> dict:
    """技術成熟度を取得."""
    service = _get_maturity_service()
    assessment = service.get_assessment(technology)
    if not assessment:
        raise HTTPException(status_code=404, detail="Technology assessment not found")
    return assessment.to_dict()


# ============================================================
# ステークホルダーレポートエンドポイント
# ============================================================


class ReportGenerateRequest(BaseModel):
    """レポート生成リクエスト."""

    report_type: str = Field(default="executive", pattern="^(executive|technical|weekly_digest)$")
    topic: str | None = None
    period_days: int = Field(default=7, ge=1, le=90)


@router.get("/stakeholder-reports")
async def list_stakeholder_reports(
    report_type: str | None = Query(default=None),
) -> dict:
    """ステークホルダーレポート一覧を取得."""
    service = _get_stakeholder_service()
    reports = service.list_reports(report_type=report_type)
    return {
        "reports": [r.to_dict() for r in reports],
        "total": len(reports),
    }


@router.get("/stakeholder-reports/{report_id}")
async def get_stakeholder_report(report_id: str) -> dict:
    """ステークホルダーレポートを取得."""
    service = _get_stakeholder_service()
    report = service.get_report(report_id)
    if not report:
        raise HTTPException(status_code=404, detail="Report not found")
    return report.to_dict()
