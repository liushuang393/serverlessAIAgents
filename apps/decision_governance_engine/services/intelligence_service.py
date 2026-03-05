"""外部情報採集サービス（Intelligence Service）.

目的:
    外部情報ソースから情報を収集し、証拠チェーン（Evidence Chain）を構築する。

機能:
    - 複数ソースからの情報取得（DuckDuckGo/SerpAPI/Bing）
    - 去重・キャッシュ
    - 信頼度スコアリング
    - 証拠-結論マッピング
"""

from __future__ import annotations

import asyncio
import hashlib
import logging
import os
import re
from datetime import datetime, timedelta
from urllib.parse import unquote, urlparse
from uuid import uuid4

from apps.decision_governance_engine.schemas.contract_schemas import (
    EvidenceItem,
    EvidenceReliability,
)
from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)

try:
    import httpx
except ImportError:  # pragma: no cover - optional dependency
    httpx = None

try:
    from duckduckgo_search import DDGS
except ImportError:  # pragma: no cover - optional dependency
    DDGS = None


# 設定値
MAX_SOURCES_FAST = 5
MAX_SOURCES_STANDARD = 15
MAX_SOURCES_AUDIT = 30
CACHE_TTL_HOURS = 24
CACHE_STALE_HOURS = 24 * 7
REQUEST_TIMEOUT_SECONDS = 10
_REAL_SOURCE_BLACKLIST = {"example.com", "localhost", "127.0.0.1", "mock"}


class IntelligenceConfig(BaseModel):
    """情報収集設定."""

    mode: str = Field(default="STANDARD", description="FAST/STANDARD/AUDIT")
    max_sources: int = Field(default=15, description="最大取得ソース数")
    enable_cache: bool = Field(default=True, description="キャッシュ有効化")
    timeout_seconds: int = Field(default=10, description="リクエストタイムアウト")
    provider_preference: list[str] = Field(
        default_factory=lambda: ["duckduckgo_search", "serpapi", "bing"],
        description="取得プロバイダ優先順",
    )
    runtime_mode: str = Field(default_factory=lambda: os.getenv("APP_RUNTIME_MODE", "dev"))
    serpapi_key: str | None = Field(default=None)
    bing_api_key: str | None = Field(default=None)
    bing_endpoint: str = Field(default="https://api.bing.microsoft.com/v7.0/search")


class IntelligenceResult(BaseModel):
    """情報収集結果."""

    query: str = Field(..., description="検索クエリ")
    evidence: list[EvidenceItem] = Field(default_factory=list, description="証拠リスト")
    total_sources_checked: int = Field(default=0, description="チェックしたソース数")
    high_reliability_count: int = Field(default=0, description="HIGH信頼度の件数")
    coverage_score: float = Field(default=0.0, ge=0.0, le=1.0, description="カバレッジスコア")
    warnings: list[str] = Field(default_factory=list, description="警告")
    provider: str = Field(default="none", description="主要取得プロバイダ")
    fallback_used: bool = Field(default=False, description="フォールバックを使用したか")
    source_count_real: int = Field(default=0, description="実ソース件数")
    fetched_at: datetime = Field(default_factory=datetime.utcnow, description="取得時刻")


class IntelligenceService:
    """外部情報採集サービス.

    外部ソースから情報を収集し、証拠チェーンを構築する。
    """

    def __init__(self, config: IntelligenceConfig | None = None) -> None:
        """初期化."""
        self.config = config or IntelligenceConfig()
        if not self.config.serpapi_key:
            self.config.serpapi_key = os.getenv("SERPAPI_API_KEY")
        if not self.config.bing_api_key:
            self.config.bing_api_key = os.getenv("BING_SEARCH_API_KEY")
        endpoint = os.getenv("BING_SEARCH_ENDPOINT")
        if endpoint:
            self.config.bing_endpoint = endpoint

        self._cache: dict[str, tuple[datetime, IntelligenceResult]] = {}

        # モードに応じた最大ソース数を設定
        mode_limits = {
            "FAST": MAX_SOURCES_FAST,
            "STANDARD": MAX_SOURCES_STANDARD,
            "AUDIT": MAX_SOURCES_AUDIT,
        }
        if self.config.max_sources == 15:
            self.config.max_sources = mode_limits.get(self.config.mode.upper(), MAX_SOURCES_STANDARD)

    def _cache_key(self, query: str) -> str:
        """キャッシュキーを生成."""
        return hashlib.sha256(query.encode()).hexdigest()[:16]

    def _get_cached(self, query: str) -> IntelligenceResult | None:
        """有効キャッシュを取得."""
        if not self.config.enable_cache:
            return None
        key = self._cache_key(query)
        if key in self._cache:
            cached_at, result = self._cache[key]
            if datetime.utcnow() - cached_at < timedelta(hours=CACHE_TTL_HOURS):
                logger.info("キャッシュヒット: %s...", query[:30])
                return result
        return None

    def _get_stale_cached(self, query: str) -> IntelligenceResult | None:
        """期限切れ含むキャッシュを取得（緊急フォールバック）."""
        if not self.config.enable_cache:
            return None
        key = self._cache_key(query)
        if key not in self._cache:
            return None
        cached_at, result = self._cache[key]
        if datetime.utcnow() - cached_at < timedelta(hours=CACHE_STALE_HOURS):
            return result
        return None

    def _set_cache(self, query: str, result: IntelligenceResult) -> None:
        """キャッシュに保存."""
        if self.config.enable_cache:
            key = self._cache_key(query)
            self._cache[key] = (datetime.utcnow(), result)

    async def gather(self, query: str, topics: list[str] | None = None) -> IntelligenceResult:
        """外部情報を収集."""
        cached = self._get_cached(query)
        if cached:
            return cached

        logger.info("情報収集開始: %s... (mode=%s)", query[:50], self.config.mode)

        warnings: list[str] = []
        evidence: list[EvidenceItem] = []
        provider_hits: dict[str, int] = {}
        fallback_used = False

        tasks = [
            self._fetch_external_intel(query, extra_tags=["web_search"]),
            self._fetch_industry_reports(query),
        ]
        if topics:
            for topic in topics[:3]:
                tasks.append(self._fetch_external_intel(topic, extra_tags=["topic_search"]))

        results = await asyncio.gather(*tasks, return_exceptions=True)
        for item in results:
            if isinstance(item, Exception):
                warnings.append(f"ソース取得エラー: {str(item)[:80]}")
                continue
            batch_evidence, provider, provider_warnings = item
            evidence.extend(batch_evidence)
            warnings.extend(provider_warnings)
            if provider:
                provider_hits[provider] = provider_hits.get(provider, 0) + len(batch_evidence)

        evidence = self._deduplicate(evidence)
        evidence = self._score_reliability(evidence)
        evidence = evidence[: self.config.max_sources]

        source_count_real = sum(1 for item in evidence if self._is_real_source(item.url))
        if source_count_real == 0:
            stale = self._get_stale_cached(query)
            if stale is not None and stale.evidence:
                warnings.append("live_sources_unavailable: stale cache used")
                evidence = stale.evidence[: self.config.max_sources]
                fallback_used = True
                provider_hits[stale.provider] = provider_hits.get(stale.provider, 0) + len(evidence)
                source_count_real = sum(1 for item in evidence if self._is_real_source(item.url))
            else:
                warnings.append("live_sources_unavailable: no evidence from external providers")

        high_count = sum(1 for item in evidence if item.reliability == EvidenceReliability.HIGH)
        coverage = min(1.0, source_count_real / 10) * (0.5 + 0.5 * (high_count / max(len(evidence), 1)))

        primary_provider = "none"
        if provider_hits:
            primary_provider = max(provider_hits.items(), key=lambda pair: pair[1])[0]

        result = IntelligenceResult(
            query=query,
            evidence=evidence,
            total_sources_checked=len(evidence),
            high_reliability_count=high_count,
            coverage_score=round(coverage, 3),
            warnings=warnings,
            provider=primary_provider,
            fallback_used=fallback_used,
            source_count_real=source_count_real,
        )
        self._set_cache(query, result)
        logger.info(
            "情報収集完了: count=%d real=%d coverage=%.2f provider=%s",
            len(evidence),
            source_count_real,
            coverage,
            primary_provider,
        )
        return result

    async def _fetch_external_intel(
        self,
        query: str,
        *,
        extra_tags: list[str] | None = None,
    ) -> tuple[list[EvidenceItem], str, list[str]]:
        """優先順に外部プロバイダへ問い合わせる."""
        warnings: list[str] = []
        provider_order = self._resolve_provider_order(self.config.provider_preference)

        for provider in provider_order:
            if provider == "serpapi":
                items, provider_warnings = await self._fetch_serpapi(query, extra_tags=extra_tags)
            elif provider == "bing":
                items, provider_warnings = await self._fetch_bing(query, extra_tags=extra_tags)
            else:
                items, provider_warnings = await self._fetch_duckduckgo(query, extra_tags=extra_tags)

            warnings.extend(provider_warnings)
            if items:
                return items, provider, warnings

        return [], "none", warnings

    async def _fetch_industry_reports(self, query: str) -> tuple[list[EvidenceItem], str, list[str]]:
        """業界レポート寄りクエリで追加取得する."""
        report_query = f"{query} market size industry report"
        evidence, provider, warnings = await self._fetch_external_intel(
            report_query,
            extra_tags=["industry_report", "market_size"],
        )
        return evidence[: min(5, self.config.max_sources)], provider, warnings

    async def _fetch_serpapi(
        self,
        query: str,
        *,
        extra_tags: list[str] | None = None,
    ) -> tuple[list[EvidenceItem], list[str]]:
        """SerpAPI から取得する."""
        if not self.config.serpapi_key:
            return [], []
        if httpx is None:
            return [], ["serpapi_skipped: httpx missing"]

        params = {
            "engine": "google",
            "q": query,
            "api_key": self.config.serpapi_key,
            "num": min(self.config.max_sources, 10),
        }
        try:
            async with httpx.AsyncClient(timeout=self.config.timeout_seconds) as client:
                response = await client.get("https://serpapi.com/search.json", params=params)
                response.raise_for_status()
            payload = response.json()
        except Exception as exc:
            return [], [f"serpapi_error: {exc}"]

        organic = payload.get("organic_results") if isinstance(payload, dict) else None
        if not isinstance(organic, list):
            return [], []

        evidence: list[EvidenceItem] = []
        for row in organic:
            if not isinstance(row, dict):
                continue
            url = self._clean_text(row.get("link"))
            if not url:
                continue
            evidence.append(
                EvidenceItem(
                    evidence_id=f"ev-{uuid4().hex[:12]}",
                    url=url,
                    title=self._clean_text(row.get("title")) or "",
                    publisher=self._extract_publisher(url=url, fallback="SerpAPI"),
                    snippet=self._clean_text(row.get("snippet")) or "",
                    summary=self._clean_text(row.get("snippet")) or "",
                    retrieved_at=datetime.utcnow(),
                    reliability=EvidenceReliability.MEDIUM,
                    tags=self._merge_tags(["web_search", "serpapi"], extra_tags),
                )
            )
        return evidence, []

    async def _fetch_bing(
        self,
        query: str,
        *,
        extra_tags: list[str] | None = None,
    ) -> tuple[list[EvidenceItem], list[str]]:
        """Bing Web Search から取得する."""
        if not self.config.bing_api_key:
            return [], []
        if httpx is None:
            return [], ["bing_skipped: httpx missing"]

        headers = {"Ocp-Apim-Subscription-Key": self.config.bing_api_key}
        params = {"q": query, "count": min(self.config.max_sources, 10), "mkt": "en-US"}

        try:
            async with httpx.AsyncClient(timeout=self.config.timeout_seconds) as client:
                response = await client.get(self.config.bing_endpoint, headers=headers, params=params)
                response.raise_for_status()
            payload = response.json()
        except Exception as exc:
            return [], [f"bing_error: {exc}"]

        web_pages = payload.get("webPages", {}).get("value") if isinstance(payload, dict) else None
        if not isinstance(web_pages, list):
            return [], []

        evidence: list[EvidenceItem] = []
        for row in web_pages:
            if not isinstance(row, dict):
                continue
            url = self._clean_text(row.get("url"))
            if not url:
                continue
            evidence.append(
                EvidenceItem(
                    evidence_id=f"ev-{uuid4().hex[:12]}",
                    url=url,
                    title=self._clean_text(row.get("name")) or "",
                    publisher=self._extract_publisher(url=url, fallback="Bing"),
                    snippet=self._clean_text(row.get("snippet")) or "",
                    summary=self._clean_text(row.get("snippet")) or "",
                    retrieved_at=datetime.utcnow(),
                    reliability=EvidenceReliability.MEDIUM,
                    tags=self._merge_tags(["web_search", "bing"], extra_tags),
                )
            )
        return evidence, []

    async def _fetch_duckduckgo(
        self,
        query: str,
        *,
        extra_tags: list[str] | None = None,
    ) -> tuple[list[EvidenceItem], list[str]]:
        """DuckDuckGo から取得する（既定プロバイダ）。"""
        if DDGS is None:
            return [], ["duckduckgo_search missing"]

        try:
            rows = await asyncio.to_thread(
                self._search_duckduckgo_sync,
                query,
                min(self.config.max_sources, 15),
            )
        except Exception as exc:
            return [], [f"duckduckgo_search_error: {exc}"]

        evidence: list[EvidenceItem] = []
        for row in rows:
            if not isinstance(row, dict):
                continue
            url = self._clean_text(row.get("href") or row.get("url"))
            if not url:
                continue
            evidence.append(
                EvidenceItem(
                    evidence_id=f"ev-{uuid4().hex[:12]}",
                    url=self._unwrap_duckduckgo_url(url),
                    title=self._clean_text(row.get("title")) or "",
                    publisher=self._extract_publisher(url=url, fallback="DuckDuckGo"),
                    snippet=self._clean_text(row.get("body") or row.get("snippet")) or "",
                    summary=self._clean_text(row.get("body") or row.get("snippet")) or "",
                    retrieved_at=datetime.utcnow(),
                    reliability=EvidenceReliability.MEDIUM,
                    tags=self._merge_tags(["web_search", "duckduckgo"], extra_tags),
                )
            )
        return evidence, []

    @staticmethod
    def _search_duckduckgo_sync(query: str, limit: int) -> list[dict[str, str]]:
        """DuckDuckGo 同期検索（to_thread 用）。"""
        if DDGS is None:
            return []
        with DDGS() as ddgs:
            return list(ddgs.text(query, max_results=limit))

    @staticmethod
    def _resolve_provider_order(raw: list[str]) -> list[str]:
        """プロバイダ順序を正規化する."""
        allowed = {"duckduckgo_search", "serpapi", "bing"}
        order: list[str] = []
        for item in raw:
            token = str(item).strip().lower()
            if token in allowed and token not in order:
                order.append(token)
        if not order:
            return ["duckduckgo_search", "serpapi", "bing"]
        return order

    def _deduplicate(self, evidence: list[EvidenceItem]) -> list[EvidenceItem]:
        """URL ベースで重複除去."""
        seen_urls: set[str] = set()
        unique: list[EvidenceItem] = []
        for item in evidence:
            normalized = self._normalize_url(item.url)
            if normalized in seen_urls:
                continue
            seen_urls.add(normalized)
            unique.append(item)
        return unique

    def _score_reliability(self, evidence: list[EvidenceItem]) -> list[EvidenceItem]:
        """信頼度スコアリング v2（数値化 + 鮮度減衰対応）."""
        from apps.decision_governance_engine.config import get_config

        try:
            config = get_config()
            domain_scores = config.evidence_reliability.domain_scores
            freshness_decay = config.evidence_reliability.freshness_decay
        except Exception as exc:
            logger.warning("設定読み込み失敗、デフォルトスコアリングを使用: %s", exc)
            return self._score_reliability_fallback(evidence)

        scored: list[EvidenceItem] = []
        for item in evidence:
            domain_score = self._get_domain_score(item.url, domain_scores)
            age_days = (datetime.utcnow() - item.retrieved_at).days
            freshness_factor = self._get_freshness_factor(age_days, freshness_decay)

            final_score = domain_score * freshness_factor
            if "industry_report" in item.tags:
                final_score = min(1.0, final_score + 0.1)

            if final_score >= 0.75:
                reliability = EvidenceReliability.HIGH
            elif final_score >= 0.50:
                reliability = EvidenceReliability.MEDIUM
            else:
                reliability = EvidenceReliability.LOW

            scored.append(item.model_copy(update={"reliability": reliability}))

        return scored

    def _get_domain_score(self, url: str, domain_scores: dict[str, float]) -> float:
        """URL からドメインスコアを取得."""
        url_lower = url.lower()
        if ".gov" in url_lower:
            return domain_scores.get("gov", 0.95)
        if ".edu" in url_lower or ".ac.jp" in url_lower:
            return domain_scores.get("edu", 0.90)
        if ".org" in url_lower:
            return domain_scores.get("org", 0.75)
        if ".com" in url_lower:
            return domain_scores.get("com", 0.60)
        return domain_scores.get("default", 0.50)

    def _get_freshness_factor(self, age_days: int, freshness_decay: dict[str, float]) -> float:
        """鮮度減衰係数を取得."""
        if age_days <= 30:
            return freshness_decay.get("days_30", 1.0)
        if age_days <= 90:
            return freshness_decay.get("days_90", 0.9)
        if age_days <= 180:
            return freshness_decay.get("days_180", 0.7)
        if age_days <= 365:
            return freshness_decay.get("days_365", 0.5)
        return freshness_decay.get("older", 0.3)

    def _score_reliability_fallback(self, evidence: list[EvidenceItem]) -> list[EvidenceItem]:
        """フォールバック用の簡易スコアリング（v1 互換）."""
        high_domains = {".gov", ".edu", ".ac.jp", "reuters.com", "bloomberg.com", "nikkei.com"}

        scored: list[EvidenceItem] = []
        for item in evidence:
            reliability = item.reliability
            url_lower = item.url.lower()

            for domain in high_domains:
                if domain in url_lower:
                    reliability = EvidenceReliability.HIGH
                    break

            if "industry_report" in item.tags:
                reliability = EvidenceReliability.HIGH

            scored.append(item.model_copy(update={"reliability": reliability}))

        return scored

    def _is_real_source(self, url: str) -> bool:
        """モック由来ではない実ソースかを判定する."""
        normalized = self._normalize_url(url)
        if not normalized:
            return False
        for token in _REAL_SOURCE_BLACKLIST:
            if token in normalized:
                return False
        return True

    @staticmethod
    def _normalize_url(value: str) -> str:
        text = str(value).strip().lower()
        if not text:
            return ""
        return text.rstrip("/")

    @staticmethod
    def _clean_text(value: object) -> str | None:
        if value is None:
            return None
        text = re.sub(r"\s+", " ", str(value)).strip()
        return text or None

    @staticmethod
    def _merge_tags(base_tags: list[str], extra_tags: list[str] | None) -> list[str]:
        tags: list[str] = []
        for tag in [*(base_tags or []), *(extra_tags or [])]:
            token = str(tag).strip().lower()
            if token and token not in tags:
                tags.append(token)
        return tags

    @staticmethod
    def _extract_publisher(*, url: str, fallback: str) -> str:
        try:
            hostname = urlparse(url).hostname or ""
            cleaned = hostname.replace("www.", "")
            return cleaned or fallback
        except Exception:
            return fallback

    @staticmethod
    def _unwrap_duckduckgo_url(url: str) -> str:
        """DuckDuckGo リダイレクト URL を可能なら復元する."""
        parsed = urlparse(url)
        if parsed.path.startswith("/l/") and parsed.query:
            for part in parsed.query.split("&"):
                if part.startswith("uddg="):
                    return unquote(part.split("=", 1)[1])
        return url


# シングルトンインスタンス（オプション）
_service: IntelligenceService | None = None


def get_intelligence_service(config: IntelligenceConfig | None = None) -> IntelligenceService:
    """IntelligenceService のシングルトン取得."""
    global _service
    if _service is None or config is not None:
        _service = IntelligenceService(config)
    return _service


__all__ = [
    "IntelligenceConfig",
    "IntelligenceResult",
    "IntelligenceService",
    "get_intelligence_service",
]
