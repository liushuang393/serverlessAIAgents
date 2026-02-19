"""外部情報採集サービス（Intelligence Service）.

目的:
    外部情報ソースから情報を収集し、証拠チェーン（Evidence Chain）を構築する。

機能:
    - 複数ソースからの情報取得（Web検索、API等）
    - 去重・キャッシュ
    - 信頼度スコアリング
    - 証拠-結論マッピング

入出力:
    入力: 検索クエリ / トピック
    出力: EvidenceItem リスト + 信頼度サマリ
"""

import asyncio
import hashlib
import logging
from datetime import datetime, timedelta
from uuid import uuid4

from apps.decision_governance_engine.schemas.contract_schemas import (
    EvidenceItem,
    EvidenceReliability,
)
from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)

# 設定値
MAX_SOURCES_FAST = 5
MAX_SOURCES_STANDARD = 15
MAX_SOURCES_AUDIT = 30
CACHE_TTL_HOURS = 24
REQUEST_TIMEOUT_SECONDS = 10


class IntelligenceConfig(BaseModel):
    """情報収集設定."""

    mode: str = Field(default="STANDARD", description="FAST/STANDARD/AUDIT")
    max_sources: int = Field(default=15, description="最大取得ソース数")
    enable_cache: bool = Field(default=True, description="キャッシュ有効化")
    timeout_seconds: int = Field(default=10, description="リクエストタイムアウト")


class IntelligenceResult(BaseModel):
    """情報収集結果."""

    query: str = Field(..., description="検索クエリ")
    evidence: list[EvidenceItem] = Field(default_factory=list, description="証拠リスト")
    total_sources_checked: int = Field(default=0, description="チェックしたソース数")
    high_reliability_count: int = Field(default=0, description="HIGH信頼度の件数")
    coverage_score: float = Field(default=0.0, ge=0.0, le=1.0, description="カバレッジスコア")
    warnings: list[str] = Field(default_factory=list, description="警告")
    fetched_at: datetime = Field(default_factory=datetime.utcnow, description="取得時刻")


class IntelligenceService:
    """外部情報採集サービス.

    外部ソースから情報を収集し、証拠チェーンを構築する。
    """

    def __init__(self, config: IntelligenceConfig | None = None) -> None:
        """初期化.

        Args:
            config: 設定（Noneの場合はデフォルト）
        """
        self.config = config or IntelligenceConfig()
        self._cache: dict[str, tuple[datetime, IntelligenceResult]] = {}

        # モードに応じた最大ソース数を設定
        mode_limits = {
            "FAST": MAX_SOURCES_FAST,
            "STANDARD": MAX_SOURCES_STANDARD,
            "AUDIT": MAX_SOURCES_AUDIT,
        }
        if self.config.max_sources == 15:  # デフォルト値の場合のみ上書き
            self.config.max_sources = mode_limits.get(self.config.mode, MAX_SOURCES_STANDARD)

    def _cache_key(self, query: str) -> str:
        """キャッシュキーを生成."""
        return hashlib.sha256(query.encode()).hexdigest()[:16]

    def _get_cached(self, query: str) -> IntelligenceResult | None:
        """キャッシュから取得."""
        if not self.config.enable_cache:
            return None
        key = self._cache_key(query)
        if key in self._cache:
            cached_at, result = self._cache[key]
            if datetime.utcnow() - cached_at < timedelta(hours=CACHE_TTL_HOURS):
                logger.info(f"キャッシュヒット: {query[:30]}...")
                return result
            del self._cache[key]
        return None

    def _set_cache(self, query: str, result: IntelligenceResult) -> None:
        """キャッシュに保存."""
        if self.config.enable_cache:
            key = self._cache_key(query)
            self._cache[key] = (datetime.utcnow(), result)

    async def gather(self, query: str, topics: list[str] | None = None) -> IntelligenceResult:
        """外部情報を収集.

        Args:
            query: メイン検索クエリ
            topics: 追加トピック（オプション）

        Returns:
            IntelligenceResult
        """
        # キャッシュチェック
        cached = self._get_cached(query)
        if cached:
            return cached

        logger.info(f"情報収集開始: {query[:50]}... (mode={self.config.mode})")

        evidence: list[EvidenceItem] = []
        warnings: list[str] = []

        # 各ソースから並列取得
        tasks = [
            self._fetch_web_search(query),
            self._fetch_industry_reports(query),
        ]
        if topics:
            for topic in topics[:3]:  # 最大3トピック
                tasks.append(self._fetch_web_search(topic))

        results = await asyncio.gather(*tasks, return_exceptions=True)

        for r in results:
            if isinstance(r, Exception):
                warnings.append(f"ソース取得エラー: {str(r)[:50]}")
            elif isinstance(r, list):
                evidence.extend(r)

        # 去重（URL ベース）
        evidence = self._deduplicate(evidence)

        # 信頼度スコアリング
        evidence = self._score_reliability(evidence)

        # 最大件数制限
        evidence = evidence[: self.config.max_sources]

        # カバレッジスコア計算
        high_count = sum(1 for e in evidence if e.reliability == EvidenceReliability.HIGH)
        coverage = min(1.0, len(evidence) / 10) * (0.5 + 0.5 * (high_count / max(len(evidence), 1)))

        result = IntelligenceResult(
            query=query,
            evidence=evidence,
            total_sources_checked=len(evidence),
            high_reliability_count=high_count,
            coverage_score=round(coverage, 3),
            warnings=warnings,
        )

        self._set_cache(query, result)
        logger.info(f"情報収集完了: {len(evidence)} 件, coverage={coverage:.2f}")

        return result

    async def _fetch_web_search(self, query: str) -> list[EvidenceItem]:
        """Web検索から情報取得（モック実装、将来はGoogle/Bing API等）."""
        # 本番環境では実際のAPIを呼び出す
        # 現状はモックデータを返す
        await asyncio.sleep(0.1)  # 模擬的な遅延

        mock_results = [
            {
                "url": f"https://example.com/article/{uuid4().hex[:8]}",
                "title": f"Analysis: {query[:30]}",
                "publisher": "Industry Report",
                "snippet": f"According to market research on {query[:20]}...",
            },
        ]

        evidence = []
        for r in mock_results:
            evidence.append(
                EvidenceItem(
                    evidence_id=f"ev-{uuid4().hex[:12]}",
                    url=r["url"],
                    title=r["title"],
                    publisher=r["publisher"],
                    snippet=r["snippet"],
                    summary=f"Summary of {r['title']}",
                    retrieved_at=datetime.utcnow(),
                    reliability=EvidenceReliability.MEDIUM,
                    tags=["web_search"],
                )
            )
        return evidence

    async def _fetch_industry_reports(self, query: str) -> list[EvidenceItem]:
        """業界レポートから情報取得（モック実装）."""
        await asyncio.sleep(0.1)

        # 本番環境では Statista, Gartner, etc. API を呼び出す
        evidence = []
        if "market" in query.lower() or "product" in query.lower():
            evidence.append(
                EvidenceItem(
                    evidence_id=f"ev-{uuid4().hex[:12]}",
                    url="https://reports.example.com/market-analysis",
                    title="Market Size Analysis 2026",
                    publisher="Industry Research Institute",
                    snippet="The global market is expected to reach...",
                    summary="Market size and growth projections",
                    retrieved_at=datetime.utcnow(),
                    reliability=EvidenceReliability.HIGH,
                    tags=["industry_report", "market_size"],
                )
            )
        return evidence

    def _deduplicate(self, evidence: list[EvidenceItem]) -> list[EvidenceItem]:
        """URL ベースで重複除去."""
        seen_urls: set[str] = set()
        unique: list[EvidenceItem] = []
        for e in evidence:
            if e.url not in seen_urls:
                seen_urls.add(e.url)
                unique.append(e)
        return unique

    def _score_reliability(self, evidence: list[EvidenceItem]) -> list[EvidenceItem]:
        """信頼度スコアリング v2（数値化 + 鮮度減衰対応）.

        ルール:
            1. ドメインスコア（config.evidence_reliability.domain_scores）
            2. 鮮度減衰（config.evidence_reliability.freshness_decay）
            3. 最終スコア = ドメインスコア × 鮮度係数
            4. 3段階に変換（HIGH >= 0.75, MEDIUM >= 0.50, LOW < 0.50）
        """
        from apps.decision_governance_engine.config import get_config

        try:
            config = get_config()
            domain_scores = config.evidence_reliability.domain_scores
            freshness_decay = config.evidence_reliability.freshness_decay
        except Exception as e:
            logger.warning(f"設定読み込み失敗、デフォルトスコアリングを使用: {e}")
            return self._score_reliability_fallback(evidence)

        scored: list[EvidenceItem] = []
        for e in evidence:
            # 1. ドメインスコア
            domain_score = self._get_domain_score(e.url, domain_scores)

            # 2. 鮮度減衰係数
            age_days = (datetime.utcnow() - e.retrieved_at).days
            freshness_factor = self._get_freshness_factor(age_days, freshness_decay)

            # 3. 最終スコア（0.0-1.0）
            final_score = domain_score * freshness_factor

            # 4. タグボーナス（industry_report は +0.1）
            if "industry_report" in e.tags:
                final_score = min(1.0, final_score + 0.1)

            # 5. 3段階に変換
            if final_score >= 0.75:
                reliability = EvidenceReliability.HIGH
            elif final_score >= 0.50:
                reliability = EvidenceReliability.MEDIUM
            else:
                reliability = EvidenceReliability.LOW

            scored.append(e.model_copy(update={"reliability": reliability}))

        return scored

    def _get_domain_score(self, url: str, domain_scores: dict[str, float]) -> float:
        """URL からドメインスコアを取得.

        Args:
            url: URL
            domain_scores: ドメイン -> スコアのマッピング

        Returns:
            ドメインスコア（0.0-1.0）
        """
        url_lower = url.lower()

        # 特定ドメインマッチ
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
        """鮮度減衰係数を取得.

        Args:
            age_days: 経過日数
            freshness_decay: 減衰設定

        Returns:
            鮮度係数（0.0-1.0）
        """
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
        for e in evidence:
            reliability = e.reliability
            url_lower = e.url.lower()

            # ドメインベース判定
            for domain in high_domains:
                if domain in url_lower:
                    reliability = EvidenceReliability.HIGH
                    break

            # タグに industry_report があれば HIGH
            if "industry_report" in e.tags:
                reliability = EvidenceReliability.HIGH

            scored.append(e.model_copy(update={"reliability": reliability}))

        return scored


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
