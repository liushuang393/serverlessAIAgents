"""分析エージェント.

収集データを分析し、トレンドを抽出します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
- 型安全：Pydantic による I/O 検証
- 健壮性：ResilientAgent によるリトライ・タイムアウト制御
- Skills体系：確定性処理をSkillスクリプトに分離（Anthropic Skills準拠）
"""

import logging
import uuid
from collections import Counter
from datetime import datetime, timedelta
from typing import Any

from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.models import (
    AnalyzerInput,
    AnalyzerOutput,
    Article,
    ArticleSchema,
    SentimentType,
    Trend,
    TrendSchema,
)
from apps.market_trend_monitor.backend.services.evidence_service import EvidenceService

from agentflow import ResilientAgent
from agentflow.skills import Skill, SkillRouter, SkillRuntime


class AnalyzerAgent(ResilientAgent[AnalyzerInput, AnalyzerOutput]):
    """分析エージェント（ResilientAgent 継承・型安全）.

    役割:
    - キーワード抽出（TF-IDF）
    - トピック分類
    - センチメント分析（LLM）
    - トレンド計算（成長率、スコア）

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます（松耦合）。
        ResilientAgent によりリトライ・タイムアウトが自動制御されます。
    """

    # ResilientAgent 設定
    name = "AnalyzerAgent"
    temperature = 0.5  # 分析タスクは中程度

    def __init__(
        self,
        *,
        skill_router: SkillRouter | None = None,
        skill_runtime: SkillRuntime | None = None,
        evidence_service: EvidenceService | None = None,
    ) -> None:
        """初期化.

        Note:
            LLM クライアントは get_llm() により自動取得されます（松耦合原則）。

        引数:
            skill_router: Skillルーター（None の場合は新規作成）
            skill_runtime: Skill実行環境（None の場合は新規作成）
        """
        super().__init__()  # ResilientAgent が内部で get_llm() を呼び出す
        self._logger = logging.getLogger(self.name)

        # Skill体系初期化（Anthropic Skills準拠）
        self._skill_router = skill_router or SkillRouter()
        self._skill_runtime = skill_runtime or SkillRuntime()
        self._market_trend_skill: Skill | None = None
        self._evidence_service = evidence_service

    def _parse_input(self, input_data: dict[str, Any]) -> AnalyzerInput:
        """入力データを Pydantic モデルに変換."""
        return AnalyzerInput(**input_data)

    async def process(self, input_data: AnalyzerInput) -> AnalyzerOutput:
        """分析を実行.

        引数:
            input_data: 型付き入力データ

        戻り値:
            型付き分析結果
        """
        articles_data = input_data.articles
        enable_sentiment = input_data.enable_sentiment

        self._logger.info("分析開始: %s件の記事", len(articles_data))

        # ArticleSchema から Article に変換
        articles = [self._schema_to_article(a) for a in articles_data]

        # キーワード抽出（Evidence 優先）
        keywords_stats = await self._extract_keywords(articles)

        # トレンド抽出
        trend_context = await self._build_trend_context()
        trends = await self._extract_trends(
            articles,
            keywords_stats,
            trend_context,
            enable_sentiment,
        )

        # サマリー生成
        summary = await self._generate_summary(trends, keywords_stats)

        self._logger.info("分析完了: %s件のトレンド", len(trends))

        # Trend を TrendSchema に変換
        trend_schemas = [
            TrendSchema(
                id=t.id,
                topic=t.topic,
                score=t.score,
                articles_count=t.articles_count,
                keywords=t.keywords,
                sentiment=t.sentiment.value,
                growth_rate=t.growth_rate,
                first_seen=t.first_seen.isoformat(),
                last_seen=t.last_seen.isoformat(),
                article_count=t.article_count,
                created_at=t.created_at.isoformat(),
                metadata=t.metadata,
            )
            for t in trends
        ]

        return AnalyzerOutput(
            trends=trend_schemas,
            summary=summary,
            keywords_stats=keywords_stats,
        )

    def _schema_to_article(self, schema: ArticleSchema) -> Article:
        """ArticleSchema から Article オブジェクトを生成."""
        from apps.market_trend_monitor.backend.models import SourceType

        # source 文字列を SourceType に変換
        source_type = SourceType.NEWS
        if schema.source in [s.value for s in SourceType]:
            source_type = SourceType(schema.source)

        return Article(
            id=schema.id,
            title=schema.title,
            url=schema.url,
            source=source_type,
            published_at=datetime.fromisoformat(schema.published_at),
            content=schema.content,
            keywords=schema.keywords,
            collected_at=datetime.fromisoformat(schema.collected_at),
            metadata=schema.metadata,
        )

    async def _extract_keywords(self, articles: list[Article]) -> dict[str, int]:
        """キーワード抽出.

        引数:
            articles: 記事リスト

        戻り値:
            キーワード出現頻度
        """
        if self._evidence_service:
            stats = await self._extract_keywords_from_evidence()
            if stats:
                return stats

        all_keywords: list[str] = []
        for article in articles:
            all_keywords.extend(article.keywords)

        counter = Counter(all_keywords)
        return dict(counter.most_common(20))

    async def _extract_trends(
        self,
        articles: list[Article],
        keywords_stats: dict[str, int],
        trend_context: dict[str, Any],
        enable_sentiment: bool,
    ) -> list[Trend]:
        """トレンド抽出."""
        trends: list[Trend] = []
        weighted_current = trend_context.get("weighted_current", {})
        weighted_previous = trend_context.get("weighted_previous", {})
        count_current = trend_context.get("count_current", {})

        if weighted_current:
            total_weight = sum(weighted_current.values())
            sorted_items = sorted(weighted_current.items(), key=lambda x: x[1], reverse=True)
            for keyword, weight in sorted_items[:10]:
                score = min(weight / max(total_weight, 1.0), 1.0)
                prev_weight = weighted_previous.get(keyword, 0.0)
                growth_rate = self._calculate_growth_rate(weight, prev_weight)

                sentiment = SentimentType.NEUTRAL
                if enable_sentiment:
                    sentiment = await self._analyze_sentiment(keyword, articles)

                now = datetime.now()
                count = int(count_current.get(keyword, 0))
                trend = Trend(
                    id=str(uuid.uuid4()),
                    topic=keyword,
                    score=score,
                    articles_count=count,
                    keywords=[keyword],
                    sentiment=sentiment,
                    growth_rate=growth_rate,
                    first_seen=now,
                    last_seen=now,
                    article_count=count,
                    created_at=now,
                    metadata={
                        "analysis_version": "3.1",
                        "llm_analyzed": enable_sentiment,
                        "weighted": True,
                    },
                )
                trends.append(trend)
            return trends

        # フォールバック: 記事キーワードのみ
        for keyword, count in list(keywords_stats.items())[:10]:
            score = min(count / max(len(articles), 1), 1.0)
            sentiment = SentimentType.NEUTRAL
            if enable_sentiment:
                sentiment = await self._analyze_sentiment(keyword, articles)

            now = datetime.now()
            trend = Trend(
                id=str(uuid.uuid4()),
                topic=keyword,
                score=score,
                articles_count=count,
                keywords=[keyword],
                sentiment=sentiment,
                growth_rate=0.0,
                first_seen=now,
                last_seen=now,
                article_count=count,
                created_at=now,
                metadata={"analysis_version": "3.0", "llm_analyzed": enable_sentiment},
            )
            trends.append(trend)

        return trends

    async def _extract_keywords_from_evidence(self) -> dict[str, int]:
        """Evidence からキーワード頻度を取得."""
        if not self._evidence_service:
            return {}

        now = datetime.now()
        window_days = config.analyzer.trend_window_days
        since = now - timedelta(days=window_days)
        evidences = await self._evidence_service.list_evidences_in_window(since, now)
        counts: dict[str, int] = {}
        for evidence in evidences:
            for keyword in evidence.extracted_data.get("keywords", []):
                counts[keyword] = counts.get(keyword, 0) + 1
        return dict(sorted(counts.items(), key=lambda x: x[1], reverse=True)[:20])

    async def _build_trend_context(self) -> dict[str, Any]:
        """時系列トレンド算定用コンテキストを構築."""
        if not self._evidence_service:
            return {}

        now = datetime.now()
        window_days = config.analyzer.trend_window_days
        current_since = now - timedelta(days=window_days)
        previous_since = current_since - timedelta(days=window_days)

        current_evidences = await self._evidence_service.list_evidences_in_window(
            current_since, now
        )
        previous_evidences = await self._evidence_service.list_evidences_in_window(
            previous_since, current_since
        )

        weighted_current: dict[str, float] = {}
        weighted_previous: dict[str, float] = {}
        count_current: dict[str, int] = {}

        for evidence in current_evidences:
            for keyword in evidence.extracted_data.get("keywords", []):
                weighted_current[keyword] = weighted_current.get(keyword, 0.0) + evidence.reliability_score
                count_current[keyword] = count_current.get(keyword, 0) + 1

        for evidence in previous_evidences:
            for keyword in evidence.extracted_data.get("keywords", []):
                weighted_previous[keyword] = weighted_previous.get(keyword, 0.0) + evidence.reliability_score

        return {
            "weighted_current": weighted_current,
            "weighted_previous": weighted_previous,
            "count_current": count_current,
        }

    def _calculate_growth_rate(self, current: float, previous: float) -> float:
        """成長率を計算."""
        if previous <= 0:
            return 1.0 if current > 0 else 0.0
        return (current - previous) / previous

    async def _analyze_sentiment(self, keyword: str, articles: list[Article]) -> SentimentType:
        """LLM を使用してセンチメント分析."""
        related_titles = [a.title for a in articles if keyword in a.keywords][:5]
        if not related_titles:
            return SentimentType.NEUTRAL

        titles_text = "\n".join(related_titles)
        prompt = f"""Analyze the sentiment of the following article titles related to "{keyword}".
Respond with only one word: POSITIVE, NEGATIVE, or NEUTRAL.

Titles:
{titles_text}

Sentiment:"""

        try:
            response = await self._call_llm(prompt)
            sentiment_text = response.strip().upper() if response else ""

            if "POSITIVE" in sentiment_text:
                return SentimentType.POSITIVE
            if "NEGATIVE" in sentiment_text:
                return SentimentType.NEGATIVE
            return SentimentType.NEUTRAL
        except Exception as e:
            self._logger.warning("センチメント分析失敗 '%s': %s", keyword, e)
            return SentimentType.NEUTRAL
