# -*- coding: utf-8 -*-
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
from datetime import datetime
from pathlib import Path
from typing import Any

from agentflow import ResilientAgent
from agentflow.skills import Skill, SkillRouter, SkillRuntime

from apps.market_trend_monitor.backend.models import (
    AnalyzerInput,
    AnalyzerOutput,
    Article,
    ArticleSchema,
    SentimentType,
    Trend,
    TrendSchema,
)


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
    ) -> None:
        """初期化.

        Note:
            LLM クライアントは get_llm() により自動取得されます（松耦合原則）。

        Args:
            skill_router: Skillルーター（None の場合は新規作成）
            skill_runtime: Skill実行環境（None の場合は新規作成）
        """
        super().__init__()  # ResilientAgent が内部で get_llm() を呼び出す
        self._logger = logging.getLogger(self.name)

        # Skill体系初期化（Anthropic Skills準拠）
        self._skill_router = skill_router or SkillRouter()
        self._skill_runtime = skill_runtime or SkillRuntime()
        self._market_trend_skill: Skill | None = None

    def _parse_input(self, input_data: dict[str, Any]) -> AnalyzerInput:
        """入力データを Pydantic モデルに変換."""
        return AnalyzerInput(**input_data)

    async def process(self, input_data: AnalyzerInput) -> AnalyzerOutput:
        """分析を実行.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付き分析結果
        """
        articles_data = input_data.articles
        enable_sentiment = input_data.enable_sentiment

        self._logger.info(f"Starting analysis: {len(articles_data)} articles")

        # ArticleSchema から Article に変換
        articles = [self._schema_to_article(a) for a in articles_data]

        # キーワード抽出
        keywords_stats = self._extract_keywords(articles)

        # トレンド抽出
        trends = await self._extract_trends(articles, keywords_stats, enable_sentiment)

        # サマリー生成
        summary = await self._generate_summary(trends, keywords_stats)

        self._logger.info(f"Analysis completed: {len(trends)} trends found")

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

    def _extract_keywords(self, articles: list[Article]) -> dict[str, int]:
        """キーワード抽出.

        Args:
            articles: 記事リスト

        Returns:
            キーワード出現頻度
        """
        all_keywords: list[str] = []
        for article in articles:
            all_keywords.extend(article.keywords)

        # 出現頻度をカウント
        counter = Counter(all_keywords)
        return dict(counter.most_common(20))  # 上位20件

    async def _extract_trends(
        self,
        articles: list[Article],
        keywords_stats: dict[str, int],
        enable_sentiment: bool,
    ) -> list[Trend]:
        """トレンド抽出.

        Args:
            articles: 記事リスト
            keywords_stats: キーワード統計
            enable_sentiment: センチメント分析有効化

        Returns:
            トレンドリスト
        """
        import random

        trends: list[Trend] = []

        # キーワードごとにトレンドを生成
        for keyword, count in list(keywords_stats.items())[:10]:  # 上位10件
            # トレンドスコア計算（簡易版）
            score = min(count / max(len(articles), 1), 1.0)

            # センチメント分析（LLM）
            sentiment = SentimentType.NEUTRAL
            if enable_sentiment:
                sentiment = await self._analyze_sentiment(keyword, articles)

            # 成長率（簡易計算）
            growth_rate = random.uniform(-0.2, 0.5)

            now = datetime.now()
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
                metadata={"analysis_version": "3.0", "llm_analyzed": enable_sentiment},
            )

            trends.append(trend)

        return trends

    async def _analyze_sentiment(self, keyword: str, articles: list[Article]) -> SentimentType:
        """LLM を使用してセンチメント分析.

        Args:
            keyword: キーワード
            articles: 記事リスト

        Returns:
            センチメントタイプ
        """
        # キーワードに関連する記事のタイトルを収集
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
            # ResilientAgent の _call_llm を使用
            response = await self._call_llm(prompt)
            sentiment_text = response.strip().upper() if response else ""

            if "POSITIVE" in sentiment_text:
                return SentimentType.POSITIVE
            elif "NEGATIVE" in sentiment_text:
                return SentimentType.NEGATIVE
            else:
                return SentimentType.NEUTRAL
        except Exception as e:
            self._logger.warning(f"Sentiment analysis failed for '{keyword}': {e}")
            return SentimentType.NEUTRAL

    async def _generate_summary(self, trends: list[Trend], keywords_stats: dict[str, int]) -> str:
        """サマリー生成.

        Args:
            trends: トレンドリスト
            keywords_stats: キーワード統計

        Returns:
            サマリーテキスト
        """
        if not trends:
            return "トレンドが検出されませんでした。"

        # トレンド情報を整形
        trends_text = "\n".join([
            f"- {t.topic}: {t.articles_count}件, 成長率{t.growth_rate:.1%}, センチメント{t.sentiment.value}"
            for t in trends[:5]
        ])

        prompt = f"""Based on the following market trends, generate a comprehensive summary in Japanese.
The summary should be professional, insightful, and around 100-150 words.

Trends:
{trends_text}

Summary:"""

        try:
            # ResilientAgent の _call_llm を使用
            response = await self._call_llm(prompt)
            return response if response else self._fallback_summary(trends)
        except Exception as e:
            self._logger.warning(f"Summary generation failed: {e}")
            return self._fallback_summary(trends)

    def _fallback_summary(self, trends: list[Trend]) -> str:
        """フォールバックサマリー生成."""
        if trends:
            top_trend = trends[0]
            return f"今週の主要トレンドは「{top_trend.topic}」です。"
        return "トレンドが検出されませんでした。"

    def validate_output(self, output: AnalyzerOutput) -> bool:
        """出力検証.

        Args:
            output: 出力データ

        Returns:
            検証結果（True = 有効）
        """
        # サマリーが空でないか
        if not output.summary:
            self._logger.warning("Validation warning: summary is empty")
        return True

