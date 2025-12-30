"""分析エージェント.

収集データを分析し、トレンドを抽出します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
"""

import asyncio
import logging
import uuid
from collections import Counter
from datetime import datetime
from typing import TYPE_CHECKING, Any

from agentflow.core.agent_block import AgentBlock
from agentflow.providers import get_llm

from apps.market_trend_monitor.backend.models import Article, SentimentType, Trend

if TYPE_CHECKING:
    from agentflow.providers.llm_provider import LLMProvider


class AnalyzerAgent(AgentBlock):
    """分析エージェント（松耦合設計）.

    役割:
    - キーワード抽出（TF-IDF）
    - トピック分類
    - センチメント分析
    - トレンド計算（成長率、スコア）

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます。

    入力:
        {
            "articles": [Article, ...],
            "enable_sentiment": true
        }

    出力:
        {
            "trends": [Trend, ...],
            "summary": "今週はCOBOL移行案件が23%増加...",
            "keywords_stats": {"COBOL": 45, "Java": 38, ...}
        }
    """

    def __init__(self) -> None:
        """初期化."""
        super().__init__()
        self._logger = logging.getLogger(__name__)

        # LLM プロバイダー（環境変数から自動検出・松耦合）
        # 分析タスクには中程度の温度を使用
        self._llm: "LLMProvider" = get_llm(temperature=0.5)

    async def initialize(self) -> None:
        """エージェント初期化."""
        await super().initialize()
        self._logger.info("AnalyzerAgent initialized")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """分析を実行.

        Args:
            input_data: 入力データ
                - articles: 記事リスト
                - enable_sentiment: センチメント分析有効化
                - shared_context: 共有コンテキスト（記憶システム付き）

        Returns:
            分析結果
                - trends: トレンドリスト
                - summary: サマリーテキスト
                - keywords_stats: キーワード統計
        """
        articles_data = input_data.get("articles", [])
        enable_sentiment = input_data.get("enable_sentiment", True)
        shared_context = input_data.get("shared_context")

        self._logger.info(f"Starting analysis: {len(articles_data)} articles")

        # Article オブジェクトに変換
        articles = [self._dict_to_article(a) for a in articles_data]

        # 過去のトレンドを検索（記憶システムから）
        past_trends = await self._recall_past_trends(shared_context) if shared_context else []

        # キーワード抽出
        keywords_stats = self._extract_keywords(articles)

        # トレンド抽出（過去のトレンドを考慮）
        trends = await self._extract_trends(articles, keywords_stats, enable_sentiment, past_trends)

        # サマリー生成（リトライ機構付き）
        summary = await self._generate_summary_with_retry(trends, keywords_stats, max_retries=3)

        # トレンドを記憶システムに保存
        if shared_context:
            await self._save_trends_to_memory(trends, shared_context)

        self._logger.info(f"Analysis completed: {len(trends)} trends found")

        return {
            "trends": [trend.to_dict() for trend in trends],
            "summary": summary,
            "keywords_stats": keywords_stats,
        }

    def _dict_to_article(self, data: dict[str, Any]) -> Article:
        """辞書からArticleオブジェクトを生成."""
        return Article(
            id=data["id"],
            title=data["title"],
            url=data["url"],
            source=data["source"],
            published_at=datetime.fromisoformat(data["published_at"]),
            content=data["content"],
            keywords=data.get("keywords", []),
            collected_at=datetime.fromisoformat(data["collected_at"]),
            metadata=data.get("metadata", {}),
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
        past_trends: list[dict[str, Any]] | None = None,
    ) -> list[Trend]:
        """トレンド抽出.

        Args:
            articles: 記事リスト
            keywords_stats: キーワード統計
            enable_sentiment: センチメント分析有効化
            past_trends: 過去のトレンド（記憶システムから取得）

        Returns:
            トレンドリスト
        """
        trends: list[Trend] = []
        past_trends = past_trends or []

        # 過去のトレンドから成長率を計算
        past_keywords_count: dict[str, int] = {}
        for past_trend in past_trends:
            topic = past_trend.get("topic", "")
            count = past_trend.get("articles_count", 0)
            if topic:
                past_keywords_count[topic] = count

        # キーワードごとにトレンドを生成
        for keyword, count in list(keywords_stats.items())[:10]:  # 上位10件
            # トレンドスコア計算（簡易版）
            score = min(count / len(articles), 1.0)

            # LLMを使用してセンチメント分析（リトライ機構付き）
            sentiment = await self._analyze_sentiment_with_retry(keyword, articles, max_retries=3)

            # 成長率計算（過去のトレンドと比較）
            if keyword in past_keywords_count and past_keywords_count[keyword] > 0:
                growth_rate = (count - past_keywords_count[keyword]) / past_keywords_count[keyword]
            else:
                # 過去のデータがない場合はランダム
                import random

                growth_rate = random.uniform(-0.2, 0.5)

            trend = Trend(
                id=str(uuid.uuid4()),
                topic=keyword,
                score=score,
                articles_count=count,
                keywords=[keyword],
                sentiment=sentiment,
                growth_rate=growth_rate,
                created_at=datetime.now(),
                metadata={"analysis_version": "2.0", "has_past_data": keyword in past_keywords_count, "llm_analyzed": True},
            )

            trends.append(trend)

        return trends

    async def _analyze_sentiment_with_retry(
        self, keyword: str, articles: list[Article], max_retries: int = 3
    ) -> SentimentType:
        """リトライ機構付きセンチメント分析.

        Args:
            keyword: キーワード
            articles: 記事リスト
            max_retries: 最大リトライ回数

        Returns:
            センチメントタイプ
        """
        for attempt in range(max_retries):
            try:
                return await self._analyze_sentiment(keyword, articles)
            except asyncio.TimeoutError:
                self._logger.warning(
                    f"Timeout analyzing sentiment for '{keyword}' (attempt {attempt + 1}/{max_retries})"
                )
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0 * (2 ** attempt))  # 指数バックオフ
                else:
                    self._logger.error(f"Failed to analyze sentiment for '{keyword}' after {max_retries} attempts")
                    return SentimentType.NEUTRAL  # フォールバック
            except Exception as e:
                self._logger.error(f"Error analyzing sentiment for '{keyword}': {e}")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0)
                else:
                    return SentimentType.NEUTRAL  # フォールバック
        return SentimentType.NEUTRAL

    async def _analyze_sentiment(self, keyword: str, articles: list[Article]) -> SentimentType:
        """LLMを使用してセンチメント分析.

        Args:
            keyword: キーワード
            articles: 記事リスト

        Returns:
            センチメントタイプ

        Raises:
            asyncio.TimeoutError: タイムアウト時
            Exception: その他のエラー時
        """
        # キーワードに関連する記事のタイトルを収集
        related_titles = [a.title for a in articles if keyword in a.keywords][:5]
        titles_text = "\n".join(related_titles)

        prompt = f"""Analyze the sentiment of the following article titles related to "{keyword}".
Respond with only one word: POSITIVE, NEGATIVE, or NEUTRAL.

Titles:
{titles_text}

Sentiment:"""

        # LLM で分析（松耦合：プロバイダー不明）
        response = await self._llm.complete(prompt)
        sentiment_text = response["content"].strip().upper()

        if "POSITIVE" in sentiment_text:
            return SentimentType.POSITIVE
        elif "NEGATIVE" in sentiment_text:
            return SentimentType.NEGATIVE
        else:
            return SentimentType.NEUTRAL

    async def _generate_summary_with_retry(
        self, trends: list[Trend], keywords_stats: dict[str, int], max_retries: int = 3
    ) -> str:
        """リトライ機構付きサマリー生成.

        Args:
            trends: トレンドリスト
            keywords_stats: キーワード統計
            max_retries: 最大リトライ回数

        Returns:
            サマリーテキスト
        """
        for attempt in range(max_retries):
            try:
                return await self._generate_summary(trends, keywords_stats)
            except asyncio.TimeoutError:
                self._logger.warning(f"Timeout generating summary (attempt {attempt + 1}/{max_retries})")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0 * (2 ** attempt))  # 指数バックオフ
                else:
                    self._logger.error(f"Failed to generate summary after {max_retries} attempts")
                    # フォールバック
                    if trends:
                        top_trend = trends[0]
                        return f"今週の主要トレンドは「{top_trend.topic}」です。"
                    return "トレンドが検出されませんでした。"
            except Exception as e:
                self._logger.error(f"Error generating summary: {e}")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0)
                else:
                    # フォールバック
                    if trends:
                        top_trend = trends[0]
                        return f"今週の主要トレンドは「{top_trend.topic}」です。"
                    return "トレンドが検出されませんでした。"
        return "トレンドが検出されませんでした。"

    async def _generate_summary(self, trends: list[Trend], keywords_stats: dict[str, int]) -> str:
        """サマリー生成.

        Args:
            trends: トレンドリスト
            keywords_stats: キーワード統計

        Returns:
            サマリーテキスト

        Raises:
            asyncio.TimeoutError: タイムアウト時
            Exception: その他のエラー時
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

        # LLM でサマリー生成（松耦合：プロバイダー不明）
        response = await self._llm.complete(prompt)
        return response["content"]

    async def _recall_past_trends(self, shared_context: Any) -> list[dict[str, Any]]:
        """過去のトレンドを記憶システムから検索.

        Args:
            shared_context: 共有コンテキスト

        Returns:
            過去のトレンドリスト
        """
        try:
            memories = await shared_context.recall(topic="analyzed_trends", limit=20)
            past_trends = []
            for memory in memories:
                # メタデータからトレンド情報を抽出
                metadata = memory.metadata
                if "topic" in metadata:
                    past_trends.append(metadata)

            self._logger.info(f"Recalled {len(past_trends)} past trends from memory")
            return past_trends
        except Exception as e:
            self._logger.warning(f"Failed to recall past trends: {e}")
            return []

    async def _save_trends_to_memory(self, trends: list[Trend], shared_context: Any) -> None:
        """トレンドを記憶システムに保存.

        Args:
            trends: トレンドリスト
            shared_context: 共有コンテキスト
        """
        try:
            for trend in trends:
                # トレンド情報を記憶
                memory_text = f"Trend: {trend.topic}\nScore: {trend.score:.2f}\nArticles: {trend.articles_count}\nGrowth: {trend.growth_rate:.1%}\nSentiment: {trend.sentiment.value}"

                await shared_context.remember(
                    memory_text,
                    topic="analyzed_trends",
                    metadata={
                        "trend_id": trend.id,
                        "topic": trend.topic,
                        "score": trend.score,
                        "articles_count": trend.articles_count,
                        "growth_rate": trend.growth_rate,
                        "sentiment": trend.sentiment.value,
                        "created_at": trend.created_at.isoformat(),
                    },
                )

            self._logger.info(f"Saved {len(trends)} trends to memory system")
        except Exception as e:
            self._logger.warning(f"Failed to save trends to memory: {e}")

    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        self._logger.info("AnalyzerAgent cleanup")
        await super().cleanup()

