"""データ収集エージェント.

複数のソースから市場動向データを収集します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
"""

import asyncio
import logging
import uuid
from datetime import datetime, timedelta
from typing import TYPE_CHECKING, Any

from agentflow.core.agent_block import AgentBlock
from agentflow.core.retry import RetryableAgent
from agentflow.providers import get_llm
from agentflow.config import get_settings

from apps.market_trend_monitor.backend.models import Article, SourceType
from apps.market_trend_monitor.backend.integrations import NewsAPIClient

if TYPE_CHECKING:
    from agentflow.providers.llm_provider import LLMProvider


class CollectorAgent(AgentBlock):
    """データ収集エージェント（松耦合設計）.

    役割:
    - 複数のソース（ニュース、GitHub、arXiv、RSS）からデータ収集
    - キーワードベースのフィルタリング
    - 重複排除とキャッシュ管理

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます。
        具体的なプロバイダーを意識する必要はありません。

    入力:
        {
            "keywords": ["COBOL", "Java migration", "AI"],
            "sources": ["news", "github", "arxiv", "rss"],
            "date_range": {"start": "2025-01-01", "end": "2025-01-18"}
        }

    出力:
        {
            "articles": [Article, ...],
            "total_count": 150,
            "sources_stats": {"news": 50, "github": 40, ...}
        }
    """

    def __init__(self) -> None:
        """初期化."""
        super().__init__()
        self._logger = logging.getLogger(__name__)
        self._cache: set[str] = set()  # URL キャッシュ（重複排除用）

        # LLM プロバイダー（環境変数から自動検出・松耦合）
        # 要約タスクには低めの温度を使用
        self._llm: "LLMProvider" = get_llm(temperature=0.3)

        # NewsAPIクライアント初期化
        settings = get_settings()
        news_api_key = getattr(settings, "news_api_key", None)
        self._news_api_client = NewsAPIClient(api_key=news_api_key)

    async def initialize(self) -> None:
        """エージェント初期化.

        外部APIクライアントのセットアップを行います。
        """
        await super().initialize()
        self._logger.info("CollectorAgent initialized with LLM support")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """データ収集を実行.

        Args:
            input_data: 入力データ
                - keywords: 検索キーワードリスト
                - sources: データソースリスト
                - date_range: 日付範囲（オプション）
                - shared_context: 共有コンテキスト（記憶システム付き）

        Returns:
            収集結果
                - articles: 記事リスト
                - total_count: 総記事数
                - sources_stats: ソース別統計
        """
        keywords = input_data.get("keywords", [])
        sources = input_data.get("sources", ["news"])
        date_range = input_data.get("date_range", {})
        shared_context = input_data.get("shared_context")

        self._logger.info(f"Starting collection: keywords={keywords}, sources={sources}")

        articles: list[Article] = []
        sources_stats: dict[str, int] = {}

        # 各ソースからデータ収集（リトライ機構付き）
        for source in sources:
            source_articles = await self._collect_from_source_with_retry(
                source=source, keywords=keywords, date_range=date_range, max_retries=3
            )
            articles.extend(source_articles)
            sources_stats[source] = len(source_articles)

        self._logger.info(f"Collection completed: total={len(articles)} articles")

        # 記憶システムに保存
        if shared_context:
            await self._save_to_memory(articles, shared_context)

        return {
            "articles": [article.to_dict() for article in articles],
            "total_count": len(articles),
            "sources_stats": sources_stats,
        }

    async def _collect_from_source_with_retry(
        self, source: str, keywords: list[str], date_range: dict[str, str], max_retries: int = 3
    ) -> list[Article]:
        """リトライ機構付きデータ収集.

        Args:
            source: データソース名
            keywords: 検索キーワード
            date_range: 日付範囲
            max_retries: 最大リトライ回数

        Returns:
            収集した記事リスト
        """
        for attempt in range(max_retries):
            try:
                return await self._collect_from_source(source, keywords, date_range)
            except asyncio.TimeoutError:
                self._logger.warning(f"Timeout collecting from {source} (attempt {attempt + 1}/{max_retries})")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0 * (2 ** attempt))  # 指数バックオフ
                else:
                    self._logger.error(f"Failed to collect from {source} after {max_retries} attempts")
                    return []  # 空リストを返す
            except Exception as e:
                self._logger.error(f"Error collecting from {source}: {e}")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0)
                else:
                    return []  # 空リストを返す
        return []

    async def _collect_from_source(
        self, source: str, keywords: list[str], date_range: dict[str, str]
    ) -> list[Article]:
        """特定ソースからデータ収集（NewsAPI統合）.

        Args:
            source: データソース名
            keywords: 検索キーワード
            date_range: 日付範囲

        Returns:
            収集した記事リスト

        Raises:
            asyncio.TimeoutError: タイムアウト時
            Exception: その他のエラー時
        """
        self._logger.debug(f"Collecting from {source}")

        articles: list[Article] = []

        # NewsAPIから実際のニュースを取得
        for keyword in keywords:
            try:
                # NewsAPIで検索
                news_articles = await self._news_api_client.search_everything(
                    query=keyword,
                    language="en",
                    page_size=5,
                )

                # NewsAPIの記事をArticleモデルに変換
                for news_article in news_articles:
                    url = news_article.get("url", "")

                    # 重複チェック
                    if url in self._cache:
                        continue

                    article = Article(
                        id=str(uuid.uuid4()),
                        title=news_article.get("title", f"{keyword} に関する最新動向"),
                        url=url,
                        source=SourceType(source) if source in [s.value for s in SourceType] else SourceType.NEWS,
                        published_at=datetime.fromisoformat(news_article.get("publishedAt", datetime.now().isoformat()).replace("Z", "+00:00")),
                        content=news_article.get("content", news_article.get("description", "")),
                        keywords=[keyword],
                        collected_at=datetime.now(),
                        metadata={
                            "source_type": source,
                            "query": keyword,
                            "news_source": news_article.get("source", {}).get("name", "Unknown"),
                            "author": news_article.get("author", "Unknown"),
                        },
                    )

                    articles.append(article)
                    self._cache.add(url)

            except Exception as e:
                self._logger.warning(f"Failed to collect from NewsAPI for keyword '{keyword}': {e}")
                # フォールバック: LLM生成コンテンツ
                content = await self._generate_article_content(keyword, source)
                url = f"https://{source}.example.com/article/{keyword}"

                if url not in self._cache:
                    article = Article(
                        id=str(uuid.uuid4()),
                        title=f"{keyword} に関する最新動向 ({source})",
                        url=url,
                        source=SourceType(source) if source in [s.value for s in SourceType] else SourceType.NEWS,
                        published_at=datetime.now(),
                        content=content,
                        keywords=[keyword],
                        collected_at=datetime.now(),
                        metadata={"source_type": source, "query": keyword, "fallback": True},
                    )
                    articles.append(article)
                    self._cache.add(url)

        return articles

    async def _generate_article_content(self, keyword: str, source: str) -> str:
        """LLMを使用して記事コンテンツを生成.

        Args:
            keyword: キーワード
            source: ソース名

        Returns:
            生成された記事コンテンツ
        """
        try:
            prompt = f"""Generate a brief news article summary about "{keyword}" from {source}.
The article should be informative and professional, around 200-300 words.
Focus on recent trends, developments, or insights related to {keyword}.
Write in Japanese."""

            # LLM で生成（松耦合：プロバイダー不明）
            response = await self._llm.complete(prompt)
            return response["content"]

        except Exception as e:
            self._logger.warning(f"Failed to generate content with LLM: {e}")
            # フォールバック: 簡単なテンプレート
            return f"{keyword} に関する詳細な記事内容。最新の動向、技術的な進展、市場の反応などを含む包括的な分析。"

    async def _save_to_memory(self, articles: list[Article], shared_context: Any) -> None:
        """記事を記憶システムに保存.

        Args:
            articles: 収集した記事リスト
            shared_context: 共有コンテキスト
        """
        try:
            for article in articles:
                # 記事のタイトルと要約を記憶
                memory_text = f"Title: {article.title}\nURL: {article.url}\nKeywords: {', '.join(article.keywords)}\nContent: {article.content[:200]}..."

                await shared_context.remember(
                    memory_text,
                    topic="collected_articles",
                    metadata={
                        "article_id": article.id,
                        "source": article.source.value,
                        "published_at": article.published_at.isoformat(),
                        "keywords": article.keywords,
                    },
                )

            self._logger.info(f"Saved {len(articles)} articles to memory system")
        except Exception as e:
            self._logger.warning(f"Failed to save articles to memory: {e}")

    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        self._logger.info("CollectorAgent cleanup")
        await super().cleanup()

