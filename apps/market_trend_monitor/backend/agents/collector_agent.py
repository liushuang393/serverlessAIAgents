# -*- coding: utf-8 -*-
"""データ収集エージェント.

複数のソースから市場動向データを収集します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
- 型安全：Pydantic による I/O 検証
- 健壮性：ResilientAgent によるリトライ・タイムアウト制御
"""

import logging
import uuid
from datetime import datetime
from typing import Any

from agentflow import ResilientAgent
from agentflow.config import get_settings

from apps.market_trend_monitor.backend.models import (
    Article,
    ArticleSchema,
    CollectorInput,
    CollectorOutput,
    SourceType,
)
from apps.market_trend_monitor.backend.integrations import NewsAPIClient


class CollectorAgent(ResilientAgent[CollectorInput, CollectorOutput]):
    """データ収集エージェント（ResilientAgent 継承・型安全）.

    役割:
    - 複数のソース（ニュース、GitHub、arXiv、RSS）からデータ収集
    - キーワードベースのフィルタリング
    - 重複排除とキャッシュ管理

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます（松耦合）。
        ResilientAgent によりリトライ・タイムアウトが自動制御されます。
    """

    # ResilientAgent 設定
    name = "CollectorAgent"
    temperature = 0.3  # 要約タスクは低め

    def __init__(self) -> None:
        """初期化.

        Note:
            LLM クライアントは get_llm() により自動取得されます（松耦合原則）。
        """
        super().__init__()  # ResilientAgent が内部で get_llm() を呼び出す
        self._logger = logging.getLogger(self.name)
        self._cache: set[str] = set()  # URL キャッシュ（重複排除用）

        # NewsAPI クライアント初期化
        settings = get_settings()
        news_api_key = getattr(settings, "news_api_key", None)
        self._news_api_client = NewsAPIClient(api_key=news_api_key)

    def _parse_input(self, input_data: dict[str, Any]) -> CollectorInput:
        """入力データを Pydantic モデルに変換."""
        return CollectorInput(**input_data)

    async def process(self, input_data: CollectorInput) -> CollectorOutput:
        """データ収集を実行.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付き収集結果
        """
        keywords = input_data.keywords
        sources = input_data.sources
        # NOTE: date_range は将来の拡張用（現在未使用）

        self._logger.info(f"Starting collection: keywords={keywords}, sources={sources}")

        articles: list[Article] = []
        sources_stats: dict[str, int] = {}

        # 各ソースからデータ収集
        # NOTE: ResilientAgent がリトライ・タイムアウトを管理
        for source in sources:
            source_articles = await self._collect_from_source(
                source=source, keywords=keywords
            )
            articles.extend(source_articles)
            sources_stats[source] = len(source_articles)

        self._logger.info(f"Collection completed: total={len(articles)} articles")

        # 記事を ArticleSchema に変換
        article_schemas = [
            ArticleSchema(
                id=a.id,
                title=a.title,
                url=a.url,
                source=a.source.value,
                published_at=a.published_at.isoformat(),
                content=a.content,
                keywords=a.keywords,
                collected_at=a.collected_at.isoformat(),
                metadata=a.metadata,
            )
            for a in articles
        ]

        return CollectorOutput(
            articles=article_schemas,
            total_count=len(articles),
            sources_stats=sources_stats,
        )

    async def _collect_from_source(
        self, source: str, keywords: list[str]
    ) -> list[Article]:
        """特定ソースからデータ収集（NewsAPI 統合）.

        Args:
            source: データソース名
            keywords: 検索キーワード

        Returns:
            収集した記事リスト
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
        """LLM を使用して記事コンテンツを生成.

        Args:
            keyword: キーワード
            source: ソース名

        Returns:
            生成された記事コンテンツ
        """
        prompt = f"""Generate a brief news article summary about "{keyword}" from {source}.
The article should be informative and professional, around 200-300 words.
Focus on recent trends, developments, or insights related to {keyword}.
Write in Japanese."""

        try:
            # ResilientAgent の _call_llm を使用
            response = await self._call_llm(prompt)
            return response if response else self._fallback_content(keyword)
        except Exception as e:
            self._logger.warning(f"Failed to generate content with LLM: {e}")
            return self._fallback_content(keyword)

    def _fallback_content(self, keyword: str) -> str:
        """フォールバックコンテンツ生成."""
        return f"{keyword} に関する詳細な記事内容。最新の動向、技術的な進展、市場の反応などを含む包括的な分析。"

    def validate_output(self, output: CollectorOutput) -> bool:
        """出力検証.

        Args:
            output: 出力データ

        Returns:
            検証結果（True = 有効）
        """
        if output.total_count < 0:
            self._logger.warning("Validation failed: negative total_count")
            return False
        return True

