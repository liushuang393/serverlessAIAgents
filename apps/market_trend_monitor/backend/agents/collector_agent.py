# -*- coding: utf-8 -*-
"""データ収集エージェント.

複数のソースから市場動向データを収集します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
- 型安全：Pydantic による I/O 検証
- 健壮性：ResilientAgent によるリトライ・タイムアウト制御
"""

import logging
import os
import re
import uuid
from datetime import datetime, timezone
from typing import Any

from agentflow import ResilientAgent
from agentflow.config import get_settings
from dateutil import parser as date_parser

from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.integrations import (
    ArxivAPIClient,
    DevToAPIClient,
    GitHubAPIClient,
    NewsAPIClient,
    RSSFetcher,
    StackOverflowAPIClient,
)
from apps.market_trend_monitor.backend.models import (
    Article,
    ArticleSchema,
    CollectorInput,
    CollectorOutput,
    SourceType,
)


class CollectorAgent(ResilientAgent[CollectorInput, CollectorOutput]):
    """データ収集エージェント（ResilientAgent 継承・型安全）.

    役割:
    - 複数のソース（ニュース、GitHub、arXiv、RSS）からデータ収集
    - キーワードベースのフィルタリング
    - 重複排除とキャッシュ管理
    - セマンティック重複排除（Phase 6）
    - クエリ拡張（Phase 6）

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます（松耦合）。
        ResilientAgent によりリトライ・タイムアウトが自動制御されます。
    """

    # ResilientAgent 設定
    name = "CollectorAgent"
    temperature = 0.3  # 要約タスクは低め

    def __init__(
        self,
        *,
        semantic_search_service: Any | None = None,
        query_expansion_service: Any | None = None,
        source_reliability_tracker: Any | None = None,
    ) -> None:
        """初期化.

        Note:
            LLM クライアントは get_llm() により自動取得されます（松耦合原則）。

        Args:
            semantic_search_service: セマンティック検索サービス（オプショナル）
            query_expansion_service: クエリ拡張サービス（オプショナル）
            source_reliability_tracker: 情報源信頼度追跡（Phase 12）
        """
        super().__init__()  # ResilientAgent が内部で get_llm() を呼び出す
        self._logger = logging.getLogger(self.name)
        self._cache: set[str] = set()  # URL キャッシュ（重複排除用）

        settings = get_settings()
        news_api_key = getattr(settings, "news_api_key", None) or os.getenv("NEWS_API_KEY")
        github_token = getattr(settings, "github_token", None) or os.getenv("GITHUB_TOKEN")
        stackexchange_key = os.getenv("STACKEXCHANGE_KEY")
        devto_api_key = os.getenv("DEVTO_API_KEY")

        self._news_api_client = NewsAPIClient(api_key=news_api_key)
        self._github_api_client = GitHubAPIClient(token=github_token)
        self._arxiv_client = ArxivAPIClient()
        self._rss_fetcher = RSSFetcher()
        # Phase 12: 新規情報源
        self._stackoverflow_client = StackOverflowAPIClient(key=stackexchange_key)
        self._devto_client = DevToAPIClient(api_key=devto_api_key)

        # Phase 6: セマンティック検索・クエリ拡張
        self._semantic_search = semantic_search_service
        self._query_expansion = query_expansion_service
        # Phase 12: 情報源信頼度追跡
        self._reliability_tracker = source_reliability_tracker

    def _parse_input(self, input_data: dict[str, Any]) -> CollectorInput:
        """入力データを Pydantic モデルに変換."""
        return CollectorInput(**input_data)

    async def _expand_keywords(self, keywords: list[str]) -> list[str]:
        """クエリ拡張を適用してキーワードを拡張."""
        if not self._query_expansion:
            return keywords
        try:
            expanded: list[str] = []
            for kw in keywords:
                expanded.extend(await self._query_expansion.expand_query(kw, max_expansions=2))
            return list(dict.fromkeys(expanded))
        except Exception as e:
            self._logger.warning("クエリ拡張失敗: %s", e)
            return keywords

    async def _is_semantic_duplicate(self, article: Article) -> bool:
        """セマンティック類似度による重複判定."""
        if not self._semantic_search:
            return False
        try:
            similar = await self._semantic_search.find_similar(article)
            return len(similar) > 0
        except Exception as e:
            self._logger.debug("セマンティック重複判定失敗: %s", e)
            return False

    async def _index_articles(self, articles: list[Article]) -> None:
        """収集済み記事をベクトルインデックスに登録."""
        if not self._semantic_search:
            return
        try:
            await self._semantic_search.index_articles_batch(articles)
        except Exception as e:
            self._logger.warning("ベクトルインデックス登録失敗: %s", e)

    async def process(self, input_data: CollectorInput) -> CollectorOutput:
        """データ収集を実行.

        引数:
            input_data: 型付き入力データ

        戻り値:
            型付き収集結果
        """
        keywords = input_data.keywords
        sources = input_data.sources
        range_start, range_end = self._parse_date_range(input_data.date_range)

        # Phase 6: クエリ拡張
        search_keywords = await self._expand_keywords(keywords)
        self._logger.info(
            "収集開始: keywords=%s, expanded=%s, sources=%s",
            keywords, len(search_keywords), sources,
        )

        articles: list[Article] = []
        sources_stats: dict[str, int] = {}

        for source in sources:
            source_articles = await self._collect_from_source(
                source=source,
                keywords=search_keywords,
                range_start=range_start,
                range_end=range_end,
            )
            articles.extend(source_articles)
            sources_stats[source] = len(source_articles)

        # Phase 6: セマンティック重複排除
        if self._semantic_search and articles:
            articles = await self._semantic_search.deduplicate_batch(articles)

        if range_start is not None or range_end is not None:
            articles = self._filter_articles_by_date_range(
                articles,
                range_start=range_start,
                range_end=range_end,
            )

        # Phase 6: ベクトルインデックス登録
        await self._index_articles(articles)

        self._logger.info("収集完了: total=%s件", len(articles))

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
        self,
        source: str,
        keywords: list[str],
        range_start: datetime | None,
        range_end: datetime | None,
    ) -> list[Article]:
        """特定ソースからデータ収集."""
        self._logger.debug("ソース収集中: %s", source)

        source_key = source.lower().strip()
        articles: list[Article] = []
        success = True

        try:
            if source_key == SourceType.NEWS.value:
                articles = await self._collect_from_news(
                    keywords,
                    from_date=range_start,
                )
            elif source_key == SourceType.GITHUB.value:
                articles = await self._collect_from_github(keywords)
            elif source_key == SourceType.ARXIV.value:
                articles = await self._collect_from_arxiv(keywords)
            elif source_key == SourceType.RSS.value:
                articles = await self._collect_from_rss(keywords)
            elif source_key == SourceType.STACKOVERFLOW.value:
                articles = await self._collect_from_stackoverflow(keywords)
            elif source_key == SourceType.DEVTO.value:
                articles = await self._collect_from_devto(keywords)
            else:
                self._logger.warning("未対応ソース: %s", source)
                return []
        except Exception as exc:
            self._logger.warning("ソース収集エラー %s: %s", source, exc)
            success = False

        # Phase 12: 信頼度追跡に結果を報告
        if self._reliability_tracker:
            avg_len = (
                sum(len(a.content) for a in articles) / len(articles)
                if articles
                else 0.0
            )
            self._reliability_tracker.report_request(
                source_type=source_key,
                success=success and len(articles) > 0,
                article_count=len(articles),
                avg_content_length=avg_len,
            )

        return articles

    async def _collect_from_news(
        self,
        keywords: list[str],
        *,
        from_date: datetime | None = None,
    ) -> list[Article]:
        """NewsAPIから収集."""
        articles: list[Article] = []
        for keyword in keywords:
            try:
                news_articles = await self._news_api_client.search_everything(
                    query=keyword,
                    language="en",
                    page_size=5,
                    from_date=from_date,
                )

                for news_article in news_articles:
                    url = news_article.get("url", "")
                    if not url or url in self._cache:
                        continue

                    # ブレイクスルー（新発見・重要動向）の検知強化
                    title = news_article.get("title", f"{keyword} に関する最新動向")
                    content = news_article.get("content", news_article.get("description", ""))
                    
                    is_breakthrough = any(
                        word in (title + content).lower() 
                        for word in ["breakthrough", "innovative", "新手法", "画期的", "突破口", "automated"]
                    )
                    
                    article = Article(
                        id=str(uuid.uuid4()),
                        title=title,
                        url=url,
                        source=SourceType.NEWS,
                        published_at=self._parse_datetime(news_article.get("publishedAt")),
                        content=content,
                        keywords=[keyword],
                        collected_at=datetime.now(),
                        metadata={
                            "source_type": "news",
                            "query": keyword,
                            "news_source": news_article.get("source", {}).get("name", "Unknown"),
                            "author": news_article.get("author", "Unknown"),
                            "is_breakthrough": is_breakthrough,
                            "language": news_article.get("language", "en"),
                        },
                    )
                    articles.append(article)
                    self._cache.add(url)
            except Exception as exc:
                self._logger.warning("NewsAPI収集失敗 '%s': %s", keyword, exc)
        return articles

    async def _collect_from_github(self, keywords: list[str]) -> list[Article]:
        """GitHubから収集."""
        articles: list[Article] = []
        for keyword in keywords:
            repos = await self._github_api_client.search_repositories(query=keyword, per_page=5)
            for repo in repos:
                url = repo.get("html_url", "")
                if not url or url in self._cache:
                    continue

                updated_at = repo.get("updated_at")
                article = Article(
                    id=str(uuid.uuid4()),
                    title=repo.get("full_name", keyword),
                    url=url,
                    source=SourceType.GITHUB,
                    published_at=self._parse_datetime(updated_at),
                    content=repo.get("description", ""),
                    keywords=[keyword],
                    collected_at=datetime.now(),
                    metadata={
                        "source_type": "github",
                        "query": keyword,
                        "stars": repo.get("stargazers_count"),
                        "language": repo.get("language"),
                    },
                )
                articles.append(article)
                self._cache.add(url)
        return articles

    async def _collect_from_arxiv(self, keywords: list[str]) -> list[Article]:
        """arXivから収集."""
        articles: list[Article] = []
        for keyword in keywords:
            entries = await self._arxiv_client.search(query=keyword, max_results=5)
            for entry in entries:
                url = entry.get("link", "")
                if not url or url in self._cache:
                    continue

                published = entry.get("published")
                article = Article(
                    id=str(uuid.uuid4()),
                    title=str(entry.get("title", keyword)).strip(),
                    url=url,
                    source=SourceType.ARXIV,
                    published_at=self._parse_datetime(published),
                    content=entry.get("summary", ""),
                    keywords=[keyword],
                    collected_at=datetime.now(),
                    metadata={
                        "source_type": "arxiv",
                        "query": keyword,
                        "authors": [a.get("name") for a in entry.get("authors", [])],
                    },
                )
                articles.append(article)
                self._cache.add(url)
        return articles

    async def _collect_from_rss(self, keywords: list[str]) -> list[Article]:
        """RSSから収集."""
        articles: list[Article] = []
        fallback_entries: list[tuple[dict[str, Any], str]] = []
        feed_urls = config.collector.rss_feeds
        for feed_url in feed_urls:
            entries = await self._rss_fetcher.fetch(feed_url)
            for entry in entries:
                title = entry.get("title", "")
                summary = entry.get("summary", "")
                url = entry.get("link", "")
                if not url or url in self._cache:
                    continue

                matched_keywords = self._match_keywords_in_text(
                    text=f"{title}\n{summary}",
                    keywords=keywords,
                )

                if keywords:
                    if not matched_keywords:
                        # すべて不一致のケースに備え、候補を保持して後段でフォールバック採用する
                        if len(fallback_entries) < 30:
                            fallback_entries.append((entry, feed_url))
                        continue

                published = entry.get("published")
                article = Article(
                    id=str(uuid.uuid4()),
                    title=title or "RSS Update",
                    url=url,
                    source=SourceType.RSS,
                    published_at=self._parse_datetime(published),
                    content=summary,
                    keywords=matched_keywords or keywords[:1],
                    collected_at=datetime.now(),
                    metadata={
                        "source_type": "rss",
                        "feed": feed_url,
                    },
                )
                articles.append(article)
                self._cache.add(url)

        # RSS が取得できているのにキーワード不一致で 0 件になる場合の救済
        if not articles and keywords and fallback_entries:
            for entry, feed_url in fallback_entries[:10]:
                url = entry.get("link", "")
                if not url or url in self._cache:
                    continue
                article = Article(
                    id=str(uuid.uuid4()),
                    title=entry.get("title", "") or "RSS Update",
                    url=url,
                    source=SourceType.RSS,
                    published_at=self._parse_datetime(entry.get("published")),
                    content=entry.get("summary", ""),
                    keywords=keywords[:1],
                    collected_at=datetime.now(),
                    metadata={
                        "source_type": "rss",
                        "feed": feed_url,
                        "keyword_fallback": True,
                    },
                )
                articles.append(article)
                self._cache.add(url)
        return articles

    async def _collect_from_stackoverflow(self, keywords: list[str]) -> list[Article]:
        """Phase 12: StackOverflowから収集."""
        articles: list[Article] = []
        for keyword in keywords:
            try:
                questions = await self._stackoverflow_client.search_questions(
                    tagged=keyword.lower().replace(" ", "-"),
                    page_size=5,
                )
                for q in questions:
                    url = q.get("link", "")
                    if not url or url in self._cache:
                        continue
                    article = Article(
                        id=str(uuid.uuid4()),
                        title=q.get("title", keyword),
                        url=url,
                        source=SourceType.STACKOVERFLOW,
                        published_at=self._parse_datetime(
                            str(q.get("creation_date", "")),
                        ),
                        content=q.get("body", "")[:500],
                        keywords=[keyword],
                        collected_at=datetime.now(),
                        metadata={
                            "source_type": "stackoverflow",
                            "query": keyword,
                            "score": q.get("score", 0),
                            "view_count": q.get("view_count", 0),
                            "answer_count": q.get("answer_count", 0),
                            "is_answered": q.get("is_answered", False),
                        },
                    )
                    articles.append(article)
                    self._cache.add(url)
            except Exception as exc:
                self._logger.warning("StackOverflow収集失敗 '%s': %s", keyword, exc)
        return articles

    async def _collect_from_devto(self, keywords: list[str]) -> list[Article]:
        """Phase 12: DEV.toから収集."""
        articles: list[Article] = []
        for keyword in keywords:
            try:
                devto_articles = await self._devto_client.search_articles(
                    tag=keyword.lower().replace(" ", ""),
                    per_page=5,
                )
                for item in devto_articles:
                    url = item.get("url", "")
                    if not url or url in self._cache:
                        continue
                    article = Article(
                        id=str(uuid.uuid4()),
                        title=item.get("title", keyword),
                        url=url,
                        source=SourceType.DEVTO,
                        published_at=self._parse_datetime(
                            item.get("published_at"),
                        ),
                        content=item.get("description", ""),
                        keywords=[keyword],
                        collected_at=datetime.now(),
                        metadata={
                            "source_type": "devto",
                            "query": keyword,
                            "positive_reactions_count": item.get(
                                "positive_reactions_count", 0,
                            ),
                            "comments_count": item.get("comments_count", 0),
                            "reading_time_minutes": item.get(
                                "reading_time_minutes", 0,
                            ),
                        },
                    )
                    articles.append(article)
                    self._cache.add(url)
            except Exception as exc:
                self._logger.warning("DEV.to収集失敗 '%s': %s", keyword, exc)
        return articles

    def _match_keywords_in_text(self, text: str, keywords: list[str]) -> list[str]:
        """テキストに一致するキーワード一覧を返す."""
        normalized = re.sub(r"\s+", " ", text).strip().lower()
        matched: list[str] = []

        for keyword in keywords:
            candidate = keyword.strip().lower()
            if not candidate:
                continue

            # 日本語など非ASCIIキーワードは部分一致
            if any(ord(ch) > 127 for ch in candidate):
                if candidate in normalized:
                    matched.append(keyword)
                continue

            # 複合語は語単位でゆるく一致
            tokens = [t for t in re.split(r"\s+", candidate) if t]
            if len(tokens) > 1:
                if all(token in normalized for token in tokens):
                    matched.append(keyword)
                continue

            # 単語は境界一致優先、失敗時は部分一致
            pattern = rf"\b{re.escape(candidate)}\b"
            if re.search(pattern, normalized) or candidate in normalized:
                matched.append(keyword)

        return list(dict.fromkeys(matched))

    def _parse_datetime(self, raw: str | None) -> datetime:
        """日時文字列をパース（失敗時は現在時刻）."""
        if not raw:
            return datetime.now()
        try:
            return date_parser.parse(raw)
        except Exception:
            return datetime.now()

    def _parse_date_range(
        self,
        date_range: Any,
    ) -> tuple[datetime | None, datetime | None]:
        """入力 date_range を datetime に変換."""
        if not date_range:
            return None, None

        start_raw = getattr(date_range, "start", None)
        end_raw = getattr(date_range, "end", None)

        range_start = self._parse_datetime(start_raw) if start_raw else None
        range_end = self._parse_datetime(end_raw) if end_raw else None

        if range_start and range_end and range_start > range_end:
            range_start, range_end = range_end, range_start
        return range_start, range_end

    @staticmethod
    def _to_epoch_seconds(value: datetime) -> float:
        """datetime を UTC epoch 秒へ正規化."""
        if value.tzinfo is None:
            return value.replace(tzinfo=timezone.utc).timestamp()
        return value.astimezone(timezone.utc).timestamp()

    def _filter_articles_by_date_range(
        self,
        articles: list[Article],
        *,
        range_start: datetime | None,
        range_end: datetime | None,
    ) -> list[Article]:
        """収集結果を期間でフィルタ."""
        if range_start is None and range_end is None:
            return articles

        start_epoch = self._to_epoch_seconds(range_start) if range_start else None
        end_epoch = self._to_epoch_seconds(range_end) if range_end else None
        filtered: list[Article] = []
        for article in articles:
            published_epoch = self._to_epoch_seconds(article.published_at)
            if start_epoch is not None and published_epoch < start_epoch:
                continue
            if end_epoch is not None and published_epoch > end_epoch:
                continue
            filtered.append(article)
        return filtered

    def validate_output(self, output: CollectorOutput) -> bool:
        """出力検証."""
        if output.total_count < 0:
            self._logger.warning("Validation failed: negative total_count")
            return False
        return True
