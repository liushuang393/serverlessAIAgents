# -*- coding: utf-8 -*-
"""ウェブ情報収集スキル - Web Crawler.

ウェブサイトから情報を収集するスキル。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from agentflow.core.agent_block import AgentBlock


logger = logging.getLogger(__name__)


@dataclass
class CrawlConfig:
    """クロール設定."""

    max_pages: int = 100
    max_depth: int = 3
    timeout_seconds: float = 30.0
    respect_robots_txt: bool = True
    user_agent: str = "AgentFlow-Crawler/1.0"


@dataclass
class CrawledPage:
    """クロールしたページ."""

    url: str
    title: str
    content: str
    published_at: datetime | None = None
    author: str | None = None
    tags: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class CrawlResult:
    """クロール結果."""

    pages: list[CrawledPage]
    total_pages: int
    sources: list[str]
    duration_seconds: float
    errors: list[str] = field(default_factory=list)


class WebCrawler(AgentBlock):
    """ウェブ情報収集スキル."""

    def __init__(self, config: CrawlConfig | None = None) -> None:
        super().__init__()
        self._config = config or CrawlConfig()

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """スキル実行."""
        urls = input_data.get("urls", [])
        keywords = input_data.get("keywords", [])

        result = await self.crawl(urls=urls, keywords=keywords)

        return {
            "pages": [
                {
                    "url": p.url,
                    "title": p.title,
                    "content": p.content[:500],
                    "tags": p.tags,
                }
                for p in result.pages
            ],
            "total_pages": result.total_pages,
            "sources": result.sources,
            "duration_seconds": result.duration_seconds,
        }

    async def crawl(
        self,
        urls: list[str] | None = None,
        keywords: list[str] | None = None,
    ) -> CrawlResult:
        """ウェブをクロール."""
        start_time = datetime.now()
        logger.info("クロール開始: %d URLs", len(urls or []))

        # プレースホルダー実装
        pages = [
            CrawledPage(
                url=url,
                title=f"Sample Page - {url}",
                content=f"Sample content for {', '.join(keywords or [])}",
                tags=keywords or [],
            )
            for url in (urls or ["https://example.com"])[:5]
        ]

        duration = (datetime.now() - start_time).total_seconds()

        return CrawlResult(
            pages=pages,
            total_pages=len(pages),
            sources=urls or [],
            duration_seconds=duration,
        )
