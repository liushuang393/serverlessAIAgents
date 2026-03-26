"""Web crawl provider 抽象."""

from __future__ import annotations

from typing import Any, Protocol

from pydantic import Field

from contracts.base import ContractModel


class CrawledDocument(ContractModel):
    """巡回で得た文書."""

    url: str = Field(..., description="ページ URL")
    markdown: str = Field(default="", description="ページ本文")
    title: str | None = Field(default=None, description="ページタイトル")
    depth: int = Field(default=0, ge=0, description="巡回深度")


class CrawlResult(ContractModel):
    """巡回結果."""

    ok: bool = Field(..., description="成功可否")
    documents: list[CrawledDocument] = Field(default_factory=list, description="収集文書")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタ情報")


class CrawlProvider(Protocol):
    """巡回 provider 契約."""

    name: str

    async def crawl(self, seed_urls: list[str], *, max_pages: int = 10, max_depth: int = 2) -> CrawlResult:
        """seed URL から巡回を実行する."""
