"""Web Router のユニットテスト."""

from __future__ import annotations

from typing import Any

import pytest

from contracts.web import (
    BrowserActionStep,
    EstimatedCostLevel,
    ExtractionSchema,
    QualityConstraints,
    WebIntent,
    WebIntentType,
    WebRetrievalMode,
    WebRouterInput,
)
from infrastructure.providers.web.browser.base import BrowserOperationRequest, BrowserOperationResult
from infrastructure.providers.web.crawl.base import CrawledDocument, CrawlResult
from infrastructure.providers.web.extract.base import ExtractionResult
from infrastructure.providers.web.fetch.base import FetchResult
from infrastructure.providers.web.search.base import SearchItem
from shared.web.cache import WebContentCache
from shared.web.router_service import DefaultWebRetrievalRouter


class _FakeSearchProvider:
    name = "fake_search"

    async def search(self, query: str, *, top_k: int = 5) -> list[SearchItem]:
        _ = query
        _ = top_k
        return [
            SearchItem(url="https://allowed.example.com/a", title="A", snippet="sa", score=0.9),
            SearchItem(url="https://blocked.example.com/b", title="B", snippet="sb", score=0.8),
        ]


class _FakeFetchProvider:
    def __init__(self, mode: WebRetrievalMode, *, ok: bool = True, markdown: str = "# Title\n\n本文") -> None:
        self.name = mode.value
        self._mode = mode
        self._ok = ok
        self._markdown = markdown
        self.calls = 0

    async def fetch(self, url: str) -> FetchResult:
        self.calls += 1
        if not self._ok:
            return FetchResult(
                ok=False,
                url=url,
                mode=self._mode,
                metadata={"provider": self.name, "reason": "failed", "dynamic_hint": self._mode is WebRetrievalMode.HTML_READABILITY},
            )
        return FetchResult(
            ok=True,
            url=url,
            mode=self._mode,
            markdown=self._markdown,
            metadata={"provider": self.name, "dynamic_hint": self._mode is WebRetrievalMode.RENDERED_MARKDOWN},
        )


class _FakeExtractProvider:
    name = "fake_extract"

    async def extract(self, markdown: str, schema: dict[str, Any] | None = None) -> ExtractionResult:
        _ = markdown
        if schema is None:
            return ExtractionResult(ok=True, data={"summary": "ok"}, metadata={"provider": self.name})
        return ExtractionResult(ok=True, data={"title": "抽出結果"}, metadata={"provider": self.name})


class _FakeBrowserOperator:
    name = "fake_browser"

    def __init__(self) -> None:
        self.last_request: BrowserOperationRequest | None = None

    async def run(self, request: BrowserOperationRequest) -> BrowserOperationResult:
        self.last_request = request
        return BrowserOperationResult(
            ok=True,
            markdown="# Browser\n\n操作済み",
            metadata={"provider": self.name},
            artifacts={"step_count": len(request.steps)},
        )


class _FakeCrawlProvider:
    name = "fake_crawl"

    async def crawl(self, seed_urls: list[str], *, max_pages: int = 10, max_depth: int = 2) -> CrawlResult:
        _ = max_pages
        _ = max_depth
        return CrawlResult(
            ok=True,
            documents=[CrawledDocument(url=url, markdown=f"# {url}\n\ncrawl", depth=0) for url in seed_urls],
            metadata={"provider": self.name},
        )


async def _identity_rerank(query: str, evidence: list[Any], *, top_k: int = 5) -> list[Any]:
    _ = query
    return evidence[:top_k]


def _build_router(
    *,
    search_provider: _FakeSearchProvider | None = None,
    direct_fetch_provider: _FakeFetchProvider | None = None,
    html_fetch_provider: _FakeFetchProvider | None = None,
    rendered_fetch_provider: _FakeFetchProvider | None = None,
    extract_provider: _FakeExtractProvider | None = None,
    browser_operator: _FakeBrowserOperator | None = None,
    crawl_provider: _FakeCrawlProvider | None = None,
    cache: WebContentCache | None = None,
) -> DefaultWebRetrievalRouter:
    return DefaultWebRetrievalRouter(
        search_provider=search_provider or _FakeSearchProvider(),
        direct_fetch_provider=direct_fetch_provider or _FakeFetchProvider(WebRetrievalMode.DIRECT_MARKDOWN, ok=False),
        html_fetch_provider=html_fetch_provider or _FakeFetchProvider(WebRetrievalMode.HTML_READABILITY),
        rendered_fetch_provider=rendered_fetch_provider or _FakeFetchProvider(WebRetrievalMode.RENDERED_MARKDOWN),
        extract_provider=extract_provider or _FakeExtractProvider(),
        browser_operator=browser_operator or _FakeBrowserOperator(),
        crawl_provider=crawl_provider or _FakeCrawlProvider(),
        cache=cache or WebContentCache(ttl_seconds=300),
    )


@pytest.fixture(autouse=True)
def _patch_reranker(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setattr("shared.web.router_service.rerank_evidence", _identity_rerank)


@pytest.mark.asyncio
async def test_read_uses_html_after_direct_failure() -> None:
    """read は direct 失敗後に html_readability へ昇格する."""
    direct = _FakeFetchProvider(WebRetrievalMode.DIRECT_MARKDOWN, ok=False)
    html = _FakeFetchProvider(WebRetrievalMode.HTML_READABILITY, ok=True)
    router = _build_router(direct_fetch_provider=direct, html_fetch_provider=html)

    result = await router.execute(
        WebRouterInput(
            request_id="req-read",
            intent=WebIntent(intent=WebIntentType.READ_URL, url="https://allowed.example.com/page"),
            allowed_domains=["allowed.example.com"],
        )
    )

    assert result.mode_used is WebRetrievalMode.HTML_READABILITY
    assert direct.calls == 1
    assert html.calls == 1
    assert result.metadata["fetch_provider"] == "html_readability"


@pytest.mark.asyncio
async def test_read_promotes_to_rendered_when_html_fails() -> None:
    """html 失敗時に rendered_markdown へ昇格する."""
    direct = _FakeFetchProvider(WebRetrievalMode.DIRECT_MARKDOWN, ok=False)
    html = _FakeFetchProvider(WebRetrievalMode.HTML_READABILITY, ok=False)
    rendered = _FakeFetchProvider(WebRetrievalMode.RENDERED_MARKDOWN, ok=True)
    router = _build_router(
        direct_fetch_provider=direct,
        html_fetch_provider=html,
        rendered_fetch_provider=rendered,
    )

    result = await router.execute(
        WebRouterInput(
            request_id="req-rendered",
            intent=WebIntent(intent=WebIntentType.READ_URL, url="https://allowed.example.com/dynamic"),
            allowed_domains=["allowed.example.com"],
        )
    )

    assert result.mode_used is WebRetrievalMode.RENDERED_MARKDOWN
    assert result.fallback_used is True
    assert result.estimated_cost_level is EstimatedCostLevel.LOW


@pytest.mark.asyncio
async def test_structured_output_applies_extract_after_read() -> None:
    """structured_output=True の場合、読取後に extract が適用される."""
    router = _build_router()

    result = await router.execute(
        WebRouterInput(
            request_id="req-extract",
            intent=WebIntent(intent=WebIntentType.EXTRACT, url="https://allowed.example.com/post"),
            constraints=QualityConstraints(structured_output=True),
            extraction_schema=ExtractionSchema(name="default", json_schema={"properties": {"title": {"type": "string"}}}),
            allowed_domains=["allowed.example.com"],
        )
    )

    assert isinstance(result.extracted_data, dict)
    assert result.extracted_data["title"] == "抽出結果"


@pytest.mark.asyncio
async def test_query_routes_to_search_then_fetch_with_domain_policy() -> None:
    """query は search_then_fetch で allow/deny を適用する."""
    router = _build_router()

    result = await router.execute(
        WebRouterInput(
            request_id="req-search",
            intent=WebIntent(intent=WebIntentType.SEARCH, query="agentflow"),
            allowed_domains=["allowed.example.com"],
            blocked_domains=["blocked.example.com"],
        )
    )

    assert result.mode_used is WebRetrievalMode.SEARCH_THEN_FETCH
    assert result.evidence
    assert all("allowed.example.com" in item.url for item in result.evidence)


@pytest.mark.asyncio
async def test_interaction_routes_to_browser_and_passes_steps() -> None:
    """interaction_required の場合 steps が browser provider に渡る."""
    browser = _FakeBrowserOperator()
    router = _build_router(browser_operator=browser)

    result = await router.execute(
        WebRouterInput(
            request_id="req-browser",
            intent=WebIntent(intent=WebIntentType.OPERATE, url="https://allowed.example.com/app"),
            constraints=QualityConstraints(interaction_required=True),
            operation_steps=[
                BrowserActionStep(action="click", selector="#open"),
                BrowserActionStep(action="type", selector="#query", text="agentflow"),
            ],
            allowed_domains=["allowed.example.com"],
        )
    )

    assert result.mode_used is WebRetrievalMode.BROWSER_MCP
    assert browser.last_request is not None
    assert len(browser.last_request.steps) == 2
    assert browser.last_request.steps[1].text == "agentflow"


@pytest.mark.asyncio
async def test_fetch_failure_falls_back_to_browser() -> None:
    """direct/html/rendered 全失敗時は browser fallback へ遷移する."""
    browser = _FakeBrowserOperator()
    router = _build_router(
        direct_fetch_provider=_FakeFetchProvider(WebRetrievalMode.DIRECT_MARKDOWN, ok=False),
        html_fetch_provider=_FakeFetchProvider(WebRetrievalMode.HTML_READABILITY, ok=False),
        rendered_fetch_provider=_FakeFetchProvider(WebRetrievalMode.RENDERED_MARKDOWN, ok=False),
        browser_operator=browser,
    )

    result = await router.execute(
        WebRouterInput(
            request_id="req-fallback",
            intent=WebIntent(intent=WebIntentType.READ_URL, url="https://allowed.example.com/fallback"),
            allowed_domains=["allowed.example.com"],
        )
    )

    assert result.mode_used is WebRetrievalMode.BROWSER_MCP
    assert result.fallback_used is True


@pytest.mark.asyncio
async def test_cache_hit_skips_refetch() -> None:
    """同一 URL の再取得時は共有 cache が効く."""
    direct = _FakeFetchProvider(WebRetrievalMode.DIRECT_MARKDOWN, ok=False)
    html = _FakeFetchProvider(WebRetrievalMode.HTML_READABILITY, ok=True)
    cache = WebContentCache(ttl_seconds=300)
    router = _build_router(direct_fetch_provider=direct, html_fetch_provider=html, cache=cache)
    request = WebRouterInput(
        request_id="req-cache",
        intent=WebIntent(intent=WebIntentType.READ_URL, url="https://allowed.example.com/cache"),
        allowed_domains=["allowed.example.com"],
    )

    first = await router.execute(request)
    second = await router.execute(request)

    assert first.mode_used is WebRetrievalMode.HTML_READABILITY
    assert second.metadata["cache_hit"] is True
    assert direct.calls == 1
    assert html.calls == 1


@pytest.mark.asyncio
async def test_crawl_intent_routes_to_crawl_mode() -> None:
    """crawl intent は crawl_mode で複数ページ根拠を返す."""
    router = _build_router()

    result = await router.execute(
        WebRouterInput(
            request_id="req-crawl",
            intent=WebIntent(
                intent=WebIntentType.CRAWL,
                urls=["https://allowed.example.com/a", "https://allowed.example.com/b"],
            ),
            allowed_domains=["allowed.example.com"],
        )
    )

    assert result.mode_used is WebRetrievalMode.CRAWL_MODE
    assert len(result.evidence) == 2
