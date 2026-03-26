"""Web Intelligence の結合テスト."""

from __future__ import annotations

import importlib
import types

import httpx
import pytest

from contracts.web import BrowserActionStep, QualityConstraints, WebIntent, WebIntentType, WebRouterInput
from infrastructure.providers.web.browser import BrowserOperationRequest
from infrastructure.providers.web.browser.adapters import MCPBrowserOperator
from infrastructure.providers.web.browser.base import BrowserOperationResult
from infrastructure.providers.web.crawl.adapters import SiteCrawlProvider
from infrastructure.providers.web.fetch.base import FetchResult
from shared.web.router_service import DefaultWebRetrievalRouter


class _FakeSearchProvider:
    name = "fake_search"

    async def search(self, query: str, *, top_k: int = 5) -> list[types.SimpleNamespace]:
        _ = top_k
        return [
            types.SimpleNamespace(
                url="https://example.com/a",
                title="A",
                snippet=f"{query} result A",
            ),
            types.SimpleNamespace(
                url="https://blocked.com/b",
                title="B",
                snippet="blocked",
            ),
        ]


class _FakeFetchProvider:
    def __init__(self, *, mode: str, markdown: str) -> None:
        self._mode = mode
        self._markdown = markdown

    async def fetch(self, url: str) -> FetchResult:
        return FetchResult(
            ok=True,
            url=url,
            mode=self._mode,
            markdown=self._markdown.format(url=url),
            metadata={"provider": f"fetch:{self._mode}"},
        )


class _FakeExtractionProvider:
    async def extract(self, markdown: str, *, schema: dict[str, object] | None = None) -> types.SimpleNamespace:
        _ = schema
        return types.SimpleNamespace(ok=True, data={"summary": markdown.splitlines()[0] if markdown else ""})


class _FakeBrowserOperator:
    async def run(self, request: BrowserOperationRequest) -> BrowserOperationResult:
        return BrowserOperationResult(
            ok=True,
            markdown=f"# Browser\n\n{request.url}",
            metadata={"provider": "fake_browser"},
        )


class _FakeCrawlProvider:
    async def crawl(self, seed_urls: list[str], *, max_pages: int = 10, max_depth: int = 2) -> types.SimpleNamespace:
        _ = max_pages, max_depth
        return types.SimpleNamespace(
            ok=True,
            documents=[
                types.SimpleNamespace(url=seed_urls[0], title="seed", markdown="# Seed\n\nbody", depth=0),
            ],
            metadata={"provider": "fake_crawl"},
        )


@pytest.mark.asyncio
async def test_router_search_pipeline_builds_citations_and_extraction() -> None:
    """検索結果を fetch して citation/extract まで通す."""
    router = DefaultWebRetrievalRouter(
        search_provider=_FakeSearchProvider(),
        direct_fetch_provider=_FakeFetchProvider(mode="direct_markdown", markdown="# {url}\n\nbody"),
        html_fetch_provider=_FakeFetchProvider(mode="html_readability", markdown="# html\n\nbody"),
        rendered_fetch_provider=_FakeFetchProvider(mode="rendered_markdown", markdown="# rendered\n\nbody"),
        extract_provider=_FakeExtractionProvider(),
        browser_operator=_FakeBrowserOperator(),
        crawl_provider=_FakeCrawlProvider(),
    )

    result = await router.execute(
        WebRouterInput(
            request_id="search-1",
                intent=WebIntent(intent=WebIntentType.SEARCH, query="agentflow web"),
            constraints=QualityConstraints(structured_output=True),
            allowed_domains=["example.com"],
            blocked_domains=["blocked.com"],
        )
    )

    assert result.mode_used.value == "search_then_fetch"
    assert len(result.evidence) == 1
    assert result.evidence[0].url == "https://example.com/a"
    assert result.citations
    assert result.extracted_data == {"summary": "# https://example.com/a"}


@pytest.mark.asyncio
async def test_mcp_browser_operator_executes_steps_and_reads_markdown(monkeypatch: pytest.MonkeyPatch) -> None:
    """Playwright MCP operator が typed steps を順に実行する."""
    calls: list[tuple[str, dict[str, object]]] = []
    original_import_module = importlib.import_module

    class _FakeClient:
        def __init__(self, _config: object, *, enable_security: bool, timeout: float) -> None:
            self.enable_security = enable_security
            self.timeout = timeout

        async def __aenter__(self) -> _FakeClient:
            return self

        async def __aexit__(self, exc_type: object, exc: object, tb: object) -> None:
            _ = exc_type, exc, tb

        def list_tools(self) -> list[str]:
            return [
                "mcp://playwright/browser_navigate",
                "mcp://playwright/browser_click",
                "mcp://playwright/browser_type",
                "mcp://playwright/browser_wait_for",
                "mcp://playwright/browser_snapshot",
            ]

        async def call_tool(self, uri: str, arguments: dict[str, object]) -> dict[str, object]:
            calls.append((uri, arguments))
            if uri.endswith("browser_snapshot"):
                return {"success": True, "result": {"markdown": "# Snapshot\n\nok"}}
            return {"success": True, "result": {"uri": uri}}

    class _FakeMCPConfig:
        def __init__(self, *, servers: list[object]) -> None:
            self.servers = servers

    class _FakeMCPServerConfig:
        def __init__(self, **kwargs: object) -> None:
            self.kwargs = kwargs

    def _fake_import(name: str) -> object:
        if name == "kernel.protocols.mcp_client":
            return types.SimpleNamespace(MCPClient=_FakeClient)
        if name == "kernel.protocols.mcp_config":
            return types.SimpleNamespace(MCPConfig=_FakeMCPConfig, MCPServerConfig=_FakeMCPServerConfig)
        return original_import_module(name)

    monkeypatch.setattr("infrastructure.providers.web.browser.adapters.importlib.import_module", _fake_import)

    operator = MCPBrowserOperator()
    result = await operator.run(
        BrowserOperationRequest(
            url="https://example.com",
            steps=[
                BrowserActionStep(action="click", selector="#login"),
                BrowserActionStep(action="type", selector="#user", text="alice"),
                BrowserActionStep(action="wait", wait_for="Loaded", timeout_ms=5000),
            ],
            return_markdown=True,
        )
    )

    assert result.ok is True
    assert result.markdown == "# Snapshot\n\nok"
    assert [uri.rsplit("/", maxsplit=1)[-1] for uri, _ in calls] == [
        "browser_navigate",
        "browser_click",
        "browser_type",
        "browser_wait_for",
        "browser_snapshot",
    ]


@pytest.mark.asyncio
async def test_site_crawl_provider_keeps_same_domain(monkeypatch: pytest.MonkeyPatch) -> None:
    """crawl provider が同一ドメインだけを巡回する."""

    class _FakeFetchProvider:
        async def fetch(self, url: str) -> FetchResult:
            return FetchResult(
                ok=True,
                url=url,
                mode="html_readability",
                markdown=f"# {url}\n\nbody",
                metadata={"provider": "fake_fetch"},
            )

    class _FakeResponse:
        def __init__(self, text: str) -> None:
            self.status_code = 200
            self.text = text

    class _FakeAsyncClient:
        async def __aenter__(self) -> _FakeAsyncClient:
            return self

        async def __aexit__(self, exc_type: object, exc: object, tb: object) -> None:
            _ = exc_type, exc, tb

        async def get(self, url: str) -> _FakeResponse:
            _ = url
            return _FakeResponse(
                '<a href="/page-2">same</a><a href="https://outside.com/page">outside</a>'
            )

    monkeypatch.setattr("infrastructure.providers.web.crawl.adapters.get_fetch_provider", lambda: _FakeFetchProvider())
    monkeypatch.setattr(httpx, "AsyncClient", lambda **kwargs: _FakeAsyncClient())

    provider = SiteCrawlProvider()
    result = await provider.crawl(["https://example.com/start"], max_pages=5, max_depth=1)

    assert result.ok is True
    assert [doc.url for doc in result.documents] == [
        "https://example.com/start",
        "https://example.com/page-2",
    ]
