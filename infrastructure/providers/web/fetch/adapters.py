"""Web fetch provider 実装."""

from __future__ import annotations

import os
import re
from html import unescape
from urllib.parse import urlparse

import httpx

from contracts.web import WebRetrievalMode
from infrastructure.providers.web.fetch.base import ContentFetchProvider, FetchResult


_TAG_RE = re.compile(r"<[^>]+>")
_SCRIPT_STYLE_RE = re.compile(r"<(script|style)[^>]*>.*?</\\1>", re.IGNORECASE | re.DOTALL)
_MULTI_LINE_RE = re.compile(r"\n{3,}")


def _build_failure_metadata(
    *,
    provider: str,
    status_code: int | None = None,
    reason: str | None = None,
    dynamic_hint: bool = False,
    content_type: str | None = None,
) -> dict[str, object]:
    metadata: dict[str, object] = {"provider": provider, "dynamic_hint": dynamic_hint}
    if status_code is not None:
        metadata["status_code"] = status_code
    if reason is not None:
        metadata["reason"] = reason
    if content_type is not None:
        metadata["content_type"] = content_type
    return metadata


def _looks_dynamic_html(html: str) -> bool:
    script_count = html.lower().count("<script")
    return script_count >= 4 or "__next" in html or "window.__" in html or "data-reactroot" in html


def _html_to_markdown_like(html: str) -> str:
    """HTML を簡易 Markdown 風テキストへ変換."""
    no_script = _SCRIPT_STYLE_RE.sub("", html)
    text = _TAG_RE.sub(" ", no_script)
    text = unescape(text)
    text = re.sub(r"[ \t]{2,}", " ", text)
    text = _MULTI_LINE_RE.sub("\n\n", text.replace("\r", ""))
    return text.strip()


class DirectMarkdownFetcher(ContentFetchProvider):
    """直接 Markdown 取得."""

    name = "direct_markdown"

    async def fetch(self, url: str) -> FetchResult:
        headers = {"Accept": "text/markdown, text/html;q=0.9, */*;q=0.1"}
        async with httpx.AsyncClient(timeout=20.0, follow_redirects=True) as client:
            response = await client.get(url, headers=headers)

        content_type = response.headers.get("content-type", "")
        if response.status_code == 200 and "text/markdown" in content_type:
            return FetchResult(
                ok=True,
                url=url,
                mode=WebRetrievalMode.DIRECT_MARKDOWN,
                markdown=response.text,
                metadata={
                    "content_type": content_type,
                    "status_code": response.status_code,
                    "provider": self.name,
                    "dynamic_hint": False,
                },
            )
        return FetchResult(
            ok=False,
            url=url,
            mode=WebRetrievalMode.DIRECT_MARKDOWN,
            metadata=_build_failure_metadata(
                provider=self.name,
                status_code=response.status_code,
                reason="content_type_not_markdown",
                dynamic_hint=_looks_dynamic_html(response.text),
                content_type=content_type,
            ),
        )


class HtmlReadabilityFetcher(ContentFetchProvider):
    """HTML の可読本文取得."""

    name = "html_readability"

    async def fetch(self, url: str) -> FetchResult:
        headers = {"Accept": "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8"}
        async with httpx.AsyncClient(timeout=20.0, follow_redirects=True) as client:
            response = await client.get(url, headers=headers)
        if response.status_code != 200:
            return FetchResult(
                ok=False,
                url=url,
                mode=WebRetrievalMode.HTML_READABILITY,
                metadata=_build_failure_metadata(
                    provider=self.name, status_code=response.status_code, reason="http_error"
                ),
            )
        markdown = _html_to_markdown_like(response.text)
        if len(markdown) < 80:
            return FetchResult(
                ok=False,
                url=url,
                mode=WebRetrievalMode.HTML_READABILITY,
                metadata=_build_failure_metadata(
                    provider=self.name,
                    status_code=response.status_code,
                    reason="too_short",
                    dynamic_hint=_looks_dynamic_html(response.text),
                ),
            )
        return FetchResult(
            ok=True,
            url=url,
            mode=WebRetrievalMode.HTML_READABILITY,
            markdown=markdown,
            metadata={
                "status_code": response.status_code,
                "provider": self.name,
                "dynamic_hint": _looks_dynamic_html(response.text),
            },
        )


class RenderedMarkdownFetcher(ContentFetchProvider):
    """レンダリング後 Markdown 取得."""

    name = "rendered_markdown"

    def __init__(self, account_id: str | None = None, api_token: str | None = None) -> None:
        self._account_id = account_id or os.getenv("CLOUDFLARE_ACCOUNT_ID")
        self._api_token = api_token or os.getenv("CLOUDFLARE_API_TOKEN")

    async def fetch(self, url: str) -> FetchResult:
        if not self._account_id or not self._api_token:
            return FetchResult(
                ok=False,
                url=url,
                mode=WebRetrievalMode.RENDERED_MARKDOWN,
                metadata=_build_failure_metadata(
                    provider=self.name,
                    reason="cloudflare_credentials_missing",
                    dynamic_hint=True,
                ),
            )
        endpoint = f"https://api.cloudflare.com/client/v4/accounts/{self._account_id}/browser-rendering/markdown"
        headers = {"Authorization": f"Bearer {self._api_token}", "Content-Type": "application/json"}
        payload = {"url": url}
        async with httpx.AsyncClient(timeout=45.0) as client:
            response = await client.post(endpoint, headers=headers, json=payload)
        if response.status_code != 200:
            return FetchResult(
                ok=False,
                url=url,
                mode=WebRetrievalMode.RENDERED_MARKDOWN,
                metadata=_build_failure_metadata(
                    provider=self.name,
                    status_code=response.status_code,
                    reason="cloudflare_error",
                    dynamic_hint=True,
                ),
            )
        data = response.json()
        markdown = data.get("result", {}).get("markdown")
        if not isinstance(markdown, str) or not markdown.strip():
            return FetchResult(
                ok=False,
                url=url,
                mode=WebRetrievalMode.RENDERED_MARKDOWN,
                metadata=_build_failure_metadata(provider=self.name, reason="empty_markdown", dynamic_hint=True),
            )
        return FetchResult(
            ok=True,
            url=url,
            mode=WebRetrievalMode.RENDERED_MARKDOWN,
            markdown=markdown,
            metadata={"provider": self.name, "host": urlparse(url).netloc, "dynamic_hint": True},
        )


class FallbackChainFetcher(ContentFetchProvider):
    """fetch フォールバック連鎖."""

    name = "fallback_chain"

    def __init__(self, providers: list[ContentFetchProvider]) -> None:
        self._providers = providers

    async def fetch(self, url: str) -> FetchResult:
        last_result: FetchResult | None = None
        attempted_modes: list[str] = []
        for provider in self._providers:
            result = await provider.fetch(url)
            attempted_modes.append(result.mode.value)
            if result.ok:
                metadata = dict(result.metadata)
                metadata["fallback_used"] = provider.name != self._providers[0].name
                metadata["attempted_modes"] = attempted_modes
                return FetchResult(
                    ok=True,
                    url=result.url,
                    mode=result.mode,
                    markdown=result.markdown,
                    metadata=metadata,
                )
            last_result = result
        if last_result is not None:
            metadata = dict(last_result.metadata)
            metadata["attempted_modes"] = attempted_modes
            metadata["fallback_used"] = False
            return last_result.model_copy(update={"metadata": metadata})
        return FetchResult(
            ok=False,
            url=url,
            mode=WebRetrievalMode.DIRECT_MARKDOWN,
            metadata={"provider": self.name, "reason": "no_provider", "attempted_modes": attempted_modes},
        )
