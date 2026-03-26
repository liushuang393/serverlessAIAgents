"""Web crawl provider 実装."""

from __future__ import annotations

import re
from collections import deque
from urllib.parse import urljoin, urlparse

import httpx

from infrastructure.providers.web.crawl.base import CrawledDocument, CrawlProvider, CrawlResult
from infrastructure.providers.web.fetch.registry import get_fetch_provider


_LINK_RE = re.compile(r'href=["\\\']([^"\\\']+)["\\\']', re.IGNORECASE)
_NAV_PATTERN = re.compile(r"(?im)^(navigation|nav|menu|footer|copyright).*$")
_MULTI_EMPTY = re.compile(r"\n{3,}")
_SCRIPT_STYLE_HINT = re.compile(r"(?im)<(script|style)[^>]*>.*?</\\1>")


def _normalize_markdown(markdown: str, max_chars: int = 30000) -> str:
    """巡回結果の Markdown を最小限正規化する."""
    text = markdown.replace("\r\n", "\n").replace("\r", "\n")
    text = _SCRIPT_STYLE_HINT.sub("", text)
    lines = [line for line in text.splitlines() if not _NAV_PATTERN.match(line.strip())]
    normalized = "\n".join(lines)
    normalized = re.sub(r"[ \t]{2,}", " ", normalized)
    return _MULTI_EMPTY.sub("\n\n", normalized).strip()[:max_chars]


def _same_domain(seed: str, candidate: str) -> bool:
    seed_host = urlparse(seed).netloc
    candidate_host = urlparse(candidate).netloc
    return bool(seed_host and candidate_host and seed_host == candidate_host)


class SiteCrawlProvider(CrawlProvider):
    """同一ドメイン巡回 provider."""

    name = "site_crawl_provider"

    async def crawl(self, seed_urls: list[str], *, max_pages: int = 10, max_depth: int = 2) -> CrawlResult:
        if not seed_urls:
            return CrawlResult(ok=False, documents=[], metadata={"provider": self.name, "reason": "empty_seed"})

        queue: deque[tuple[str, int]] = deque((url, 0) for url in seed_urls)
        visited: set[str] = set()
        docs: list[CrawledDocument] = []
        fetch_provider = get_fetch_provider()
        seed_primary = seed_urls[0]

        while queue and len(docs) < max_pages:
            url, depth = queue.popleft()
            if url in visited or depth > max_depth:
                continue
            visited.add(url)

            fetched = await fetch_provider.fetch(url)
            if fetched.ok and isinstance(fetched.markdown, str):
                docs.append(CrawledDocument(url=url, markdown=_normalize_markdown(fetched.markdown), depth=depth))

            if depth >= max_depth:
                continue

            try:
                async with httpx.AsyncClient(timeout=10.0, follow_redirects=True) as client:
                    response = await client.get(url)
                if response.status_code != 200:
                    continue
                links = _LINK_RE.findall(response.text)
                for raw_link in links:
                    absolute = urljoin(url, raw_link)
                    if absolute.startswith("http") and _same_domain(seed_primary, absolute):
                        queue.append((absolute, depth + 1))
            except Exception:
                continue

        return CrawlResult(
            ok=True,
            documents=docs,
            metadata={"provider": self.name, "visited": str(len(visited))},
        )


class RecursiveFetchProvider(CrawlProvider):
    """単純な複数 URL 取得 provider."""

    name = "recursive_fetch_provider"

    async def crawl(self, seed_urls: list[str], *, max_pages: int = 10, max_depth: int = 2) -> CrawlResult:
        _ = max_depth
        if not seed_urls:
            return CrawlResult(ok=False, documents=[], metadata={"provider": self.name, "reason": "empty_seed"})
        fetch_provider = get_fetch_provider()
        docs: list[CrawledDocument] = []
        for url in seed_urls[:max_pages]:
            fetched = await fetch_provider.fetch(url)
            if fetched.ok and isinstance(fetched.markdown, str):
                docs.append(CrawledDocument(url=url, markdown=_normalize_markdown(fetched.markdown), depth=0))
        return CrawlResult(ok=True, documents=docs, metadata={"provider": self.name})
