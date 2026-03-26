"""Web provider chain / utility テスト."""

from __future__ import annotations

import pytest

from contracts.web import EvidenceItem, WebRetrievalMode
from infrastructure.providers.web.fetch.adapters import FallbackChainFetcher
from infrastructure.providers.web.fetch.base import FetchResult
from shared.web import build_citations, dedupe_evidence, filter_allowed_urls, get_web_content_cache, score_confidence


class _FailingFetcher:
    name = "first"

    async def fetch(self, url: str) -> FetchResult:
        return FetchResult(
            ok=False,
            url=url,
            mode=WebRetrievalMode.DIRECT_MARKDOWN,
            metadata={"provider": self.name},
        )


class _SuccessfulFetcher:
    name = "second"

    async def fetch(self, url: str) -> FetchResult:
        return FetchResult(
            ok=True,
            url=url,
            mode=WebRetrievalMode.HTML_READABILITY,
            markdown="# ok",
            metadata={"provider": self.name},
        )


@pytest.mark.asyncio
async def test_fallback_chain_marks_fallback_used() -> None:
    """chain の後段成功時に fallback_used=true を付与する."""
    chain = FallbackChainFetcher([_FailingFetcher(), _SuccessfulFetcher()])

    result = await chain.fetch("https://example.com")

    assert result.ok is True
    assert result.mode is WebRetrievalMode.HTML_READABILITY
    assert result.metadata.get("fallback_used") is True
    assert result.metadata.get("attempted_modes") == ["direct_markdown", "html_readability"]


def test_domain_policy_filter_allow_and_block() -> None:
    """allow/deny の domain policy が URL フィルタに反映される."""
    urls = [
        "https://allowed.example.com/a",
        "https://blocked.example.com/b",
        "https://other.example.com/c",
    ]
    filtered = filter_allowed_urls(
        urls,
        allowed_domains=["allowed.example.com", "blocked.example.com"],
        blocked_domains=["blocked.example.com"],
    )

    assert filtered == ["https://allowed.example.com/a"]


def test_dedupe_citation_and_quality_score() -> None:
    """dedupe/citation/quality score の基本動作."""
    evidence = [
        EvidenceItem(url="https://example.com/a", markdown="same", confidence=0.7),
        EvidenceItem(url="https://example.com/a", markdown="same", confidence=0.6),
        EvidenceItem(url="https://example.com/b", markdown="different", confidence=0.8),
    ]

    deduped = dedupe_evidence(evidence)
    citations = build_citations(deduped)
    score = score_confidence(deduped, fallback_used=False)

    assert len(deduped) == 2
    assert len(citations) == 2
    assert 0.0 <= score <= 1.0


def test_get_web_content_cache_returns_singleton() -> None:
    """共有キャッシュが singleton で返ること."""
    first = get_web_content_cache()
    second = get_web_content_cache()

    assert first is second
