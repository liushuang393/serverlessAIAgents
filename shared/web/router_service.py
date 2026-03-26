"""Web Router orchestration service."""

from __future__ import annotations

import time
from typing import TYPE_CHECKING, Any

from contracts.base import ComponentToggle
from contracts.web import (
    EstimatedCostLevel,
    EvidenceItem,
    ExtractionSchema,
    WebIntentType,
    WebRetrievalMode,
    WebRetrievalRouter,
    WebRouterInput,
    WebRouterOutput,
)
from infrastructure.providers.web import (
    get_browser_operator,
    get_crawl_provider,
    get_extraction_provider,
    get_fetch_provider,
    get_search_provider,
)
from infrastructure.providers.web.browser import BrowserOperationRequest, BrowserOperator
from infrastructure.providers.web.fetch import ContentFetchProvider, FetchResult
from shared.web.cache import WebContentCache, get_web_content_cache
from shared.web.citation_builder import build_citations
from shared.web.deduper import dedupe_evidence
from shared.web.domain_policy import filter_allowed_urls
from shared.web.markdown_pipeline import finalize_markdown
from shared.web.quality_score import score_confidence
from shared.web.reranker import rerank_evidence


if TYPE_CHECKING:
    from infrastructure.providers.web.crawl import CrawlProvider
    from infrastructure.providers.web.extract import StructuredExtractionProvider
    from infrastructure.providers.web.search import SearchItem, SearchProvider


class DefaultWebRetrievalRouter(WebRetrievalRouter):
    """設計書 7章に準拠した Web Router 実装."""

    def __init__(
        self,
        *,
        search_provider: SearchProvider | None = None,
        direct_fetch_provider: ContentFetchProvider | None = None,
        html_fetch_provider: ContentFetchProvider | None = None,
        rendered_fetch_provider: ContentFetchProvider | None = None,
        extract_provider: StructuredExtractionProvider | None = None,
        browser_operator: BrowserOperator | None = None,
        crawl_provider: CrawlProvider | None = None,
        cache: WebContentCache | None = None,
    ) -> None:
        self._search = search_provider or get_search_provider()
        self._direct_fetch = direct_fetch_provider or get_fetch_provider(ComponentToggle(implementation="direct"))
        self._html_fetch = html_fetch_provider or get_fetch_provider(ComponentToggle(implementation="html"))
        self._rendered_fetch = rendered_fetch_provider or get_fetch_provider(ComponentToggle(implementation="rendered"))
        self._extract = extract_provider or get_extraction_provider()
        self._browser = browser_operator or get_browser_operator()
        self._crawl = crawl_provider or get_crawl_provider()
        self._cache = cache or get_web_content_cache()

    async def execute(self, req: WebRouterInput) -> WebRouterOutput:
        """要求を実行し、観測情報付きで返す."""
        started = time.perf_counter()
        output = await self._route(req)
        latency_ms = int((time.perf_counter() - started) * 1000)
        metadata = dict(output.metadata)
        metadata["request_id"] = req.request_id
        return output.model_copy(update={"latency_ms": latency_ms, "metadata": metadata})

    async def _route(self, req: WebRouterInput) -> WebRouterOutput:
        if req.constraints.auth_required or req.constraints.interaction_required:
            return await self._run_browser(req, fallback_used=False)

        if req.intent.intent == WebIntentType.CRAWL:
            return await self._run_crawl(req)

        if isinstance(req.intent.url, str) and req.intent.url.strip():
            return await self._run_read(req)

        if isinstance(req.intent.query, str) and req.intent.query.strip():
            return await self._run_search(req)

        return self._empty_output(
            mode=WebRetrievalMode.DIRECT_MARKDOWN,
            message="No retrievable content.",
            cost_level=EstimatedCostLevel.LOW,
            metadata={"reason": "empty_request"},
        )

    async def _run_read(self, req: WebRouterInput) -> WebRouterOutput:
        target_url = self._select_single_url(req)
        if target_url is None:
            return self._empty_output(
                mode=WebRetrievalMode.DIRECT_MARKDOWN,
                message="Domain policy blocked the URL.",
                cost_level=EstimatedCostLevel.LOW,
                metadata={"reason": "domain_blocked"},
            )

        fetch_result, cache_hit = await self._fetch_url(target_url)
        if fetch_result is None:
            browser_result = await self._run_browser(req, fallback_used=True)
            browser_metadata = dict(browser_result.metadata)
            browser_metadata["read_fallback"] = True
            return browser_result.model_copy(update={"fallback_used": True, "metadata": browser_metadata})

        evidence = [self._build_evidence_from_fetch(url=target_url, fetch_result=fetch_result, cache_hit=cache_hit)]
        return await self._compose_output(
            req=req,
            mode=fetch_result.mode,
            evidence=evidence,
            fallback_used=bool(fetch_result.metadata.get("fallback_used", False)),
            cost_level=self._estimate_cost(fetch_result.mode),
            metadata={
                "cache_hit": cache_hit,
                "attempted_modes": fetch_result.metadata.get("attempted_modes", []),
                "fetch_provider": fetch_result.metadata.get("provider"),
            },
        )

    async def _run_search(self, req: WebRouterInput) -> WebRouterOutput:
        query = req.intent.query or ""
        candidates = await self._search.search(query, top_k=5)
        filtered = self._filter_search_candidates(candidates, req.allowed_domains, req.blocked_domains)
        evidence: list[EvidenceItem] = []
        fallback_used = False
        for item in filtered[:5]:
            fetch_result, cache_hit = await self._fetch_url(item.url)
            if fetch_result is None:
                fallback_used = True
                continue
            evidence.append(
                self._build_evidence_from_fetch(
                    url=item.url,
                    fetch_result=fetch_result,
                    cache_hit=cache_hit,
                    title=item.title,
                    snippet=item.snippet,
                )
            )
            fallback_used = fallback_used or bool(fetch_result.metadata.get("fallback_used", False))

        deduped = dedupe_evidence(evidence)
        reranked = await rerank_evidence(query=query, evidence=deduped, top_k=5)
        return await self._compose_output(
            req=req,
            mode=WebRetrievalMode.SEARCH_THEN_FETCH,
            evidence=reranked,
            fallback_used=fallback_used,
            cost_level=EstimatedCostLevel.MEDIUM,
            metadata={
                "search_provider": getattr(self._search, "name", "unknown"),
                "candidate_count": len(candidates),
                "filtered_candidate_count": len(filtered),
            },
        )

    async def _run_browser(self, req: WebRouterInput, *, fallback_used: bool) -> WebRouterOutput:
        target_url = self._select_single_url(req)
        if target_url is None:
            return self._empty_output(
                mode=WebRetrievalMode.BROWSER_MCP,
                message="Domain policy blocked browser operation.",
                cost_level=EstimatedCostLevel.HIGH,
                metadata={"reason": "domain_blocked"},
            )

        browser_request = BrowserOperationRequest(
            url=target_url,
            steps=req.operation_steps,
            return_markdown=True,
            allowed_domains=req.allowed_domains,
        )
        result = await self._browser.run(browser_request)
        if not result.ok or not isinstance(result.markdown, str) or not result.markdown.strip():
            return self._empty_output(
                mode=WebRetrievalMode.BROWSER_MCP,
                message="Browser operation failed.",
                cost_level=EstimatedCostLevel.HIGH,
                metadata={"browser_metadata": result.metadata, "artifacts": result.artifacts},
            )

        normalized = finalize_markdown(result.markdown)
        evidence = [
            EvidenceItem(
                url=target_url,
                markdown=normalized,
                snippet=" ".join(normalized.splitlines()[:3]).strip()[:280],
                confidence=0.74,
                metadata={
                    "provider": result.metadata.get("provider", "browser"),
                    "artifacts": result.artifacts,
                },
            )
        ]
        return await self._compose_output(
            req=req,
            mode=WebRetrievalMode.BROWSER_MCP,
            evidence=evidence,
            fallback_used=fallback_used,
            cost_level=EstimatedCostLevel.HIGH,
            metadata={"browser_metadata": result.metadata, "artifacts": result.artifacts},
        )

    async def _run_crawl(self, req: WebRouterInput) -> WebRouterOutput:
        seeds = req.intent.urls or ([req.intent.url] if req.intent.url else [])
        filtered_seeds = filter_allowed_urls(seeds, req.allowed_domains, req.blocked_domains)
        if not filtered_seeds:
            return self._empty_output(
                mode=WebRetrievalMode.CRAWL_MODE,
                message="No allowed crawl seed URL.",
                cost_level=EstimatedCostLevel.MEDIUM,
                metadata={"reason": "domain_blocked"},
            )

        crawl_result = await self._crawl.crawl(filtered_seeds, max_pages=10, max_depth=2)
        if not crawl_result.ok:
            return self._empty_output(
                mode=WebRetrievalMode.CRAWL_MODE,
                message="Crawl failed.",
                cost_level=EstimatedCostLevel.MEDIUM,
                metadata={"crawl_metadata": crawl_result.metadata},
            )

        evidence = [
            EvidenceItem(
                url=document.url,
                title=document.title,
                markdown=finalize_markdown(document.markdown),
                snippet=" ".join(document.markdown.splitlines()[:3]).strip()[:280],
                confidence=0.66,
                metadata={"depth": document.depth},
            )
            for document in crawl_result.documents
            if document.markdown.strip()
        ]
        return await self._compose_output(
            req=req,
            mode=WebRetrievalMode.CRAWL_MODE,
            evidence=evidence,
            fallback_used=False,
            cost_level=EstimatedCostLevel.MEDIUM,
            metadata={"crawl_metadata": crawl_result.metadata, "seed_count": len(filtered_seeds)},
        )

    async def _fetch_url(self, url: str) -> tuple[FetchResult | None, bool]:
        cache_key = f"read::{url}"
        cached = self._cache.get(cache_key)
        if isinstance(cached, dict):
            mode_raw = cached.get("mode", WebRetrievalMode.DIRECT_MARKDOWN.value)
            markdown_raw = cached.get("markdown", "")
            metadata_raw = cached.get("metadata", {})
            if isinstance(mode_raw, str) and isinstance(markdown_raw, str) and isinstance(metadata_raw, dict):
                return (
                    FetchResult(
                        ok=True,
                        url=url,
                        mode=WebRetrievalMode(mode_raw),
                        markdown=markdown_raw,
                        metadata=dict(metadata_raw),
                    ),
                    True,
                )

        attempts: list[FetchResult] = []
        for provider in (self._direct_fetch, self._html_fetch, self._rendered_fetch):
            result = await provider.fetch(url)
            attempts.append(result)
            if result.ok and isinstance(result.markdown, str) and result.markdown.strip():
                normalized = finalize_markdown(result.markdown)
                metadata = dict(result.metadata)
                metadata["attempted_modes"] = [attempt.mode.value for attempt in attempts]
                metadata["fallback_used"] = len(attempts) > 1
                success = FetchResult(
                    ok=True,
                    url=url,
                    mode=result.mode,
                    markdown=normalized,
                    metadata=metadata,
                )
                self._cache.set(
                    cache_key,
                    {
                        "mode": success.mode.value,
                        "markdown": normalized,
                        "metadata": metadata,
                    },
                )
                return success, False

        return None, False

    async def _compose_output(
        self,
        *,
        req: WebRouterInput,
        mode: WebRetrievalMode,
        evidence: list[EvidenceItem],
        fallback_used: bool,
        cost_level: EstimatedCostLevel,
        metadata: dict[str, Any],
    ) -> WebRouterOutput:
        normalized_evidence = dedupe_evidence(evidence)
        citations = build_citations(normalized_evidence)
        answer_markdown = self._build_answer(normalized_evidence)
        extracted_data = await self._maybe_extract(
            evidence=normalized_evidence,
            structured=req.constraints.structured_output,
            extraction_schema=req.extraction_schema,
        )
        confidence = score_confidence(normalized_evidence, fallback_used=fallback_used)
        output_metadata = dict(metadata)
        output_metadata["evidence_count"] = len(normalized_evidence)
        output_metadata["structured_output"] = req.constraints.structured_output
        return WebRouterOutput(
            mode_used=mode,
            answer_markdown=answer_markdown,
            extracted_data=extracted_data,
            evidence=normalized_evidence,
            citations=citations,
            latency_ms=0,
            confidence=confidence,
            estimated_cost_level=cost_level,
            fallback_used=fallback_used,
            metadata=output_metadata,
        )

    async def _maybe_extract(
        self,
        *,
        evidence: list[EvidenceItem],
        structured: bool,
        extraction_schema: ExtractionSchema | None,
    ) -> dict[str, Any] | list[dict[str, Any]] | None:
        if not structured:
            return None
        merged = "\n\n".join(item.markdown or "" for item in evidence if isinstance(item.markdown, str))
        schema_payload = extraction_schema.json_schema if extraction_schema is not None else None
        result = await self._extract.extract(merged, schema=schema_payload)
        if not result.ok:
            return None
        return result.data

    @staticmethod
    def _select_single_url(req: WebRouterInput) -> str | None:
        candidates = [req.intent.url or "", *(req.intent.urls or [])]
        filtered = filter_allowed_urls(candidates, req.allowed_domains, req.blocked_domains)
        return filtered[0] if filtered else None

    @staticmethod
    def _filter_search_candidates(
        candidates: list[SearchItem],
        allowed_domains: list[str] | None,
        blocked_domains: list[str] | None,
    ) -> list[SearchItem]:
        filtered_urls = set(filter_allowed_urls([item.url for item in candidates], allowed_domains, blocked_domains))
        return [item for item in candidates if item.url in filtered_urls]

    @staticmethod
    def _build_evidence_from_fetch(
        *,
        url: str,
        fetch_result: FetchResult,
        cache_hit: bool,
        title: str | None = None,
        snippet: str | None = None,
    ) -> EvidenceItem:
        markdown = fetch_result.markdown or ""
        evidence_snippet = snippet or " ".join(markdown.splitlines()[:3]).strip()[:280]
        return EvidenceItem(
            url=url,
            title=title,
            snippet=evidence_snippet,
            markdown=markdown,
            confidence=0.7 if cache_hit else 0.76,
            metadata={
                "provider": fetch_result.metadata.get("provider"),
                "cache_hit": cache_hit,
                "attempted_modes": fetch_result.metadata.get("attempted_modes", []),
            },
        )

    @staticmethod
    def _build_answer(evidence: list[EvidenceItem]) -> str:
        if not evidence:
            return "No retrievable content."
        lines: list[str] = ["# Web Retrieval Result", ""]
        for index, item in enumerate(evidence[:3], start=1):
            title = item.title or item.url
            lines.append(f"## Source {index}: {title}")
            lines.append(item.url)
            lines.append("")
            if isinstance(item.snippet, str) and item.snippet:
                lines.append(item.snippet)
                lines.append("")
        return "\n".join(lines).strip()

    @staticmethod
    def _estimate_cost(mode: WebRetrievalMode) -> EstimatedCostLevel:
        if mode is WebRetrievalMode.BROWSER_MCP:
            return EstimatedCostLevel.HIGH
        if mode in {WebRetrievalMode.SEARCH_THEN_FETCH, WebRetrievalMode.CRAWL_MODE}:
            return EstimatedCostLevel.MEDIUM
        return EstimatedCostLevel.LOW

    @staticmethod
    def _empty_output(
        *,
        mode: WebRetrievalMode,
        message: str,
        cost_level: EstimatedCostLevel,
        metadata: dict[str, Any],
    ) -> WebRouterOutput:
        return WebRouterOutput(
            mode_used=mode,
            answer_markdown=message,
            evidence=[],
            citations=[],
            latency_ms=0,
            confidence=0.0,
            estimated_cost_level=cost_level,
            fallback_used=False,
            metadata=metadata,
        )
