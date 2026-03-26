"""Web Intelligence Router."""

from __future__ import annotations

import time
from typing import Any

from contracts.web import (
    EstimatedCostLevel,
    EvidenceItem,
    ExtractionSchema,
    WebIntentType,
    WebRetrievalMode,
    WebRouterInput,
    WebRouterOutput,
)
from shared.web import (
    WebContentCache,
    build_citations,
    dedupe_evidence,
    filter_allowed_urls,
    finalize_markdown,
    rerank_evidence,
    score_confidence,
)


class WebIntelligenceRouter:
    """統一 Web 取得 Router."""

    def __init__(self, cache: WebContentCache | None = None) -> None:
        # 遅延: kernel→infrastructure
        from infrastructure.providers.web import (
            get_browser_operator,
            get_crawl_provider,
            get_extraction_provider,
            get_fetch_provider,
            get_search_provider,
        )

        self._cache = cache or WebContentCache(ttl_seconds=300)
        self._search = get_search_provider()
        self._fetch = get_fetch_provider()
        self._extract = get_extraction_provider()
        self._browser = get_browser_operator()
        self._crawl = get_crawl_provider()

    async def execute(self, req: WebRouterInput) -> WebRouterOutput:
        """要求を実行し、統一出力を返す."""
        started = time.perf_counter()
        output = await self._route(req)
        latency_ms = int((time.perf_counter() - started) * 1000)
        return output.model_copy(update={"latency_ms": latency_ms})

    async def _route(self, req: WebRouterInput) -> WebRouterOutput:
        constraints = req.constraints
        if constraints.auth_required or constraints.interaction_required:
            return await self._run_browser(req)

        if req.intent.intent == WebIntentType.CRAWL:
            return await self._run_crawl(req)

        if isinstance(req.intent.url, str) and req.intent.url.strip():
            return await self._run_read(req)

        if isinstance(req.intent.query, str) and req.intent.query.strip():
            return await self._run_search(req)

        return WebRouterOutput(
            mode_used=WebRetrievalMode.DIRECT_MARKDOWN,
            answer_markdown="No retrievable content.",
            evidence=[],
            citations=[],
            latency_ms=0,
            confidence=0.0,
            estimated_cost_level=EstimatedCostLevel.LOW,
            fallback_used=False,
        )

    async def _run_read(self, req: WebRouterInput) -> WebRouterOutput:
        url = req.intent.url or ""
        allowed = filter_allowed_urls([url], req.allowed_domains, req.blocked_domains)
        if not allowed:
            return self._empty_output(WebRetrievalMode.DIRECT_MARKDOWN, "Domain policy blocked the URL.")
        target_url = allowed[0]

        cache_key = f"read::{target_url}"
        cached = self._cache.get(cache_key)
        if isinstance(cached, dict):
            markdown = str(cached.get("markdown", ""))
            mode_str = str(cached.get("mode", WebRetrievalMode.DIRECT_MARKDOWN.value))
            mode = WebRetrievalMode(mode_str)
            evidence = [self._build_evidence(target_url, markdown, confidence=0.7)]
            return await self._compose_output(
                req=req,
                mode=mode,
                evidence=evidence,
                fallback_used=False,
                cost_level=self._estimate_cost(mode),
            )

        fetched = await self._fetch.fetch(target_url)
        if fetched.ok and isinstance(fetched.markdown, str):
            normalized = finalize_markdown(fetched.markdown)
            self._cache.set(cache_key, {"markdown": normalized, "mode": fetched.mode.value})
            evidence = [self._build_evidence(target_url, normalized, confidence=0.75)]
            fallback_used = fetched.metadata.get("fallback_used", "false").lower() == "true"
            return await self._compose_output(
                req=req,
                mode=fetched.mode,
                evidence=evidence,
                fallback_used=fallback_used,
                cost_level=self._estimate_cost(fetched.mode),
            )

        browser_fallback = await self._run_browser(req)
        return browser_fallback.model_copy(update={"fallback_used": True})

    async def _run_search(self, req: WebRouterInput) -> WebRouterOutput:
        query = req.intent.query or ""
        candidates = await self._search.search(query, top_k=5)
        filtered_urls = filter_allowed_urls(
            [candidate.url for candidate in candidates],
            req.allowed_domains,
            req.blocked_domains,
        )
        evidence: list[EvidenceItem] = []
        fallback_used = False
        for url in filtered_urls[:3]:
            cached = self._cache.get(f"read::{url}")
            if isinstance(cached, dict):
                markdown = str(cached.get("markdown", ""))
                evidence.append(self._build_evidence(url, markdown, confidence=0.68))
                continue
            fetched = await self._fetch.fetch(url)
            if not fetched.ok or not isinstance(fetched.markdown, str):
                fallback_used = True
                continue
            normalized = finalize_markdown(fetched.markdown)
            self._cache.set(f"read::{url}", {"markdown": normalized, "mode": fetched.mode.value})
            evidence.append(self._build_evidence(url, normalized, confidence=0.62))
            if fetched.metadata.get("fallback_used", "false").lower() == "true":
                fallback_used = True

        evidence = dedupe_evidence(evidence)
        reranked = await rerank_evidence(query=query, evidence=evidence, top_k=5)
        return await self._compose_output(
            req=req,
            mode=WebRetrievalMode.SEARCH_THEN_FETCH,
            evidence=reranked,
            fallback_used=fallback_used,
            cost_level=EstimatedCostLevel.MEDIUM,
        )

    async def _run_browser(self, req: WebRouterInput) -> WebRouterOutput:
        url_candidates = [
            req.intent.url or "",
            *(req.intent.urls or []),
        ]
        filtered = filter_allowed_urls(url_candidates, req.allowed_domains, req.blocked_domains)
        if not filtered:
            return self._empty_output(WebRetrievalMode.BROWSER_MCP, "Domain policy blocked browser operation.")
        from infrastructure.providers.web.browser import BrowserOperationRequest  # 遅延: kernel→infrastructure

        browser_request = BrowserOperationRequest(
            url=filtered[0],
            steps=[],
            return_markdown=True,
            allowed_domains=req.allowed_domains,
        )
        result = await self._browser.run(browser_request)
        if not result.ok or not isinstance(result.markdown, str):
            return self._empty_output(WebRetrievalMode.BROWSER_MCP, "Browser operation failed.")
        evidence = [self._build_evidence(filtered[0], finalize_markdown(result.markdown), confidence=0.7)]
        return await self._compose_output(
            req=req,
            mode=WebRetrievalMode.BROWSER_MCP,
            evidence=evidence,
            fallback_used=False,
            cost_level=EstimatedCostLevel.HIGH,
        )

    async def _run_crawl(self, req: WebRouterInput) -> WebRouterOutput:
        seeds = req.intent.urls or ([req.intent.url] if req.intent.url else [])
        filtered_seeds = filter_allowed_urls(seeds, req.allowed_domains, req.blocked_domains)
        if not filtered_seeds:
            return self._empty_output(WebRetrievalMode.CRAWL_MODE, "No allowed crawl seed URL.")

        crawl_result = await self._crawl.crawl(filtered_seeds, max_pages=10, max_depth=2)
        if not crawl_result.ok:
            return self._empty_output(WebRetrievalMode.CRAWL_MODE, "Crawl failed.")

        evidence = [
            self._build_evidence(doc.url, finalize_markdown(doc.markdown), confidence=0.65)
            for doc in crawl_result.documents
            if doc.markdown.strip()
        ]
        evidence = dedupe_evidence(evidence)
        return await self._compose_output(
            req=req,
            mode=WebRetrievalMode.CRAWL_MODE,
            evidence=evidence,
            fallback_used=False,
            cost_level=EstimatedCostLevel.MEDIUM,
        )

    async def _compose_output(
        self,
        *,
        req: WebRouterInput,
        mode: WebRetrievalMode,
        evidence: list[EvidenceItem],
        fallback_used: bool,
        cost_level: EstimatedCostLevel,
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
    def _build_evidence(url: str, markdown: str, *, confidence: float) -> EvidenceItem:
        snippet = " ".join(markdown.splitlines()[:3]).strip()[:280]
        return EvidenceItem(
            url=url,
            snippet=snippet,
            markdown=markdown,
            confidence=confidence,
        )

    @staticmethod
    def _build_answer(evidence: list[EvidenceItem]) -> str:
        if not evidence:
            return "No retrievable content."
        lines: list[str] = ["# Web Retrieval Result", ""]
        for index, item in enumerate(evidence[:3], start=1):
            lines.append(f"## Source {index}")
            lines.append(f"- URL: {item.url}")
            lines.append("")
            if isinstance(item.snippet, str) and item.snippet:
                lines.append(item.snippet)
                lines.append("")
        return "\n".join(lines).strip()

    @staticmethod
    def _estimate_cost(mode: WebRetrievalMode) -> EstimatedCostLevel:
        if mode in {WebRetrievalMode.BROWSER_MCP}:
            return EstimatedCostLevel.HIGH
        if mode in {WebRetrievalMode.SEARCH_THEN_FETCH, WebRetrievalMode.CRAWL_MODE}:
            return EstimatedCostLevel.MEDIUM
        return EstimatedCostLevel.LOW

    @staticmethod
    def _empty_output(mode: WebRetrievalMode, message: str) -> WebRouterOutput:
        return WebRouterOutput(
            mode_used=mode,
            answer_markdown=message,
            evidence=[],
            citations=[],
            latency_ms=0,
            confidence=0.0,
            estimated_cost_level=EstimatedCostLevel.LOW,
            fallback_used=False,
        )
