"""Web 共通基盤."""

from shared.web.cache import WebContentCache, get_web_content_cache
from shared.web.citation_builder import build_citations
from shared.web.deduper import dedupe_evidence
from shared.web.domain_policy import filter_allowed_urls, is_domain_allowed
from shared.web.markdown_pipeline import finalize_markdown
from shared.web.normalizer import normalize_markdown
from shared.web.quality_score import score_confidence
from shared.web.reranker import rerank_evidence


__all__ = [
    "WebContentCache",
    "build_citations",
    "dedupe_evidence",
    "filter_allowed_urls",
    "finalize_markdown",
    "get_web_content_cache",
    "is_domain_allowed",
    "normalize_markdown",
    "rerank_evidence",
    "score_confidence",
]
