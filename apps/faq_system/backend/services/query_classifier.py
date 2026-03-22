"""後方互換: shared/services/query_classifier からの re-export."""

from __future__ import annotations

from shared.services.query_classifier import QueryClassifier, QueryType, classify_query


__all__ = ["QueryClassifier", "QueryType", "classify_query"]
