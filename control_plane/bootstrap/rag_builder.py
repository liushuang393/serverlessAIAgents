"""Compatibility wrapper for runtime RAG bootstrap helpers."""

from kernel.runtime.rag_builder import (
    RagBootstrapConfig,
    _normalize_rag_payload,
    build_rag_engine,
)


__all__ = ["RagBootstrapConfig", "_normalize_rag_payload", "build_rag_engine"]
