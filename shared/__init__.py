"""Shared-layer public API."""

from __future__ import annotations

from shared.access.contracts import AccessContext
from shared.access.service import build_access_context
from shared.artifacts.service import ArtifactStore
from shared.audit.service import AuditService
from shared.config.manifest import load_app_manifest
from shared.gateway import SharedEmbeddingGateway, SharedLLMGateway, SharedRerankGateway
from shared.rag.service import SharedRAGService
from shared.registry import ServiceRegistry
from shared.scope.service import resolve_scope
from shared.trace.service import TraceService


__all__ = [
    "AccessContext",
    "ArtifactStore",
    "AuditService",
    "ServiceRegistry",
    "SharedEmbeddingGateway",
    "SharedLLMGateway",
    "SharedRAGService",
    "SharedRerankGateway",
    "TraceService",
    "build_access_context",
    "load_app_manifest",
    "resolve_scope",
]
