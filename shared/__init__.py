"""Layer 2 の shared services 公開 API."""

from __future__ import annotations

import importlib
import sys

from shared.access.service import build_access_context
from shared.artifacts.service import ArtifactStore
from shared.audit.service import AuditService
from shared.config.manifest import load_app_manifest
from shared.gateway import SharedEmbeddingGateway, SharedLLMGateway, SharedRerankGateway
from shared.rag.service import SharedRAGService
from shared.registry import ServiceRegistry
from shared.scope.service import resolve_scope
from shared.trace.service import TraceService


def _register_legacy_package_aliases() -> None:
    """旧 shared 配下の package を新しい契約層へ割り当てる。"""
    if "domain.commerce" not in sys.modules:
        sys.modules["domain.commerce"] = importlib.import_module("contracts.protocol.commerce")


_register_legacy_package_aliases()


__all__ = [
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
