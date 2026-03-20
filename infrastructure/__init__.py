"""Layer 1 の infrastructure 公開 API.

循環インポートを回避するため遅延インポートを使用。
"""

from __future__ import annotations

import importlib
import sys
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from infrastructure.embeddings import EmbeddingBackendRegistry, get_embedding_backend
    from infrastructure.llm import (
        AgentFlowLLMBackend,
        LLMBackend,
        LLMBackendRegistry,
        MockLLMBackend,
        NoOpLLMBackend,
        get_llm_backend,
    )
    from infrastructure.observability import (
        InMemoryTraceExporter,
        LoggingTraceExporter,
        NoOpTraceExporter,
        TraceExporter,
        TraceExporterRegistry,
        get_trace_exporter,
    )
    from infrastructure.rerank import RerankBackendRegistry, get_rerank_backend
    from infrastructure.storage import BinaryStorage, InMemoryBinaryStorage, NoOpBinaryStorage


__all__ = [
    "AgentFlowLLMBackend",
    "BinaryStorage",
    "EmbeddingBackendRegistry",
    "InMemoryBinaryStorage",
    "InMemoryTraceExporter",
    "LLMBackend",
    "LLMBackendRegistry",
    "LoggingTraceExporter",
    "MockLLMBackend",
    "NoOpBinaryStorage",
    "NoOpLLMBackend",
    "NoOpTraceExporter",
    "RerankBackendRegistry",
    "TraceExporter",
    "TraceExporterRegistry",
    "get_embedding_backend",
    "get_llm_backend",
    "get_rerank_backend",
    "get_trace_exporter",
]

_SUBMODULE_MAP: dict[str, str] = {
    "AgentFlowLLMBackend": "infrastructure.llm",
    "LLMBackend": "infrastructure.llm",
    "LLMBackendRegistry": "infrastructure.llm",
    "MockLLMBackend": "infrastructure.llm",
    "NoOpLLMBackend": "infrastructure.llm",
    "get_llm_backend": "infrastructure.llm",
    "InMemoryTraceExporter": "infrastructure.observability",
    "LoggingTraceExporter": "infrastructure.observability",
    "NoOpTraceExporter": "infrastructure.observability",
    "TraceExporter": "infrastructure.observability",
    "TraceExporterRegistry": "infrastructure.observability",
    "get_trace_exporter": "infrastructure.observability",
    "BinaryStorage": "infrastructure.storage",
    "InMemoryBinaryStorage": "infrastructure.storage",
    "NoOpBinaryStorage": "infrastructure.storage",
    "EmbeddingBackendRegistry": "infrastructure.embeddings",
    "get_embedding_backend": "infrastructure.embeddings",
    "RerankBackendRegistry": "infrastructure.rerank",
    "get_rerank_backend": "infrastructure.rerank",
}

_LEGACY_PACKAGE_ALIASES: dict[str, str] = {
    "infrastructure.database": "infrastructure.storage.database",
    "infrastructure.memory": "infrastructure.storage.memory",
    "infrastructure.os": "infrastructure.sandbox.os",
    # NOTE: infrastructure.security は正規パスのため legacy alias 不要
    # "infrastructure.security": "infrastructure.secrets.security" は
    # infrastructure/secrets/ ディレクトリが存在しないため削除
}


def _register_legacy_package_aliases() -> None:
    """仕様外 root を新しい正規パスへ割り当てる。"""
    for legacy_name, target_name in _LEGACY_PACKAGE_ALIASES.items():
        if legacy_name in sys.modules:
            continue
        sys.modules[legacy_name] = importlib.import_module(target_name)


_register_legacy_package_aliases()


def __getattr__(name: str) -> object:
    """遅延インポートで循環依存を回避."""
    if name in _SUBMODULE_MAP:
        mod = importlib.import_module(_SUBMODULE_MAP[name])
        return getattr(mod, name)
    msg = f"module 'infrastructure' has no attribute {name!r}"
    raise AttributeError(msg)
