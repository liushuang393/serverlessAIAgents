"""Layer 1 の infrastructure 公開 API.

循環インポートを回避するため遅延インポートを使用。
"""

from __future__ import annotations

import importlib
from typing import TYPE_CHECKING

if TYPE_CHECKING:
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
    from infrastructure.storage import BinaryStorage, InMemoryBinaryStorage, NoOpBinaryStorage


__all__ = [
    "AgentFlowLLMBackend",
    "BinaryStorage",
    "InMemoryBinaryStorage",
    "InMemoryTraceExporter",
    "LLMBackend",
    "LLMBackendRegistry",
    "LoggingTraceExporter",
    "MockLLMBackend",
    "NoOpBinaryStorage",
    "NoOpLLMBackend",
    "NoOpTraceExporter",
    "TraceExporter",
    "TraceExporterRegistry",
    "get_llm_backend",
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
}


def __getattr__(name: str) -> object:
    """遅延インポートで循環依存を回避."""
    if name in _SUBMODULE_MAP:
        mod = importlib.import_module(_SUBMODULE_MAP[name])
        return getattr(mod, name)
    msg = f"module 'infrastructure' has no attribute {name!r}"
    raise AttributeError(msg)
