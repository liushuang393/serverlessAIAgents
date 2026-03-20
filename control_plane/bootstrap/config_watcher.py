"""Compatibility wrapper for the runtime config watcher."""

from kernel.runtime.config_watcher import (
    ConfigWatcher,
    _extract_contracts_llm,
    _extract_contracts_rag,
)


__all__ = ["ConfigWatcher", "_extract_contracts_llm", "_extract_contracts_rag"]
