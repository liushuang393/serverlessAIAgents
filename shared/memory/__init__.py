"""memory パッケージ — 後方互換 re-export ブリッジ.

実装は kernel/memory/ に移動済み。新規コードは kernel.memory を使うこと。
"""

import warnings
from importlib import import_module


warnings.warn(
    "shared.memory は非推奨です。kernel.memory を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

_memory_manager = import_module("kernel.memory.memory_manager")
MemoryManager = _memory_manager.MemoryManager


__all__ = ["MemoryManager"]
