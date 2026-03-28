"""memory パッケージ — 後方互換 re-export ブリッジ.

実装は kernel/memory/ に移動済み。新規コードは kernel.memory を使うこと。
"""
import warnings

warnings.warn(
    "shared.memory は非推奨です。kernel.memory を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

from kernel.memory.memory_manager import MemoryManager  # noqa: E402

__all__ = ["MemoryManager"]
