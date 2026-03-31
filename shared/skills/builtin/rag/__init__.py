"""後方互換ブリッジ — 実装は kernel/skills/builtin/ に移動済み."""

from __future__ import annotations

import warnings
from importlib import import_module


_rag = import_module("kernel.skills.builtin.rag.rag")
RAGConfig = _rag.RAGConfig
RAGResult = _rag.RAGResult
RAGSkill = _rag.RAGSkill


warnings.warn(
    "shared.skills.builtin.rag は非推奨です。kernel.skills.builtin.rag を使用してください。",
    DeprecationWarning,
    stacklevel=2,
)

__all__ = ["RAGConfig", "RAGResult", "RAGSkill"]
