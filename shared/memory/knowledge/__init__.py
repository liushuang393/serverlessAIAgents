"""knowledge パッケージ — 知識管理の統一エントリポイント."""

from __future__ import annotations

from shared.memory.knowledge.knowledge_manager import KnowledgeManager
from shared.memory.knowledge.memory_store import InMemoryKnowledgeStore
from shared.memory.knowledge.memvid_store import is_memvid_available
from shared.memory.knowledge.types import KnowledgeEntry, KnowledgeSource

# グローバルシングルトン
_global_manager: KnowledgeManager | None = None


def get_knowledge_manager() -> KnowledgeManager:
    """グローバル KnowledgeManager を取得（未作成なら自動生成）."""
    global _global_manager  # noqa: PLW0603
    if _global_manager is None:
        _global_manager = KnowledgeManager()
    return _global_manager


def get_knowledge_store() -> InMemoryKnowledgeStore:
    """デフォルトの InMemoryKnowledgeStore を返すファクトリ."""
    return InMemoryKnowledgeStore()


def reset_knowledge_manager() -> None:
    """グローバル KnowledgeManager をリセット（テスト用）."""
    global _global_manager  # noqa: PLW0603
    _global_manager = None


__all__ = [
    "InMemoryKnowledgeStore",
    "KnowledgeEntry",
    "KnowledgeManager",
    "KnowledgeSource",
    "get_knowledge_manager",
    "get_knowledge_store",
    "is_memvid_available",
    "reset_knowledge_manager",
]
