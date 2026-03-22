"""knowledge パッケージ — 知識管理の統一エントリポイント."""

from __future__ import annotations

from shared.memory.knowledge.knowledge_manager import KnowledgeManager
from shared.memory.knowledge.memory_store import InMemoryKnowledgeStore
from shared.memory.knowledge.memvid_store import is_memvid_available
from shared.memory.knowledge.types import KnowledgeEntry, KnowledgeSource


# グローバルシングルトン
_global_manager: KnowledgeManager | None = None


def get_knowledge_manager(*, _new_instance: bool = False) -> KnowledgeManager:
    """グローバル KnowledgeManager を取得（未作成なら自動生成）.

    Args:
        _new_instance: True の場合、新しいインスタンスを生成して返す（テスト用）。
    """
    global _global_manager
    if _new_instance:
        return KnowledgeManager()
    if _global_manager is None:
        _global_manager = KnowledgeManager()
    return _global_manager


def get_knowledge_store(*, backend: str = "memory") -> InMemoryKnowledgeStore:
    """KnowledgeStore を返すファクトリ.

    Args:
        backend: バックエンド種別。"memory" または "auto"。
                 "auto" の場合、memvid が利用可能ならそちらを使用。
    """
    if backend == "auto" and is_memvid_available():
        from shared.memory.knowledge.memvid_store import MemvidKnowledgeStore

        return MemvidKnowledgeStore()  # type: ignore[return-value]
    return InMemoryKnowledgeStore()


def reset_knowledge_manager() -> None:
    """グローバル KnowledgeManager をリセット（テスト用）."""
    global _global_manager
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
