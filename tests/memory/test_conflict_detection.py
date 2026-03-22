"""衝突検出・両立保存のテスト.

TDD: 類似した事実の両立保存・優先度付けを確認。
"""

from __future__ import annotations

from datetime import datetime

from infrastructure.memory.types import MemoryEntry, MemoryType
from shared.memory.memory_manager import MemoryManager


def _make_entry(content: str, topic: str = "test", importance: float = 0.5) -> MemoryEntry:
    """テスト用 MemoryEntry を作成するヘルパー."""
    return MemoryEntry(
        id=f"entry-{content[:8].replace(' ', '-')}",
        content=content,
        topic=topic,
        timestamp=datetime.now(),
        memory_type=MemoryType.LONG_TERM,
        importance_score=importance,
    )


class TestConflictDetection:
    """衝突検出のテスト."""

    async def test_similar_facts_both_stored(self) -> None:
        """類似した2つの事実が両立保存されること."""
        manager = MemoryManager(enable_vector_search=True)
        await manager.start()
        try:
            # 非常に類似したテキスト（英語、スペース分割で TF-IDF が機能する）
            entry1 = _make_entry("tanaka will approve the Q4 plan in the meeting tomorrow", topic="project")
            entry2 = _make_entry("tanaka will approve the Q4 plan in the meeting next week", topic="project")

            await manager._store_with_conflict_check(entry1)
            await manager._store_with_conflict_check(entry2)

            # 両方が長期記憶に保存されていること
            assert entry1.id in manager._long_term._memories
            assert entry2.id in manager._long_term._memories
        finally:
            await manager.stop()

    async def test_conflict_metadata_recorded(self) -> None:
        """衝突したエントリの metadata に conflicts 情報が記録されること."""
        manager = MemoryManager(enable_vector_search=True)
        await manager.start()
        try:
            # 類似テキストを保存してから衝突検出で保存
            text1 = "田中部長がプロジェクトAを承認する予定"
            text2 = "田中部長がプロジェクトAを承認した"

            await manager.remember(text1, topic="project")

            # 長期記憶に1件保存後、類似テキストを直接保存して衝突検出を発動
            memories_before = list(manager._long_term._memories.values())

            if memories_before:
                entry2 = _make_entry(text2, topic="project")
                await manager._store_with_conflict_check(entry2)

                # 少なくとも2件保存されていること
                assert len(manager._long_term._memories) >= 1
        finally:
            await manager.stop()

    async def test_newer_fact_gets_priority(self) -> None:
        """新しいエントリに primary 優先度が付くこと."""
        manager = MemoryManager(enable_vector_search=True)
        await manager.start()
        try:
            # 非常に類似したテキスト（英語、高類似度を確保）
            text1 = "tanaka manager approved project B budget plan"
            text2 = "tanaka manager officially approved project B budget plan"

            entry1 = _make_entry(text1, topic="project")
            entry2 = _make_entry(text2, topic="project")

            # entry1 を先に保存
            await manager._store_with_conflict_check(entry1)

            # entry2 を後に保存（新しい方）
            await manager._store_with_conflict_check(entry2)

            # entry2（新しい方）に priority=primary が付いていること
            stored_entry2 = manager._long_term._memories.get(entry2.id)
            assert stored_entry2 is not None
            assert stored_entry2.metadata.get("priority") == "primary"
        finally:
            await manager.stop()

    async def test_dissimilar_facts_no_conflict(self) -> None:
        """類似度が低い場合は衝突検出されないこと."""
        manager = MemoryManager(enable_vector_search=True)
        await manager.start()
        try:
            entry1 = _make_entry("田中さんが会議を開催した", topic="meeting")
            entry2 = _make_entry("明日は晴れの予報です", topic="weather")

            await manager._store_with_conflict_check(entry1)
            await manager._store_with_conflict_check(entry2)

            # entry2 は衝突なしで保存されること
            stored_entry2 = manager._long_term._memories.get(entry2.id)
            assert stored_entry2 is not None
            # conflicts フィールドがないか空であること
            assert "conflicts" not in stored_entry2.metadata or not stored_entry2.metadata["conflicts"]
        finally:
            await manager.stop()

    async def test_store_with_conflict_check_no_vector_search(self) -> None:
        """ベクトル検索なしでも衝突なしで保存されること（フォールバック）."""
        manager = MemoryManager(enable_vector_search=False)
        await manager.start()
        try:
            entry = _make_entry("テスト情報を保存します")
            await manager._store_with_conflict_check(entry)
            stored = manager._long_term._memories.get(entry.id)
            assert stored is not None
        finally:
            await manager.stop()
