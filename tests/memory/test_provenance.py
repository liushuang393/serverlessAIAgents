"""MemoryEntry 出典フィールド（provenance）のテスト.

TDD: source_id/source_excerpt/SPO フィールドの保存・パイプライン統合を確認。
"""

from __future__ import annotations

from infrastructure.memory.types import MemoryEntry
from shared.memory.fact_atomizer import FactAtomizer
from shared.memory.memory_manager import MemoryManager
from shared.memory.sensory_memory import SensoryMemory


class TestMemoryEntryProvenanceFields:
    """MemoryEntry の出典フィールドのテスト."""

    def test_source_id_field_exists(self) -> None:
        """MemoryEntry に source_id フィールドが存在すること."""
        from datetime import datetime

        from infrastructure.memory.types import MemoryType

        entry = MemoryEntry(
            id="test-id",
            content="テスト内容",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.SENSORY,
            source_id="msg-001",
        )
        assert entry.source_id == "msg-001"

    def test_source_excerpt_field_exists(self) -> None:
        """MemoryEntry に source_excerpt フィールドが存在すること."""
        from datetime import datetime

        from infrastructure.memory.types import MemoryType

        entry = MemoryEntry(
            id="test-id",
            content="テスト内容",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.SENSORY,
            source_excerpt="元テキストの抜粋",
        )
        assert entry.source_excerpt == "元テキストの抜粋"

    def test_spo_fields_exist(self) -> None:
        """MemoryEntry に SPO フィールドが存在すること."""
        from datetime import datetime

        from infrastructure.memory.types import MemoryType

        entry = MemoryEntry(
            id="test-id",
            content="テスト内容",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.SENSORY,
            atomic_subject="田中さん",
            atomic_predicate="承認する",
            atomic_object="プランA",
        )
        assert entry.atomic_subject == "田中さん"
        assert entry.atomic_predicate == "承認する"
        assert entry.atomic_object == "プランA"

    def test_needs_coreference_field_exists(self) -> None:
        """MemoryEntry に needs_coreference フィールドが存在すること."""
        from datetime import datetime

        from infrastructure.memory.types import MemoryType

        entry = MemoryEntry(
            id="test-id",
            content="テスト内容",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.SENSORY,
            needs_coreference=True,
        )
        assert entry.needs_coreference is True

    def test_default_values(self) -> None:
        """デフォルト値が None/False であること."""
        from datetime import datetime

        from infrastructure.memory.types import MemoryType

        entry = MemoryEntry(
            id="test-id",
            content="テスト内容",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.SENSORY,
        )
        assert entry.source_id is None
        assert entry.source_excerpt is None
        assert entry.atomic_subject is None
        assert entry.atomic_predicate is None
        assert entry.atomic_object is None
        assert entry.needs_coreference is False

    def test_to_dict_includes_provenance(self) -> None:
        """to_dict() に出典フィールドが含まれること."""
        from datetime import datetime

        from infrastructure.memory.types import MemoryType

        entry = MemoryEntry(
            id="test-id",
            content="テスト内容",
            topic="test",
            timestamp=datetime.now(),
            memory_type=MemoryType.SENSORY,
            source_id="msg-001",
            source_excerpt="元テキスト",
            atomic_subject="田中さん",
        )
        d = entry.to_dict()
        assert d["source_id"] == "msg-001"
        assert d["source_excerpt"] == "元テキスト"
        assert d["atomic_subject"] == "田中さん"


class TestSensoryMemoryWithAtomizer:
    """SensoryMemory + FactAtomizer 統合のテスト."""

    async def test_source_excerpt_stored(self) -> None:
        """source_excerpt が MemoryEntry に保存されること."""
        atomizer = FactAtomizer(llm_client=None)
        sensory = SensoryMemory(fact_atomizer=atomizer)
        entry = await sensory.process(
            "田中部長は来週月曜にプロジェクトAの予算を承認する予定です",
            source_id="msg-001",
        )
        assert entry is not None
        assert entry.source_excerpt is not None
        assert len(entry.source_excerpt) > 0

    async def test_discard_returns_none(self) -> None:
        """エントロピーゲートが discard すると None を返すこと."""
        atomizer = FactAtomizer(llm_client=None)
        sensory = SensoryMemory(fact_atomizer=atomizer)
        result = await sensory.process("こんにちは！")
        assert result is None

    async def test_spo_fields_stored(self) -> None:
        """AtomicFact の SPO フィールドが MemoryEntry に格納されること."""
        atomizer = FactAtomizer(llm_client=None)
        sensory = SensoryMemory(fact_atomizer=atomizer)
        entry = await sensory.process(
            "田中さんがプロジェクトAを承認しました",
            source_id="msg-002",
        )
        assert entry is not None
        # SPO フィールドが設定されていること（空でも可）
        assert entry.atomic_subject is not None or entry.atomic_object is not None

    async def test_source_id_stored(self) -> None:
        """source_id が MemoryEntry に保存されること."""
        atomizer = FactAtomizer(llm_client=None)
        sensory = SensoryMemory(fact_atomizer=atomizer)
        entry = await sensory.process(
            "田中さんは明日の会議でQ4プランを確認します",
            source_id="msg-003",
        )
        assert entry is not None
        assert entry.source_id == "msg-003"

    async def test_no_atomizer_works_as_before(self) -> None:
        """FactAtomizer なしは従来通り動作すること."""
        sensory = SensoryMemory()
        entry = await sensory.process("テスト情報")
        assert entry is not None
        assert entry.source_id is None


class TestMemoryManagerWithAtomizer:
    """MemoryManager + FactAtomizer 統合のテスト."""

    async def test_discard_returns_none(self) -> None:
        """エントロピーゲートが discard すると remember() が None を返すこと."""
        atomizer = FactAtomizer(llm_client=None)
        manager = MemoryManager(fact_atomizer=atomizer)
        await manager.start()
        try:
            result = await manager.remember("こんにちは！")
            assert result is None
        finally:
            await manager.stop()

    async def test_keep_returns_entry(self) -> None:
        """意味のあるテキストは remember() が MemoryEntry を返すこと."""
        atomizer = FactAtomizer(llm_client=None)
        manager = MemoryManager(fact_atomizer=atomizer)
        await manager.start()
        try:
            result = await manager.remember(
                "田中部長は来週月曜にプロジェクトAの予算を承認します",
                source_id="msg-001",
            )
            assert result is not None
            assert result.source_id == "msg-001"
        finally:
            await manager.stop()

    async def test_no_atomizer_backward_compatible(self) -> None:
        """FactAtomizer なしは従来通り MemoryEntry を返すこと."""
        manager = MemoryManager()
        await manager.start()
        try:
            result = await manager.remember("テスト情報")
            assert result is not None
        finally:
            await manager.stop()
