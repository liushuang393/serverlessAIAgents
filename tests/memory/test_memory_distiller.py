"""記憶蒸留（MemoryDistiller）のテスト."""

import pytest
from datetime import datetime
from unittest.mock import AsyncMock, MagicMock

from agentflow.memory.memory_distiller import MemoryDistiller
from agentflow.memory.types import (
    MemoryEntry,
    MemorySemanticLevel,
    MemoryStability,
    MemoryType,
)


class TestMemoryDistiller:
    """MemoryDistillerクラスのテスト."""

    @pytest.fixture
    def mock_llm_client(self) -> MagicMock:
        """LLMクライアントのモック作成."""
        client = MagicMock()
        client.generate = AsyncMock(return_value="蒸留された抽象知識")
        return client

    @pytest.fixture
    def distiller(self, mock_llm_client: MagicMock) -> MemoryDistiller:
        """MemoryDistillerインスタンスを作成."""
        return MemoryDistiller(
            llm_client=mock_llm_client,
            similarity_threshold=0.7,
            min_cluster_size=2,  # テスト用に小さく
        )

    @pytest.fixture
    def distiller_no_llm(self) -> MemoryDistiller:
        """LLMなしのMemoryDistillerインスタンスを作成."""
        return MemoryDistiller(
            llm_client=None,
            similarity_threshold=0.7,
            min_cluster_size=2,
        )

    @pytest.fixture
    def sample_memories(self) -> list[MemoryEntry]:
        """テスト用記憶エントリ作成."""
        base_time = datetime.now()
        return [
            MemoryEntry(
                id="mem_1",
                content="AIは画像認識に強い",
                topic="AI",
                timestamp=base_time,
                memory_type=MemoryType.LONG_TERM,
                importance_score=0.6,
                semantic_level=MemorySemanticLevel.EPISODIC,
            ),
            MemoryEntry(
                id="mem_2",
                content="AIは自然言語処理が得意",
                topic="AI",
                timestamp=base_time,
                memory_type=MemoryType.LONG_TERM,
                importance_score=0.7,
                semantic_level=MemorySemanticLevel.EPISODIC,
            ),
            MemoryEntry(
                id="mem_3",
                content="AIは音声認識で高精度",
                topic="AI",
                timestamp=base_time,
                memory_type=MemoryType.LONG_TERM,
                importance_score=0.5,
                semantic_level=MemorySemanticLevel.EPISODIC,
            ),
        ]

    @pytest.mark.asyncio
    async def test_distill_basic(
        self, distiller_no_llm: MemoryDistiller, sample_memories: list[MemoryEntry]
    ) -> None:
        """基本的な蒸留テスト."""
        results = await distiller_no_llm.distill(sample_memories)

        # 蒸留結果が存在する
        assert len(results) >= 1
        result = results[0]
        # セマンティックレベルに昇格
        assert result.semantic_level == MemorySemanticLevel.SEMANTIC
        # 安定性がCONSOLIDATEDに
        assert result.stability == MemoryStability.CONSOLIDATED
        # メタデータに元のIDが含まれる
        assert "distilled_from" in result.metadata

    @pytest.mark.asyncio
    async def test_distill_not_enough(
        self, distiller_no_llm: MemoryDistiller
    ) -> None:
        """記憶が少なすぎる場合のテスト."""
        single_memory = [
            MemoryEntry(
                id="single",
                content="単一の記憶",
                topic="test",
                timestamp=datetime.now(),
                memory_type=MemoryType.LONG_TERM,
                importance_score=0.5,
            )
        ]

        results = await distiller_no_llm.distill(single_memory)
        # 最小クラスタサイズ未満なので蒸留されない
        assert len(results) == 0

    @pytest.mark.asyncio
    async def test_distill_with_llm(
        self, distiller: MemoryDistiller, sample_memories: list[MemoryEntry]
    ) -> None:
        """LLMを使用した蒸留テスト."""
        results = await distiller.distill(sample_memories)

        assert len(results) >= 1
        # LLMが呼び出されたことを確認
        assert distiller._llm_client.generate.called

    @pytest.mark.asyncio
    async def test_cluster_memories_empty(
        self, distiller_no_llm: MemoryDistiller
    ) -> None:
        """空リストの場合のクラスタリングテスト."""
        clusters = await distiller_no_llm._cluster_memories([])
        assert len(clusters) == 0

    def test_should_distill_true(
        self, distiller_no_llm: MemoryDistiller, sample_memories: list[MemoryEntry]
    ) -> None:
        """蒸留が必要な場合のテスト."""
        should = distiller_no_llm.should_distill(sample_memories)
        assert should is True

    def test_should_distill_false(
        self, distiller_no_llm: MemoryDistiller
    ) -> None:
        """蒸留が不要な場合のテスト."""
        single_memory = [
            MemoryEntry(
                id="single",
                content="単一の記憶",
                topic="test",
                timestamp=datetime.now(),
                memory_type=MemoryType.LONG_TERM,
                importance_score=0.5,
                semantic_level=MemorySemanticLevel.EPISODIC,
            )
        ]
        should = distiller_no_llm.should_distill(single_memory)
        assert should is False

    @pytest.mark.asyncio
    async def test_distill_importance_boost(
        self, distiller_no_llm: MemoryDistiller, sample_memories: list[MemoryEntry]
    ) -> None:
        """蒸留後の重要度ブーストテスト."""
        avg_importance = sum(m.importance_score for m in sample_memories) / len(
            sample_memories
        )

        results = await distiller_no_llm.distill(sample_memories)

        assert len(results) >= 1
        # 蒸留後は平均より重要度が上がる
        assert results[0].importance_score >= avg_importance


