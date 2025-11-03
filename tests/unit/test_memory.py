"""
Memoryコンポーネントの単体テスト
"""

from datetime import datetime

import numpy as np
import pytest

from ai_blocks.core.memory import InMemoryVectorStore, MockEmbedder, VectorMemory
from ai_blocks.core.models import MemoryItem


class TestInMemoryVectorStore:
    """InMemoryVectorStoreのテスト"""

    def setup_method(self):
        """各テストメソッドの前に実行される"""
        self.store = InMemoryVectorStore(embedding_dim=128)

    def test_initialization(self):
        """初期化のテスト"""
        assert self.store.embedding_dim == 128
        assert len(self.store.vectors) == 0
        assert len(self.store.contents) == 0
        assert len(self.store.metadata) == 0
        assert len(self.store.created_at) == 0

    def test_add_and_get(self):
        """追加と取得のテスト"""
        import numpy as np

        # テストデータを追加
        vector = np.random.randn(128)
        content = "テストコンテンツ"
        metadata = {"type": "test"}

        self.store.add("test_id", vector, content, metadata)

        # データを取得
        result = self.store.get("test_id")
        assert result is not None

        stored_vector, stored_content, stored_metadata, created_at = result
        assert stored_content == content
        assert stored_metadata == metadata
        assert isinstance(created_at, datetime)

        # ベクトルが正規化されているかチェック
        assert abs(np.linalg.norm(stored_vector) - 1.0) < 1e-6

    def test_search(self):
        """検索のテスト"""
        import numpy as np

        # 複数のベクトルを追加
        for i in range(5):
            vector = np.random.randn(128)
            content = f"コンテンツ{i}"
            metadata = {"index": i}
            self.store.add(f"id_{i}", vector, content, metadata)

        # 検索を実行
        query_vector = np.random.randn(128)
        results = self.store.search(query_vector, limit=3)

        assert len(results) <= 3
        assert len(results) <= 5  # 追加したアイテム数以下

        # 結果が類似度順にソートされているかチェック
        if len(results) > 1:
            for i in range(len(results) - 1):
                assert results[i][1] >= results[i + 1][1]

    def test_delete(self):
        """削除のテスト"""
        import numpy as np

        # データを追加
        vector = np.random.randn(128)
        self.store.add("test_id", vector, "content", {})

        # 削除前の確認
        assert self.store.get("test_id") is not None
        assert self.store.count() == 1

        # 削除
        result = self.store.delete("test_id")
        assert result is True

        # 削除後の確認
        assert self.store.get("test_id") is None
        assert self.store.count() == 0

        # 存在しないIDの削除
        result = self.store.delete("non_existent")
        assert result is False

    def test_clear(self):
        """クリアのテスト"""
        import numpy as np

        # 複数のデータを追加
        for i in range(3):
            vector = np.random.randn(128)
            self.store.add(f"id_{i}", vector, f"content_{i}", {})

        assert self.store.count() == 3

        # クリア
        self.store.clear()

        assert self.store.count() == 0
        assert len(self.store.vectors) == 0


class TestMockEmbedder:
    """MockEmbedderのテスト"""

    def setup_method(self):
        """各テストメソッドの前に実行される"""
        self.embedder = MockEmbedder(embedding_dim=256)

    @pytest.mark.asyncio
    async def test_embed(self):
        """埋め込みのテスト"""
        text = "テストテキスト"
        vector = await self.embedder.embed(text)

        assert vector.shape == (256,)
        assert abs(np.linalg.norm(vector) - 1.0) < 1e-6  # 正規化されているか

        # 同じテキストは同じベクトルを返すか
        vector2 = await self.embedder.embed(text)
        assert np.allclose(vector, vector2)

        # 異なるテキストは異なるベクトルを返すか
        vector3 = await self.embedder.embed("異なるテキスト")
        assert not np.allclose(vector, vector3)


class TestVectorMemory:
    """VectorMemoryのテスト"""

    def setup_method(self):
        """各テストメソッドの前に実行される"""
        self.memory = VectorMemory(max_items=10, similarity_threshold=0.5)

    @pytest.mark.asyncio
    async def test_store_and_get(self):
        """保存と取得のテスト"""
        content = "これはテストコンテンツです"
        metadata = {"type": "test", "category": "unit_test"}

        # 保存
        memory_id = await self.memory.store(content, metadata)
        assert isinstance(memory_id, str)
        assert len(memory_id) > 0

        # 取得
        memory_item = await self.memory.get(memory_id)
        assert memory_item is not None
        assert memory_item.content == content
        assert memory_item.metadata["type"] == "test"
        assert memory_item.metadata["category"] == "unit_test"
        assert isinstance(memory_item.created_at, datetime)

    @pytest.mark.asyncio
    async def test_search(self):
        """検索のテスト"""
        # 複数のコンテンツを保存
        contents = [
            "Python プログラミング言語について",
            "機械学習とAIの基礎",
            "データベース設計の原則",
            "ウェブ開発のベストプラクティス",
            "ソフトウェアテストの重要性",
        ]

        for content in contents:
            await self.memory.store(content, {"type": "knowledge"})

        # 検索を実行
        query = "プログラミング"
        results = await self.memory.search(query, limit=3)

        assert isinstance(results, list)
        assert len(results) <= 3

        # 結果の形式をチェック
        for item in results:
            assert isinstance(item, MemoryItem)
            assert hasattr(item, "content")
            assert hasattr(item, "similarity_score")
            assert item.similarity_score is not None

    @pytest.mark.asyncio
    async def test_delete(self):
        """削除のテスト"""
        content = "削除テスト用コンテンツ"
        memory_id = await self.memory.store(content)

        # 削除前の確認
        item = await self.memory.get(memory_id)
        assert item is not None

        # 削除
        result = await self.memory.delete(memory_id)
        assert result is True

        # 削除後の確認
        item = await self.memory.get(memory_id)
        assert item is None

    @pytest.mark.asyncio
    async def test_clear(self):
        """クリアのテスト"""
        # 複数のアイテムを保存
        for i in range(5):
            await self.memory.store(f"コンテンツ{i}")

        count_before = await self.memory.count()
        assert count_before == 5

        # クリア
        await self.memory.clear()

        count_after = await self.memory.count()
        assert count_after == 0

    @pytest.mark.asyncio
    async def test_count(self):
        """カウントのテスト"""
        initial_count = await self.memory.count()
        assert initial_count == 0

        # アイテムを追加
        for i in range(3):
            await self.memory.store(f"コンテンツ{i}")

        count = await self.memory.count()
        assert count == 3

    @pytest.mark.asyncio
    async def test_max_items_limit(self):
        """最大アイテム数制限のテスト"""
        memory = VectorMemory(max_items=3)

        # 制限を超えてアイテムを追加
        for i in range(5):
            await memory.store(f"コンテンツ{i}")

        count = await memory.count()
        assert count <= 3  # 最大数を超えない

    @pytest.mark.asyncio
    async def test_empty_content_error(self):
        """空のコンテンツエラーのテスト"""
        with pytest.raises(ValueError, match="空のコンテンツは保存できません"):
            await self.memory.store("")

        with pytest.raises(ValueError, match="空のコンテンツは保存できません"):
            await self.memory.store("   ")

    @pytest.mark.asyncio
    async def test_empty_query_error(self):
        """空のクエリエラーのテスト"""
        with pytest.raises(ValueError, match="空のクエリでは検索できません"):
            await self.memory.search("")

        with pytest.raises(ValueError, match="空のクエリでは検索できません"):
            await self.memory.search("   ")

    @pytest.mark.asyncio
    async def test_similarity_threshold(self):
        """類似度閾値のテスト"""
        memory = VectorMemory(similarity_threshold=0.9)  # 高い閾値

        # コンテンツを保存
        await memory.store("テストコンテンツ")

        # 低い類似度での検索（結果が少なくなるはず）
        results = await memory.search("全く関係ないクエリ", threshold=0.9)

        # 閾値が高いため、結果が少ないか空になる可能性が高い
        assert isinstance(results, list)


if __name__ == "__main__":
    pytest.main([__file__])
