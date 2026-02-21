"""知識ストアシステムの単体テスト.

InMemoryKnowledgeStore、KnowledgeManager、ファクトリ関数のテスト。
"""

import tempfile
import uuid
from datetime import UTC, datetime
from pathlib import Path

import pytest

from agentflow.memory.knowledge import (
    InMemoryKnowledgeStore,
    KnowledgeEntry,
    KnowledgeManager,
    KnowledgeSource,
    get_knowledge_manager,
    get_knowledge_store,
    is_memvid_available,
    reset_knowledge_manager,
)


class TestKnowledgeEntry:
    """KnowledgeEntryクラスのテスト."""

    def test_create_with_defaults(self) -> None:
        """デフォルト値で作成."""
        entry = KnowledgeEntry(
            id="test-1",
            title="テストタイトル",
            body="テスト本文",
        )
        assert entry.id == "test-1"
        assert entry.title == "テストタイトル"
        assert entry.body == "テスト本文"
        assert entry.tags == []
        assert entry.source == KnowledgeSource.DOCUMENT
        assert entry.importance == 0.5
        assert entry.access_count == 0

    def test_create_with_all_params(self) -> None:
        """全パラメータで作成."""
        now = datetime.now(UTC)
        entry = KnowledgeEntry(
            id="test-2",
            title="FAQ質問",
            body="FAQ回答",
            tags=["faq", "important"],
            source=KnowledgeSource.FAQ,
            source_id="faq-001",
            timestamp=now,
            importance=0.9,
            metadata={"category": "support"},
        )
        assert entry.source == KnowledgeSource.FAQ
        assert len(entry.tags) == 2
        assert entry.importance == 0.9
        assert entry.metadata["category"] == "support"

    def test_to_dict(self) -> None:
        """辞書形式に変換."""
        entry = KnowledgeEntry(
            id="test-3",
            title="辞書テスト",
            body="本文",
            tags=["tag1"],
        )
        result = entry.to_dict()
        assert result["id"] == "test-3"
        assert result["title"] == "辞書テスト"
        assert result["tags"] == ["tag1"]

    def test_from_dict(self) -> None:
        """辞書から生成."""
        data = {
            "id": "test-4",
            "title": "復元テスト",
            "body": "本文",
            "tags": ["restored"],
            "source": "faq",
            "importance": 0.8,
            "timestamp": datetime.now(UTC).isoformat(),
        }
        entry = KnowledgeEntry.from_dict(data)
        assert entry.id == "test-4"
        assert entry.source == KnowledgeSource.FAQ
        assert entry.importance == 0.8

    def test_record_access(self) -> None:
        """アクセス記録."""
        entry = KnowledgeEntry(id="test-5", title="アクセス", body="本文")
        assert entry.access_count == 0
        entry.record_access()
        assert entry.access_count == 1
        entry.record_access()
        assert entry.access_count == 2

    def test_to_text(self) -> None:
        """検索用テキストに変換."""
        entry = KnowledgeEntry(
            id="test-6",
            title="タイトル",
            body="本文内容",
            tags=["tag1", "tag2"],
        )
        text = entry.to_text()
        assert "タイトル" in text
        assert "本文内容" in text
        assert "tag1" in text


class TestInMemoryKnowledgeStore:
    """InMemoryKnowledgeStoreのテスト."""

    @pytest.fixture
    def store(self) -> InMemoryKnowledgeStore:
        """テスト用ストアを作成."""
        return InMemoryKnowledgeStore()

    @pytest.mark.asyncio
    async def test_connect_disconnect(self, store: InMemoryKnowledgeStore) -> None:
        """接続・切断のテスト."""
        await store.connect()
        assert store.get_provider_name() == "memory"
        await store.disconnect()

    @pytest.mark.asyncio
    async def test_store_and_get(self, store: InMemoryKnowledgeStore) -> None:
        """保存と取得."""
        await store.connect()
        entry = KnowledgeEntry(
            id=str(uuid.uuid4()),
            title="テストエントリ",
            body="保存テスト",
        )
        entry_id = await store.store(entry)
        retrieved = await store.get(entry_id)
        assert retrieved is not None
        assert retrieved.title == "テストエントリ"
        await store.disconnect()

    @pytest.mark.asyncio
    async def test_store_auto_id(self, store: InMemoryKnowledgeStore) -> None:
        """ID自動生成テスト."""
        await store.connect()
        entry = KnowledgeEntry(id="", title="自動ID", body="本文")
        entry_id = await store.store(entry)
        assert entry_id != ""
        assert len(entry_id) > 0
        await store.disconnect()

    @pytest.mark.asyncio
    async def test_search_bm25(self, store: InMemoryKnowledgeStore) -> None:
        """BM25検索テスト."""
        await store.connect()

        # テストデータを追加
        entries = [
            KnowledgeEntry(id="1", title="Pythonプログラミング", body="Pythonは人気のプログラミング言語です"),
            KnowledgeEntry(id="2", title="Javaプログラミング", body="Javaはエンタープライズ向け言語です"),
            KnowledgeEntry(id="3", title="機械学習入門", body="機械学習はAIの重要な分野です"),
        ]
        await store.store_batch(entries)

        # Python関連を検索
        results = await store.search("Python プログラミング", top_k=2)
        assert len(results) > 0
        assert results[0].entry.id == "1"  # Python記事が最上位

        await store.disconnect()

    @pytest.mark.asyncio
    async def test_search_with_tags(self, store: InMemoryKnowledgeStore) -> None:
        """タグフィルター検索テスト."""
        await store.connect()

        entries = [
            KnowledgeEntry(id="1", title="FAQ1", body="よくある質問1", tags=["faq"]),
            KnowledgeEntry(id="2", title="ドキュメント", body="マニュアル", tags=["doc"]),
        ]
        await store.store_batch(entries)

        # FAQタグで検索
        results = await store.search("質問", tags=["faq"])
        assert len(results) == 1
        assert results[0].entry.id == "1"

        await store.disconnect()

    @pytest.mark.asyncio
    async def test_update(self, store: InMemoryKnowledgeStore) -> None:
        """更新テスト."""
        await store.connect()
        entry = KnowledgeEntry(id="upd-1", title="元タイトル", body="元本文")
        await store.store(entry)

        # 更新
        entry.title = "新タイトル"
        entry.body = "新本文"
        await store.update(entry)

        retrieved = await store.get("upd-1")
        assert retrieved is not None
        assert retrieved.title == "新タイトル"
        assert retrieved.updated_at is not None

        await store.disconnect()

    @pytest.mark.asyncio
    async def test_delete(self, store: InMemoryKnowledgeStore) -> None:
        """削除テスト."""
        await store.connect()
        entry = KnowledgeEntry(id="del-1", title="削除対象", body="本文")
        await store.store(entry)

        result = await store.delete("del-1")
        assert result is True

        retrieved = await store.get("del-1")
        assert retrieved is None

        await store.disconnect()

    @pytest.mark.asyncio
    async def test_clear_and_count(self, store: InMemoryKnowledgeStore) -> None:
        """クリアとカウントテスト."""
        await store.connect()
        entries = [KnowledgeEntry(id=f"cnt-{i}", title=f"エントリ{i}", body="本文") for i in range(5)]
        await store.store_batch(entries)

        count = await store.count()
        assert count == 5

        cleared = await store.clear()
        assert cleared == 5

        count = await store.count()
        assert count == 0

        await store.disconnect()

    @pytest.mark.asyncio
    async def test_persistence(self) -> None:
        """永続化テスト."""
        with tempfile.TemporaryDirectory() as tmpdir:
            persist_path = Path(tmpdir) / "knowledge.json"

            # 保存
            store1 = InMemoryKnowledgeStore(persist_path=persist_path, auto_save=True)
            await store1.connect()
            await store1.store(KnowledgeEntry(id="persist-1", title="永続化", body="テスト"))
            await store1.disconnect()

            # 確認
            assert persist_path.exists()

            # 復元
            store2 = InMemoryKnowledgeStore(persist_path=persist_path)
            await store2.connect()
            retrieved = await store2.get("persist-1")
            assert retrieved is not None
            assert retrieved.title == "永続化"
            await store2.disconnect()

    @pytest.mark.asyncio
    async def test_not_connected_error(self, store: InMemoryKnowledgeStore) -> None:
        """未接続エラーテスト."""
        entry = KnowledgeEntry(id="err-1", title="エラー", body="本文")
        with pytest.raises(RuntimeError, match="not connected"):
            await store.store(entry)


class TestKnowledgeManager:
    """KnowledgeManagerのテスト."""

    @pytest.fixture
    def manager(self) -> KnowledgeManager:
        """テスト用マネージャーを作成."""
        return KnowledgeManager(use_memvid=False)

    @pytest.mark.asyncio
    async def test_start_stop(self, manager: KnowledgeManager) -> None:
        """開始・停止テスト."""
        await manager.start()
        await manager.stop()

    @pytest.mark.asyncio
    async def test_add_and_query(self, manager: KnowledgeManager) -> None:
        """追加と検索テスト."""
        await manager.start()

        entry_id = await manager.add(
            content="Pythonは優れたプログラミング言語です",
            title="Pythonについて",
            tags=["python", "programming"],
            source=KnowledgeSource.FAQ,
        )
        assert entry_id is not None

        results = await manager.query("Python プログラミング")
        assert len(results) > 0

        await manager.stop()

    @pytest.mark.asyncio
    async def test_add_batch(self, manager: KnowledgeManager) -> None:
        """一括追加テスト."""
        await manager.start()

        # 既存データをクリア
        await manager.clear()

        entries = [
            {"content": "内容1", "title": "タイトル1", "tags": ["tag1"]},
            {"content": "内容2", "title": "タイトル2", "tags": ["tag2"]},
        ]
        ids = await manager.add_batch(entries)
        assert len(ids) == 2

        count = await manager.count()
        assert count == 2

        await manager.stop()

    @pytest.mark.asyncio
    async def test_update_and_delete(self, manager: KnowledgeManager) -> None:
        """更新・削除テスト."""
        await manager.start()

        entry_id = await manager.add(content="元の内容", title="元タイトル")

        # 更新
        result = await manager.update(entry_id, content="新しい内容", title="新タイトル")
        assert result is True

        entry = await manager.get(entry_id)
        assert entry is not None
        assert entry.title == "新タイトル"

        # 削除
        result = await manager.delete(entry_id)
        assert result is True

        entry = await manager.get(entry_id)
        assert entry is None

        await manager.stop()

    @pytest.mark.asyncio
    async def test_format_context(self, manager: KnowledgeManager) -> None:
        """コンテキスト整形テスト."""
        await manager.start()

        await manager.add(content="FAQ回答1", title="FAQ質問1", tags=["faq"])
        await manager.add(content="FAQ回答2", title="FAQ質問2", tags=["faq"])

        results = await manager.query("FAQ")
        context = manager.format_context(results)

        assert "関連知識" in context
        assert "FAQ質問" in context

        await manager.stop()

    @pytest.mark.asyncio
    async def test_not_started_error(self, manager: KnowledgeManager) -> None:
        """未開始エラーテスト."""
        with pytest.raises(RuntimeError, match="not started"):
            await manager.add(content="テスト", title="テスト")


class TestFactoryFunctions:
    """ファクトリ関数のテスト."""

    def setup_method(self) -> None:
        """各テスト前にシングルトンをリセット."""
        reset_knowledge_manager()

    def test_is_memvid_available(self) -> None:
        """Memvid利用可能性チェック."""
        # memvid-sdkがインストールされていない場合はFalse
        result = is_memvid_available()
        assert isinstance(result, bool)

    def test_get_knowledge_manager_singleton(self) -> None:
        """シングルトンパターンのテスト."""
        manager1 = get_knowledge_manager()
        manager2 = get_knowledge_manager()
        assert manager1 is manager2

    def test_get_knowledge_manager_new_instance(self) -> None:
        """新規インスタンス生成テスト."""
        manager1 = get_knowledge_manager()
        manager2 = get_knowledge_manager(_new_instance=True)
        assert manager1 is not manager2

    def test_get_knowledge_store_memory(self) -> None:
        """メモリストア取得テスト."""
        store = get_knowledge_store(backend="memory")
        assert store.get_provider_name() == "memory"

    def test_get_knowledge_store_auto(self) -> None:
        """自動選択テスト（memvid未インストール時はmemory）."""
        store = get_knowledge_store(backend="auto")
        # memvid-sdkがない場合はmemoryにフォールバック
        provider = store.get_provider_name()
        assert provider in ("memvid", "memory")
