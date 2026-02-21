"""StorageBackend 単体テスト."""

from __future__ import annotations

import pytest

from agentflow.storage import MemoryStorageBackend, get_backend


class TestMemoryStorageBackend:
    """MemoryStorageBackend テスト."""

    @pytest.fixture
    def backend(self) -> MemoryStorageBackend:
        """バックエンドのフィクスチャ."""
        return MemoryStorageBackend(namespace="test")

    @pytest.mark.asyncio
    async def test_connect_disconnect(self, backend: MemoryStorageBackend) -> None:
        """接続・切断."""
        assert await backend.is_connected() is False
        await backend.connect()
        assert await backend.is_connected() is True
        await backend.disconnect()
        assert await backend.is_connected() is False

    @pytest.mark.asyncio
    async def test_ping(self, backend: MemoryStorageBackend) -> None:
        """疎通確認."""
        assert await backend.ping() is True

    # =========================================================================
    # Key-Value 操作
    # =========================================================================

    @pytest.mark.asyncio
    async def test_set_and_get(self, backend: MemoryStorageBackend) -> None:
        """値の設定と取得."""
        await backend.set("key1", {"data": "value"})
        result = await backend.get("key1")
        assert result == {"data": "value"}

    @pytest.mark.asyncio
    async def test_get_nonexistent(self, backend: MemoryStorageBackend) -> None:
        """存在しないキーの取得."""
        result = await backend.get("nonexistent")
        assert result is None

    @pytest.mark.asyncio
    async def test_delete(self, backend: MemoryStorageBackend) -> None:
        """削除."""
        await backend.set("key1", "value")
        assert await backend.delete("key1") is True
        assert await backend.get("key1") is None
        assert await backend.delete("key1") is False

    @pytest.mark.asyncio
    async def test_exists(self, backend: MemoryStorageBackend) -> None:
        """存在確認."""
        await backend.set("key1", "value")
        assert await backend.exists("key1") is True
        assert await backend.exists("nonexistent") is False

    @pytest.mark.asyncio
    async def test_keys_pattern(self, backend: MemoryStorageBackend) -> None:
        """キー一覧とパターン."""
        await backend.set("user:1", "a")
        await backend.set("user:2", "b")
        await backend.set("order:1", "c")

        # 全件
        all_keys = await backend.keys("*")
        assert len(all_keys) == 3

        # パターン
        user_keys = await backend.keys("user:*")
        assert len(user_keys) == 2
        assert "user:1" in user_keys
        assert "user:2" in user_keys

    @pytest.mark.asyncio
    async def test_clear(self, backend: MemoryStorageBackend) -> None:
        """クリア."""
        await backend.set("a", "1")
        await backend.set("b", "2")

        count = await backend.clear("*")
        assert count == 2
        assert await backend.keys("*") == []

    @pytest.mark.asyncio
    async def test_ttl(self, backend: MemoryStorageBackend) -> None:
        """TTL（有効期限）."""
        import asyncio

        await backend.set("temp", "value", ttl=1)  # 1秒
        assert await backend.get("temp") == "value"

        # 期限切れを待つ
        await asyncio.sleep(1.1)
        assert await backend.get("temp") is None

    # =========================================================================
    # バッチ操作
    # =========================================================================

    @pytest.mark.asyncio
    async def test_mget(self, backend: MemoryStorageBackend) -> None:
        """一括取得."""
        await backend.set("a", "1")
        await backend.set("b", "2")

        result = await backend.mget(["a", "b", "c"])
        assert result == {"a": "1", "b": "2"}

    @pytest.mark.asyncio
    async def test_mset(self, backend: MemoryStorageBackend) -> None:
        """一括設定."""
        await backend.mset({"x": "1", "y": "2", "z": "3"})

        assert await backend.get("x") == "1"
        assert await backend.get("y") == "2"
        assert await backend.get("z") == "3"

    # =========================================================================
    # Virtual Filesystem
    # =========================================================================

    @pytest.mark.asyncio
    async def test_write_and_read_file(self, backend: MemoryStorageBackend) -> None:
        """ファイル書き込み・読み込み."""
        await backend.write_file("/test.txt", "Hello, World!")
        content = await backend.read_file("/test.txt")
        assert content == b"Hello, World!"

    @pytest.mark.asyncio
    async def test_write_binary_file(self, backend: MemoryStorageBackend) -> None:
        """バイナリファイル."""
        binary = b"\x00\x01\x02\x03"
        await backend.write_file("/binary.bin", binary)
        content = await backend.read_file("/binary.bin")
        assert content == binary

    @pytest.mark.asyncio
    async def test_file_metadata(self, backend: MemoryStorageBackend) -> None:
        """ファイルメタデータ."""
        await backend.write_file(
            "/doc.md",
            "# Title",
            metadata={"author": "test", "type": "markdown"},
        )

        files = await backend.list_files("/doc")
        assert len(files) == 1
        assert files[0]["author"] == "test"
        assert files[0]["type"] == "markdown"

    @pytest.mark.asyncio
    async def test_list_files(self, backend: MemoryStorageBackend) -> None:
        """ファイル一覧."""
        await backend.write_file("/a/1.txt", "1")
        await backend.write_file("/a/2.txt", "2")
        await backend.write_file("/b/3.txt", "3")

        all_files = await backend.list_files("")
        assert len(all_files) == 3

        a_files = await backend.list_files("/a")
        assert len(a_files) == 2

    @pytest.mark.asyncio
    async def test_delete_file(self, backend: MemoryStorageBackend) -> None:
        """ファイル削除."""
        await backend.write_file("/temp.txt", "temp")
        assert await backend.delete_file("/temp.txt") is True
        assert await backend.read_file("/temp.txt") is None
        assert await backend.delete_file("/temp.txt") is False

    @pytest.mark.asyncio
    async def test_file_exists(self, backend: MemoryStorageBackend) -> None:
        """ファイル存在確認."""
        await backend.write_file("/exists.txt", "data")
        assert await backend.file_exists("/exists.txt") is True
        assert await backend.file_exists("/nope.txt") is False

    # =========================================================================
    # 名前空間
    # =========================================================================

    @pytest.mark.asyncio
    async def test_namespace_isolation(self) -> None:
        """名前空間の分離."""
        backend1 = MemoryStorageBackend(namespace="ns1")
        backend2 = MemoryStorageBackend(namespace="ns2")

        # 注: 同じインスタンス変数を共有しない独立したインスタンス
        await backend1.set("key", "value1")
        await backend2.set("key", "value2")

        # 独立したバックエンドなので別々の値
        assert await backend1.get("key") == "value1"
        assert await backend2.get("key") == "value2"

    # =========================================================================
    # ステータス
    # =========================================================================

    @pytest.mark.asyncio
    async def test_get_stats(self, backend: MemoryStorageBackend) -> None:
        """統計情報."""
        await backend.connect()
        await backend.set("key1", "value1")
        await backend.write_file("/test.txt", "content")

        stats = await backend.get_stats()
        assert stats["backend"] == "memory"
        assert stats["namespace"] == "test"
        assert stats["connected"] is True
        assert stats["key_count"] == 1
        assert stats["file_count"] == 1


class TestGetBackend:
    """get_backend ファクトリ関数テスト."""

    def test_default_memory(self) -> None:
        """デフォルトはメモリ."""
        backend = get_backend()
        assert isinstance(backend, MemoryStorageBackend)

    def test_explicit_memory(self) -> None:
        """明示的なメモリ指定."""
        backend = get_backend("memory://")
        assert isinstance(backend, MemoryStorageBackend)

    def test_with_namespace(self) -> None:
        """名前空間指定."""
        backend = get_backend(namespace="myns")
        assert isinstance(backend, MemoryStorageBackend)
        assert backend._namespace == "myns"

    def test_unknown_scheme(self) -> None:
        """不明なスキーム."""
        with pytest.raises(ValueError, match="Unknown storage scheme"):
            get_backend("unknown://localhost")
