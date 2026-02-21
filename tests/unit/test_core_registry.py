"""Core Registry 基類のテスト.

このテストは agentflow/core/registry.py の Registry 基類をテストします。
- 登録・取得・削除
- 型検証
- スレッドセーフ
"""

import threading

import pytest

from agentflow.core.registry import ProtocolRegistry, Registry


class ConcreteRegistry(Registry[str]):
    """テスト用の具象レジストリ."""


class TestRegistry:
    """Registry 基類のテスト."""

    @pytest.fixture
    def registry(self) -> ConcreteRegistry:
        """テスト用レジストリを作成."""
        return ConcreteRegistry()

    def test_register_and_get(self, registry: ConcreteRegistry) -> None:
        """登録と取得ができることをテスト."""
        registry.register("item1", "value1")
        assert registry.get("item1") == "value1"

    def test_register_empty_name_raises(self, registry: ConcreteRegistry) -> None:
        """空の名前で登録すると ValueError が発生することをテスト."""
        with pytest.raises(ValueError, match="cannot be empty"):
            registry.register("", "value")

    def test_register_whitespace_name_raises(self, registry: ConcreteRegistry) -> None:
        """空白のみの名前で登録すると ValueError が発生することをテスト."""
        with pytest.raises(ValueError, match="cannot be empty"):
            registry.register("   ", "value")

    def test_register_non_string_name_raises(self, registry: ConcreteRegistry) -> None:
        """文字列以外の名前で登録すると TypeError が発生することをテスト."""
        with pytest.raises(TypeError, match="must be a string"):
            registry.register(123, "value")  # type: ignore[arg-type]

    def test_register_none_item_raises(self, registry: ConcreteRegistry) -> None:
        """None を登録すると TypeError が発生することをテスト."""
        with pytest.raises(TypeError, match="cannot be None"):
            registry.register("item", None)  # type: ignore[arg-type]

    def test_get_nonexistent_returns_none(self, registry: ConcreteRegistry) -> None:
        """存在しないアイテムの取得で None が返ることをテスト."""
        assert registry.get("nonexistent") is None

    def test_get_or_raise(self, registry: ConcreteRegistry) -> None:
        """get_or_raise が正しく動作することをテスト."""
        registry.register("item1", "value1")
        assert registry.get_or_raise("item1") == "value1"

    def test_get_or_raise_nonexistent(self, registry: ConcreteRegistry) -> None:
        """存在しないアイテムで KeyError が発生することをテスト."""
        with pytest.raises(KeyError, match="not found"):
            registry.get_or_raise("nonexistent")

    def test_unregister(self, registry: ConcreteRegistry) -> None:
        """削除ができることをテスト."""
        registry.register("item1", "value1")
        assert registry.unregister("item1") is True
        assert registry.get("item1") is None

    def test_unregister_nonexistent(self, registry: ConcreteRegistry) -> None:
        """存在しないアイテムの削除で False が返ることをテスト."""
        assert registry.unregister("nonexistent") is False

    def test_list_names(self, registry: ConcreteRegistry) -> None:
        """名前一覧が取得できることをテスト."""
        registry.register("a", "1")
        registry.register("b", "2")
        names = registry.list_names()
        assert set(names) == {"a", "b"}

    def test_list_all(self, registry: ConcreteRegistry) -> None:
        """全アイテムが取得できることをテスト."""
        registry.register("a", "1")
        registry.register("b", "2")
        items = registry.list_all()
        assert items == {"a": "1", "b": "2"}

    def test_clear(self, registry: ConcreteRegistry) -> None:
        """クリアができることをテスト."""
        registry.register("a", "1")
        registry.register("b", "2")
        registry.clear()
        assert len(registry) == 0

    def test_len(self, registry: ConcreteRegistry) -> None:
        """長さが取得できることをテスト."""
        assert len(registry) == 0
        registry.register("a", "1")
        assert len(registry) == 1

    def test_contains(self, registry: ConcreteRegistry) -> None:
        """存在確認ができることをテスト."""
        assert "item1" not in registry
        registry.register("item1", "value1")
        assert "item1" in registry

    def test_overwrite_warning(self, registry: ConcreteRegistry, caplog: pytest.LogCaptureFixture) -> None:
        """上書き時に警告が出ることをテスト."""
        registry.register("item1", "value1")
        registry.register("item1", "value2")
        assert "Overwriting" in caplog.text
        assert registry.get("item1") == "value2"

    def test_thread_safety(self, registry: ConcreteRegistry) -> None:
        """スレッドセーフであることをテスト."""
        errors: list[Exception] = []

        def register_items(start: int) -> None:
            try:
                for i in range(start, start + 100):
                    registry.register(f"item_{i}", f"value_{i}")
            except Exception as e:
                errors.append(e)

        threads = [threading.Thread(target=register_items, args=(i * 100,)) for i in range(5)]
        for t in threads:
            t.start()
        for t in threads:
            t.join()

        assert len(errors) == 0
        assert len(registry) == 500


class TestProtocolRegistry:
    """ProtocolRegistry のテスト."""

    def test_singleton(self) -> None:
        """シングルトンであることをテスト."""
        r1 = ProtocolRegistry()
        r2 = ProtocolRegistry()
        assert r1 is r2
