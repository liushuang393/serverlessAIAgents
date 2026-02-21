"""ToolCatalogManager のユニットテスト.

統一ツールカタログの初期化、検索、フィルタリング、
LLMフォーマット変換、シングルトンアクセサのテスト。
"""

import pytest

from agentflow.core.tool_catalog import (
    CatalogEntry,
    CatalogSource,
    ToolCatalogManager,
    get_tool_catalog,
    reset_tool_catalog,
)


@pytest.fixture
def catalog():
    """空の ToolCatalogManager インスタンスを作成."""
    return ToolCatalogManager()


@pytest.fixture
def sample_entry():
    """サンプル CatalogEntry を作成."""
    return CatalogEntry(
        uri="tool://builtin/test_tool",
        name="test_tool",
        description="テスト用ツール",
        source=CatalogSource.BUILTIN,
        category="utility",
        tags=["test", "utility"],
        available=True,
    )


@pytest.fixture(autouse=True)
def _reset_singleton():
    """各テストの前後にシングルトンをリセット."""
    reset_tool_catalog()
    yield
    reset_tool_catalog()


class TestCatalogEntry:
    """CatalogEntry のテスト."""

    def test_matches_name(self, sample_entry):
        """名前にマッチした場合スコア > 0."""
        score = sample_entry.matches("test_tool")
        assert score > 0

    def test_matches_description(self, sample_entry):
        """説明にマッチした場合スコア > 0."""
        score = sample_entry.matches("テスト")
        assert score > 0

    def test_matches_tag(self, sample_entry):
        """タグにマッチした場合スコア > 0."""
        score = sample_entry.matches("utility")
        assert score > 0

    def test_matches_no_match(self, sample_entry):
        """マッチしない場合スコア == 0."""
        score = sample_entry.matches("zzzzz_nonexistent")
        assert score == 0.0

    def test_matches_score_capped(self, sample_entry):
        """スコアは最大 1.0."""
        score = sample_entry.matches("test")
        assert score <= 1.0


class TestToolCatalogManager:
    """ToolCatalogManager のテスト."""

    def test_initial_state(self, catalog):
        """初期状態は未初期化."""
        assert catalog.initialized is False
        assert catalog.list_catalog() == []

    @pytest.mark.asyncio
    async def test_initialize(self, catalog):
        """初期化後にエントリが登録される."""
        await catalog.initialize()
        assert catalog.initialized is True
        entries = catalog.list_catalog()
        assert len(entries) > 0

    @pytest.mark.asyncio
    async def test_initialize_idempotent(self, catalog):
        """二重初期化は無害."""
        await catalog.initialize()
        count1 = len(catalog.list_catalog())
        await catalog.initialize()
        count2 = len(catalog.list_catalog())
        assert count1 == count2

    @pytest.mark.asyncio
    async def test_has_gateway_skills(self, catalog):
        """初期化後にゲートウェイスキルが含まれる."""
        await catalog.initialize()
        gateways = catalog.get_by_source(CatalogSource.GATEWAY)
        assert len(gateways) >= 20

    @pytest.mark.asyncio
    async def test_has_builtin_tools(self, catalog):
        """初期化後に内蔵ツールが含まれる."""
        await catalog.initialize()
        builtins = catalog.get_by_source(CatalogSource.BUILTIN)
        assert len(builtins) >= 4

    @pytest.mark.asyncio
    async def test_search(self, catalog):
        """検索が結果を返す."""
        await catalog.initialize()
        results = catalog.search("file")
        assert len(results) > 0
        assert all(isinstance(r, CatalogEntry) for r in results)

    @pytest.mark.asyncio
    async def test_search_empty_query(self, catalog):
        """空クエリは全カタログを返す."""
        await catalog.initialize()
        results = catalog.search("", limit=5)
        assert len(results) <= 5

    @pytest.mark.asyncio
    async def test_search_with_limit(self, catalog):
        """limit パラメータが機能する."""
        await catalog.initialize()
        results = catalog.search("file", limit=2)
        assert len(results) <= 2

    def test_register_and_get(self, catalog):
        """手動登録と URI 取得."""
        entry = CatalogEntry(
            uri="tool://test/manual",
            name="manual_tool",
            description="手動登録ツール",
            source=CatalogSource.BUILTIN,
        )
        catalog._register(entry)
        assert catalog.get("tool://test/manual") is not None
        assert catalog.get("tool://test/manual").name == "manual_tool"

    def test_get_nonexistent(self, catalog):
        """存在しない URI は None."""
        assert catalog.get("tool://nonexistent/xxx") is None

    @pytest.mark.asyncio
    async def test_get_by_category(self, catalog):
        """カテゴリフィルタが機能する."""
        await catalog.initialize()
        gw = catalog.get_by_category("os_read")
        assert len(gw) > 0
        assert all(e.category == "os_read" for e in gw)

    @pytest.mark.asyncio
    async def test_get_for_llm(self, catalog):
        """LLM フォーマットが OpenAI Function Calling 形式."""
        await catalog.initialize()
        tools = catalog.get_for_llm()
        assert len(tools) > 0
        first = tools[0]
        assert first["type"] == "function"
        assert "function" in first
        assert "name" in first["function"]
        assert "description" in first["function"]
        assert "parameters" in first["function"]

    @pytest.mark.asyncio
    async def test_get_for_llm_excludes_unavailable(self, catalog):
        """available=False のエントリは LLM 出力に含まれない."""
        entry = CatalogEntry(
            uri="tool://test/disabled",
            name="disabled_tool",
            description="無効ツール",
            source=CatalogSource.BUILTIN,
            available=False,
        )
        catalog._register(entry)
        tools = catalog.get_for_llm()
        names = [t["function"]["name"] for t in tools]
        assert "disabled_tool" not in names

    @pytest.mark.asyncio
    async def test_get_stats(self, catalog):
        """統計情報が正しい形式."""
        await catalog.initialize()
        stats = catalog.get_stats()
        assert stats["initialized"] is True
        assert stats["total"] > 0
        assert isinstance(stats["by_source"], dict)
        assert isinstance(stats["by_category"], dict)
        assert stats["available"] <= stats["total"]

    @pytest.mark.asyncio
    async def test_register_overwrite(self, catalog):
        """同じ URI の再登録は上書き."""
        entry1 = CatalogEntry(
            uri="tool://test/dup",
            name="dup",
            description="初回",
            source=CatalogSource.BUILTIN,
        )
        entry2 = CatalogEntry(
            uri="tool://test/dup",
            name="dup",
            description="更新後",
            source=CatalogSource.BUILTIN,
        )
        catalog._register(entry1)
        catalog._register(entry2)
        assert catalog.get("tool://test/dup").description == "更新後"


class TestSingleton:
    """シングルトンアクセサのテスト."""

    @pytest.mark.asyncio
    async def test_get_tool_catalog_returns_initialized(self):
        """get_tool_catalog は初期化済みインスタンスを返す."""
        catalog = await get_tool_catalog()
        assert catalog.initialized is True
        assert len(catalog.list_catalog()) > 0

    @pytest.mark.asyncio
    async def test_get_tool_catalog_is_singleton(self):
        """同一インスタンスが返される."""
        c1 = await get_tool_catalog()
        c2 = await get_tool_catalog()
        assert c1 is c2

    @pytest.mark.asyncio
    async def test_reset_and_reinitialize(self):
        """リセット後に新しいインスタンスが作られる."""
        c1 = await get_tool_catalog()
        reset_tool_catalog()
        c2 = await get_tool_catalog()
        assert c1 is not c2
        assert c2.initialized is True
