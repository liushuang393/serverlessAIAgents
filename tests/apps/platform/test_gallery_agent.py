"""Unit tests for GalleryAgent.

Note: These tests are in tests/apps/platform/ instead of apps/platform/tests/
to avoid Python's built-in 'platform' module name conflict.

MarketplaceClient はホームディレクトリへのアクセスが必要なため、
全テストクラスで patch を使用してファイルシステムへの副作用を防ぐ。
"""

from unittest.mock import AsyncMock, MagicMock

import pytest
from apps.platform.agents.gallery_agent import GalleryAgent


def _make_agent(tmp_path: Path) -> GalleryAgent:
    """MarketplaceClient のインストールディレクトリをテンポラリに差し替えてエージェントを生成する。"""
    with patch(
        "agentflow.marketplace.client.MarketplaceClient.__init__",
        lambda self, **kw: setattr(self, "_mocked", True) or None,
    ), patch(
        "apps.platform.services.gallery_service.MarketplaceClient",
        return_value=MagicMock(),
    ):
        return GalleryAgent()


@pytest.fixture
def agent(tmp_path: Path) -> GalleryAgent:
    """ファイルシステム副作用のない GalleryAgent インスタンスを返す fixture。"""
    with patch(
        "apps.platform.services.gallery_service.MarketplaceClient",
        return_value=MagicMock(),
    ):
        return GalleryAgent()


class TestGalleryAgentInit:
    """GalleryAgent 初期化のテスト。"""

    def test_agent_creation(self, agent: GalleryAgent) -> None:
        """GalleryAgent インスタンスを正常に生成できる。"""
        assert agent.name == "gallery-agent"
        assert agent._gallery is not None

    def test_agent_has_system_prompt(self, agent: GalleryAgent) -> None:
        """システムプロンプトが空でない。"""
        prompt = agent._get_system_prompt()
        assert len(prompt) > 0


class TestGalleryAgentRun:
    """GalleryAgent.run メソッドのテスト。"""

    @pytest.mark.asyncio
    async def test_run_without_query_returns_error(self, agent: GalleryAgent) -> None:
        """クエリなしで run を呼ぶとエラーを返す。"""
        result = await agent.run({})

        assert result["success"] is False
        assert "error" in result

    @pytest.mark.asyncio
    async def test_run_with_empty_query_returns_error(self, agent: GalleryAgent) -> None:
        """空クエリで run を呼ぶとエラーを返す。"""
        result = await agent.run({"query": ""})

        assert result["success"] is False
        assert "error" in result

    @pytest.mark.asyncio
    async def test_run_with_valid_query(self, agent: GalleryAgent) -> None:
        """有効なクエリで run を呼ぶと結果を返す。"""
        mock_result = MagicMock()
        mock_result.items = []
        mock_result.total = 0
        mock_result.has_more = False
        agent._gallery.search = AsyncMock(return_value=mock_result)

        result = await agent.run({"query": "PDF処理Agent"})

        assert result["success"] is True
        assert result["query"] == "PDF処理Agent"
        assert "items" in result
        assert "total" in result
