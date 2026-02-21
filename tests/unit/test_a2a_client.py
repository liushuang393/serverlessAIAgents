"""A2A クライアントのユニットテスト."""

from unittest.mock import AsyncMock, MagicMock, patch

import httpx
import pytest

from agentflow.protocols.a2a_client import A2AClient


@pytest.fixture
def sample_agent_card_data() -> dict:
    """サンプルエージェントカードデータを作成.

    Returns:
        AgentCard の辞書表現
    """
    return {
        "name": "test-agent",
        "description": "A test agent",
        "version": "1.0.0",
        "author": "Test Author",
        "skills": [
            {
                "name": "greet",
                "description": "Greet a person",
                "input_schema": {"type": "object"},
                "output_schema": {"type": "object"},
            }
        ],
        "metadata": {},
    }


@pytest.fixture
def mock_http_client() -> AsyncMock:
    """モック HTTP クライアントを作成.

    Returns:
        AsyncMock インスタンス
    """
    return AsyncMock(spec=httpx.AsyncClient)


class TestA2AClient:
    """A2A クライアントのテストスイート."""

    async def test_client_initialization(self) -> None:
        """クライアントの初期化をテスト."""
        async with A2AClient() as client:
            assert client._default_timeout == 30.0
            assert client._max_retries == 3
            assert client._cache_ttl == 300.0
            assert len(client._cache) == 0

    async def test_discover_agent_success(self, sample_agent_card_data: dict) -> None:
        """エージェント発見の成功をテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()
            mock_response = MagicMock()
            mock_response.json.return_value = sample_agent_card_data
            mock_response.raise_for_status = MagicMock()
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient() as client:
                card = await client.discover_agent("https://example.com/agent")

                assert card.name == "test-agent"
                assert len(card.skills) == 1
                mock_client.get.assert_called_once_with("https://example.com/agent/card")

    async def test_discover_agent_uses_cache(self, sample_agent_card_data: dict) -> None:
        """エージェント発見がキャッシュを使用することをテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()
            mock_response = MagicMock()
            mock_response.json.return_value = sample_agent_card_data
            mock_response.raise_for_status = MagicMock()
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient() as client:
                # 最初の呼び出し
                card1 = await client.discover_agent("https://example.com/agent")
                # 2回目の呼び出し (キャッシュから取得)
                card2 = await client.discover_agent("https://example.com/agent")

                assert card1.name == card2.name
                # HTTP リクエストは1回だけ
                assert mock_client.get.call_count == 1

    async def test_discover_agent_force_refresh(self, sample_agent_card_data: dict) -> None:
        """強制リフレッシュをテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()
            mock_response = MagicMock()
            mock_response.json.return_value = sample_agent_card_data
            mock_response.raise_for_status = MagicMock()
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient() as client:
                # 最初の呼び出し
                await client.discover_agent("https://example.com/agent")
                # 強制リフレッシュ
                await client.discover_agent("https://example.com/agent", force_refresh=True)

                # HTTP リクエストは2回
                assert mock_client.get.call_count == 2

    async def test_discover_agent_retry_on_failure(self, sample_agent_card_data: dict) -> None:
        """失敗時の再試行をテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()
            # 最初の2回は失敗、3回目は成功
            mock_response_fail = MagicMock()
            mock_response_fail.raise_for_status.side_effect = httpx.HTTPError("Connection error")
            mock_response_success = MagicMock()
            mock_response_success.json.return_value = sample_agent_card_data
            mock_response_success.raise_for_status = MagicMock()

            mock_client.get = AsyncMock(
                side_effect=[
                    mock_response_fail,
                    mock_response_fail,
                    mock_response_success,
                ]
            )
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient() as client:
                card = await client.discover_agent("https://example.com/agent")

                assert card.name == "test-agent"
                # 3回試行
                assert mock_client.get.call_count == 3

    async def test_discover_agent_max_retries_exceeded(self) -> None:
        """最大再試行回数を超えた場合をテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()
            mock_response = MagicMock()
            mock_response.raise_for_status.side_effect = httpx.HTTPError("Connection error")
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient(max_retries=2) as client:
                with pytest.raises(httpx.HTTPError):
                    await client.discover_agent("https://example.com/agent")

                # 2回試行
                assert mock_client.get.call_count == 2

    async def test_call_remote_agent_success(self, sample_agent_card_data: dict) -> None:
        """リモートエージェント呼び出しの成功をテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()

            # discover_agent 用のレスポンス
            mock_card_response = MagicMock()
            mock_card_response.json.return_value = sample_agent_card_data
            mock_card_response.raise_for_status = MagicMock()

            # call_remote_agent 用のレスポンス
            mock_task_response = MagicMock()
            mock_task_response.json.return_value = {
                "status": "success",
                "result": {"message": "Hello, World!"},
            }
            mock_task_response.raise_for_status = MagicMock()

            mock_client.get = AsyncMock(return_value=mock_card_response)
            mock_client.post = AsyncMock(return_value=mock_task_response)
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient() as client:
                result = await client.call_remote_agent("https://example.com/agent", "greet", {"name": "World"})

                assert result["status"] == "success"
                assert result["result"]["message"] == "Hello, World!"

    async def test_call_remote_agent_skill_not_found(self, sample_agent_card_data: dict) -> None:
        """存在しないスキルの呼び出しをテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()
            mock_response = MagicMock()
            mock_response.json.return_value = sample_agent_card_data
            mock_response.raise_for_status = MagicMock()
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient() as client:
                with pytest.raises(ValueError, match="Skill not found"):
                    await client.call_remote_agent("https://example.com/agent", "nonexistent", {})

    async def test_clear_cache(self, sample_agent_card_data: dict) -> None:
        """キャッシュクリアをテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()
            mock_response = MagicMock()
            mock_response.json.return_value = sample_agent_card_data
            mock_response.raise_for_status = MagicMock()
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient() as client:
                await client.discover_agent("https://example.com/agent")
                assert len(client._cache) == 1

                client.clear_cache("https://example.com/agent")
                assert len(client._cache) == 0

    async def test_clear_all_cache(self, sample_agent_card_data: dict) -> None:
        """全キャッシュクリアをテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()
            mock_response = MagicMock()
            mock_response.json.return_value = sample_agent_card_data
            mock_response.raise_for_status = MagicMock()
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient() as client:
                await client.discover_agent("https://example.com/agent1")
                await client.discover_agent("https://example.com/agent2")
                assert len(client._cache) == 2

                client.clear_cache()
                assert len(client._cache) == 0

    async def test_get_cached_endpoints(self, sample_agent_card_data: dict) -> None:
        """キャッシュされたエンドポイント取得をテスト."""
        with patch("agentflow.protocols.a2a_client.httpx.AsyncClient") as mock_client_class:
            mock_client = AsyncMock()
            mock_response = MagicMock()
            mock_response.json.return_value = sample_agent_card_data
            mock_response.raise_for_status = MagicMock()
            mock_client.get = AsyncMock(return_value=mock_response)
            mock_client.aclose = AsyncMock()
            mock_client_class.return_value = mock_client

            async with A2AClient() as client:
                await client.discover_agent("https://example.com/agent1")
                await client.discover_agent("https://example.com/agent2")

                endpoints = client.get_cached_endpoints()
                assert len(endpoints) == 2
                assert "https://example.com/agent1" in endpoints
                assert "https://example.com/agent2" in endpoints
