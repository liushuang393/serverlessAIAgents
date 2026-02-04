# -*- coding: utf-8 -*-
"""LLMProvider のユニットテスト.

このモジュールは、agentflow/providers/llm_provider.py の機能をテストします。
"""

from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.providers.llm_provider import (
    LLMProvider,
    LLMProviderConfig,
    _detect_provider_from_env,
    get_llm,
    reset_llm,
)


@pytest.fixture(autouse=True)
def reset_singleton():
    """各テスト前後にシングルトンをリセット."""
    reset_llm()
    yield
    reset_llm()


class TestLLMProviderConfig:
    """LLMProviderConfig のテスト."""

    def test_default_values(self):
        """デフォルト値が正しく設定される."""
        config = LLMProviderConfig()
        assert config.temperature == 0.7
        assert config.max_tokens == 2000
        assert config.timeout == 180

    def test_custom_values(self):
        """カスタム値が正しく設定される."""
        config = LLMProviderConfig(temperature=0.5, max_tokens=1000, timeout=30)
        assert config.temperature == 0.5
        assert config.max_tokens == 1000
        assert config.timeout == 30

    def test_temperature_validation(self):
        """温度パラメータの範囲検証."""
        # 有効な範囲
        config = LLMProviderConfig(temperature=0.0)
        assert config.temperature == 0.0
        config = LLMProviderConfig(temperature=2.0)
        assert config.temperature == 2.0

        # 無効な範囲
        with pytest.raises(ValueError):
            LLMProviderConfig(temperature=-0.1)
        with pytest.raises(ValueError):
            LLMProviderConfig(temperature=2.1)

    def test_max_tokens_validation(self):
        """最大トークン数の検証."""
        config = LLMProviderConfig(max_tokens=1)
        assert config.max_tokens == 1

        with pytest.raises(ValueError):
            LLMProviderConfig(max_tokens=0)
        with pytest.raises(ValueError):
            LLMProviderConfig(max_tokens=-1)


class TestDetectProviderFromEnv:
    """_detect_provider_from_env のテスト."""

    @patch("agentflow.config.get_settings")
    def test_detect_openai_provider(self, mock_get_settings):
        """OpenAI プロバイダーの検出."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test-key",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings

        provider, model, api_key, base_url, _timeout = _detect_provider_from_env()
        assert provider == "openai"
        assert model == "gpt-4o"
        assert api_key == "sk-test-key"
        assert base_url is None

    @patch("agentflow.config.get_settings")
    def test_detect_anthropic_provider(self, mock_get_settings):
        """Anthropic プロバイダーの検出."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "anthropic",
            "model": "claude-3-opus",
            "api_key": "sk-ant-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings

        provider, model, api_key, base_url, _timeout = _detect_provider_from_env()
        assert provider == "anthropic"
        assert model == "claude-3-opus"
        assert api_key == "sk-ant-test"

    @patch("agentflow.config.get_settings")
    def test_detect_mock_provider_when_no_key(self, mock_get_settings):
        """APIキーがない場合はmockプロバイダー."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "mock",
            "model": "mock",
            "api_key": None,
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings

        provider, model, api_key, base_url, _timeout = _detect_provider_from_env()
        assert provider == "mock"
        assert model == "mock"


@pytest.mark.asyncio
class TestLLMProvider:
    """LLMProvider のテスト."""

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    async def test_initialization(self, mock_get_settings, mock_llm_client):
        """初期化が正しく行われる."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings
        mock_client_instance = MagicMock()
        mock_llm_client.return_value = mock_client_instance

        provider = LLMProvider()

        assert provider._provider_info == ("openai", "gpt-4o", "sk-test", None)

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    async def test_chat_method(self, mock_get_settings, mock_llm_client):
        """chat メソッドが正しく動作する."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings
        mock_response = MagicMock()
        mock_response.model_dump.return_value = {
            "content": "Hello!",
            "model": "gpt-4o",
            "usage": {"prompt_tokens": 10, "completion_tokens": 5},
        }
        mock_client_instance = MagicMock()
        mock_client_instance.chat = AsyncMock(return_value=mock_response)
        mock_llm_client.return_value = mock_client_instance

        provider = LLMProvider()
        messages = [{"role": "user", "content": "Hi"}]
        result = await provider.chat(messages)

        assert result["content"] == "Hello!"
        assert result["model"] == "gpt-4o"
        mock_client_instance.chat.assert_called_once()

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    async def test_complete_method(self, mock_get_settings, mock_llm_client):
        """complete メソッドが正しく動作する."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings
        mock_response = MagicMock()
        mock_response.model_dump.return_value = {
            "content": "Completed text",
            "model": "gpt-4o",
            "usage": {},
        }
        mock_client_instance = MagicMock()
        mock_client_instance.complete = AsyncMock(return_value=mock_response)
        mock_llm_client.return_value = mock_client_instance

        provider = LLMProvider()
        result = await provider.complete("Test prompt")

        assert result["content"] == "Completed text"
        mock_client_instance.complete.assert_called_once_with("Test prompt")

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    async def test_stream_method(self, mock_get_settings, mock_llm_client):
        """stream メソッドが正しく動作する."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings

        async def mock_stream(*args, **kwargs):
            for chunk in ["Hello", " ", "World"]:
                yield chunk

        mock_client_instance = MagicMock()
        mock_client_instance.stream = mock_stream
        mock_llm_client.return_value = mock_client_instance

        provider = LLMProvider()
        messages = [{"role": "user", "content": "Hi"}]
        chunks = []
        async for chunk in provider.stream(messages):
            chunks.append(chunk)

        assert chunks == ["Hello", " ", "World"]

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    async def test_config_property(self, mock_get_settings, mock_llm_client):
        """config プロパティが正しく動作する."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings
        mock_llm_client.return_value = MagicMock()

        config = LLMProviderConfig(temperature=0.5, max_tokens=1500)
        provider = LLMProvider(config=config)

        assert provider.config.temperature == 0.5
        assert provider.config.max_tokens == 1500

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    async def test_temperature_override(self, mock_get_settings, mock_llm_client):
        """温度パラメータのオーバーライド."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings
        mock_llm_client.return_value = MagicMock()

        provider = LLMProvider(temperature=0.3)
        assert provider._temperature_override == 0.3


class TestGetLLM:
    """get_llm 関数のテスト."""

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    def test_singleton_behavior(self, mock_get_settings, mock_llm_client):
        """シングルトンパターンが正しく動作する."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings
        mock_llm_client.return_value = MagicMock()

        llm1 = get_llm()
        llm2 = get_llm()

        assert llm1 is llm2

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    def test_new_instance_with_custom_params(self, mock_get_settings, mock_llm_client):
        """カスタムパラメータで新しいインスタンスが作成される."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings
        mock_llm_client.return_value = MagicMock()

        llm1 = get_llm()
        llm2 = get_llm(temperature=0.5)

        assert llm1 is not llm2

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    def test_new_instance_flag(self, mock_get_settings, mock_llm_client):
        """_new_instance フラグで新しいインスタンスが作成される."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings
        mock_llm_client.return_value = MagicMock()

        llm1 = get_llm()
        llm2 = get_llm(_new_instance=True)

        assert llm1 is not llm2


class TestResetLLM:
    """reset_llm 関数のテスト."""

    @patch("agentflow.llm.llm_client.LLMClient")
    @patch("agentflow.config.get_settings")
    def test_reset_clears_singleton(self, mock_get_settings, mock_llm_client):
        """reset_llm がシングルトンをクリアする."""
        mock_settings = MagicMock()
        mock_settings.get_active_llm_config.return_value = {
            "provider": "openai",
            "model": "gpt-4o",
            "api_key": "sk-test",
            "base_url": None,
        }
        mock_get_settings.return_value = mock_settings
        mock_llm_client.return_value = MagicMock()

        llm1 = get_llm()
        reset_llm()
        llm2 = get_llm()

        assert llm1 is not llm2
