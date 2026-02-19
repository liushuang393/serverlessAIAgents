"""サンドボックスプロバイダのテスト.

各プロバイダの生命周期追跡機能をテスト。
"""

import pytest

from agentflow.sandbox import SandboxConfig, SandboxState, get_sandbox
from agentflow.sandbox.docker_provider import DockerProvider
from agentflow.sandbox.e2b_provider import E2BProvider
from agentflow.sandbox.microsandbox_provider import MicrosandboxProvider


class TestDockerProvider:
    """DockerProviderのテスト."""

    def test_initial_state(self) -> None:
        """初期状態がCREATEDであること."""
        provider = DockerProvider()
        assert provider.state == SandboxState.CREATED
        assert provider.execution_count == 0

    def test_get_stats(self) -> None:
        """統計情報が取得できること."""
        provider = DockerProvider()
        stats = provider.get_stats()

        assert stats["provider"] == "docker"
        assert stats["state"] == "created"
        assert stats["execution_count"] == 0
        assert "created_at" in stats
        assert "last_activity_at" in stats

    @pytest.mark.asyncio
    async def test_close_changes_state(self) -> None:
        """closeで状態がSTOPPEDに変わること."""
        provider = DockerProvider()
        await provider.close()
        assert provider.state == SandboxState.STOPPED


class TestMicrosandboxProvider:
    """MicrosandboxProviderのテスト."""

    def test_initial_state(self) -> None:
        """初期状態がCREATEDであること."""
        provider = MicrosandboxProvider()
        assert provider.state == SandboxState.CREATED
        assert provider.execution_count == 0

    def test_get_stats(self) -> None:
        """統計情報が取得できること."""
        provider = MicrosandboxProvider()
        stats = provider.get_stats()

        assert stats["provider"] == "microsandbox"
        assert stats["state"] == "created"
        assert "sdk_available" in stats

    @pytest.mark.asyncio
    async def test_close_changes_state(self) -> None:
        """closeで状態がSTOPPEDに変わること."""
        provider = MicrosandboxProvider()
        await provider.close()
        assert provider.state == SandboxState.STOPPED


class TestE2BProvider:
    """E2BProviderのテスト."""

    def test_initial_state(self) -> None:
        """初期状態がCREATEDであること."""
        provider = E2BProvider()
        assert provider.state == SandboxState.CREATED
        assert provider.execution_count == 0

    def test_get_stats(self) -> None:
        """統計情報が取得できること."""
        provider = E2BProvider()
        stats = provider.get_stats()

        assert stats["provider"] == "e2b"
        assert stats["state"] == "created"
        assert "api_key_set" in stats

    @pytest.mark.asyncio
    async def test_close_changes_state(self) -> None:
        """closeで状態がSTOPPEDに変わること."""
        provider = E2BProvider()
        await provider.close()
        assert provider.state == SandboxState.STOPPED


class TestGetSandbox:
    """get_sandbox関数のテスト."""

    def test_get_docker_provider(self) -> None:
        """dockerプロバイダが取得できること."""
        provider = get_sandbox(provider="docker")
        assert isinstance(provider, DockerProvider)
        assert provider.state == SandboxState.CREATED

    def test_get_microsandbox_provider(self) -> None:
        """microsandboxプロバイダが取得できること."""
        provider = get_sandbox(provider="microsandbox")
        assert isinstance(provider, MicrosandboxProvider)

    def test_get_e2b_provider(self) -> None:
        """e2bプロバイダが取得できること."""
        provider = get_sandbox(provider="e2b")
        assert isinstance(provider, E2BProvider)

    def test_unknown_provider_raises(self) -> None:
        """不明なプロバイダでエラーが発生すること."""
        with pytest.raises(ValueError, match="Unknown sandbox provider"):
            get_sandbox(provider="unknown")

    def test_with_config(self) -> None:
        """設定付きでプロバイダが取得できること."""
        config = SandboxConfig(timeout=60.0, memory_mb=1024)
        provider = get_sandbox(provider="docker", config=config)
        assert provider._config.timeout == 60.0
        assert provider._config.memory_mb == 1024
