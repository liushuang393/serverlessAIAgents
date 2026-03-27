"""kernel ToolProvider Protocol のテスト."""

from __future__ import annotations

from typing import Any


def test_tool_provider_protocol_exists() -> None:
    """ToolProviderService Protocol がインポートできること."""
    from kernel.interfaces.tool_provider import ToolProviderService

    assert hasattr(ToolProviderService, "get_tool")
    assert hasattr(ToolProviderService, "list_tools")


def test_tool_provider_is_runtime_checkable() -> None:
    """ToolProviderService が runtime_checkable であること."""
    from kernel.interfaces.tool_provider import ToolProviderService

    assert isinstance(ToolProviderService, type)


class _DummyToolProvider:
    """テスト用のダミー実装."""

    async def get_tool(self, name: str) -> Any:
        return None

    async def list_tools(self) -> list[dict[str, Any]]:
        return []


def test_tool_provider_isinstance_check() -> None:
    """ダミー実装が ToolProviderService の isinstance チェックを通ること."""
    from kernel.interfaces.tool_provider import ToolProviderService

    dummy = _DummyToolProvider()
    assert isinstance(dummy, ToolProviderService)


def test_tool_provider_reexported_from_init() -> None:
    """kernel.interfaces から ToolProviderService を直接インポートできること."""
    from kernel.interfaces import ToolProviderService

    assert hasattr(ToolProviderService, "get_tool")
