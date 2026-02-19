"""ToolProvider の plugin metadata テスト."""

from __future__ import annotations

import pytest

from agentflow.providers.tool_provider import (
    RegisteredTool,
    ToolProvider,
    _tool_registry,
    tool,
)


@pytest.fixture(autouse=True)
def clear_registry() -> None:
    """各テスト前後でレジストリを初期化する."""
    _tool_registry.clear()
    ToolProvider._default_instance = None
    yield
    _tool_registry.clear()
    ToolProvider._default_instance = None


def test_tool_decorator_registers_plugin_metadata() -> None:
    """@tool で plugin_id/plugin_version を保持できる."""

    @tool(
        plugin_id="official.cobol-migration-pack",
        plugin_version="1.0.0",
        operation_type="write",
    )
    def migrate_code(module: str) -> str:
        return module

    registered = _tool_registry.get("migrate_code")
    assert registered is not None
    assert registered.plugin_id == "official.cobol-migration-pack"
    assert registered.plugin_version == "1.0.0"


def test_manual_registration_keeps_plugin_metadata() -> None:
    """手動登録でも plugin metadata を保持できる."""

    def call_external() -> dict[str, str]:
        return {"status": "ok"}

    ToolProvider.register(
        RegisteredTool(
            name="external_call",
            description="外部連携",
            func=call_external,
            plugin_id="official.enterprise-connector-pack",
            plugin_version="1.0.0",
        )
    )

    provider = ToolProvider.discover()
    tool_info = provider.get_tool("external_call")
    assert tool_info is not None
    assert tool_info.plugin_id == "official.enterprise-connector-pack"
    assert tool_info.plugin_version == "1.0.0"
