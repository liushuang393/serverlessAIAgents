"""SimpleEngine と ToolBinder 統合テスト."""

import pytest

from agentflow.core.tool_registry import reset_global_tool_registry


@pytest.fixture(autouse=True)
def reset_registries():
    """テスト前後にレジストリをリセット."""
    reset_global_tool_registry()
    yield
    reset_global_tool_registry()


@pytest.mark.asyncio
async def test_simple_engine_auto_binds_tools():
    """SimpleEngine がツールを自動バインドすることを確認."""
    from agentflow.core.tool_discovery import ToolDiscoveryService
    from agentflow.core.tool_registry import get_global_tool_registry
    from agentflow.engines import SimpleEngine

    # ツールを登録
    registry = get_global_tool_registry()
    service = ToolDiscoveryService(registry)
    service.register_builtin(
        name="test_tool",
        description="テストツール",
        input_schema={"type": "object", "properties": {"input": {"type": "string"}}},
    )

    # モックAgent
    class MockAgent:
        _tools = None
        _tool_executor = None

        async def run(self, inputs):
            return {"result": "ok", "has_tools": self._tools is not None}

    # SimpleEngine で実行
    engine = SimpleEngine(
        agent=MockAgent,
        tools=["tool://builtin/test_tool"],
    )

    result = await engine.run({"input": "test"})

    # ツールがバインドされていることを確認
    assert result.get("has_tools") is True


@pytest.mark.asyncio
async def test_simple_engine_binds_skills_as_tools():
    """SimpleEngine がスキルをツールとしてバインドすることを確認."""
    from agentflow.core.tool_discovery import ToolDiscoveryService
    from agentflow.core.tool_registry import get_global_tool_registry
    from agentflow.engines import SimpleEngine

    # スキルを発見
    registry = get_global_tool_registry()
    service = ToolDiscoveryService(registry)
    await service.discover_skills_from_engine()

    class MockAgent:
        _tools = None

        async def run(self, inputs):
            tools_count = 0
            if self._tools is not None:
                if hasattr(self._tools, "__len__"):
                    tools_count = len(self._tools)
                elif hasattr(self._tools, "tools"):
                    tools_count = len(self._tools.tools)
            return {"tools_count": tools_count, "has_tools": self._tools is not None}

    # スキル指定で実行
    engine = SimpleEngine(
        agent=MockAgent,
        skills=["rag"],
    )

    result = await engine.run({"query": "test"})

    # スキルがツールとしてバインドされていることを確認
    assert result.get("has_tools") is True


@pytest.mark.asyncio
async def test_simple_engine_without_tools():
    """SimpleEngine がツールなしで動作することを確認."""
    from agentflow.engines import SimpleEngine

    class MockAgent:
        async def run(self, inputs):
            return {"result": "ok"}

    engine = SimpleEngine(agent=MockAgent)
    result = await engine.run({"input": "test"})

    assert result.get("result") == "ok"
