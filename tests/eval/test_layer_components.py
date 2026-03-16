"""新しい層コンポーネントの差し替えテスト."""

from __future__ import annotations

import pytest
from harness import ApprovalService, HarnessedToolRuntime, ToolGate
from infrastructure import LLMBackendRegistry, TraceExporterRegistry
from shared import SharedLLMGateway
from shared.registry import ComponentToggle

from agentflow.providers.tool_provider import RegisteredTool, ToolProvider
from contracts import ToolRequest


@pytest.mark.asyncio
async def test_shared_llm_gateway_can_switch_to_mock_backend() -> None:
    """LLM Gateway が mock backend へ切り替わること."""
    gateway = SharedLLMGateway(ComponentToggle(implementation="mock"))

    result = await gateway.generate(role="reasoning", prompt="hello")

    assert result["provider"] == "mock"
    assert result["content"] == "hello"


def test_llm_backend_registry_supports_noop_and_mock() -> None:
    """LLM registry が複数実装を公開すること."""
    registry = LLMBackendRegistry()

    assert "default" in registry.implementations()
    assert "mock" in registry.implementations()
    assert "noop" in registry.implementations()


def test_trace_exporter_registry_can_switch_to_memory() -> None:
    """Trace exporter が memory 実装へ切り替わること."""
    registry = TraceExporterRegistry()
    exporter = registry.resolve(ComponentToggle(implementation="memory"))

    from contracts import TraceRecord

    record = TraceRecord(trace_id="trace-1", span_id="span-1", name="test-span")
    exporter.export(record)

    assert exporter.records[0].trace_id == "trace-1"


@pytest.mark.asyncio
async def test_harness_runtime_runs_kernel_executor_when_gate_disabled() -> None:
    """gate を切ると kernel executor 単体で流れること."""

    async def layer_echo(value: str) -> str:
        return f"echo:{value}"

    tool_name = "layer_echo_runtime"
    ToolProvider.register(
        RegisteredTool(
            name=tool_name,
            description="layer test tool",
            func=layer_echo,
        )
    )
    provider = ToolProvider.discover()
    tool = provider.get_tool(tool_name)
    assert tool is not None

    runtime = HarnessedToolRuntime(
        tool_provider=provider,
        tool_gate=ToolGate(toggle=ComponentToggle(enabled=False)),
        approval_service=ApprovalService(),
    )
    request = ToolRequest(tool_call_id="call-1", name=tool_name, arguments={"value": "demo"})

    result = await runtime.execute(request, tool=tool)

    assert result.status.value == "success"
    assert result.content == "echo:demo"
