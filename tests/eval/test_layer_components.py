"""新しい層コンポーネントの差し替えテスト."""

from __future__ import annotations

import pytest

from contracts import ToolRequest
from harness import ApprovalService, HarnessedToolRuntime, ToolGate
from infrastructure import EmbeddingBackendRegistry, LLMBackendRegistry, TraceExporterRegistry
from infrastructure.llm.providers.tool_provider import RegisteredTool, ToolProvider
from infrastructure.rerank import RerankBackendRegistry
from shared import SharedEmbeddingGateway, SharedLLMGateway, SharedRAGService, SharedRerankGateway
from shared.registry import ComponentToggle


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
async def test_embedding_gateway_can_switch_to_mock_backend() -> None:
    """Embedding Gateway が mock backend を利用できること."""
    gateway = SharedEmbeddingGateway(ComponentToggle(implementation="mock"))

    vector = await gateway.embed_query("hello")

    assert len(vector) == 8
    assert gateway.model_name() == "mock"


def test_embedding_and_rerank_registries_expose_toggle_targets() -> None:
    """Embedding / Rerank の差し替え実装が公開されていること."""
    embedding_registry = EmbeddingBackendRegistry()
    rerank_registry = RerankBackendRegistry()

    assert "default" in embedding_registry.implementations()
    assert "mock" in embedding_registry.implementations()
    assert "noop" in embedding_registry.implementations()
    assert "default" in rerank_registry.implementations()
    assert "mock" in rerank_registry.implementations()
    assert "noop" in rerank_registry.implementations()


@pytest.mark.asyncio
async def test_rag_service_can_run_with_mock_embedding_and_rerank() -> None:
    """RAG Service が mock 実装で最小パイプラインを実行できること."""
    rag = SharedRAGService(
        llm_gateway=SharedLLMGateway(ComponentToggle(implementation="mock")),
        embedding_gateway=SharedEmbeddingGateway(ComponentToggle(implementation="mock")),
        rerank_gateway=SharedRerankGateway(ComponentToggle(implementation="mock")),
    )

    result = await rag.query(
        "alpha beta",
        documents=["alpha beta gamma", "zzz"],
        top_k=1,
    )

    assert result.sources
    assert result.sources[0]["index"] == 0


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

    # kernel 層の具体 executor を DI で注入
    from kernel.tools import KernelToolExecutor

    executor = KernelToolExecutor(provider)
    runtime = HarnessedToolRuntime(
        executor=executor,
        tool_gate=ToolGate(toggle=ComponentToggle(enabled=False)),
        approval_service=ApprovalService(),
    )
    request = ToolRequest(tool_call_id="call-1", name=tool_name, arguments={"value": "demo"})

    result = await runtime.execute(request, tool=tool)

    assert result.status.value == "success"
    assert result.content == "echo:demo"
