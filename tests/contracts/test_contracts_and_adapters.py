"""新しい契約と互換 adapter のテスト."""

from __future__ import annotations

from contracts import ApprovalRequest as FacadeApprovalRequest
from kernel.core.flow_definition import AgentDefinition, FlowDefinition
from harness.approval.types import ApprovalRequest as LegacyApprovalRequest
from infrastructure.llm.providers.tool_executor import ToolResult as LegacyToolResult
from contracts import ApprovalRequest as ContractApprovalRequest
from contracts import FlowDefinition as ContractFlowDefinition
from contracts import PluginBinding, PluginDescriptor, PluginRuntimeAssessment
from contracts import ToolResult as ContractToolResult
from kernel.tools.unified_tool import ToolResult as LegacyUnifiedToolResult
from kernel.tools.unified_tool import ToolStatus, ToolType


def test_contract_models_expose_schema() -> None:
    """主要契約が JSON Schema を生成できること."""
    flow_schema = ContractFlowDefinition.model_json_schema()
    tool_schema = ContractToolResult.model_json_schema()

    assert "properties" in flow_schema
    assert "roles" in flow_schema["properties"]
    assert "properties" in tool_schema
    assert "tool_call_id" in tool_schema["properties"]


def test_legacy_flow_definition_round_trips_through_contract() -> None:
    """legacy FlowDefinition が契約へ往復変換できること."""
    legacy = FlowDefinition(
        flow_id="demo-flow",
        name="Demo Flow",
        agents=[
            AgentDefinition(id="designer", name="Designer", label="design"),
            AgentDefinition(id="reviewer", name="Reviewer", label="review"),
        ],
    )

    contract = legacy.to_contract()
    restored = FlowDefinition.from_contract(contract)

    assert contract.flow_id == "demo-flow"
    assert contract.roles[0].role_id == "designer"
    assert restored.get_agent_ids() == ["designer", "reviewer"]


def test_legacy_approval_request_reuses_contract_model() -> None:
    """ApprovalRequest が契約モデルへ統合されていること."""
    request = LegacyApprovalRequest(action="publish", reason="review required")

    assert isinstance(request, ContractApprovalRequest)
    assert isinstance(request, FacadeApprovalRequest)
    assert request.action == "publish"


def test_legacy_tool_result_reuses_contract_model() -> None:
    """ToolExecutor の ToolResult が契約モデルを再利用すること."""
    result = LegacyToolResult(tool_call_id="call-1", name="echo", content="ok")

    assert isinstance(result, ContractToolResult)
    assert result.name == "echo"


def test_plugin_contract_models_expose_schema() -> None:
    """Plugin 契約が JSON Schema を生成できること."""
    descriptor_schema = PluginDescriptor.model_json_schema()
    assessment_schema = PluginRuntimeAssessment.model_json_schema()

    assert "properties" in descriptor_schema
    assert "risk_tier" in descriptor_schema["properties"]
    assert "properties" in assessment_schema
    assert "plugin_id" in assessment_schema["properties"]


def test_legacy_unified_tool_result_round_trips_through_contract() -> None:
    """legacy unified tool result が canonical contract を経由できること."""
    legacy = LegacyUnifiedToolResult(
        success=True,
        status=ToolStatus.SUCCESS,
        tool_uri="builtin://echo",
        tool_type=ToolType.BUILTIN,
        output={"message": "ok"},
        duration_ms=12.5,
    )

    contract = legacy.to_contract(tool_call_id="call-123", name="echo")
    restored = LegacyUnifiedToolResult.from_contract(
        contract,
        tool_uri="builtin://echo",
        tool_type=ToolType.BUILTIN,
    )

    assert isinstance(contract, ContractToolResult)
    assert contract.name == "echo"
    assert restored.success is True
    assert restored.output == {"message": "ok"}


def test_plugin_binding_contract_normalizes_identifier() -> None:
    """PluginBinding は ID を正規化する."""
    binding = PluginBinding(id=" Official.Test-Pack ", version="1.0.0")

    assert binding.id == "official.test-pack"
