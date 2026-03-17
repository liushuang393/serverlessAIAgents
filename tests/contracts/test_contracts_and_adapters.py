"""新しい契約と互換 adapter のテスト."""

from __future__ import annotations

from contracts import ApprovalRequest as FacadeApprovalRequest
from kernel.core.flow_definition import AgentDefinition, FlowDefinition
from harness.approval.types import ApprovalRequest as LegacyApprovalRequest
from infrastructure.llm.providers.tool_executor import ToolResult as LegacyToolResult
from contracts import ApprovalRequest as ContractApprovalRequest
from contracts import FlowDefinition as ContractFlowDefinition
from contracts import ToolResult as ContractToolResult


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
