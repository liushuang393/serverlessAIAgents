"""Approval manager contract adapter tests."""

from __future__ import annotations

from apps.messaging_hub.approval_manager import ApprovalRequest
from contracts.policy import ApprovalRequest as ContractApprovalRequest
from kernel.skills.gateway import RiskLevel


def test_messaging_hub_approval_request_reuses_contract_model() -> None:
    """Messaging Hub approval request should extend the canonical contract."""
    request = ApprovalRequest(
        id="req-1",
        skill_name="write_file",
        risk_level=RiskLevel.HIGH,
        params={"path": "a.txt"},
        user_id="tester",
    )

    assert isinstance(request, ContractApprovalRequest)
    assert request.action == "write_file"
    assert request.requester == "tester"
    assert request.context["params"] == {"path": "a.txt"}
    assert request.priority == "high"


def test_messaging_hub_approval_request_round_trips_legacy_payload() -> None:
    """Legacy persistence payload should round-trip through the adapter."""
    payload = {
        "id": "req-2",
        "skill_name": "delete_file",
        "risk_level": "critical",
        "params": {"path": "danger.txt"},
        "user_id": "operator",
        "status": "pending",
        "metadata": {"channel": "admin-ui"},
    }

    restored = ApprovalRequest.from_dict(payload)
    serialized = restored.to_dict()

    assert restored.action == "delete_file"
    assert restored.reason.startswith("Approval required for skill 'delete_file'")
    assert serialized["risk_level"] == "critical"
    assert serialized["context"]["params"] == {"path": "danger.txt"}
    assert serialized["requester"] == "operator"
