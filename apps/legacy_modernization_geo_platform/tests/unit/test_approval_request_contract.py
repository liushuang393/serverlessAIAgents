"""ApprovalRequest adapter tests for the GEO platform."""

from __future__ import annotations

from contracts.policy import ApprovalRequest as ContractApprovalRequest

from apps.legacy_modernization_geo_platform.backend.schemas import ApprovalRequest, ApprovalStatus


def test_approval_request_reuses_canonical_contract() -> None:
    """Legacy GEO approval payload should extend the canonical contract."""
    request = ApprovalRequest(approved=False, reviewer_name="pytest", comment="needs review")

    assert isinstance(request, ContractApprovalRequest)
    assert request.approved is False
    assert request.action == ApprovalStatus.REJECTED
    assert request.requester == "pytest"
    assert request.reason == "needs review"
    assert request.context["reviewer_name"] == "pytest"
    assert request.context["approved"] is False
    assert request.context["risk_level"] == "high"


def test_approval_request_legacy_payload_round_trips() -> None:
    """Legacy JSON payload should round-trip through the adapter."""
    request = ApprovalRequest.model_validate(
        {
            "approved": True,
            "reviewer_name": "operator",
            "comment": None,
        }
    )

    dumped = request.model_dump(mode="json")

    assert dumped["approved"] is True
    assert dumped["reviewer_name"] == "operator"
    assert dumped["action"] == "approved"
    assert dumped["requester"] == "operator"
