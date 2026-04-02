"""Focused subagents / semantic tool contracts tests."""

from __future__ import annotations

from apps.messaging_hub.focused_subagents import (
    SemanticOperationType,
    SemanticToolContractRegistry,
    SummarySubagent,
    TaskTriageSubagent,
)


def test_report_template_exposes_draft_and_persist_contracts_for_markdown() -> None:
    """report テンプレートが markdown 保存時に write contract を持つこと."""
    registry = SemanticToolContractRegistry()

    contracts = registry.contracts_for_template(
        template_name="report",
        params={"format": "markdown"},
    )

    action_names = [contract.action_name for contract in contracts]
    assert action_names == ["create_report_draft", "persist_report_draft"]
    assert contracts[1].operation_type == SemanticOperationType.WRITE
    assert contracts[1].requires_human_gate is True


def test_triage_subagent_builds_policy_hooks_from_semantic_contracts() -> None:
    """triage subagent が semantic contract から focused plan を組み立てること."""
    triage = TaskTriageSubagent(SemanticToolContractRegistry())

    plan = triage.plan(
        template_name="report",
        parameters={"format": "markdown", "title": "Q2"},
        original_text="Q2 report",
        security_mode="approval_required",
    )

    assert plan.handler_name == "_execute_report"
    assert "pre_write_checkpoint" in plan.policy_hooks
    assert "human_gate" in plan.policy_hooks
    assert "strict_control_overlay" in plan.policy_hooks
    assert "mutation_path" in plan.route_tags


def test_summary_subagent_enriches_result_with_route_metadata() -> None:
    """summary subagent が route metadata を結果へ付与すること."""
    triage = TaskTriageSubagent(SemanticToolContractRegistry())
    plan = triage.plan(
        template_name="research",
        parameters={"topic": "Figma"},
        original_text="Research Figma",
        security_mode="autonomous",
    )

    enriched = SummarySubagent().enrich_result(
        plan=plan,
        result={"summary_points": ["調査完了"]},
    )

    assert enriched["semantic_actions"] == ["collect_research_sources"]
    assert "focused_workers" in enriched
    assert "semantic_contracts" in enriched
    assert enriched["summary_points"][-1].startswith("semantic actions:")
