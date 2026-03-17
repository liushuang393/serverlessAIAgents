"""Agent taxonomy サービスのユニットテスト."""

from __future__ import annotations

import pytest
from control_plane.services.agent_taxonomy import AgentTaxonomyService


@pytest.mark.parametrize(
    ("legacy_pattern", "expected_agent_type"),
    [
        ("specialist", "specialist"),
        ("coordinator", "planner"),
        ("analyzer", "reactor"),
        ("pipeline_stage", "executor"),
        ("executor", "executor"),
        ("reviewer", "reviewer"),
        ("gatekeeper", "gatekeeper"),
        ("router", "router"),
        ("reporter", "reporter"),
        ("custom", "custom"),
        ("unknown_pattern", "custom"),
    ],
)
def test_pattern_to_agent_type_mapping(legacy_pattern: str, expected_agent_type: str) -> None:
    """legacy pattern から agent_type へ正しく変換する."""
    assert AgentTaxonomyService.pattern_to_agent_type(legacy_pattern) == expected_agent_type


def test_infer_agent_type_prefers_explicit_type() -> None:
    """infer_agent_type は raw_agent_type を優先する."""
    taxonomy = AgentTaxonomyService()
    result = taxonomy.infer_agent_type(
        raw_agent_type="Planner",
        raw_pattern="analyzer",
        name="Foo",
        module="apps.foo.agent",
        engine_pattern="flow",
    )
    assert result == "planner"


def test_infer_agent_type_from_pattern() -> None:
    """raw_agent_type がない場合は pattern から推定する."""
    taxonomy = AgentTaxonomyService()
    result = taxonomy.infer_agent_type(
        raw_agent_type=None,
        raw_pattern="coordinator",
        name="CoordinatorAgent",
        module="apps.foo.coordinator",
        engine_pattern="flow",
    )
    assert result == "planner"


def test_infer_app_template_from_product_line() -> None:
    """product_line から app_template を推定する."""
    taxonomy = AgentTaxonomyService()
    assert (
        taxonomy.infer_app_template(product_line="faq", engine_pattern="simple", tags=[])
        == "faq_knowledge_service"
    )
    assert (
        taxonomy.infer_app_template(product_line="assistant", engine_pattern="flow", tags=[])
        == "multichannel_assistant"
    )
    assert (
        taxonomy.infer_app_template(product_line="migration", engine_pattern="pipeline", tags=[])
        == "ops_automation_runner"
    )


def test_infer_app_template_falls_back_to_workflow_orchestrator() -> None:
    """推定不能時は workflow_orchestrator にフォールバックする."""
    taxonomy = AgentTaxonomyService()
    result = taxonomy.infer_app_template(
        product_line="framework",
        engine_pattern="simple",
        tags=["misc", "demo"],
    )
    assert result == "workflow_orchestrator"
