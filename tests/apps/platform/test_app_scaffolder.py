"""AppScaffolderService のユニットテスト."""

from __future__ import annotations

from typing import TYPE_CHECKING

from apps.platform.schemas.provisioning_schemas import AgentBlueprintInput, AppCreateRequest
from apps.platform.services.app_scaffolder import AppScaffolderService


if TYPE_CHECKING:
    from pathlib import Path

    from apps.platform.services.app_discovery import AppDiscoveryService


def test_create_options_contains_agent_type_and_app_template() -> None:
    """create/options に agent_type / app_template 選択肢を含む."""
    options = AppScaffolderService.create_options(surface_profile="developer")
    assert "agent_type_options" in options
    assert "app_template_options" in options
    assert any(item["value"] == "specialist" for item in options["agent_type_options"])
    assert any(item["value"] == "workflow_orchestrator" for item in options["app_template_options"])


def test_normalize_agents_defaults_agent_type_and_pattern(
    discovery: AppDiscoveryService,
    tmp_path: Path,
) -> None:
    """agents 未指定時は specialist + pattern 互換値を補完する."""
    scaffolder = AppScaffolderService(discovery=discovery, apps_dir=tmp_path / "apps")
    normalized = scaffolder._normalize_agents([], "demo_app")
    assert len(normalized) == 1
    assert normalized[0]["agent_type"] == "specialist"
    assert normalized[0]["pattern"] == "specialist"


def test_normalize_agents_uses_role_when_agent_type_missing(
    discovery: AppDiscoveryService,
    tmp_path: Path,
) -> None:
    """agent_type 未指定時は role から推定し、互換 pattern も設定する."""
    scaffolder = AppScaffolderService(discovery=discovery, apps_dir=tmp_path / "apps")
    normalized = scaffolder._normalize_agents(
        [
            AgentBlueprintInput(
                name="PlannerAgent",
                role="planner",
                agent_type=None,
                prompt="plan",
                capabilities=["analysis"],
            )
        ],
        "demo_app",
    )
    assert normalized[0]["agent_type"] == "planner"
    assert normalized[0]["pattern"] == "coordinator"


def test_resolve_app_template_defaults_to_workflow_orchestrator(
    discovery: AppDiscoveryService,
    tmp_path: Path,
) -> None:
    """app_template 未指定時は workflow_orchestrator へフォールバックする."""
    scaffolder = AppScaffolderService(discovery=discovery, apps_dir=tmp_path / "apps")
    request = AppCreateRequest(
        name="demo_app",
        display_name="Demo App",
        description="Demo",
        product_line="framework",
        surface_profile="developer",
        audit_profile="developer",
        plugin_bindings=[],
        app_template=None,
        engine_pattern="simple",
    )
    assert scaffolder._resolve_app_template(request) == "workflow_orchestrator"


def test_resolve_app_template_normalizes_explicit_value(
    discovery: AppDiscoveryService,
    tmp_path: Path,
) -> None:
    """明示された app_template は正規化して採用する."""
    scaffolder = AppScaffolderService(discovery=discovery, apps_dir=tmp_path / "apps")
    request = AppCreateRequest(
        name="demo_app_explicit",
        display_name="Demo App",
        description="Demo",
        product_line="framework",
        surface_profile="developer",
        audit_profile="developer",
        plugin_bindings=[],
        app_template="workflow_orchestrator",
        engine_pattern="simple",
    )
    assert scaffolder._resolve_app_template(request) == "workflow_orchestrator"
