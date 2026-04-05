"""Architecture refactor contract tests for code_migration_assistant."""

from __future__ import annotations

from pathlib import Path

from apps.code_migration_assistant.adapters.factory import AdapterFactory

from contracts.runtime.migration_execution import MigrationTaskProfile, StageExecutionPlan


def test_duplicate_entrypoint_files_removed() -> None:
    root = Path("apps/code_migration_assistant")
    assert not (root / "api.py").exists()
    assert not (root / "frontend" / "webapp.py").exists()
    assert not (root / "web" / "main.py").exists()


def test_cli_no_longer_imports_legacy_pipeline_runtime() -> None:
    cli_source = Path("apps/code_migration_assistant/cli.py").read_text(encoding="utf-8")
    assert "apps.code_migration_assistant.pipeline.engine" not in cli_source


def test_adapter_factory_exposes_migration_task_profile() -> None:
    factory = AdapterFactory()

    profile = factory.get_task_profile("cobol-to-java")

    assert isinstance(profile, MigrationTaskProfile)
    assert profile.migration_type == "cobol-to-java"
    assert profile.migration_family == "language_modernization"
    assert profile.source_profile["language"] == "COBOL"
    assert profile.target_profile["language"] == "Java"
    assert profile.analysis_capability_set == ["legacy-ingestion", "business-semantics"]
    assert profile.delivery_strategy == "download_package"


def test_adapter_factory_exposes_stage_execution_plan() -> None:
    factory = AdapterFactory()

    plan = factory.get_stage_execution_plan("cobol-to-java", "transform")

    assert isinstance(plan, StageExecutionPlan)
    assert plan.stage == "transform"
    assert plan.default_executor == "codex_cli"
    assert plan.fallback_executor == "native"
    assert plan.max_retries == 2
