"""Contract alignment tests for backend/frontend/docs."""

from __future__ import annotations

import asyncio
from pathlib import Path

from agentflow.integrations.context_bridge import FlowContext, SourceSystemType
from apps.code_migration_assistant.backend import app as backend_app
from apps.code_migration_assistant.engine import CodeMigrationEngine


def test_frontend_mapping_includes_business_semantics_and_same_origin() -> None:
    app_js = Path("apps/code_migration_assistant/frontend/app.js").read_text(encoding="utf-8")
    assert "window.location.origin" in app_js
    assert '"migration.extract_business_semantics"' in app_js
    assert "'business_semantics'" in app_js


def test_readme_declared_capabilities_have_wired_paths() -> None:
    readme = Path("apps/code_migration_assistant/README.md").read_text(encoding="utf-8")
    capabilities_py = Path(
        "apps/code_migration_assistant/workflow/capabilities.py"
    ).read_text(encoding="utf-8")

    declared = [
        "legacy-ingestion",
        "business-semantics",
        "modernization-generator",
        "compliance-reporter",
    ]
    for capability in declared:
        assert capability in readme
        assert capability in capabilities_py


def test_state_response_contains_contract_and_capability_trace(tmp_path: Path) -> None:
    backend_app.active_tasks.clear()
    task_id = "task-state-contract"
    runtime = backend_app.TaskRuntime(
        task_id=task_id,
        engine=CodeMigrationEngine(),
        inputs={"source_code": "IDENTIFICATION DIVISION."},
        flow_context=FlowContext(
            session_id=f"session-{task_id}",
            user_id="tester",
            tenant_id="tenant-test",
            source_system="pytest",
            source_system_type=SourceSystemType.API,
            user_context={"role": "manager", "permissions": ["read", "write", "execute", "manage"]},
        ),
        artifacts_dir=tmp_path / task_id,
        result={
            "success": True,
            "capability_trace": [
                {
                    "stage": "analysis",
                    "capability_id": "legacy-ingestion",
                    "provider": "skill",
                    "status": "applied",
                }
            ],
        },
    )
    backend_app.active_tasks[task_id] = runtime

    response = asyncio.run(backend_app.get_migration_state(task_id))
    assert response["contract_version"] == backend_app.CONTRACT_VERSION
    assert response["observation_events_count"] == 0
    assert response["capability_trace"][0]["capability_id"] == "legacy-ingestion"
