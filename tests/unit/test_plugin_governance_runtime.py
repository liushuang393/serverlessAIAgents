"""plugin manifest 実行時ガバナンスのテスト."""

from __future__ import annotations

import json
from dataclasses import dataclass
from pathlib import Path

import pytest

from agentflow.governance import (
    AuditEvent,
    AuditLogger,
    GovernanceDecision,
    GovernanceEngine,
    ToolExecutionContext,
)
from agentflow.governance.plugin_registry import PluginRegistry
from agentflow.providers.tool_provider import OperationType, RegisteredTool
from agentflow.security.policy_engine import AuthContext, AuthMode


@dataclass
class _InMemoryAuditLogger(AuditLogger):
    """テスト用監査ロガー."""

    events: list[AuditEvent]

    def __init__(self) -> None:
        self.events = []

    def log_event(self, event: AuditEvent) -> None:
        self.events.append(event)


def _auth_context() -> AuthContext:
    """admin 権限の認証コンテキストを返す."""
    return AuthContext(
        subject={
            "user_id": "tester",
            "role": "admin",
            "permissions": ["repo.read", "repo.write", "network.egress", "os.exec"],
        },
        resource={"type": "tool"},
        action="",
    )


def _write_tool(
    *,
    plugin_id: str | None,
    plugin_version: str | None,
    required_permissions: list[str] | None = None,
) -> RegisteredTool:
    """副作用ツールの RegisteredTool を生成."""

    def _run(path: str) -> str:
        return path

    return RegisteredTool(
        name="write_tool",
        description="副作用ツール",
        func=_run,
        operation_type=OperationType.WRITE,
        required_permissions=required_permissions or [],
        plugin_id=plugin_id,
        plugin_version=plugin_version,
    )


def _write_plugin_fixture(
    tmp_path: Path,
    *,
    include_sig: bool,
) -> tuple[Path, Path]:
    plugins_dir = tmp_path / "plugins"
    apps_dir = tmp_path / "apps"
    plugin_dir = plugins_dir / "official.test-signature-pack"
    app_dir = apps_dir / "migration_app"
    plugin_dir.mkdir(parents=True)
    app_dir.mkdir(parents=True)

    manifest = {
        "id": "official.test-signature-pack",
        "version": "1.0.0",
        "type": "tool",
        "capabilities": ["test.capability"],
        "risk_tier": "medium",
        "side_effects": ["filesystem.write"],
        "required_permissions": ["repo.write"],
        "signature": {
            "algorithm": "ed25519",
            "issuer": "agentflow-official",
            "key_id": "af-official-2026-q1",
        },
        "compatibility": {"kernel": ">=1.0.0", "product_lines": ["migration"]},
        "tests_required": ["unit"],
    }
    (plugin_dir / "plugin_manifest.json").write_text(
        json.dumps(manifest, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )
    if include_sig:
        (plugin_dir / "plugin_manifest.sig").write_text("invalid-signature\n", encoding="utf-8")

    trust_store = {
        "agentflow-official": {
            "af-official-2026-q1": {
                "algorithm": "ed25519",
                "public_key_base64": "AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=",
            }
        }
    }
    (plugins_dir / "trust_store.json").write_text(
        json.dumps(trust_store, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )

    app_config = {
        "name": "migration_app",
        "display_name": "Migration App",
        "description": "",
        "business_base": "operations",
        "product_line": "migration",
        "surface_profile": "business",
        "audit_profile": "business",
        "version": "1.0.0",
        "icon": "M",
        "ports": {"api": None, "frontend": None, "db": None, "redis": None},
        "entry_points": {"api_module": None, "health": None},
        "agents": [],
        "services": {},
        "dependencies": {"database": None, "redis": False, "external": []},
        "runtime": {
            "urls": {"backend": None, "frontend": None, "health": None, "database": None},
            "database": {
                "kind": None,
                "url": None,
                "host": None,
                "port": None,
                "name": None,
                "user": None,
                "password": None,
                "password_env": None,
                "note": None,
            },
            "commands": {
                "backend_dev": None,
                "frontend_dev": None,
                "publish": None,
                "start": None,
                "stop": None,
            },
        },
        "contracts": {"auth": {"enabled": False, "allow_anonymous": True}},
        "plugin_bindings": [{"id": "official.test-signature-pack", "version": "1.0.0"}],
        "blueprint": {"engine_pattern": "simple", "default_skills": [], "mcp_servers": []},
        "visibility": {"mode": "private", "tenants": []},
        "tags": [],
    }
    (app_dir / "app_config.json").write_text(
        json.dumps(app_config, ensure_ascii=False, indent=2) + "\n",
        encoding="utf-8",
    )
    return plugins_dir, apps_dir


@pytest.mark.asyncio
async def test_studio_side_effect_tool_without_plugin_is_denied() -> None:
    """Studio では副作用ツールに plugin_id が無いと拒否される."""
    audit = _InMemoryAuditLogger()
    engine = GovernanceEngine(audit_logger=audit, auth_mode=AuthMode.RBAC)
    tool = _write_tool(plugin_id=None, plugin_version=None)
    context = ToolExecutionContext(
        auth_context=_auth_context(),
        app_name="code_migration_assistant",
        product_line="migration",
    )

    result = await engine.evaluate_tool(
        tool,
        tool_call_id="call-1",
        arguments={"path": "src/main.cobol"},
        context=context,
    )

    assert result.decision == GovernanceDecision.DENY
    assert "plugin_id" in result.reason


@pytest.mark.asyncio
async def test_studio_manifest_mismatch_is_denied() -> None:
    """Studio では manifest の product_line 不整合を拒否する."""
    audit = _InMemoryAuditLogger()
    engine = GovernanceEngine(audit_logger=audit, auth_mode=AuthMode.RBAC)
    tool = _write_tool(
        plugin_id="official.enterprise-connector-pack",
        plugin_version="1.0.0",
    )
    context = ToolExecutionContext(
        auth_context=_auth_context(),
        app_name="code_migration_assistant",
        product_line="migration",
    )

    result = await engine.evaluate_tool(
        tool,
        tool_call_id="call-2",
        arguments={"path": "src/main.cobol"},
        context=context,
    )

    assert result.decision == GovernanceDecision.DENY
    assert result.plugin_id == "official.enterprise-connector-pack"
    assert result.plugin_version == "1.0.0"
    assert result.plugin_risk_tier == "medium"


@pytest.mark.asyncio
async def test_framework_manifest_mismatch_is_allowed_with_warning() -> None:
    """Framework では不整合を warning で許可する."""
    audit = _InMemoryAuditLogger()
    engine = GovernanceEngine(audit_logger=audit, auth_mode=AuthMode.RBAC)
    tool = _write_tool(
        plugin_id="official.enterprise-connector-pack",
        plugin_version="1.0.0",
    )
    context = ToolExecutionContext(
        auth_context=_auth_context(),
        app_name="platform",
        product_line="framework",
    )

    result = await engine.evaluate_tool(
        tool,
        tool_call_id="call-3",
        arguments={"path": "src/platform.py"},
        context=context,
    )

    assert result.decision == GovernanceDecision.ALLOW
    assert len(result.warnings) >= 1
    assert result.plugin_risk_tier == "medium"


@pytest.mark.asyncio
async def test_audit_event_contains_plugin_metadata() -> None:
    """監査イベントに plugin metadata が含まれる."""
    audit = _InMemoryAuditLogger()
    engine = GovernanceEngine(audit_logger=audit, auth_mode=AuthMode.RBAC)
    tool = _write_tool(
        plugin_id="official.enterprise-connector-pack",
        plugin_version="1.0.0",
    )
    context = ToolExecutionContext(
        auth_context=_auth_context(),
        app_name="platform",
        product_line="framework",
    )

    _ = await engine.evaluate_tool(
        tool,
        tool_call_id="call-4",
        arguments={"path": "src/platform.py"},
        context=context,
    )

    assert len(audit.events) == 1
    metadata = audit.events[0].metadata
    assert metadata.get("plugin_id") == "official.enterprise-connector-pack"
    assert metadata.get("plugin_version") == "1.0.0"
    assert metadata.get("plugin_risk_tier") == "medium"
    assert metadata.get("plugin_signature_status") == "verified"
    assert "plugin_signature_reason" in metadata


@pytest.mark.asyncio
async def test_studio_side_effect_tool_requires_plugin_version() -> None:
    """strict product_line では plugin_version 未指定を拒否する."""
    audit = _InMemoryAuditLogger()
    engine = GovernanceEngine(audit_logger=audit, auth_mode=AuthMode.RBAC)
    tool = _write_tool(
        plugin_id="official.cobol-migration-pack",
        plugin_version=None,
    )
    context = ToolExecutionContext(
        auth_context=_auth_context(),
        app_name="code_migration_assistant",
        product_line="migration",
    )

    result = await engine.evaluate_tool(
        tool,
        tool_call_id="call-5",
        arguments={"path": "src/main.cobol"},
        context=context,
    )

    assert result.decision == GovernanceDecision.DENY
    assert "plugin_version" in result.reason


@pytest.mark.asyncio
async def test_framework_permission_mismatch_is_warning_only() -> None:
    """framework では manifest 権限との差分を warning として扱う."""
    audit = _InMemoryAuditLogger()
    engine = GovernanceEngine(audit_logger=audit, auth_mode=AuthMode.RBAC)
    tool = _write_tool(
        plugin_id="official.enterprise-connector-pack",
        plugin_version="1.0.0",
        required_permissions=[],
    )
    context = ToolExecutionContext(
        auth_context=_auth_context(),
        app_name="platform",
        product_line="framework",
    )

    result = await engine.evaluate_tool(
        tool,
        tool_call_id="call-6",
        arguments={"path": "src/platform.py"},
        context=context,
    )

    assert result.decision == GovernanceDecision.ALLOW
    assert any("必須権限" in warning for warning in result.warnings)


@pytest.mark.asyncio
async def test_signature_mismatch_is_warning_only_for_strict_product_line(
    tmp_path: Path,
) -> None:
    """strict product_line でも署名不整合は warning のみ."""
    plugins_dir, apps_dir = _write_plugin_fixture(tmp_path, include_sig=True)
    registry = PluginRegistry(plugins_dir=plugins_dir, apps_dir=apps_dir)
    engine = GovernanceEngine(
        audit_logger=_InMemoryAuditLogger(),
        auth_mode=AuthMode.RBAC,
        plugin_registry=registry,
    )
    tool = _write_tool(
        plugin_id="official.test-signature-pack",
        plugin_version="1.0.0",
        required_permissions=["repo.write"],
    )
    context = ToolExecutionContext(
        auth_context=_auth_context(),
        app_name="migration_app",
        product_line="migration",
    )

    result = await engine.evaluate_tool(
        tool,
        tool_call_id="call-signature-warn",
        arguments={"path": "src/main.cobol"},
        context=context,
    )

    assert result.decision == GovernanceDecision.ALLOW
    assert any("plugin 署名検証 warning" in warning for warning in result.warnings)
