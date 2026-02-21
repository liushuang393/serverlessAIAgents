"""plugin manifest 実行時ガバナンスのテスト."""

from __future__ import annotations

from dataclasses import dataclass

import pytest

from agentflow.governance import (
    AuditEvent,
    AuditLogger,
    GovernanceDecision,
    GovernanceEngine,
    ToolExecutionContext,
)
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
) -> RegisteredTool:
    """副作用ツールの RegisteredTool を生成."""

    def _run(path: str) -> str:
        return path

    return RegisteredTool(
        name="write_tool",
        description="副作用ツール",
        func=_run,
        operation_type=OperationType.WRITE,
        plugin_id=plugin_id,
        plugin_version=plugin_version,
    )


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
