"""harness/security/policy_engine.py の単体テスト.

PolicyEngine の RBAC/ABAC/PBAC/HYBRID モード、
および AuthorizationService DI ブリッジのテスト。
"""

from __future__ import annotations

from unittest.mock import AsyncMock

from harness.security.policy_engine import (
    AuthContext,
    AuthDecision,
    AuthMode,
    Policy,
    PolicyEngine,
)


class TestPolicyEngineRBAC:
    """RBAC モードのテスト."""

    async def test_admin_allows_any_action(self) -> None:
        """admin ロールは全アクションを許可."""
        engine = PolicyEngine()
        ctx = AuthContext(
            subject={"user_id": "u1", "role": "admin"},
            action="delete",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert result.allowed

    async def test_viewer_allows_read(self) -> None:
        """viewer ロールは read を許可."""
        engine = PolicyEngine()
        ctx = AuthContext(
            subject={"user_id": "u2", "role": "viewer"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert result.allowed

    async def test_viewer_denies_write(self) -> None:
        """viewer ロールは write を拒否."""
        engine = PolicyEngine()
        ctx = AuthContext(
            subject={"user_id": "u3", "role": "viewer"},
            action="write",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert not result.allowed

    async def test_guest_denies_all(self) -> None:
        """guest ロールは全アクションを拒否."""
        engine = PolicyEngine()
        ctx = AuthContext(
            subject={"user_id": "u4", "role": "guest"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert not result.allowed

    async def test_unknown_role_denies(self) -> None:
        """未知ロールは拒否."""
        engine = PolicyEngine()
        ctx = AuthContext(
            subject={"user_id": "u5", "role": "unknown_role"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert not result.allowed


class TestPolicyEngineRBACWithDI:
    """AuthorizationService DI ブリッジのテスト."""

    async def test_di_service_allows(self) -> None:
        """DI の AuthorizationService が許可を返す場合."""
        mock_authz = AsyncMock()
        mock_authz.has_permission = AsyncMock(return_value=True)

        engine = PolicyEngine(authorization_service=mock_authz)
        ctx = AuthContext(
            subject={"user_id": "u1", "role": "viewer"},
            action="admin_action",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert result.allowed
        assert "DB RBAC" in result.reason
        mock_authz.has_permission.assert_awaited_once_with("u1", "admin_action")

    async def test_di_service_denies(self) -> None:
        """DI の AuthorizationService が拒否を返す場合."""
        mock_authz = AsyncMock()
        mock_authz.has_permission = AsyncMock(return_value=False)

        engine = PolicyEngine(authorization_service=mock_authz)
        ctx = AuthContext(
            subject={"user_id": "u2", "role": "admin"},
            action="restricted",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert not result.allowed
        assert "DB RBAC" in result.reason

    async def test_di_service_fallback_on_error(self) -> None:
        """DI サービスがエラーの場合、ハードコード辞書にフォールバック."""
        mock_authz = AsyncMock()
        mock_authz.has_permission = AsyncMock(side_effect=RuntimeError("DB down"))

        engine = PolicyEngine(authorization_service=mock_authz)
        ctx = AuthContext(
            subject={"user_id": "u3", "role": "admin"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        # admin のハードコード辞書にフォールバック → 許可
        assert result.allowed

    async def test_di_service_skipped_without_user_id(self) -> None:
        """user_id なしの場合、DI サービスをスキップしてフォールバック."""
        mock_authz = AsyncMock()
        mock_authz.has_permission = AsyncMock(return_value=False)

        engine = PolicyEngine(authorization_service=mock_authz)
        ctx = AuthContext(
            subject={"role": "admin"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        # user_id がないので DI スキップ → admin ハードコード辞書で許可
        assert result.allowed
        mock_authz.has_permission.assert_not_awaited()


class TestPolicyEnginePBAC:
    """PBAC モードのテスト."""

    async def test_matching_policy_applied(self) -> None:
        """マッチするポリシーが適用される."""
        engine = PolicyEngine()
        engine.add_policy(
            Policy(
                policy_id="p1",
                name="Allow analysts to read reports",
                subject_conditions={"role": "analyst"},
                resource_conditions={"type": "report"},
                action_conditions=["read"],
                effect=AuthDecision.ALLOW,
                priority=10,
            )
        )

        ctx = AuthContext(
            subject={"role": "analyst"},
            resource={"type": "report"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.PBAC)
        assert result.allowed
        assert "p1" in result.matched_policies

    async def test_no_matching_policy(self) -> None:
        """マッチするポリシーがない場合 NOT_APPLICABLE."""
        engine = PolicyEngine()
        ctx = AuthContext(
            subject={"role": "guest"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.PBAC)
        assert result.decision == AuthDecision.NOT_APPLICABLE

    async def test_higher_priority_wins(self) -> None:
        """優先度の高いポリシーが適用される."""
        engine = PolicyEngine()
        engine.add_policy(
            Policy(
                policy_id="deny",
                name="Deny all",
                effect=AuthDecision.DENY,
                priority=1,
            )
        )
        engine.add_policy(
            Policy(
                policy_id="allow",
                name="Allow all",
                effect=AuthDecision.ALLOW,
                priority=10,
            )
        )

        ctx = AuthContext(action="anything")
        result = await engine.authorize(ctx, mode=AuthMode.PBAC)
        assert result.allowed
        assert result.matched_policies[0] == "allow"


class TestPolicyEngineManagement:
    """ポリシー管理メソッドのテスト."""

    def test_add_and_list_policies(self) -> None:
        """ポリシーの追加と一覧取得."""
        engine = PolicyEngine()
        assert engine.list_policies() == []

        engine.add_policy(Policy(policy_id="p1", name="Policy 1"))
        assert "p1" in engine.list_policies()

    def test_remove_policy(self) -> None:
        """ポリシーの削除."""
        engine = PolicyEngine()
        engine.add_policy(Policy(policy_id="p1", name="Policy 1"))
        assert engine.remove_policy("p1")
        assert engine.list_policies() == []
        assert not engine.remove_policy("nonexistent")

    def test_add_custom_role(self) -> None:
        """カスタムロールの追加."""
        engine = PolicyEngine()
        engine.add_role("custom", {"read", "write"})
        assert engine._role_permissions["custom"] == {"read", "write"}
