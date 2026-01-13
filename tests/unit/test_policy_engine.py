# -*- coding: utf-8 -*-
"""PolicyEngine 単体テスト."""

from __future__ import annotations

import pytest

from agentflow.security.policy_engine import (
    AuthContext,
    AuthDecision,
    AuthMode,
    AuthResult,
    Policy,
    PolicyEngine,
)


class TestAuthContext:
    """AuthContext テスト."""

    def test_create(self) -> None:
        """作成."""
        ctx = AuthContext(
            subject={"user_id": "123", "role": "admin"},
            resource={"type": "report"},
            action="read",
        )
        assert ctx.subject["user_id"] == "123"
        assert ctx.action == "read"


class TestAuthResult:
    """AuthResult テスト."""

    def test_allowed_property(self) -> None:
        """allowed プロパティ."""
        allow_result = AuthResult(decision=AuthDecision.ALLOW)
        deny_result = AuthResult(decision=AuthDecision.DENY)

        assert allow_result.allowed is True
        assert deny_result.allowed is False


class TestPolicy:
    """Policy テスト."""

    def test_matches_subject(self) -> None:
        """主体条件マッチ."""
        policy = Policy(
            policy_id="p1",
            name="Admin Only",
            subject_conditions={"role": "admin"},
        )

        admin_ctx = AuthContext(subject={"role": "admin"})
        user_ctx = AuthContext(subject={"role": "user"})

        assert policy.matches(admin_ctx) is True
        assert policy.matches(user_ctx) is False

    def test_matches_resource(self) -> None:
        """リソース条件マッチ."""
        policy = Policy(
            policy_id="p1",
            name="Report Access",
            resource_conditions={"type": "report"},
        )

        report_ctx = AuthContext(resource={"type": "report"})
        other_ctx = AuthContext(resource={"type": "other"})

        assert policy.matches(report_ctx) is True
        assert policy.matches(other_ctx) is False

    def test_matches_action_list(self) -> None:
        """アクション条件マッチ（リスト）."""
        policy = Policy(
            policy_id="p1",
            name="Read/Write",
            action_conditions=["read", "write"],
        )

        read_ctx = AuthContext(action="read")
        execute_ctx = AuthContext(action="execute")

        assert policy.matches(read_ctx) is True
        assert policy.matches(execute_ctx) is False

    def test_matches_list_condition(self) -> None:
        """リスト条件（いずれかにマッチ）."""
        policy = Policy(
            policy_id="p1",
            name="Multiple Roles",
            subject_conditions={"role": ["admin", "manager"]},
        )

        admin_ctx = AuthContext(subject={"role": "admin"})
        manager_ctx = AuthContext(subject={"role": "manager"})
        user_ctx = AuthContext(subject={"role": "user"})

        assert policy.matches(admin_ctx) is True
        assert policy.matches(manager_ctx) is True
        assert policy.matches(user_ctx) is False

    def test_matches_custom_condition(self) -> None:
        """カスタム条件."""

        def time_check(ctx: AuthContext) -> bool:
            return ctx.environment.get("is_business_hours", False)

        policy = Policy(
            policy_id="p1",
            name="Business Hours Only",
            custom_condition=time_check,
        )

        biz_ctx = AuthContext(environment={"is_business_hours": True})
        off_ctx = AuthContext(environment={"is_business_hours": False})

        assert policy.matches(biz_ctx) is True
        assert policy.matches(off_ctx) is False


class TestPolicyEngine:
    """PolicyEngine テスト."""

    @pytest.fixture
    def engine(self) -> PolicyEngine:
        """エンジンのフィクスチャ."""
        return PolicyEngine()

    # =========================================================================
    # RBAC
    # =========================================================================

    @pytest.mark.asyncio
    async def test_rbac_admin(self, engine: PolicyEngine) -> None:
        """RBAC: 管理者."""
        ctx = AuthContext(
            subject={"role": "admin"},
            action="anything",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert result.allowed is True

    @pytest.mark.asyncio
    async def test_rbac_analyst_read(self, engine: PolicyEngine) -> None:
        """RBAC: アナリストの読み取り."""
        ctx = AuthContext(
            subject={"role": "analyst"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert result.allowed is True

    @pytest.mark.asyncio
    async def test_rbac_analyst_write_denied(self, engine: PolicyEngine) -> None:
        """RBAC: アナリストの書き込み拒否."""
        ctx = AuthContext(
            subject={"role": "analyst"},
            action="write",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert result.allowed is False

    @pytest.mark.asyncio
    async def test_rbac_guest(self, engine: PolicyEngine) -> None:
        """RBAC: ゲスト（権限なし）."""
        ctx = AuthContext(
            subject={"role": "guest"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert result.allowed is False

    @pytest.mark.asyncio
    async def test_rbac_custom_role(self, engine: PolicyEngine) -> None:
        """RBAC: カスタムロール."""
        engine.add_role("custom_role", {"read", "custom_action"})

        ctx = AuthContext(
            subject={"role": "custom_role"},
            action="custom_action",
        )
        result = await engine.authorize(ctx, mode=AuthMode.RBAC)
        assert result.allowed is True

    # =========================================================================
    # ABAC
    # =========================================================================

    @pytest.mark.asyncio
    async def test_abac_high_sensitivity_admin(self, engine: PolicyEngine) -> None:
        """ABAC: 高機密リソース（管理者）."""
        ctx = AuthContext(
            subject={"role": "admin"},
            resource={"sensitivity": "high"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.ABAC)
        assert result.allowed is True

    @pytest.mark.asyncio
    async def test_abac_high_sensitivity_analyst(self, engine: PolicyEngine) -> None:
        """ABAC: 高機密リソース（アナリスト）- 拒否."""
        ctx = AuthContext(
            subject={"role": "analyst"},
            resource={"sensitivity": "high"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.ABAC)
        assert result.allowed is False
        assert "高機密" in result.reason

    @pytest.mark.asyncio
    async def test_abac_department_check(self, engine: PolicyEngine) -> None:
        """ABAC: 部門チェック."""
        # 同じ部門
        ctx_same = AuthContext(
            subject={"role": "analyst", "department": "finance"},
            resource={"department": "finance"},
            action="read",
        )
        result_same = await engine.authorize(ctx_same, mode=AuthMode.ABAC)
        assert result_same.allowed is True

        # 異なる部門
        ctx_diff = AuthContext(
            subject={"role": "analyst", "department": "hr"},
            resource={"department": "finance"},
            action="read",
        )
        result_diff = await engine.authorize(ctx_diff, mode=AuthMode.ABAC)
        assert result_diff.allowed is False

    # =========================================================================
    # PBAC
    # =========================================================================

    @pytest.mark.asyncio
    async def test_pbac_no_policies(self, engine: PolicyEngine) -> None:
        """PBAC: ポリシーなし."""
        ctx = AuthContext(action="read")
        result = await engine.authorize(ctx, mode=AuthMode.PBAC)
        assert result.decision == AuthDecision.NOT_APPLICABLE

    @pytest.mark.asyncio
    async def test_pbac_matching_policy(self, engine: PolicyEngine) -> None:
        """PBAC: マッチするポリシー."""
        policy = Policy(
            policy_id="allow-read",
            name="Allow Read",
            action_conditions=["read"],
            effect=AuthDecision.ALLOW,
        )
        engine.add_policy(policy)

        ctx = AuthContext(action="read")
        result = await engine.authorize(ctx, mode=AuthMode.PBAC)
        assert result.allowed is True
        assert "allow-read" in result.matched_policies

    @pytest.mark.asyncio
    async def test_pbac_priority(self, engine: PolicyEngine) -> None:
        """PBAC: 優先度."""
        # 低優先度のDENY
        engine.add_policy(Policy(
            policy_id="deny-all",
            name="Deny All",
            priority=1,
            effect=AuthDecision.DENY,
        ))

        # 高優先度のALLOW
        engine.add_policy(Policy(
            policy_id="allow-admin",
            name="Allow Admin",
            subject_conditions={"role": "admin"},
            priority=10,
            effect=AuthDecision.ALLOW,
        ))

        ctx = AuthContext(subject={"role": "admin"}, action="read")
        result = await engine.authorize(ctx, mode=AuthMode.PBAC)

        # 高優先度が適用される
        assert result.allowed is True

    # =========================================================================
    # HYBRID
    # =========================================================================

    @pytest.mark.asyncio
    async def test_hybrid_mode(self, engine: PolicyEngine) -> None:
        """HYBRID: 複合モード."""
        ctx = AuthContext(
            subject={"role": "admin"},
            action="read",
        )
        result = await engine.authorize(ctx, mode=AuthMode.HYBRID)
        assert result.allowed is True
        assert result.mode == AuthMode.HYBRID

    # =========================================================================
    # 便利メソッド
    # =========================================================================

    @pytest.mark.asyncio
    async def test_can_helper(self, engine: PolicyEngine) -> None:
        """can() ヘルパー."""
        # 管理者は可能
        assert await engine.can("user1", "read", "report", role="admin") is True

        # アナリストは可能
        assert await engine.can("user2", "read", "report", role="analyst") is True

        # アナリストは書き込み不可
        assert await engine.can("user2", "write", "report", role="analyst") is False

    # =========================================================================
    # ポリシー管理
    # =========================================================================

    def test_add_remove_policy(self, engine: PolicyEngine) -> None:
        """ポリシー追加・削除."""
        policy = Policy(policy_id="test", name="Test")
        engine.add_policy(policy)

        assert "test" in engine.list_policies()

        assert engine.remove_policy("test") is True
        assert "test" not in engine.list_policies()
        assert engine.remove_policy("test") is False
