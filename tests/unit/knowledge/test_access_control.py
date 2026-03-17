"""ロールベースアクセス制御のユニットテスト.

ScopeResolver のフォールバック RBAC マッピングと
RAGAccessControl の KB タイプアクセス検証をテストする。
"""

from __future__ import annotations

import pytest

from shared.rag.rag_access_control import RAGAccessControl
from shared.rag.scope_resolver import FALLBACK_ROLE_KB_MAP, ScopeResolver


# ===========================================================================
# ScopeResolver フォールバック RBAC テスト
# ===========================================================================


class TestFallbackRBACMap:
    """フォールバック RBAC マッピングの検証."""

    def test_admin_has_all_access(self) -> None:
        """admin ロールは全 KB タイプにアクセス可能."""
        assert ScopeResolver.check_kb_type_access("admin", "internal")
        assert ScopeResolver.check_kb_type_access("admin", "external")
        assert ScopeResolver.check_kb_type_access("admin", "confidential")

    def test_manager_internal_external(self) -> None:
        """manager ロールは internal / external にアクセス可能."""
        assert ScopeResolver.check_kb_type_access("manager", "internal")
        assert ScopeResolver.check_kb_type_access("manager", "external")
        assert not ScopeResolver.check_kb_type_access("manager", "confidential")

    def test_employee_internal_external(self) -> None:
        """employee ロールは internal / external にアクセス可能."""
        assert ScopeResolver.check_kb_type_access("employee", "internal")
        assert ScopeResolver.check_kb_type_access("employee", "external")
        assert not ScopeResolver.check_kb_type_access("employee", "confidential")

    def test_guest_external_only(self) -> None:
        """guest ロールは external のみアクセス可能."""
        assert not ScopeResolver.check_kb_type_access("guest", "internal")
        assert ScopeResolver.check_kb_type_access("guest", "external")
        assert not ScopeResolver.check_kb_type_access("guest", "confidential")

    def test_unknown_role_defaults_to_external(self) -> None:
        """未知のロールは external のみアクセス可能."""
        assert not ScopeResolver.check_kb_type_access("unknown_role", "internal")
        assert ScopeResolver.check_kb_type_access("unknown_role", "external")
        assert not ScopeResolver.check_kb_type_access("unknown_role", "confidential")

    def test_get_allowed_kb_types(self) -> None:
        """各ロールの許可 KB タイプ一覧を検証."""
        assert ScopeResolver.get_allowed_kb_types("admin") == ["internal", "external", "confidential"]
        assert ScopeResolver.get_allowed_kb_types("guest") == ["external"]
        assert ScopeResolver.get_allowed_kb_types("random") == ["external"]


# ===========================================================================
# RAGAccessControl テスト
# ===========================================================================


class TestRAGAccessControl:
    """RAGAccessControl の KB タイプアクセス検証."""

    def test_check_kb_type_access_admin(self) -> None:
        """admin は全タイプにアクセス可能."""
        assert RAGAccessControl.check_kb_type_access("admin", "internal")
        assert RAGAccessControl.check_kb_type_access("admin", "confidential")

    def test_check_kb_type_access_guest_denied(self) -> None:
        """guest は confidential にアクセス不可."""
        assert not RAGAccessControl.check_kb_type_access("guest", "confidential")
        assert not RAGAccessControl.check_kb_type_access("guest", "internal")

    def test_init_without_resolver(self) -> None:
        """ScopeResolver なしでインスタンス化可能."""
        ac = RAGAccessControl()
        assert ac._resolver is None

    @pytest.mark.asyncio
    async def test_get_search_targets_no_resolver(self) -> None:
        """ScopeResolver なしの場合は空リストを返す."""
        ac = RAGAccessControl()
        targets = await ac.get_search_targets(role="admin", app_name="test")
        assert targets == []


# ===========================================================================
# FALLBACK_ROLE_KB_MAP 公開テスト
# ===========================================================================


class TestFallbackRoleKBMap:
    """エクスポートされた FALLBACK_ROLE_KB_MAP の検証."""

    def test_map_has_expected_roles(self) -> None:
        """4つの標準ロールが定義されていること."""
        assert set(FALLBACK_ROLE_KB_MAP.keys()) == {"admin", "manager", "employee", "guest"}

    def test_admin_includes_confidential(self) -> None:
        """admin は confidential を含む."""
        assert "confidential" in FALLBACK_ROLE_KB_MAP["admin"]

    def test_guest_only_external(self) -> None:
        """guest は external のみ."""
        assert FALLBACK_ROLE_KB_MAP["guest"] == ["external"]
