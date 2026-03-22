"""FAQ 認証設定の既定値フォールバックを検証する単体テスト."""

from __future__ import annotations

from typing import TYPE_CHECKING

from apps.faq_system.backend.auth import dependencies as auth_dependencies
from apps.faq_system.backend.auth import router as auth_router


if TYPE_CHECKING:
    import pytest


def test_auth_mode_defaults_to_tenant_sso_when_config_missing(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.delenv("FAQ_AUTH_MODE", raising=False)
    monkeypatch.setattr(auth_dependencies, "_auth_contract", dict)
    monkeypatch.setattr(auth_router, "_load_contract_auth_mode", lambda: None)

    assert auth_dependencies._resolve_auth_mode() == "tenant_sso"
    assert auth_router._resolve_auth_mode() == "tenant_sso"


def test_request_tenant_fallbacks_to_default_tenant_id(monkeypatch: pytest.MonkeyPatch) -> None:
    monkeypatch.setenv("FAQ_DEFAULT_TENANT_ID", "default-tenant")

    assert auth_dependencies._resolve_request_tenant({}, "/api/auth/me") == "default-tenant"
    assert auth_dependencies._resolve_request_tenant({"host": "localhost:8005"}, "/api/auth/me") == "default-tenant"
