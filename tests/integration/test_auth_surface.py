from __future__ import annotations


def test_auth_service_shell_uses_platform_entrypoint() -> None:
    from platform.api.auth_app import app as platform_app

    from apps.auth_service.main import app as legacy_app

    assert legacy_app.title == "Auth Service"
    assert legacy_app is platform_app


def test_shared_access_exports_auth_service() -> None:
    from shared.access import AuthService, get_auth_service

    assert AuthService.__name__ == "AuthService"
    assert isinstance(get_auth_service(), AuthService)
