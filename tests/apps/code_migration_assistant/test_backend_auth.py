"""Code Migration backend auth contract tests."""

from __future__ import annotations

import pytest
from apps.code_migration_assistant.backend import app as backend_app
from fastapi import HTTPException


def test_verify_api_key_requires_configured_env(monkeypatch) -> None:
    """Auth required app must fail fast when key env is missing."""
    monkeypatch.delenv("CODE_MIGRATION_API_KEY", raising=False)
    monkeypatch.delenv("CODE_MIGRATION_API_KEY_ENV", raising=False)

    with pytest.raises(HTTPException) as exc_info:
        backend_app._verify_api_key("any")
    assert exc_info.value.status_code == 503


def test_verify_api_key_rejects_invalid_key(monkeypatch) -> None:
    """Invalid API key must be rejected."""
    monkeypatch.setenv("CODE_MIGRATION_API_KEY", "expected-key")
    monkeypatch.delenv("CODE_MIGRATION_API_KEY_ENV", raising=False)

    with pytest.raises(HTTPException) as exc_info:
        backend_app._verify_api_key("wrong-key")
    assert exc_info.value.status_code == 401


def test_protected_http_path_scope() -> None:
    """Only /api/* except public paths should require auth."""
    assert backend_app._should_protect_http_path("/api/migration/start") is True
    assert backend_app._should_protect_http_path("/api/health") is False
    assert backend_app._should_protect_http_path("/") is False
