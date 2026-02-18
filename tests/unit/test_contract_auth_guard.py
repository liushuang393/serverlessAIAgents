# -*- coding: utf-8 -*-
"""ContractAuthGuard の単体テスト."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from fastapi import HTTPException
from starlette.requests import Request
from starlette.websockets import WebSocket

from agentflow.security.contract_auth_guard import ContractAuthGuard, ContractAuthGuardConfig


def _write_app_config(path: Path) -> None:
    payload = {
        "contracts": {
            "auth": {
                "enabled": True,
                "allow_anonymous": False,
            }
        }
    }
    path.write_text(json.dumps(payload, ensure_ascii=False, indent=2) + "\n", encoding="utf-8")


def _build_guard(config_path: Path) -> ContractAuthGuard:
    return ContractAuthGuard(
        ContractAuthGuardConfig(
            app_config_path=config_path,
            public_http_paths={"/api/health"},
            auth_header_name="x-api-key",
            ws_query_key="api_key",
            api_key_env_selector_var=None,
            default_api_key_env_var="TEST_API_KEY",
        ),
    )


def _build_request(path: str, headers: dict[str, str] | None = None) -> Request:
    raw_headers = [
        (key.lower().encode("latin-1"), value.encode("latin-1"))
        for key, value in (headers or {}).items()
    ]
    scope = {
        "type": "http",
        "method": "GET",
        "path": path,
        "raw_path": path.encode("ascii"),
        "headers": raw_headers,
        "query_string": b"",
        "scheme": "http",
        "server": ("testserver", 80),
        "client": ("127.0.0.1", 0),
        "http_version": "1.1",
    }
    return Request(scope)


def _build_websocket(
    *,
    path: str = "/ws",
    headers: dict[str, str] | None = None,
    query: str = "",
) -> tuple[WebSocket, list[dict[str, object]]]:
    sent: list[dict[str, object]] = []

    async def _receive() -> dict[str, object]:
        return {"type": "websocket.connect"}

    async def _send(message: dict[str, object]) -> None:
        sent.append(message)

    raw_headers = [
        (key.lower().encode("latin-1"), value.encode("latin-1"))
        for key, value in (headers or {}).items()
    ]
    scope = {
        "type": "websocket",
        "path": path,
        "raw_path": path.encode("ascii"),
        "headers": raw_headers,
        "query_string": query.encode("ascii"),
        "scheme": "ws",
        "server": ("testserver", 80),
        "client": ("127.0.0.1", 0),
        "subprotocols": [],
    }
    return WebSocket(scope=scope, receive=_receive, send=_send), sent


def test_verify_api_key_returns_503_when_env_missing(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    config_path = tmp_path / "app_config.json"
    _write_app_config(config_path)
    monkeypatch.delenv("TEST_API_KEY", raising=False)
    guard = _build_guard(config_path)

    with pytest.raises(HTTPException) as exc_info:
        guard.verify_api_key("any")
    assert exc_info.value.status_code == 503


def test_verify_api_key_returns_401_on_invalid_key(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config_path = tmp_path / "app_config.json"
    _write_app_config(config_path)
    monkeypatch.setenv("TEST_API_KEY", "expected")
    guard = _build_guard(config_path)

    with pytest.raises(HTTPException) as exc_info:
        guard.verify_api_key("invalid")
    assert exc_info.value.status_code == 401


@pytest.mark.asyncio
async def test_public_path_bypass(tmp_path: Path, monkeypatch: pytest.MonkeyPatch) -> None:
    config_path = tmp_path / "app_config.json"
    _write_app_config(config_path)
    monkeypatch.delenv("TEST_API_KEY", raising=False)
    guard = _build_guard(config_path)

    request = _build_request("/api/health")
    result = await guard.require_http(request)
    assert result is None


@pytest.mark.asyncio
async def test_ws_auth_accepts_header_and_query(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config_path = tmp_path / "app_config.json"
    _write_app_config(config_path)
    monkeypatch.setenv("TEST_API_KEY", "expected")
    guard = _build_guard(config_path)

    ws_header, sent_header = _build_websocket(headers={"x-api-key": "expected"})
    ok_header, _ = await guard.require_ws(ws_header)
    assert ok_header is True
    assert sent_header == []

    ws_query, sent_query = _build_websocket(query="api_key=expected")
    ok_query, _ = await guard.require_ws(ws_query)
    assert ok_query is True
    assert sent_query == []


@pytest.mark.asyncio
async def test_ws_close_code_on_auth_failure(
    tmp_path: Path,
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    config_path = tmp_path / "app_config.json"
    _write_app_config(config_path)
    guard = _build_guard(config_path)

    monkeypatch.setenv("TEST_API_KEY", "expected")
    ws_invalid, sent_invalid = _build_websocket(headers={"x-api-key": "wrong"})
    ok_invalid, _ = await guard.require_ws(ws_invalid)
    assert ok_invalid is False
    assert sent_invalid[-1]["type"] == "websocket.close"
    assert sent_invalid[-1]["code"] == 4401

    monkeypatch.delenv("TEST_API_KEY", raising=False)
    ws_missing, sent_missing = _build_websocket(headers={"x-api-key": "anything"})
    ok_missing, _ = await guard.require_ws(ws_missing)
    assert ok_missing is False
    assert sent_missing[-1]["type"] == "websocket.close"
    assert sent_missing[-1]["code"] == 1011
