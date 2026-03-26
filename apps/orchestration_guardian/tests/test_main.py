"""Orchestration Guardian の最小回帰テスト."""

from __future__ import annotations

from typing import Any

import httpx
import pytest

from apps.orchestration_guardian.main import _build_verify_result, app


@pytest.mark.asyncio
async def test_health_endpoint() -> None:
    """ヘルスエンドポイントが公開されている."""
    transport = httpx.ASGITransport(app=app)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as client:
        response = await client.get("/api/health")

    assert response.status_code == 200
    payload = response.json()
    assert payload["status"] == "healthy"
    assert payload["service"] == "orchestration_guardian"


def test_build_verify_result_marks_ready_when_threshold_is_met() -> None:
    """十分なチェックが通れば ready 判定になる."""
    checks: dict[str, bool] = {
        "streaming": True,
        "a2a": True,
        "rag_contract": True,
        "auth_baseline": True,
    }

    result: dict[str, Any] = _build_verify_result("decision_governance_engine", checks)

    assert result["app_name"] == "decision_governance_engine"
    assert result["passed"] == 4
    assert result["total"] == 4
    assert result["status"] == "ready"
    assert result["risk"]["is_acceptable"] is True


@pytest.mark.asyncio
async def test_verify_endpoint_returns_structured_assessment() -> None:
    """verify API がスコアとリスク情報を返す."""
    transport = httpx.ASGITransport(app=app)
    async with httpx.AsyncClient(transport=transport, base_url="http://testserver") as client:
        response = await client.post(
            "/api/verify",
            json={
                "app_name": "market_trend_monitor",
                "has_streaming": True,
                "has_a2a": False,
                "has_rag_contract": True,
                "has_auth_baseline": False,
            },
        )

    assert response.status_code == 200
    payload = response.json()
    assert payload["app_name"] == "market_trend_monitor"
    assert payload["passed"] == 2
    assert payload["total"] == 4
    assert payload["status"] == "needs_improvement"
    assert payload["risk"]["level"] in {"medium", "high"}
