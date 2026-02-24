# -*- coding: utf-8 -*-
"""report ルーターの署名閾値テスト."""

import pytest

from apps.decision_governance_engine.routers import report as report_router
from apps.decision_governance_engine.routers.auth import UserInfo
from apps.decision_governance_engine.schemas.output_schemas import (
    DecisionReport,
    ExecutiveSummary,
)


def _build_report(confidence_score: float) -> DecisionReport:
    return DecisionReport(
        report_id="PROP-202602-SIGN01",
        original_question="PoCを承認してよいか",
        dao={
            "problem_type": "TRADE_OFF",
            "essence": "PoC前提を明確化する",
            "immutable_constraints": [],
            "hidden_assumptions": [],
        },
        fa={"recommended_paths": [], "rejected_paths": [], "decision_criteria": []},
        shu={"phases": [], "first_action": "責任者を決める", "dependencies": []},
        qi={
            "implementations": [],
            "tool_recommendations": [],
            "integration_points": [],
            "technical_debt_warnings": [],
        },
        review={
            "overall_verdict": "REVISE",
            "confidence_score": confidence_score,
            "findings": [],
            "final_warnings": [],
        },
        executive_summary=ExecutiveSummary(
            one_line_decision="検証結果を確認",
            recommended_action="不足条件を補完",
            key_risks=[],
            first_step="チェック項目を反映",
            estimated_impact="中",
        ),
    )


def _build_user() -> UserInfo:
    return UserInfo(
        user_id="user-admin",
        username="admin",
        display_name="管理者 太郎",
        department="経営企画部",
        position="部長",
        created_at="2026-02-24T00:00:00Z",
    )


@pytest.mark.asyncio
async def test_sign_report_rejects_when_confidence_below_threshold(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """信頼度39%未満では署名できないこと."""
    report_router._signed_reports.clear()
    report = _build_report(0.38)

    async def _mock_get_report(_report_id: str) -> DecisionReport:
        return report

    monkeypatch.setattr(report_router, "_get_report_from_db", _mock_get_report)

    response = await report_router.sign_report(
        report_id=report.report_id,
        req=report_router.SignatureRequest(report_id=report.report_id, confirmation=True),
        user=_build_user(),
    )

    assert response.success is False
    assert "39%未満" in response.message


@pytest.mark.asyncio
async def test_sign_report_accepts_when_confidence_reaches_threshold(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """信頼度39%以上なら署名できること."""
    report_router._signed_reports.clear()
    report = _build_report(0.39)

    async def _mock_get_report(_report_id: str) -> DecisionReport:
        return report

    monkeypatch.setattr(report_router, "_get_report_from_db", _mock_get_report)

    response = await report_router.sign_report(
        report_id=report.report_id,
        req=report_router.SignatureRequest(report_id=report.report_id, confirmation=True),
        user=_build_user(),
    )

    assert response.success is True
    assert response.signature is not None
