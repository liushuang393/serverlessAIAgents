# -*- coding: utf-8 -*-
"""human_review ルーターの単体テスト."""

import pytest

from apps.decision_governance_engine.routers import human_review as human_review_router
from apps.decision_governance_engine.schemas.output_schemas import (
    DecisionReport,
    ExecutiveSummary,
)


def _build_report() -> DecisionReport:
    """再判定テスト用レポートを作成."""
    return DecisionReport(
        report_id="PROP-202602-TEST01",
        original_question="PoC 提案を進めるべきか",
        dao={
            "problem_type": "TRADE_OFF",
            "essence": "責任分担と承認経路が未定義",
            "immutable_constraints": [],
            "hidden_assumptions": [],
        },
        fa={"recommended_paths": [], "rejected_paths": [], "decision_criteria": []},
        shu={"phases": [], "first_action": "責任者を確定する", "dependencies": []},
        qi={
            "implementations": [],
            "tool_recommendations": [],
            "integration_points": [],
            "technical_debt_warnings": [],
        },
        review={
            "overall_verdict": "REVISE",
            "confidence_score": 0.75,
            "findings": [
                {
                    "severity": "WARNING",
                    "category": "RESPONSIBILITY_GAP",
                    "description": "RACI が未定義",
                    "affected_agent": "DaoAgent",
                    "suggested_revision": "責任分担を明記する",
                }
            ],
            "final_warnings": ["RACI を定義すること"],
        },
        executive_summary=ExecutiveSummary(
            one_line_decision="責任分担の明確化が必要",
            recommended_action="RACI を合意後に実行",
            key_risks=["責任の空白"],
            first_step="関係者会議を設定",
            estimated_impact="意思決定遅延を回避",
        ),
    )


@pytest.mark.asyncio
async def test_recheck_finding_resolved_updates_review(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """解消判定時に review が更新されること."""
    report = _build_report()

    async def _mock_get_report(report_id: str) -> DecisionReport:
        assert report_id == report.report_id
        return report

    async def _mock_evaluate(*, finding, confirmation_note):  # type: ignore[no-untyped-def]
        assert finding.description == "RACI が未定義"
        assert "RACI" in confirmation_note
        return True, []

    monkeypatch.setattr(human_review_router, "_get_report_from_db", _mock_get_report)
    monkeypatch.setattr(
        human_review_router,
        "_evaluate_resolution_with_ai",
        _mock_evaluate,
    )

    response = await human_review_router.recheck_finding(
        human_review_router.FindingRecheckRequest(
            report_id=report.report_id,
            finding_index=0,
            confirmation_note="RACI を定義し、承認者と実装責任者を明記しました。",
            acknowledged=True,
            reviewer_name="山田 太郎",
        )
    )

    assert response.success is True
    assert response.resolved is True
    assert response.updated_review is not None
    assert response.updated_review["overall_verdict"] == "PASS"
    assert response.updated_review["findings"] == []


@pytest.mark.asyncio
async def test_recheck_finding_unresolved_returns_issues(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """未解消判定時に不足点のみ返すこと."""
    report = _build_report()

    async def _mock_get_report(_report_id: str) -> DecisionReport:
        return report

    async def _mock_evaluate(*, finding, confirmation_note):  # type: ignore[no-untyped-def]
        _ = finding
        _ = confirmation_note
        return False, ["責任者の氏名と承認手順が不足しています。"]

    monkeypatch.setattr(human_review_router, "_get_report_from_db", _mock_get_report)
    monkeypatch.setattr(
        human_review_router,
        "_evaluate_resolution_with_ai",
        _mock_evaluate,
    )

    response = await human_review_router.recheck_finding(
        human_review_router.FindingRecheckRequest(
            report_id=report.report_id,
            finding_index=0,
            confirmation_note="対応済みです。",
            acknowledged=True,
            reviewer_name="山田 太郎",
        )
    )

    assert response.success is True
    assert response.resolved is False
    assert response.updated_review is None
    assert response.issues == ["責任者の氏名と承認手順が不足しています。"]
