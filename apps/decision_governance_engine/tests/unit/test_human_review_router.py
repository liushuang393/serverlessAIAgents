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
                    "score_improvements": [
                        {
                            "target_score": "input_sufficiency",
                            "current_estimate": 30,
                            "improved_estimate": 37,
                            "delta": 7,
                        }
                    ],
                }
            ],
            "final_warnings": ["RACI を定義すること"],
            "checkpoint_items": [
                {
                    "item_id": "approver_confirmed",
                    "label": "承認者（ロール）確認済み",
                    "checked": False,
                    "annotation": "",
                    "score_boost": 13.0,
                    "target_component": "input_sufficiency",
                    "default_suggestion": "暫定: プロジェクトオーナー（PO）が承認責任者",
                }
            ],
            "auto_recalc_enabled": True,
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

    async def _mock_persist(**_: object) -> None:
        return None

    monkeypatch.setattr(human_review_router, "_get_report_from_db", _mock_get_report)
    monkeypatch.setattr(
        human_review_router,
        "_evaluate_resolution_with_ai",
        _mock_evaluate,
    )
    monkeypatch.setattr(
        human_review_router,
        "_persist_human_review_record",
        _mock_persist,
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

    async def _mock_persist(**_: object) -> None:
        return None

    monkeypatch.setattr(human_review_router, "_get_report_from_db", _mock_get_report)
    monkeypatch.setattr(
        human_review_router,
        "_evaluate_resolution_with_ai",
        _mock_evaluate,
    )
    monkeypatch.setattr(
        human_review_router,
        "_persist_human_review_record",
        _mock_persist,
    )

    response = await human_review_router.recheck_finding(
        human_review_router.FindingRecheckRequest(
            report_id=report.report_id,
            finding_index=0,
            confirmation_note="対応済みですが、責任者と承認手順の記載はこれから追記します。",
            acknowledged=True,
            reviewer_name="山田 太郎",
        )
    )

    assert response.success is True
    assert response.resolved is False
    assert response.updated_review is None
    assert response.issues == ["責任者の氏名と承認手順が不足しています。"]


@pytest.mark.asyncio
async def test_apply_checkpoints_with_fixed_boost_only(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """checkbox固定加点のみで 40% に到達できること."""
    report = _build_report()
    report.review["confidence_score"] = 0.20

    persisted: dict[str, object] = {}

    async def _mock_get_report(_report_id: str) -> DecisionReport:
        return report

    async def _mock_bonus(_checked_items: list[dict[str, object]]) -> tuple[float, list[str]]:
        return 0.0, []

    async def _mock_persist(**kwargs: object) -> None:
        persisted.update(kwargs)

    monkeypatch.setattr(human_review_router, "_get_report_from_db", _mock_get_report)
    monkeypatch.setattr(
        human_review_router,
        "_evaluate_finding_bonus_with_ai",
        _mock_bonus,
    )
    monkeypatch.setattr(
        human_review_router,
        "_persist_human_review_record",
        _mock_persist,
    )

    response = await human_review_router.apply_checkpoints(
        human_review_router.CheckpointApplyRequest(
            report_id=report.report_id,
            request_id="00000000-0000-0000-0000-000000000001",
            reviewer_name="山田 太郎",
            items=[
                human_review_router.CheckpointApplyItem(
                    item_id="approver_confirmed",
                    checked=True,
                    annotation="PO 承認",
                )
            ],
            finding_confirmations=[
                human_review_router.FindingConfirmationItem(
                    finding_index=0,
                    checked=True,
                )
            ],
        )
    )

    assert response.success is True
    assert response.base_confidence_pct == 20
    assert response.checkpoint_boost_pct == 13
    assert response.finding_boost_pct == 7
    assert response.llm_bonus_pct == 0
    assert response.recalculated_confidence_pct == 40
    assert response.signature_eligible is True
    assert response.updated_review["overall_verdict"] == "PASS"
    assert persisted.get("updated_review") is not None


@pytest.mark.asyncio
async def test_apply_checkpoints_with_llm_bonus(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """checkbox加点に加えて LLM 補足加点が反映されること."""
    report = _build_report()
    report.review["confidence_score"] = 0.25

    async def _mock_get_report(_report_id: str) -> DecisionReport:
        return report

    async def _mock_bonus(_checked_items: list[dict[str, object]]) -> tuple[float, list[str]]:
        return 5.0, ["責任者・期限・検証基準が明確です。"]

    async def _mock_persist(**_: object) -> None:
        return None

    monkeypatch.setattr(human_review_router, "_get_report_from_db", _mock_get_report)
    monkeypatch.setattr(
        human_review_router,
        "_evaluate_finding_bonus_with_ai",
        _mock_bonus,
    )
    monkeypatch.setattr(
        human_review_router,
        "_persist_human_review_record",
        _mock_persist,
    )

    response = await human_review_router.apply_checkpoints(
        human_review_router.CheckpointApplyRequest(
            report_id=report.report_id,
            reviewer_name="山田 太郎",
            items=[
                human_review_router.CheckpointApplyItem(
                    item_id="approver_confirmed",
                    checked=True,
                )
            ],
            finding_confirmations=[
                human_review_router.FindingConfirmationItem(
                    finding_index=0,
                    checked=True,
                    note="POを責任者に指定。3/31までにRACIを確定し、承認ログで検証する。",
                )
            ],
        )
    )

    assert response.success is True
    assert response.base_confidence_pct == 25
    assert response.checkpoint_boost_pct == 13
    assert response.finding_boost_pct == 7
    assert response.llm_bonus_pct == 5
    assert response.recalculated_confidence_pct == 50
    assert response.bonus_reasons == ["責任者・期限・検証基準が明確です。"]
    assert response.signature_eligible is True


@pytest.mark.asyncio
async def test_apply_checkpoints_low_score_stays_unsigned(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """再計算後も 40% 未満なら署名不可のままであること."""
    report = _build_report()
    report.review["overall_verdict"] = "COACH"
    report.review["confidence_score"] = 0.30

    async def _mock_get_report(_report_id: str) -> DecisionReport:
        return report

    async def _mock_bonus(_checked_items: list[dict[str, object]]) -> tuple[float, list[str]]:
        return 0.0, ["補足内容が抽象的なため追加加点なし。"]

    async def _mock_persist(**_: object) -> None:
        return None

    monkeypatch.setattr(human_review_router, "_get_report_from_db", _mock_get_report)
    monkeypatch.setattr(
        human_review_router,
        "_evaluate_finding_bonus_with_ai",
        _mock_bonus,
    )
    monkeypatch.setattr(
        human_review_router,
        "_persist_human_review_record",
        _mock_persist,
    )

    response = await human_review_router.apply_checkpoints(
        human_review_router.CheckpointApplyRequest(
            report_id=report.report_id,
            items=[],
            finding_confirmations=[
                human_review_router.FindingConfirmationItem(
                    finding_index=0,
                    checked=True,
                    note="対応予定です。",
                )
            ],
        )
    )

    assert response.success is True
    assert response.base_confidence_pct == 30
    assert response.recalculated_confidence_pct == 37
    assert response.signature_eligible is False
    assert response.updated_review["overall_verdict"] == "COACH"
    assert response.llm_bonus_pct == 0


@pytest.mark.asyncio
async def test_apply_checkpoints_persists_bonus_reasons(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """監査ログに bonus 理由が保存されること."""
    report = _build_report()
    report.review["confidence_score"] = 0.28

    persisted: dict[str, object] = {}

    async def _mock_get_report(_report_id: str) -> DecisionReport:
        return report

    async def _mock_bonus(_checked_items: list[dict[str, object]]) -> tuple[float, list[str]]:
        return 4.0, ["責任者と期限が具体化されています。"]

    async def _mock_persist(**kwargs: object) -> None:
        persisted.update(kwargs)

    monkeypatch.setattr(human_review_router, "_get_report_from_db", _mock_get_report)
    monkeypatch.setattr(
        human_review_router,
        "_evaluate_finding_bonus_with_ai",
        _mock_bonus,
    )
    monkeypatch.setattr(
        human_review_router,
        "_persist_human_review_record",
        _mock_persist,
    )

    await human_review_router.apply_checkpoints(
        human_review_router.CheckpointApplyRequest(
            report_id=report.report_id,
            request_id="00000000-0000-0000-0000-000000000002",
            reviewer_name="山田 太郎",
            items=[
                human_review_router.CheckpointApplyItem(
                    item_id="approver_confirmed",
                    checked=True,
                    annotation="承認責任者: PO",
                )
            ],
            finding_confirmations=[
                human_review_router.FindingConfirmationItem(
                    finding_index=0,
                    checked=True,
                    note="POが責任者。3月末までにRACIを確定し、承認ログで検証。",
                )
            ],
        )
    )

    review_record = persisted.get("review_record")
    assert isinstance(review_record, dict)
    assert review_record.get("llm_bonus_pct") == 4
    assert review_record.get("bonus_reasons") == ["責任者と期限が具体化されています。"]


@pytest.mark.asyncio
async def test_persist_human_review_record_self_heals_when_record_missing(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """対象レコードが未作成でも骨組み作成後に人間確認ログを保存できること."""

    class _FakeRepo:
        append_calls: int = 0
        upsert_calls: list[dict[str, object]] = []

        async def append_human_review_record(  # type: ignore[no-untyped-def]
            self,
            *,
            request_id,
            review_record,
            updated_review=None,
        ) -> bool:
            _ = request_id
            _ = review_record
            _ = updated_review
            self.append_calls += 1
            # 初回は未作成を返し、2回目で成功させる
            return self.append_calls >= 2

        async def upsert_stage(  # type: ignore[no-untyped-def]
            self,
            *,
            request_id,
            question,
            stage_name,
            stage_result,
        ):
            self.upsert_calls.append(
                {
                    "request_id": request_id,
                    "question": question,
                    "stage_name": stage_name,
                    "stage_result": stage_result,
                }
            )
            return object()

    fake_repo = _FakeRepo()
    monkeypatch.setattr(human_review_router, "DecisionRepository", lambda: fake_repo)

    await human_review_router._persist_human_review_record(
        request_id_text="34fe4cac-81cd-4d77-96cf-290493734779",
        report_id_text="PROP-202603-9345D6",
        review_record={"event_type": "finding_note", "memo": "test"},
        updated_review={"overall_verdict": "PASS"},
    )

    assert fake_repo.append_calls == 2
    assert len(fake_repo.upsert_calls) == 1
    upsert_payload = fake_repo.upsert_calls[0]
    assert upsert_payload["stage_name"] == "review"
    assert upsert_payload["question"] == "[human-review] report_id=PROP-202603-9345D6"
