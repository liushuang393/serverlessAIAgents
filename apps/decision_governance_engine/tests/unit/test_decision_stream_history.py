"""decision ストリーム履歴保存の回帰テスト."""

from __future__ import annotations

from typing import Any
from uuid import UUID

import pytest

from apps.decision_governance_engine.routers import decision as decision_router


class _DummyEngine:
    """flow.complete を返す最小 Engine モック."""

    async def run_stream(self, _inputs: dict[str, Any]):  # type: ignore[no-untyped-def]
        yield {"event_type": "flow.start", "data": {}}
        yield {
            "event_type": "flow.complete",
            "result": {
                "report_id": "PROP-TEST-STREAM",
                "dao": {},
                "fa": {"recommended_paths": []},
                "shu": {},
                "qi": {},
                "review": {
                    "overall_verdict": "PASS",
                    "confidence_score": 0.62,
                },
            },
        }


class _DummyRepository:
    """履歴保存呼び出しを記録するモック."""

    finalize_calls: list[dict[str, Any]] = []
    save_calls: list[dict[str, Any]] = []

    async def finalize(self, **kwargs: Any) -> bool:
        self.finalize_calls.append(kwargs)
        return False

    async def save(self, **kwargs: Any) -> object:
        self.save_calls.append(kwargs)
        return object()


@pytest.mark.asyncio
async def test_stream_saves_history_on_flow_complete_before_client_close(
    monkeypatch: pytest.MonkeyPatch,
) -> None:
    """flow.complete 受信時点で履歴保存され、早期切断でも失われないこと."""
    # グローバル状態をテスト前に初期化
    _DummyRepository.finalize_calls = []
    _DummyRepository.save_calls = []

    monkeypatch.setattr(decision_router, "ENABLE_HISTORY", True)
    monkeypatch.setattr(decision_router, "get_engine", lambda: _DummyEngine())
    monkeypatch.setattr(decision_router, "_cache_report_from_result", lambda *_a, **_k: None)

    import apps.decision_governance_engine.repositories as repo_pkg

    monkeypatch.setattr(repo_pkg, "DecisionRepository", _DummyRepository)

    response = await decision_router.process_decision_stream(
        question="意思決定ストリーム履歴保存テスト用の質問です。十分な長さを確保します。",
        budget=None,
        timeline_months=None,
        stakeholder_product_owner="",
        stakeholder_tech_lead="",
        stakeholder_business_owner="",
        stakeholder_legal_reviewer="",
        technical_constraints=[],
        regulatory_constraints=[],
        human_resources="",
        request_id=None,
        resume=False,
    )

    stream = response.body_iterator
    saw_flow_complete = False

    for _ in range(10):
        chunk = await anext(stream)
        text = chunk.decode("utf-8") if isinstance(chunk, bytes) else str(chunk)
        if "flow.complete" in text:
            saw_flow_complete = True
            break

    # クライアントが flow.complete 直後に切断するケースを再現
    await stream.aclose()

    assert saw_flow_complete is True
    assert len(_DummyRepository.finalize_calls) == 1
    assert len(_DummyRepository.save_calls) == 1

    save_payload = _DummyRepository.save_calls[0]
    assert isinstance(save_payload.get("request_id"), UUID)
    assert save_payload.get("report_case_id") == "PROP-TEST-STREAM"

    results = save_payload.get("results")
    assert isinstance(results, dict)
    assert results.get("review", {}).get("overall_verdict") == "PASS"
