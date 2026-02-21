"""RetrievalGate V2 tests."""

from __future__ import annotations

import pytest

from agentflow.context.retrieval_gate import RetrievalGate, RetrievalReason
from agentflow.evolution.types import RetrievalDecisionInput, RetrievalMode, StalenessRisk


@pytest.mark.asyncio
async def test_skip_for_high_confidence_low_complexity() -> None:
    gate = RetrievalGate()
    decision = await gate.should_retrieve(
        RetrievalDecisionInput(
            query="Summarize this short chat.",
            context={"existing_info": "short chat summary"},
            complexity=0.2,
            self_confidence=0.9,
            novelty=0.2,
            recent_failures=0,
            explicit_request=False,
            staleness_risk=StalenessRisk.LOW,
        )
    )

    assert decision.mode == RetrievalMode.SKIP
    assert decision.should_retrieve is False
    assert decision.reason in {RetrievalReason.SAFE_SKIP, RetrievalReason.SIMPLE_TASK}


@pytest.mark.asyncio
async def test_deep_for_low_confidence() -> None:
    gate = RetrievalGate()
    decision = await gate.should_retrieve(
        RetrievalDecisionInput(
            query="Need migration strategy for legacy module",
            complexity=0.6,
            self_confidence=0.4,
            novelty=0.5,
            recent_failures=0,
            explicit_request=False,
            staleness_risk=StalenessRisk.MEDIUM,
        )
    )

    assert decision.mode == RetrievalMode.DEEP
    assert decision.should_retrieve is True
    assert RetrievalReason.LOW_CONFIDENCE.value in decision.reason_codes


@pytest.mark.asyncio
async def test_deep_for_recent_failures() -> None:
    gate = RetrievalGate()
    decision = await gate.should_retrieve(
        RetrievalDecisionInput(
            query="Run deployment checks",
            complexity=0.4,
            self_confidence=0.8,
            novelty=0.4,
            recent_failures=3,
            explicit_request=False,
            staleness_risk=StalenessRisk.MEDIUM,
        )
    )

    assert decision.mode == RetrievalMode.DEEP
    assert RetrievalReason.RECENT_FAILURES.value in decision.reason_codes
