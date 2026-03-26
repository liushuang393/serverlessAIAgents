"""Web 契約モデルのテスト."""

from __future__ import annotations

import pytest
from pydantic import ValidationError

from contracts.web import (
    BrowserActionStep,
    EstimatedCostLevel,
    EvidenceItem,
    QualityConstraints,
    WebIntent,
    WebIntentType,
    WebRetrievalMode,
    WebRetrievalRouter,
    WebRouterInput,
    WebRouterOutput,
)


def test_web_router_input_keeps_domain_policy() -> None:
    """Router 入力が allow/deny ドメインを保持すること."""
    model = WebRouterInput(
        request_id="req-1",
        intent=WebIntent(intent=WebIntentType.READ_URL, url="https://example.com"),
        constraints=QualityConstraints(),
        operation_steps=[BrowserActionStep(action="click", selector="#submit")],
        allowed_domains=["example.com"],
        blocked_domains=["blocked.com"],
    )

    assert model.allowed_domains == ["example.com"]
    assert model.blocked_domains == ["blocked.com"]
    assert len(model.operation_steps) == 1


def test_browser_action_step_validates_required_fields() -> None:
    """action ごとの必須項目不足を検出すること."""
    with pytest.raises(ValidationError):
        BrowserActionStep(action="click")

    with pytest.raises(ValidationError):
        BrowserActionStep(action="type", selector="#email")

    step = BrowserActionStep(action="wait", timeout_ms=1000)
    assert step.timeout_ms == 1000


def test_web_router_output_requires_observability_fields() -> None:
    """mode/citations/confidence/latency/cost/fallback が必須であること."""
    with pytest.raises(ValidationError):
        WebRouterOutput.model_validate(
            {
                "mode_used": WebRetrievalMode.DIRECT_MARKDOWN.value,
                "answer_markdown": "ok",
                "evidence": [],
            }
        )


def test_web_router_output_accepts_all_required_fields() -> None:
    """必須フィールドを渡した場合に妥当なモデルを生成できること."""
    output = WebRouterOutput(
        mode_used=WebRetrievalMode.SEARCH_THEN_FETCH,
        answer_markdown="summary",
        evidence=[
            EvidenceItem(
                url="https://example.com/post",
                snippet="snippet",
                confidence=0.7,
            )
        ],
        citations=["[1] Example - https://example.com/post"],
        latency_ms=123,
        estimated_cost_level=EstimatedCostLevel.MEDIUM,
        confidence=0.82,
        fallback_used=False,
        metadata={"provider": "test"},
    )

    assert output.mode_used is WebRetrievalMode.SEARCH_THEN_FETCH
    assert output.latency_ms == 123
    assert output.estimated_cost_level is EstimatedCostLevel.MEDIUM
    assert output.metadata["provider"] == "test"


def test_web_retrieval_mode_values_are_canonical() -> None:
    """mode enum が 7章準拠の canonical 値を持つこと."""
    values = {mode.value for mode in WebRetrievalMode}
    assert values == {
        "direct_markdown",
        "html_readability",
        "rendered_markdown",
        "search_then_fetch",
        "browser_mcp",
        "crawl_mode",
    }


def test_web_router_protocol_can_be_runtime_checked() -> None:
    """Router Protocol が runtime_checkable であること."""

    class _Router:
        async def execute(self, req: WebRouterInput) -> WebRouterOutput:
            _ = req
            return WebRouterOutput(
                mode_used=WebRetrievalMode.DIRECT_MARKDOWN,
                answer_markdown="ok",
                evidence=[],
                citations=[],
                latency_ms=0,
                estimated_cost_level=EstimatedCostLevel.LOW,
                confidence=0.0,
                fallback_used=False,
                metadata={},
            )

    router = _Router()
    assert isinstance(router, WebRetrievalRouter)
