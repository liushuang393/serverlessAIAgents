"""web-operate スクリプト."""

from __future__ import annotations

import asyncio
from concurrent.futures import ThreadPoolExecutor
from typing import Any

from contracts.web import BrowserActionStep, QualityConstraints, WebIntent, WebIntentType, WebRouterInput
from kernel.web import WebIntelligenceRouter


def _build_steps(raw_steps: Any) -> list[BrowserActionStep]:
    if not isinstance(raw_steps, list):
        return []
    steps: list[BrowserActionStep] = []
    for raw_step in raw_steps:
        if not isinstance(raw_step, dict):
            continue
        steps.append(BrowserActionStep.model_validate(raw_step))
    return steps


def _build_request(input_data: dict[str, Any]) -> WebRouterInput:
    steps = _build_steps(input_data.get("steps"))
    if not steps:
        msg = "web-operate requires non-empty steps"
        raise ValueError(msg)
    return WebRouterInput(
        request_id=str(input_data.get("request_id", "web-operate")),
        intent=WebIntent(intent=WebIntentType.OPERATE, url=str(input_data.get("url", ""))),
        constraints=QualityConstraints(
            auth_required=bool(input_data.get("auth_required", False)),
            interaction_required=True,
        ),
        operation_steps=steps,
        allowed_domains=input_data.get("allowed_domains"),
        blocked_domains=input_data.get("blocked_domains"),
    )


def operate(input_data: dict[str, Any]) -> dict[str, Any]:
    """SkillRuntime エントリポイント."""

    async def _inner() -> dict[str, Any]:
        router = WebIntelligenceRouter()
        request = _build_request(input_data)
        result = await router.execute(request)
        return result.model_dump(mode="json")

    try:
        with ThreadPoolExecutor(max_workers=1) as executor:
            return executor.submit(lambda: asyncio.run(_inner())).result(timeout=120)
    except Exception as exc:
        return {
            "error": str(exc),
            "mode_used": "browser_mcp",
            "answer_markdown": None,
            "extracted_data": None,
            "evidence": [],
            "citations": [],
            "latency_ms": 0,
            "estimated_cost_level": "high",
            "confidence": 0.0,
            "fallback_used": False,
            "metadata": {"provider": "skill_runtime"},
        }
