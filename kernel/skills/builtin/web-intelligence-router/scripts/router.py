"""web-intelligence-router スクリプト."""

from __future__ import annotations

import asyncio
from concurrent.futures import ThreadPoolExecutor
from typing import Any

from contracts.web import WebRouterInput
from kernel.web import WebIntelligenceRouter


def _run_async_in_thread(payload: dict[str, Any]) -> dict[str, Any]:
    async def _inner() -> dict[str, Any]:
        router = WebIntelligenceRouter()
        request = WebRouterInput.model_validate(payload)
        result = await router.execute(request)
        return result.model_dump(mode="json")

    with ThreadPoolExecutor(max_workers=1) as executor:
        future = executor.submit(lambda: asyncio.run(_inner()))
        return future.result(timeout=120)


def router(input_data: dict[str, Any]) -> dict[str, Any]:
    """SkillRuntime エントリポイント."""
    try:
        return _run_async_in_thread(input_data)
    except Exception as exc:
        return {
            "mode_used": "direct_markdown",
            "answer_markdown": f"router execution failed: {exc}",
            "extracted_data": None,
            "evidence": [],
            "citations": [],
            "latency_ms": 0,
            "estimated_cost_level": "low",
            "confidence": 0.0,
            "fallback_used": False,
            "metadata": {"provider": "skill_runtime"},
        }
