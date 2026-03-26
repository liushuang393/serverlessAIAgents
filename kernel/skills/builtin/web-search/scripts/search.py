"""web-search スクリプト."""

from __future__ import annotations

import asyncio
from concurrent.futures import ThreadPoolExecutor
from typing import Any

from contracts.web import WebIntent, WebIntentType, WebRouterInput
from kernel.web import WebIntelligenceRouter


def _build_request(input_data: dict[str, Any]) -> WebRouterInput:
    return WebRouterInput(
        request_id=str(input_data.get("request_id", "web-search")),
        intent=WebIntent(intent=WebIntentType.SEARCH, query=str(input_data.get("query", ""))),
        allowed_domains=input_data.get("allowed_domains"),
        blocked_domains=input_data.get("blocked_domains"),
    )


def search(input_data: dict[str, Any]) -> dict[str, Any]:
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
            "mode_used": "search_then_fetch",
            "answer_markdown": None,
            "extracted_data": None,
            "evidence": [],
            "citations": [],
            "latency_ms": 0,
            "estimated_cost_level": "medium",
            "confidence": 0.0,
            "fallback_used": False,
            "metadata": {"provider": "skill_runtime"},
        }
