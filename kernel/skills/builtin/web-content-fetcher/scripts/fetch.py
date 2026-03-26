"""web-content-fetcher 互換スクリプト."""

from __future__ import annotations

import asyncio
from concurrent.futures import ThreadPoolExecutor
from typing import Any

from contracts.web import WebIntent, WebIntentType, WebRouterInput
from kernel.web import WebIntelligenceRouter
from shared.web.markdown_pipeline import finalize_markdown


def _parse_max_chars(raw_value: Any) -> int:
    """max_chars を安全に整数化する."""
    if isinstance(raw_value, int | float):
        return max(1, int(raw_value))
    if isinstance(raw_value, str):
        try:
            return max(1, int(raw_value.strip()))
        except ValueError:
            return 30000
    return 30000


def _build_request(input_data: dict[str, Any] | str) -> tuple[WebRouterInput, int]:
    """互換入力を Router 契約へ変換する."""
    if isinstance(input_data, str):
        url = input_data.strip()
        max_chars = 30000
        allowed_domains: list[str] | None = None
        blocked_domains: list[str] | None = None
        request_id = "web-content-fetcher"
    else:
        url = str(input_data.get("url", "")).strip()
        max_chars = _parse_max_chars(input_data.get("max_chars", 30000))
        allowed_raw = input_data.get("allowed_domains")
        blocked_raw = input_data.get("blocked_domains")
        allowed_domains = allowed_raw if isinstance(allowed_raw, list) else None
        blocked_domains = blocked_raw if isinstance(blocked_raw, list) else None
        request_id = str(input_data.get("request_id", "web-content-fetcher"))

    request = WebRouterInput(
        request_id=request_id,
        intent=WebIntent(intent=WebIntentType.READ_URL, url=url),
        allowed_domains=allowed_domains,
        blocked_domains=blocked_domains,
    )
    return request, max_chars


def fetch(input_data: dict[str, Any] | str) -> dict[str, Any]:
    """SkillRuntime 用エントリポイント."""

    request, max_chars = _build_request(input_data)
    if not request.intent.url or not request.intent.url.strip():
        return {"error": "url is required", "markdown": "", "method": "none", "chars": 0}

    async def _inner() -> dict[str, Any]:
        router = WebIntelligenceRouter()
        result = await router.execute(request)
        evidence_markdown = ""
        if result.evidence and isinstance(result.evidence[0].markdown, str):
            evidence_markdown = result.evidence[0].markdown
        elif isinstance(result.answer_markdown, str):
            evidence_markdown = result.answer_markdown

        normalized = finalize_markdown(evidence_markdown, max_chars=max_chars)
        return {
            "markdown": normalized,
            "method": f"web-read:{result.mode_used.value}",
            "chars": len(normalized),
            "mode_used": result.mode_used.value,
            "citations": result.citations,
            "confidence": result.confidence,
            "fallback_used": result.fallback_used,
        }

    try:
        with ThreadPoolExecutor(max_workers=1) as executor:
            return executor.submit(lambda: asyncio.run(_inner())).result(timeout=120)
    except Exception as exc:  # pragma: no cover - 例外は上位へ返却
        return {"error": str(exc), "markdown": "", "method": "none", "chars": 0}
