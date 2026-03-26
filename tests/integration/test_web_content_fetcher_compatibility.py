"""web-content-fetcher 互換ラッパーの回帰テスト."""

from __future__ import annotations

from pathlib import Path

import pytest

from contracts.web import EstimatedCostLevel, EvidenceItem, WebRetrievalMode, WebRouterOutput
from kernel.skills.base import Skill
from kernel.skills.core.runtime import SkillRuntime
from kernel.web import WebIntelligenceRouter


@pytest.mark.asyncio
async def test_web_content_fetcher_runtime_uses_router_pipeline(monkeypatch: pytest.MonkeyPatch) -> None:
    """runtime 実行時に互換スキルが Router 経由で Markdown を返す."""

    async def _fake_execute(self: WebIntelligenceRouter, _req: object) -> WebRouterOutput:
        _ = self
        return WebRouterOutput(
            mode_used=WebRetrievalMode.HTML_READABILITY,
            answer_markdown="",
            evidence=[
                EvidenceItem(
                    url="https://example.com/article",
                    markdown="# Title\n\n本文",
                    confidence=0.8,
                )
            ],
            citations=["[1] https://example.com/article"],
            latency_ms=12,
            estimated_cost_level=EstimatedCostLevel.LOW,
            confidence=0.8,
            fallback_used=False,
        )

    monkeypatch.setattr(WebIntelligenceRouter, "execute", _fake_execute)

    skill_path = Path(__file__).resolve().parents[2] / "kernel/skills/builtin/web-content-fetcher"
    skill = Skill.load(skill_path)
    runtime = SkillRuntime()

    result = await runtime.run(
        skill=skill,
        input_data={"url": "https://example.com/article", "max_chars": 2000},
    )

    assert result.success is True
    assert result.output.get("mode_used") == "html_readability"
    assert result.output.get("method") == "web-read:html_readability"
    assert isinstance(result.output.get("markdown"), str)
    assert result.output.get("chars", 0) > 0
