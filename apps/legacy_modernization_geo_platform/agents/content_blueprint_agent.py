"""ContentBlueprintAgent - コンテンツ設計図生成."""

from __future__ import annotations

import re
from typing import Any

from apps.legacy_modernization_geo_platform.agents._models import (
    ContentBlueprintInput,
    ContentBlueprintOutput,
)
from apps.legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    ContentBlueprintArtifact,
    ContentBlueprintPage,
    normalize_content_language,
)
from kernel.agents.resilient_agent import ResilientAgent


def _slugify(value: str) -> str:
    """URL安全なスラグを生成."""
    lowered = value.strip().lower()
    normalized = re.sub(r"[^a-z0-9]+", "-", lowered)
    return normalized.strip("-") or "geo-page"


_BLUEPRINT_COPY: dict[str, dict[str, str]] = {
    "ja": {
        "default_industry": "製造業",
        "default_stack": "COBOL",
        "title_pattern": "{industry} 向け {stack} モダナイゼーションガイド",
        "cta": "無料診断を依頼する",
        "primary_question": "{industry} での {stack} 段階移行事例はあるか",
    },
    "en": {
        "default_industry": "Manufacturing",
        "default_stack": "COBOL",
        "title_pattern": "{industry} {stack} Modernization Guide",
        "cta": "Request a Free Assessment",
        "primary_question": "Are there phased {stack} migration cases in {industry}?",
    },
    "zh": {
        "default_industry": "制造业",
        "default_stack": "COBOL",
        "title_pattern": "{industry}{stack} 现代化指南",
        "cta": "申请免费诊断",
        "primary_question": "{industry} 是否有 {stack} 分阶段迁移案例？",
    },
}


class ContentBlueprintAgent(
    ResilientAgent[ContentBlueprintInput, ContentBlueprintOutput],
):
    """ページ設計図アーティファクトを生成."""

    name = "ContentBlueprint"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(self, input_data: ContentBlueprintInput) -> ContentBlueprintOutput:
        request = input_data.request
        task_id = input_data.task_id
        question_graph = input_data.question_graph
        target_language = normalize_content_language((request.inputs.content_languages or ["ja"])[0])
        copy = _BLUEPRINT_COPY[target_language]
        industry = _slugify((request.targets.industries or ["enterprise"])[0])
        stack = _slugify((request.targets.legacy_stacks or ["legacy"])[0])
        industry_label = (request.targets.industries or [copy["default_industry"]])[0]
        stack_label = (request.targets.legacy_stacks or [copy["default_stack"]])[0]
        primary_question = copy["primary_question"].format(
            industry=industry_label,
            stack=stack_label,
        )
        if target_language == "ja" and question_graph.personas and question_graph.personas[0].high_intent_questions:
            primary_question = question_graph.personas[0].high_intent_questions[0]
        page = ContentBlueprintPage(
            slug=f"{industry}-{stack}-modernization-guide",
            title=copy["title_pattern"].format(industry=industry_label, stack=stack_label),
            persona="it_manager",
            primary_question=primary_question,
            page_type="industry_lp",
            cta=copy["cta"],
        )
        artifact = ContentBlueprintArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:strategy",
                stage="strategy",
            ),
            pages=[page],
            target_language=target_language,
        )
        return ContentBlueprintOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> ContentBlueprintInput:
        return ContentBlueprintInput.model_validate(input_data)
