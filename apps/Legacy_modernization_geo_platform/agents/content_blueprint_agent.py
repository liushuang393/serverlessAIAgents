"""ContentBlueprintAgent - コンテンツ設計図生成."""

from __future__ import annotations

import re
from typing import Any

from agentflow.core.resilient_agent import ResilientAgent

from apps.Legacy_modernization_geo_platform.agents._models import (
    ContentBlueprintInput,
    ContentBlueprintOutput,
)
from apps.Legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    ContentBlueprintArtifact,
    ContentBlueprintPage,
)


def _slugify(value: str) -> str:
    """URL安全なスラグを生成."""
    lowered = value.strip().lower()
    normalized = re.sub(r"[^a-z0-9]+", "-", lowered)
    return normalized.strip("-") or "geo-page"


class ContentBlueprintAgent(
    ResilientAgent[ContentBlueprintInput, ContentBlueprintOutput],
):
    """ページ設計図アーティファクトを生成."""

    name = "ContentBlueprint"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(
        self, input_data: ContentBlueprintInput
    ) -> ContentBlueprintOutput:
        request = input_data.request
        task_id = input_data.task_id
        question_graph = input_data.question_graph
        industry = _slugify((request.targets.industries or ["enterprise"])[0])
        stack = _slugify((request.targets.legacy_stacks or ["legacy"])[0])
        primary_question = question_graph.personas[0].high_intent_questions[0]
        page = ContentBlueprintPage(
            slug=f"{industry}-{stack}-modernization-guide",
            title=(
                f"{(request.targets.industries or ['製造業'])[0]} 向け "
                f"{(request.targets.legacy_stacks or ['COBOL'])[0]} モダナイゼーションガイド"
            ),
            persona="it_manager",
            primary_question=primary_question,
            page_type="industry_lp",
            cta="無料診断を依頼する",
        )
        artifact = ContentBlueprintArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:strategy",
                stage="strategy",
            ),
            pages=[page],
            target_language=(request.inputs.content_languages or ["ja"])[0],
        )
        return ContentBlueprintOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> ContentBlueprintInput:
        return ContentBlueprintInput.model_validate(input_data)
