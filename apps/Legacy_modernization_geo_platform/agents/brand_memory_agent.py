"""BrandMemoryAgent - ブランド記憶アーティファクト生成."""

from __future__ import annotations

from typing import Any

from apps.Legacy_modernization_geo_platform.agents._models import (
    BrandMemoryInput,
    BrandMemoryOutput,
)
from apps.Legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    BrandMemoryArtifact,
)

from kernel.agents.resilient_agent import ResilientAgent


class BrandMemoryAgent(ResilientAgent[BrandMemoryInput, BrandMemoryOutput]):
    """ブランドポジショニングと内部制約を符号化する."""

    name = "BrandMemory"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(self, input_data: BrandMemoryInput) -> BrandMemoryOutput:
        request = input_data.request
        task_id = input_data.task_id
        stacks = request.targets.legacy_stacks or request.inputs.target_services
        regions = request.targets.regions or request.inputs.regions or ["Japan"]
        artifact = BrandMemoryArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:brand_memory",
                stage="brand_memory",
            ),
            positioning="AI検索時代に旧システム刷新の検討需要を捕捉し、診断から段階移行まで導く。",
            differentiators=[
                "COBOL/RPG/旧Java を横断した刷新提案",
                "Business semantics を前提にした段階移行",
                "AI検索向けの根拠付き情報資産を継続生成",
            ],
            supported_stacks=stacks or ["COBOL", "RPG", "Struts"],
            target_regions=regions,
        )
        return BrandMemoryOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> BrandMemoryInput:
        return BrandMemoryInput.model_validate(input_data)
