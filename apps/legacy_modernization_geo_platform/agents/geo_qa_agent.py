"""GeoQAAgent - GEO quality gate wrapper."""

from __future__ import annotations

from typing import Any

from apps.legacy_modernization_geo_platform.agents._models import GeoQAInput, GeoQAOutput
from apps.legacy_modernization_geo_platform.backend.qa import GeoQualityGate

from kernel.agents.resilient_agent import ResilientAgent


class GeoQAAgent(ResilientAgent[GeoQAInput, GeoQAOutput]):
    """GeoQualityGate を executable agent として公開する."""

    name = "GeoQA"
    timeout_seconds = 30
    max_retries = 1
    enable_code_execution = False

    def __init__(self, quality_gate: GeoQualityGate | None = None, **kwargs: Any) -> None:
        super().__init__(**kwargs)
        self._quality_gate = quality_gate or GeoQualityGate()

    async def process(self, input_data: GeoQAInput) -> GeoQAOutput:
        artifact = self._quality_gate.evaluate(
            task_id=input_data.task_id,
            draft=input_data.draft,
            evidence_matrix=input_data.evidence_matrix,
        )
        return GeoQAOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> GeoQAInput:
        return GeoQAInput.model_validate(input_data)
