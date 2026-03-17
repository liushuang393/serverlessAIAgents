"""SupervisorAgent - GEO pipeline coordinator."""

from __future__ import annotations

from typing import Any

from apps.Legacy_modernization_geo_platform.agents._models import (
    SupervisorInput,
    SupervisorOutput,
)

from kernel.agents.resilient_agent import ResilientAgent


class SupervisorAgent(ResilientAgent[SupervisorInput, SupervisorOutput]):
    """実行計画と human gate を宣言する coordinator."""

    name = "Supervisor"
    timeout_seconds = 30
    max_retries = 1
    enable_code_execution = False

    async def process(self, input_data: SupervisorInput) -> SupervisorOutput:
        request = input_data.request
        plan = [
            "BrandMemory",
            "DemandSignal",
            "AccountScore",
            "QuestionGraph",
            "EvidenceMatrix",
            "LegacySemantics",
            "ContentBlueprint",
            "ContentDraft",
            "GeoQA",
            "Publishing",
            "ReportAssembler",
        ]
        approval_required = bool(request.constraints.human_approval_before_publish)
        return SupervisorOutput(plan=plan, approval_required=approval_required)

    def _parse_input(self, input_data: dict[str, Any]) -> SupervisorInput:
        return SupervisorInput.model_validate(input_data)
