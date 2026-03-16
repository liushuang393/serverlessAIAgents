"""ReportAssemblerAgent - GEO report wrapper."""

from __future__ import annotations

from typing import Any

from apps.Legacy_modernization_geo_platform.agents._models import (
    ReportAssemblerInput,
    ReportAssemblerOutput,
)
from apps.Legacy_modernization_geo_platform.backend.reporting import build_campaign_report

from agentflow.core.resilient_agent import ResilientAgent


class ReportAssemblerAgent(
    ResilientAgent[ReportAssemblerInput, ReportAssemblerOutput]
):
    """レポート組み立てを executable agent として公開する."""

    name = "ReportAssembler"
    timeout_seconds = 30
    max_retries = 1
    enable_code_execution = False

    async def process(
        self,
        input_data: ReportAssemblerInput,
    ) -> ReportAssemblerOutput:
        artifact, markdown, summary = build_campaign_report(
            task_id=input_data.task_id,
            request=input_data.request,
            signal_artifact=input_data.signal_artifact,
            qa_report=input_data.qa_report,
            publish_manifest=input_data.publish_manifest,
            provider_status=input_data.provider_status,
        )
        return ReportAssemblerOutput(
            artifact=artifact,
            markdown=markdown,
            summary=summary,
        )

    def _parse_input(self, input_data: dict[str, Any]) -> ReportAssemblerInput:
        return ReportAssemblerInput.model_validate(input_data)
