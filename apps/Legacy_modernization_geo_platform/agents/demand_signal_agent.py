"""DemandSignalAgent - 需要シグナルアーティファクト生成."""

from __future__ import annotations

from typing import Any

from apps.Legacy_modernization_geo_platform.agents._models import (
    DemandSignalInput,
    DemandSignalOutput,
)
from apps.Legacy_modernization_geo_platform.backend.schemas import (
    AccountSignalArtifact,
    ArtifactMeta,
    SignalEntry,
)

from agentflow.core.resilient_agent import ResilientAgent


class DemandSignalAgent(ResilientAgent[DemandSignalInput, DemandSignalOutput]):
    """ライブインテリジェンスからアカウントシグナルを生成."""

    name = "DemandSignal"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(self, input_data: DemandSignalInput) -> DemandSignalOutput:
        request = input_data.request
        task_id = input_data.task_id
        intelligence_snapshot = input_data.intelligence_snapshot
        company = (
            request.inputs.target_accounts[0]
            if request.inputs.target_accounts
            else f"{(request.targets.industries or ['enterprise'])[0]} prospects"
        )
        signals = [
            SignalEntry(
                type="tech_stack",
                description=source.summary or source.snippet or source.title,
                source=f"{source.title} ({source.publisher})",
                confidence=0.78 if source.reliability == "HIGH" else 0.62,
            )
            for source in intelligence_snapshot.sources[:5]
        ]
        fit_score = min(
            96, 58 + len(signals) * 7 + len(request.targets.legacy_stacks) * 4
        )
        artifact = AccountSignalArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:demand_signal",
                stage="demand_signal",
            ),
            company=company,
            signals=signals,
            urgency_hypothesis="保守要員不足と技術負債圧縮の同時要請が強い",
            modernization_fit_score=fit_score,
            evidence=[
                item.to_evidence_dict()
                for item in intelligence_snapshot.sources[:5]
            ],
            unknowns=list(dict.fromkeys(intelligence_snapshot.warnings)),
            extensions={
                "primary_provider": intelligence_snapshot.primary_provider,
            },
        )
        return DemandSignalOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> DemandSignalInput:
        return DemandSignalInput.model_validate(input_data)
