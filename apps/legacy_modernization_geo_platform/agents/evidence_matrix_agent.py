"""EvidenceMatrixAgent - エビデンスマトリクス生成."""

from __future__ import annotations

from typing import Any

from apps.legacy_modernization_geo_platform.agents._models import (
    EvidenceMatrixInput,
    EvidenceMatrixOutput,
)
from apps.legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    EvidenceMatrixArtifact,
    EvidenceMatrixEntry,
)

from kernel.agents.resilient_agent import ResilientAgent


class EvidenceMatrixAgent(
    ResilientAgent[EvidenceMatrixInput, EvidenceMatrixOutput],
):
    """ライブソースからエビデンス行を生成."""

    name = "EvidenceMatrix"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(self, input_data: EvidenceMatrixInput) -> EvidenceMatrixOutput:
        task_id = input_data.task_id
        question_graph = input_data.question_graph
        intelligence_snapshot = input_data.intelligence_snapshot
        questions = [
            question
            for persona in question_graph.personas
            for question in [*persona.high_intent_questions, *persona.questions]
        ]
        entries: list[EvidenceMatrixEntry] = []
        for index, question in enumerate(questions[:6]):
            if not intelligence_snapshot.sources:
                break
            source = intelligence_snapshot.sources[
                index % len(intelligence_snapshot.sources)
            ]
            entries.append(
                EvidenceMatrixEntry(
                    claim=f"{question} に対して、段階移行と業務ルール保持の両立が重要である。",
                    question_ref=question,
                    source_url=source.url,
                    title=source.title,
                    publisher=source.publisher,
                    summary=source.summary,
                    snippet=source.snippet,
                    reliability=source.reliability,
                    citation_ready=source.citation_ready,
                    fresh=source.is_fresh,
                ),
            )
        artifact = EvidenceMatrixArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:evidence",
                stage="evidence_collection",
            ),
            entries=entries,
            provider_status={
                "primary_provider": intelligence_snapshot.primary_provider,
                "warnings": intelligence_snapshot.warnings,
                "source_count": len(entries),
            },
            evidence=[item.model_dump(mode="json") for item in entries],
            unknowns=list(dict.fromkeys(intelligence_snapshot.warnings)),
        )
        return EvidenceMatrixOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> EvidenceMatrixInput:
        return EvidenceMatrixInput.model_validate(input_data)
