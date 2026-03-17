"""AccountScoreAgent - アカウントスコアリング."""

from __future__ import annotations

from typing import Any

from apps.Legacy_modernization_geo_platform.agents._models import (
    AccountScoreInput,
    AccountScoreOutput,
)
from apps.Legacy_modernization_geo_platform.backend.schemas import (
    AccountScoreArtifact,
    AccountScoreEntry,
    ArtifactMeta,
)

from kernel.agents.resilient_agent import ResilientAgent


class AccountScoreAgent(ResilientAgent[AccountScoreInput, AccountScoreOutput]):
    """ICP スコアリングアーティファクトを生成."""

    name = "AccountScore"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(self, input_data: AccountScoreInput) -> AccountScoreOutput:
        request = input_data.request
        task_id = input_data.task_id
        signal_artifact = input_data.signal_artifact
        accounts = request.inputs.target_accounts or [signal_artifact.company]
        base_fit = signal_artifact.modernization_fit_score
        urgency_seed = 70 if "不足" in signal_artifact.urgency_hypothesis else 58
        scores = [
            AccountScoreEntry(
                company=company,
                fit_score=max(55, min(97, base_fit - index * 3)),
                urgency_score=max(
                    52,
                    min(
                        95,
                        urgency_seed
                        + len(request.targets.legacy_stacks) * 3
                        - index * 2,
                    ),
                ),
                priority="high" if base_fit - index * 3 >= 80 else "medium",
                rationale=[
                    signal_artifact.urgency_hypothesis,
                    f"Primary stacks: {', '.join(request.targets.legacy_stacks or ['legacy'])}",
                ],
            )
            for index, company in enumerate(accounts[:3])
        ]
        artifact = AccountScoreArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:icp_scoring",
                stage="icp_scoring",
            ),
            account_scores=scores,
            recommended_focus=request.targets.legacy_stacks
            or request.inputs.target_services
            or ["COBOL modernization"],
            evidence=[item.model_dump(mode="json") for item in scores],
        )
        return AccountScoreOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> AccountScoreInput:
        return AccountScoreInput.model_validate(input_data)
