"""LegacySemanticsAgent - レガシーセマンティクスアーティファクト生成."""

from __future__ import annotations

from typing import Any

from apps.legacy_modernization_geo_platform.agents._models import (
    LegacySemanticsInput,
    LegacySemanticsOutput,
)
from apps.legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    LegacySemanticsArtifact,
)

from kernel.agents.resilient_agent import ResilientAgent


class LegacySemanticsAgent(
    ResilientAgent[LegacySemanticsInput, LegacySemanticsOutput],
):
    """業務プロセスのセマンティクスを構造化."""

    name = "LegacySemantics"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(
        self, input_data: LegacySemanticsInput
    ) -> LegacySemanticsOutput:
        request = input_data.request
        task_id = input_data.task_id
        brand_memory = input_data.brand_memory
        stack = (
            request.targets.legacy_stacks
            or brand_memory.supported_stacks
            or ["legacy"]
        )[0]
        artifact = LegacySemanticsArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:legacy_semantics",
                stage="legacy_semantics",
            ),
            business_processes=["受注処理", "請求管理", "在庫更新"],
            business_events=["バッチ締め処理", "帳票出力", "マスタ同期"],
            state_model={
                "source_stack": stack,
                "migration_style": "incremental",
                "target_runtime": "Java / Spring Boot",
            },
            business_rules=[
                "業務ルールをコード変換前に明文化する",
                "影響分析の単位はジョブ・画面・帳票で分割する",
                "品質ゲートを段階移行ごとに設ける",
            ],
        )
        return LegacySemanticsOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> LegacySemanticsInput:
        return LegacySemanticsInput.model_validate(input_data)
