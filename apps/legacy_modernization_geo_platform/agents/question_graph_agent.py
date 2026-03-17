"""QuestionGraphAgent - バイヤー質問クラスタ生成."""

from __future__ import annotations

from typing import Any

from apps.legacy_modernization_geo_platform.agents._models import (
    QuestionGraphInput,
    QuestionGraphOutput,
)
from apps.legacy_modernization_geo_platform.backend.schemas import (
    ArtifactMeta,
    FunnelCluster,
    PersonaQuestionSet,
    QuestionGraphArtifact,
)

from kernel.agents.resilient_agent import ResilientAgent


class QuestionGraphAgent(ResilientAgent[QuestionGraphInput, QuestionGraphOutput]):
    """バイヤーペルソナ別の質問グラフを生成."""

    name = "QuestionGraph"
    timeout_seconds = 60
    max_retries = 1
    enable_code_execution = False

    async def process(self, input_data: QuestionGraphInput) -> QuestionGraphOutput:
        request = input_data.request
        task_id = input_data.task_id
        score_artifact = input_data.score_artifact
        stack = (request.targets.legacy_stacks or ["COBOL"])[0]
        industry = (request.targets.industries or ["manufacturing"])[0]
        personas = [
            PersonaQuestionSet(
                role="cio",
                questions=[
                    f"{stack} を全面刷新せずに段階移行できるか",
                    "投資対効果をどう説明するか",
                ],
                high_intent_questions=[
                    f"{industry} での {stack} 段階移行事例はあるか",
                ],
            ),
            PersonaQuestionSet(
                role="it_manager",
                questions=[
                    "既存業務ルールを失わずに移行設計できるか",
                    "保守要員不足にどう備えるか",
                ],
                high_intent_questions=[
                    f"{stack} から Java/Spring Boot への移行順序はどう決めるか",
                ],
            ),
            PersonaQuestionSet(
                role="engineering_lead",
                questions=[
                    "影響分析とテスト生成をどこまで自動化できるか",
                    "既存バッチとAPIを共存させられるか",
                ],
                high_intent_questions=[
                    "段階移行中の品質ゲートをどう設計するか",
                ],
            ),
        ]
        funnel_clusters = [
            FunnelCluster(
                stage="comparison",
                questions=[
                    item
                    for persona in personas
                    for item in persona.high_intent_questions[:1]
                ],
                recommended_page_type="comparison_page",
            ),
            FunnelCluster(
                stage="approval",
                questions=[
                    "経営層に説明できる移行ロードマップはどう作るか",
                    "一括刷新と段階移行のリスク差分は何か",
                ],
                recommended_page_type="executive_lp",
            ),
        ]
        artifact = QuestionGraphArtifact(
            meta=ArtifactMeta(
                task_id=task_id,
                trace_id=f"{task_id}:question_map",
                stage="question_map",
            ),
            personas=personas,
            funnel_clusters=funnel_clusters,
            content_clusters=[
                {
                    "theme": f"{industry} {stack} modernization",
                    "recommended_asset": "industry_lp",
                    "priority": score_artifact.account_scores[0].priority
                    if score_artifact.account_scores
                    else "medium",
                }
            ],
        )
        return QuestionGraphOutput(artifact=artifact)

    def _parse_input(self, input_data: dict[str, Any]) -> QuestionGraphInput:
        return QuestionGraphInput.model_validate(input_data)
