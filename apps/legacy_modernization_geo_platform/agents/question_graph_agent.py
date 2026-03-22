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
    normalize_content_language,
)
from kernel.agents.resilient_agent import ResilientAgent


_QUESTION_GRAPH_COPY: dict[str, dict[str, list[str] | str]] = {
    "ja": {
        "cio_questions": [
            "{stack} を全面刷新せずに段階移行できるか",
            "投資対効果をどう説明するか",
        ],
        "cio_high_intent": "{industry} での {stack} 段階移行事例はあるか",
        "it_manager_questions": [
            "既存業務ルールを失わずに移行設計できるか",
            "保守要員不足にどう備えるか",
        ],
        "it_manager_high_intent": "{stack} から Java/Spring Boot への移行順序はどう決めるか",
        "engineering_lead_questions": [
            "影響分析とテスト生成をどこまで自動化できるか",
            "既存バッチとAPIを共存させられるか",
        ],
        "engineering_lead_high_intent": "段階移行中の品質ゲートをどう設計するか",
        "approval_questions": [
            "経営層に説明できる移行ロードマップはどう作るか",
            "一括刷新と段階移行のリスク差分は何か",
        ],
    },
    "en": {
        "cio_questions": [
            "Can we migrate {stack} in phases without a full replacement?",
            "How should we explain ROI to executives?",
        ],
        "cio_high_intent": "Are there phased {stack} migration cases in {industry}?",
        "it_manager_questions": [
            "Can we preserve business rules while redesigning migration architecture?",
            "How do we mitigate maintenance talent shortages?",
        ],
        "it_manager_high_intent": "How should we sequence migration from {stack} to Java/Spring Boot?",
        "engineering_lead_questions": [
            "How far can impact analysis and test generation be automated?",
            "Can legacy batch and APIs run in parallel during migration?",
        ],
        "engineering_lead_high_intent": "How should quality gates be designed during phased migration?",
        "approval_questions": [
            "How do we produce an executive-ready migration roadmap?",
            "What are the risk differences between full rewrite and phased migration?",
        ],
    },
    "zh": {
        "cio_questions": [
            "能否在不一次性替换的前提下分阶段迁移 {stack}？",
            "如何向管理层说明投资回报？",
        ],
        "cio_high_intent": "{industry} 是否有 {stack} 分阶段迁移案例？",
        "it_manager_questions": [
            "能否在保留业务规则的同时完成迁移设计？",
            "如何应对维护人才不足问题？",
        ],
        "it_manager_high_intent": "从 {stack} 到 Java/Spring Boot 的迁移顺序应如何规划？",
        "engineering_lead_questions": [
            "影响分析与测试生成可以自动化到什么程度？",
            "迁移期间能否让旧批处理与 API 并行运行？",
        ],
        "engineering_lead_high_intent": "分阶段迁移期间应如何设计质量门控？",
        "approval_questions": [
            "如何形成可向管理层汇报的迁移路线图？",
            "一次性重构与分阶段迁移的风险差异是什么？",
        ],
    },
}


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
        language = normalize_content_language((request.inputs.content_languages or ["ja"])[0])
        copy = _QUESTION_GRAPH_COPY[language]
        cio_questions = copy["cio_questions"] if isinstance(copy["cio_questions"], list) else []
        it_manager_questions = copy["it_manager_questions"] if isinstance(copy["it_manager_questions"], list) else []
        engineering_lead_questions = (
            copy["engineering_lead_questions"] if isinstance(copy["engineering_lead_questions"], list) else []
        )
        approval_questions = copy["approval_questions"] if isinstance(copy["approval_questions"], list) else []
        stack = (request.targets.legacy_stacks or ["COBOL"])[0]
        industry = (request.targets.industries or ["manufacturing"])[0]
        personas = [
            PersonaQuestionSet(
                role="cio",
                questions=[item.format(stack=stack, industry=industry) for item in cio_questions],
                high_intent_questions=[str(copy["cio_high_intent"]).format(stack=stack, industry=industry)],
            ),
            PersonaQuestionSet(
                role="it_manager",
                questions=[item.format(stack=stack, industry=industry) for item in it_manager_questions],
                high_intent_questions=[str(copy["it_manager_high_intent"]).format(stack=stack, industry=industry)],
            ),
            PersonaQuestionSet(
                role="engineering_lead",
                questions=[item.format(stack=stack, industry=industry) for item in engineering_lead_questions],
                high_intent_questions=[
                    str(copy["engineering_lead_high_intent"]).format(stack=stack, industry=industry),
                ],
            ),
        ]
        funnel_clusters = [
            FunnelCluster(
                stage="comparison",
                questions=[item for persona in personas for item in persona.high_intent_questions[:1]],
                recommended_page_type="comparison_page",
            ),
            FunnelCluster(
                stage="approval",
                questions=[str(item) for item in approval_questions],
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
