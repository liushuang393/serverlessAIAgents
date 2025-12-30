# -*- coding: utf-8 -*-
"""FaAgent - 戦略選定Agent（法）.

戦略パスを評価し、推奨と不推奨を明確に区別する。
RAG使用禁止。最大2つの推奨パスのみ。
"""

import json
from typing import Any

from apps.decision_governance_engine.agents.base_agent import BaseDecisionAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    DaoOutput,
    FaInput,
    FaOutput,
    PathOption,
    ProblemType,
)


class FaAgent(BaseDecisionAgent[FaInput, FaOutput]):
    """戦略選定Agent.

    職責:
    - 戦略オプションの評価
    - 推奨パス（1-2個）の選定
    - 不推奨パスの明示
    - 判断基準の提示

    制約:
    - 推奨パスは最大2個
    - 不推奨を必ず含める
    """

    name = "FaAgent"
    max_tokens = 800
    temperature = 0.4

    # RAG使用禁止
    USE_RAG = False
    MAX_RECOMMENDED_PATHS = 2
    MUST_INCLUDE_REJECTED = True

    SYSTEM_PROMPT = """あなたはFaAgent（法）です。
戦略的パスを評価し、最適な選択肢を提示します。

【あなたの唯一の責任】
問題に対する戦略オプションを評価し、推奨と不推奨を明確に区別すること。

【制約】
- 推奨パスは最大2個まで
- 必ず1つ以上の不推奨パスを明示すること
- 各パスにはメリット・デメリット・成功確率を含めること

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "recommended_paths": [
        {
            "path_id": "A",
            "name": "パス名（10字以内）",
            "description": "説明（100字以内）",
            "pros": ["メリット1", "メリット2"],
            "cons": ["デメリット1"],
            "success_probability": 0.7
        }
    ],
    "rejected_paths": [...],
    "decision_criteria": ["判断基準1", "判断基準2"]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> FaInput:
        """入力をパース."""
        # DaoOutputがネストされている場合の処理
        if "dao_result" in input_data and isinstance(input_data["dao_result"], dict):
            input_data["dao_result"] = DaoOutput(**input_data["dao_result"])
        return FaInput(**input_data)

    async def process(self, input_data: FaInput) -> FaOutput:
        """戦略選定を実行."""
        dao_result = input_data.dao_result

        # LLMで分析（ある場合）
        if self._llm:
            return await self._analyze_with_llm(dao_result, input_data)

        # ルールベース分析
        return self._analyze_rule_based(dao_result, input_data)

    async def _analyze_with_llm(self, dao_result: DaoOutput, input_data: FaInput) -> FaOutput:
        """LLMを使用した分析."""
        user_prompt = f"""【問題タイプ】{dao_result.problem_type.value}
【本質】{dao_result.essence}
【不可変制約】{', '.join(dao_result.immutable_constraints)}
【時間軸】{input_data.time_horizon or "未指定"}

上記に基づき、戦略パスを評価してJSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        try:
            json_match = response[response.find("{"):response.rfind("}") + 1]
            data = json.loads(json_match)

            recommended = [PathOption(**p) for p in data.get("recommended_paths", [])][:2]
            rejected = [PathOption(**p) for p in data.get("rejected_paths", [])]

            return FaOutput(
                recommended_paths=recommended,
                rejected_paths=rejected,
                decision_criteria=data.get("decision_criteria", []),
            )
        except json.JSONDecodeError:
            return self._analyze_rule_based(dao_result, input_data)

    def _analyze_rule_based(self, dao_result: DaoOutput, input_data: FaInput) -> FaOutput:
        """ルールベース分析."""
        # 問題タイプに応じたデフォルトパス生成
        recommended, rejected = self._generate_default_paths(dao_result.problem_type)

        criteria = self._generate_criteria(dao_result)

        return FaOutput(
            recommended_paths=recommended,
            rejected_paths=rejected,
            decision_criteria=criteria,
        )

    def _generate_default_paths(
        self,
        problem_type: ProblemType,
    ) -> tuple[list[PathOption], list[PathOption]]:
        """デフォルトパスを生成."""
        # 推奨パス
        recommended = [
            PathOption(
                path_id="A",
                name="推奨案A",
                description="段階的アプローチで低リスクに進める選択肢",
                pros=["リスク低減", "柔軟性確保", "学習機会"],
                cons=["時間がかかる"],
                success_probability=0.7,
            ),
        ]

        # 不推奨パス
        rejected = [
            PathOption(
                path_id="X",
                name="現状維持",
                description="何も変えない選択肢",
                pros=["即時コストなし"],
                cons=["競争力低下", "機会損失", "長期リスク増大"],
                success_probability=0.3,
            ),
        ]

        return recommended, rejected

    def _generate_criteria(self, dao_result: DaoOutput) -> list[str]:
        """判断基準を生成."""
        return [
            "不可変制約との整合性",
            "リスク対リターンのバランス",
            "実行可能性",
            "時間軸との適合",
        ]

