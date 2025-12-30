# -*- coding: utf-8 -*-
"""DaoAgent - 本質判定Agent（道）.

問題の本質を抽出し、不可変制約と隠れた前提を明らかにする。
RAG使用禁止。
"""

import json
from typing import Any

from apps.decision_governance_engine.agents.base_agent import BaseDecisionAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    DaoInput,
    DaoOutput,
    ProblemType,
)


class DaoAgent(BaseDecisionAgent[DaoInput, DaoOutput]):
    """本質判定Agent.

    職責:
    - 問題タイプの分類
    - 一文での本質抽出
    - 不可変制約の特定
    - 隠れた前提の発見

    禁止事項:
    - 解決策の提示
    - 行動の推奨
    - 楽観的予測
    """

    name = "DaoAgent"
    max_tokens = 500
    temperature = 0.3  # 低め＝安定判断

    # RAG使用禁止
    USE_RAG = False

    # 問題タイプ判定キーワード
    PROBLEM_TYPE_KEYWORDS: dict[ProblemType, list[str]] = {
        ProblemType.RESOURCE_ALLOCATION: ["予算", "リソース", "配分", "投資", "人員"],
        ProblemType.TIMING_DECISION: ["いつ", "タイミング", "時期", "着手", "開始"],
        ProblemType.TRADE_OFF: ["どちら", "AとB", "選択", "両立", "トレードオフ"],
        ProblemType.RISK_ASSESSMENT: ["リスク", "危険", "懸念", "不確実"],
        ProblemType.STRATEGY_DIRECTION: ["戦略", "方針", "方向性", "ビジョン", "中長期"],
    }

    SYSTEM_PROMPT = """あなたはDaoAgent（道）です。
問題の本質を見抜き、構造化された分析を提供します。

【あなたの唯一の責任】
問題の「本質」を一文で表現し、不可変の制約と隠れた前提を明らかにすること。

【禁止事項】
- 解決策を提示してはいけません
- 行動を推奨してはいけません
- 楽観的な予測をしてはいけません
- これらの判断は後続のAgentに委ねてください

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "problem_type": "TRADE_OFF" | "RESOURCE_ALLOCATION" | "TIMING_DECISION" | "RISK_ASSESSMENT" | "STRATEGY_DIRECTION",
    "essence": "問題の本質を一文で（50字以内）",
    "immutable_constraints": ["変えられない制約1", "制約2", ...],
    "hidden_assumptions": ["暗黙の前提1", "前提2", ...]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> DaoInput:
        """入力をパース."""
        return DaoInput(**input_data)

    async def process(self, input_data: DaoInput) -> DaoOutput:
        """本質分析を実行."""
        question = input_data.question
        constraints = input_data.constraints

        # Step 1: 問題タイプを推定
        problem_type = self._infer_problem_type(question)

        # Step 2: LLMで詳細分析（LLMがある場合）
        if self._llm:
            return await self._analyze_with_llm(question, constraints, problem_type)

        # Step 3: LLMなしの場合はルールベース分析
        return self._analyze_rule_based(question, constraints, problem_type)

    def _infer_problem_type(self, question: str) -> ProblemType:
        """問題タイプを推定."""
        for ptype, keywords in self.PROBLEM_TYPE_KEYWORDS.items():
            if any(kw in question for kw in keywords):
                return ptype
        return ProblemType.STRATEGY_DIRECTION  # デフォルト

    async def _analyze_with_llm(
        self,
        question: str,
        constraints: list[str],
        inferred_type: ProblemType,
    ) -> DaoOutput:
        """LLMを使用した分析."""
        user_prompt = f"""【問題】
{question}

【現実制約】
{chr(10).join(f"- {c}" for c in constraints) if constraints else "特になし"}

【推定された問題タイプ】
{inferred_type.value}

上記の問題を分析し、JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        try:
            # JSON部分を抽出してパース
            json_match = response[response.find("{"):response.rfind("}") + 1]
            data = json.loads(json_match)
            return DaoOutput(
                problem_type=ProblemType(data.get("problem_type", inferred_type.value)),
                essence=data.get("essence", "")[:50],
                immutable_constraints=data.get("immutable_constraints", [])[:5],
                hidden_assumptions=data.get("hidden_assumptions", [])[:3],
            )
        except (json.JSONDecodeError, ValueError):
            # パース失敗時はルールベースにフォールバック
            return self._analyze_rule_based(question, constraints, inferred_type)

    def _analyze_rule_based(
        self,
        question: str,
        constraints: list[str],
        problem_type: ProblemType,
    ) -> DaoOutput:
        """ルールベース分析（LLMなし）."""
        # 本質の抽出（簡易版）
        essence = self._extract_essence(question, problem_type)

        # 不可変制約
        immutable = constraints[:5] if constraints else ["制約情報なし"]

        # 隠れた前提（一般的なもの）
        hidden = [
            "現状の市場環境が継続する",
            "チームの能力が維持される",
            "資金調達に大きな変化がない",
        ][:3]

        return DaoOutput(
            problem_type=problem_type,
            essence=essence[:50],
            immutable_constraints=immutable,
            hidden_assumptions=hidden,
        )

    def _extract_essence(self, question: str, problem_type: ProblemType) -> str:
        """本質を一文で抽出."""
        essence_templates = {
            ProblemType.TRADE_OFF: "複数の選択肢間の最適なバランス判断",
            ProblemType.RESOURCE_ALLOCATION: "限られたリソースの最適配分判断",
            ProblemType.TIMING_DECISION: "実行タイミングの最適化判断",
            ProblemType.RISK_ASSESSMENT: "リスクと機会のバランス評価",
            ProblemType.STRATEGY_DIRECTION: "中長期的な方向性の決定",
        }
        return essence_templates.get(problem_type, "意思決定が必要な課題")

