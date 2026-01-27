# -*- coding: utf-8 -*-
"""CognitiveGateAgent - 認知前処理Agent.

道・法・術・器の分析に入る前に、「認知的前提条件」を検査する。
評価対象・動機・評価軸・不可逆性が明確でない場合は分析を進めない。

【認知前処理の核心】
「何を判断するか」「なぜ判断が必要か」「どう評価するか」が
明確でない限り、どんな分析も無意味である。
"""

import json
import logging
from typing import Any

from agentflow import ResilientAgent
from agentflow.core.exceptions import AgentOutputValidationError
from apps.decision_governance_engine.prompts import build_full_prompt
from apps.decision_governance_engine.schemas.agent_schemas import (
    ClarificationOutput,
    CognitiveGateInput,
    CognitiveGateOutput,
    Irreversibility,
    IrreversibilityLevel,
)


class CognitiveGateAgent(ResilientAgent[CognitiveGateInput, CognitiveGateOutput]):
    """認知前処理Agent.

    職責:
    - 評価対象（Object）の特定
    - 判断目的・動機（Intent）の特定
    - 評価軸（Criteria）の列挙
    - 不可逆性（Irreversibility）の確認
    - 進行可否の判断

    【判断ルール】
    - いずれかが未定義・曖昧 → 進行不可（proceed=False）
    - 全て明確 → 進行可（proceed=True）
    """

    name = "CognitiveGateAgent"
    # timeout_seconds, max_retries, max_tokens は ResilientAgent のデフォルト値を使用
    temperature = 0.3  # 低め（厳密な判断）

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    def _get_system_prompt(self) -> str:
        """システムプロンプトを取得."""
        try:
            return build_full_prompt("cognitive_gate")
        except FileNotFoundError:
            self._logger.warning("Prompt file not found, using fallback")
            return self._FALLBACK_PROMPT

    _FALLBACK_PROMPT = """あなたはCognitiveGateAgent（認知前処理）です。
道・法・術・器の分析に進む前に、認知的前提条件を検査します。

診断対象:
1. 評価対象（何を判断するか）
2. 動機（なぜ判断が必要か）
3. 評価軸（どう評価するか）
4. 不可逆性（取り消し可能か）

全て明確な場合のみ proceed=true を返してください。
JSON形式で出力してください。"""

    def _parse_input(self, input_data: dict[str, Any]) -> CognitiveGateInput:
        """入力をパース."""
        if "clarification_result" in input_data and isinstance(
            input_data["clarification_result"], dict
        ):
            input_data["clarification_result"] = ClarificationOutput(
                **input_data["clarification_result"]
            )
        return CognitiveGateInput(**input_data)

    async def process(self, input_data: CognitiveGateInput) -> CognitiveGateOutput:
        """認知前処理を実行."""
        if self._llm:
            return await self._analyze_with_llm(input_data)
        return self._analyze_rule_based(input_data)

    async def _analyze_with_llm(
        self, input_data: CognitiveGateInput
    ) -> CognitiveGateOutput:
        """LLMを使用した分析."""
        # Clarification結果があれば活用
        clarification_info = ""
        if input_data.clarification_result:
            cr = input_data.clarification_result
            clarification_info = f"""
【診断情報】
- 精緻化質問: {cr.refined_question}
- 暗黙の仮定: {[a.assumption for a in cr.hidden_assumptions]}
- 認知バイアス: {[b.bias for b in cr.cognitive_biases]}"""

        user_prompt = f"""【質問】{input_data.raw_question}
【制約条件】{', '.join(input_data.constraints) if input_data.constraints else "なし"}
{clarification_info}

上記について、認知的前提条件を診断してください。
JSON形式で出力してください。"""

        system_prompt = self._get_system_prompt()
        response = await self._call_llm(f"{system_prompt}\n\n{user_prompt}")

        # 詳細ログ: LLM生出力を記録（デバッグ用）
        self._logger.debug(f"LLM raw response (first 500 chars): {response[:500] if response else 'EMPTY'}")

        try:
            # JSON部分を抽出してパース（堅牢な抽出）
            from agentflow.utils import extract_json
            data = extract_json(response)

            if data is None:
                self._logger.error(f"JSON extraction failed. Raw response: {response[:1000]}")
                raise json.JSONDecodeError("No valid JSON found", response, 0)

            # 詳細ログ: 抽出されたJSON
            self._logger.debug(f"Extracted JSON: {data}")

            # 業務ロジック検証（Pydantic検証前）- 失敗時はリトライをトリガー
            self._validate_llm_output_fields(data)

            return self._parse_llm_output(data)

        except json.JSONDecodeError as e:
            self._logger.warning(f"LLM response parse failed: {e}")
            # JSONパース失敗 → ルールベースにフォールバック
            return self._analyze_rule_based(input_data)

    def _validate_llm_output_fields(self, data: dict[str, Any]) -> None:
        """LLM出力の業務ロジック検証（Pydantic検証前）.

        重要なフィールドが空の場合はAgentOutputValidationErrorを発生させ、
        リトライをトリガーする。

        Args:
            data: 抽出されたJSONデータ

        Raises:
            AgentOutputValidationError: 必須フィールドが空の場合
        """
        # criteria の検証（min_length=1）
        criteria = data.get("criteria")
        if not criteria or not isinstance(criteria, list) or len(criteria) == 0:
            self._logger.warning(f"LLM returned empty criteria. Full data: {data}")
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="criteria",
                expected="list with at least 1 item",
                actual=f"empty or invalid: {criteria}",
            )

        # 空文字列のみの criteria もNG
        valid_criteria = [c for c in criteria if c and str(c).strip()]
        if len(valid_criteria) == 0:
            self._logger.warning(f"LLM returned criteria with only empty strings: {criteria}")
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="criteria",
                expected="list with at least 1 non-empty item",
                actual=f"all items empty: {criteria}",
            )

        # evaluation_object の検証
        eval_obj = data.get("evaluation_object")
        if not eval_obj or not isinstance(eval_obj, str) or not eval_obj.strip():
            self._logger.warning(f"LLM returned empty evaluation_object. Full data: {data}")
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="evaluation_object",
                expected="non-empty string",
                actual=f"empty or invalid: {eval_obj}",
            )

        # intent の検証
        intent = data.get("intent")
        if not intent or not isinstance(intent, str) or not intent.strip():
            self._logger.warning(f"LLM returned empty intent. Full data: {data}")
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="intent",
                expected="non-empty string",
                actual=f"empty or invalid: {intent}",
            )

    def _parse_llm_output(self, data: dict[str, Any]) -> CognitiveGateOutput:
        """LLM出力をパース.

        注意: _validate_llm_output_fields() で事前検証済みのデータが渡される前提。
        ただし、保守的にデフォルト値を設定（防御的プログラミング）。
        """
        # 不可逆性をパース
        irr_data = data.get("irreversibility", {})
        if not isinstance(irr_data, dict):
            irr_data = {}
        level_str = irr_data.get("level", "MEDIUM")
        level = IrreversibilityLevel(level_str) if level_str in IrreversibilityLevel.__members__ else IrreversibilityLevel.MEDIUM

        irreversibility = Irreversibility(
            level=level,
            description=str(irr_data.get("description", "要評価"))[:50],
        )

        # criteria: 事前検証済みだが、保守的にデフォルト値を設定
        raw_criteria = data.get("criteria")
        if not raw_criteria or not isinstance(raw_criteria, list) or len(raw_criteria) == 0:
            self._logger.warning("criteria was empty after validation - using default (should not happen)")
            criteria = ["要定義"]
        else:
            criteria = [str(c)[:50] for c in raw_criteria[:5] if c and str(c).strip()]
            if not criteria:
                self._logger.warning("criteria had only empty strings - using default")
                criteria = ["要定義"]

        # evaluation_object: 事前検証済みだが、保守的にデフォルト値を設定
        evaluation_object = data.get("evaluation_object", "")
        if not evaluation_object or not isinstance(evaluation_object, str) or not str(evaluation_object).strip():
            self._logger.warning("evaluation_object was empty after validation - using default")
            evaluation_object = "評価対象未特定"

        # intent: 事前検証済みだが、保守的にデフォルト値を設定
        intent = data.get("intent", "")
        if not intent or not isinstance(intent, str) or not str(intent).strip():
            self._logger.warning("intent was empty after validation - using default")
            intent = "動機未特定"

        return CognitiveGateOutput(
            evaluation_object=str(evaluation_object).strip()[:50],
            intent=str(intent).strip()[:100],
            criteria=criteria,
            irreversibility=irreversibility,
            proceed=data.get("proceed", False),
            missing_info=data.get("missing_info", [])[:3],
            clarification_questions=data.get("clarification_questions", [])[:3],
        )

    def _analyze_rule_based(
        self, input_data: CognitiveGateInput
    ) -> CognitiveGateOutput:
        """ルールベース分析."""
        question = input_data.raw_question
        constraints = input_data.constraints

        # 評価対象の推定
        evaluation_object = self._infer_evaluation_object(question)

        # 動機の推定
        intent = self._infer_intent(question, constraints)

        # 評価軸の推定
        criteria = self._infer_criteria(question, constraints)

        # 不可逆性の推定
        irreversibility = self._infer_irreversibility(question)

        # 不足情報のチェック
        missing_info = []
        if not evaluation_object or "不明" in evaluation_object:
            missing_info.append("評価対象が不明確")
        if not intent or "要確認" in intent:
            missing_info.append("判断の動機が不明")
        if len(criteria) < 2:
            missing_info.append("評価軸が不足")

        proceed = len(missing_info) == 0

        return CognitiveGateOutput(
            evaluation_object=evaluation_object,
            intent=intent,
            criteria=criteria,
            irreversibility=irreversibility,
            proceed=proceed,
            missing_info=missing_info,
            clarification_questions=self._generate_questions(missing_info),
        )

    def _infer_evaluation_object(self, question: str) -> str:
        """評価対象を推定."""
        # キーワードベースの簡易推定
        if "構築" in question or "開発" in question:
            return "システム構築の是非判断"
        if "投資" in question:
            return "投資判断"
        if "どちら" in question or "選択" in question:
            return "選択肢間の優先順位判断"
        if "すべきか" in question:
            return question[:30] + "の判断"
        return "要確認: 評価対象が不明確"

    def _infer_intent(self, question: str, constraints: list[str]) -> str:
        """動機を推定."""
        if constraints:
            return f"制約条件（{constraints[0][:20]}...）下での判断が必要"
        if "既存" in question or "代替" in question:
            return "既存解が使えない状況での代替手段の検討"
        if "投資" in question:
            return "投資判断による事業成長の追求"
        if "どちら" in question or "選択" in question:
            return "複数選択肢からの最適解選定"
        if "すべきか" in question or "判断" in question:
            return "意思決定による方向性の確定"
        if "構築" in question or "開発" in question:
            return "新規構築の必要性判断"
        # デフォルト: 質問から動機を推定
        return f"「{question[:20]}...」に関する判断が必要"

    def _infer_criteria(self, question: str, constraints: list[str]) -> list[str]:
        """評価軸を推定."""
        criteria = []
        if "コスト" in question or "予算" in question or any("予算" in c for c in constraints):
            criteria.append("コスト効率")
        if "リスク" in question:
            criteria.append("リスク")
        if "時間" in question or "期限" in question or any("期限" in c for c in constraints):
            criteria.append("時間的実現性")
        if "規制" in question or any("法" in c or "規制" in c for c in constraints):
            criteria.append("法規制適合性")

        # デフォルト軸
        if not criteria:
            criteria = ["実現可能性", "リスク", "コスト"]
        return criteria[:5]

    def _infer_irreversibility(self, question: str) -> Irreversibility:
        """不可逆性を推定."""
        high_keywords = ["構築", "投資", "採用", "契約", "買収"]
        low_keywords = ["検討", "調査", "テスト", "試験"]

        for kw in high_keywords:
            if kw in question:
                return Irreversibility(
                    level=IrreversibilityLevel.HIGH,
                    description="一度決定すると撤退コストが大きい"
                )
        for kw in low_keywords:
            if kw in question:
                return Irreversibility(
                    level=IrreversibilityLevel.LOW,
                    description="容易に方向転換可能"
                )
        return Irreversibility(
            level=IrreversibilityLevel.MEDIUM,
            description="中程度の不可逆性"
        )

    def _generate_questions(self, missing_info: list[str]) -> list[str]:
        """追加質問を生成."""
        questions = []
        for info in missing_info:
            if "評価対象" in info:
                questions.append("具体的に何を判断しようとしていますか？")
            if "動機" in info:
                questions.append("なぜこの判断が今必要なのですか？")
            if "評価軸" in info:
                questions.append("何を基準に判断しますか？（コスト、リスク、時間等）")
        return questions[:3]

