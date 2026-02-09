"""ClarificationAgent - 問題診断/澄清Agent.

ユーザーの質問に即座に答えるのではなく、まず深く診断する。
論理的な穴、暗黙の仮定、認知バイアスを明らかにする。
"""

import json
import logging
from typing import Any

from apps.decision_governance_engine.schemas.agent_schemas import (
    Ambiguity,
    ClarificationInput,
    ClarificationOutput,
    CognitiveBias,
    HiddenAssumption,
)

from agentflow import ResilientAgent


class ClarificationAgent(ResilientAgent[ClarificationInput, ClarificationOutput]):
    """問題診断Agent.

    職責:
    - 質問の復唱（理解確認）
    - 曖昧な点の特定
    - 暗黙の仮定の発見
    - 認知バイアスの指摘
    - 精緻化された質問の生成

    原則:
    - ユーザーに迎合しない
    - 空虚な励ましを与えない
    - 構造分析をスキップしない
    - まず診断、それから提案
    """

    name = "ClarificationAgent"
    # timeout_seconds, max_retries, max_tokens は ResilientAgent のデフォルト値を使用
    temperature = 0.3  # 低め = 安定した診断

    # RAG使用禁止
    USE_RAG = False

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    # 認知バイアスパターン
    COGNITIVE_BIAS_PATTERNS: dict[str, dict[str, str]] = {
        "sunk_cost": {
            "keywords": ["既に", "投資した", "使った", "続けてきた"],
            "name": "サンクコスト（埋没費用）",
            "description": "過去の投資にこだわり、将来の判断を歪める傾向",
        },
        "confirmation": {
            "keywords": ["思う", "信じる", "確信", "きっと"],
            "name": "確証バイアス",
            "description": "自分の信念を裏付ける情報だけを集める傾向",
        },
        "anchoring": {
            "keywords": ["最初に", "当初", "元々", "基準"],
            "name": "アンカリング",
            "description": "最初に得た情報に過度に依存する傾向",
        },
        "availability": {
            "keywords": ["最近", "先日", "この前", "よく聞く"],
            "name": "可用性ヒューリスティック",
            "description": "思い出しやすい事例を過大評価する傾向",
        },
        "optimism": {
            "keywords": ["うまくいく", "成功する", "問題ない", "大丈夫"],
            "name": "楽観バイアス",
            "description": "リスクを過小評価し、成功を過大評価する傾向",
        },
        "status_quo": {
            "keywords": ["今のまま", "変えたくない", "現状", "維持"],
            "name": "現状維持バイアス",
            "description": "変化を避け、現状を維持しようとする傾向",
        },
        "dichotomy": {
            "keywords": ["AかBか", "どちらか", "二択", "しかない"],
            "name": "二分法的思考",
            "description": "複雑な問題を二者択一に単純化する傾向",
        },
    }

    # 曖昧さパターン
    AMBIGUITY_PATTERNS: list[dict[str, str]] = [
        {"pattern": "成功", "question": "「成功」とは具体的に何を指しますか？数値目標はありますか？"},
        {"pattern": "うまく", "question": "「うまくいく」とはどのような状態を指しますか？"},
        {"pattern": "良い", "question": "「良い」の判断基準は何ですか？"},
        {"pattern": "適切", "question": "「適切」とは誰にとって、何の観点から適切ですか？"},
        {"pattern": "効率", "question": "「効率」は何を指標として測定しますか？"},
        {"pattern": "すべき", "question": "「すべき」の判断基準は何ですか？誰の視点ですか？"},
    ]

    SYSTEM_PROMPT = """あなたはClarificationAgent（問題診断）です。
ユーザーの質問に即座に答えるのではなく、まず深く診断します。

【あなたの唯一の責任】
質問の中の論理的な穴、暗黙の仮定、認知バイアスを明らかにすること。

【原則】
- ユーザーに迎合しない
- 空虚な励ましを与えない
- 構造分析をスキップしない
- まず診断、それから提案

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "restated_question": "一文で正確に復唱した質問（100字以内）",
    "ambiguities": [
        {"point": "曖昧な点", "clarification_needed": "明確化が必要な内容"}
    ],
    "hidden_assumptions": [
        {"assumption": "暗黙の仮定", "validity_question": "この仮定は成り立つか？"}
    ],
    "cognitive_biases": [
        {"bias": "バイアス名", "manifestation": "この質問でどう現れているか"}
    ],
    "refined_question": "診断後の精緻化された質問",
    "diagnosis_confidence": 0.8
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> ClarificationInput:
        """入力をパース."""
        return ClarificationInput(**input_data)

    async def process(self, input_data: ClarificationInput) -> ClarificationOutput:
        """問題診断を実行."""
        question = input_data.raw_question
        constraints = input_data.constraints

        # LLMで診断（ある場合）
        if self._llm:
            return await self._diagnose_with_llm(question, constraints)

        # ルールベース診断
        return self._diagnose_rule_based(question, constraints)

    async def _diagnose_with_llm(
        self, question: str, constraints: list[str]
    ) -> ClarificationOutput:
        """LLMを使用した診断."""
        user_prompt = f"""【質問】
{question}

【制約条件】
{chr(10).join(f"- {c}" for c in constraints) if constraints else "特になし"}

上記の質問を深く診断し、JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        # 詳細ログ: LLM生出力を記録（デバッグ用）
        self._logger.debug(f"LLM raw response (first 500 chars): {response[:500] if response else 'EMPTY'}")

        try:
            # JSON部分を抽出してパース（堅牢な抽出）
            from agentflow.utils import extract_json
            data = extract_json(response)

            if data is None:
                self._logger.error(f"JSON extraction failed. Raw response: {response[:1000]}")
                msg = "No valid JSON found"
                raise json.JSONDecodeError(msg, response, 0)

            # 詳細ログ: 抽出されたJSON
            self._logger.debug(f"Extracted JSON: {data}")

            ambiguities = [
                Ambiguity(**a) for a in data.get("ambiguities", [])
            ][:3]
            hidden_assumptions = [
                HiddenAssumption(**h) for h in data.get("hidden_assumptions", [])
            ][:3]
            cognitive_biases = [
                CognitiveBias(**c) for c in data.get("cognitive_biases", [])
            ][:2]

            # diagnosis_confidence の範囲を正規化（ge=0.0, le=1.0）
            confidence = data.get("diagnosis_confidence", 0.7)
            if not isinstance(confidence, (int, float)):
                confidence = 0.7
            confidence = max(0.0, min(1.0, float(confidence)))

            return ClarificationOutput(
                restated_question=data.get("restated_question", question)[:100],
                ambiguities=ambiguities,
                hidden_assumptions=hidden_assumptions,
                cognitive_biases=cognitive_biases,
                refined_question=data.get("refined_question", question),
                diagnosis_confidence=confidence,
            )
        except json.JSONDecodeError:
            return self._diagnose_rule_based(question, constraints)

    def _diagnose_rule_based(
        self, question: str, constraints: list[str]
    ) -> ClarificationOutput:
        """ルールベース診断（LLMなし）."""
        # Step 1: 質問の復唱
        restated = self._restate_question(question)

        # Step 2: 曖昧な点を検出
        ambiguities = self._detect_ambiguities(question)

        # Step 3: 暗黙の仮定を検出
        hidden_assumptions = self._detect_hidden_assumptions(question, constraints)

        # Step 4: 認知バイアスを検出
        cognitive_biases = self._detect_cognitive_biases(question)

        # Step 5: 精緻化された質問を生成
        refined = self._refine_question(question, ambiguities, hidden_assumptions)

        # 確信度を計算
        confidence = self._calculate_confidence(
            ambiguities, hidden_assumptions, cognitive_biases
        )

        return ClarificationOutput(
            restated_question=restated[:100],
            ambiguities=ambiguities[:3],
            hidden_assumptions=hidden_assumptions[:3],
            cognitive_biases=cognitive_biases[:2],
            refined_question=refined,
            diagnosis_confidence=confidence,
        )

    def _restate_question(self, question: str) -> str:
        """質問を一文で復唱."""
        # 感情的な言葉を除去して核心を抽出
        remove_words = ["悩んでいます", "困っています", "迷っています", "どうしたら"]
        result = question
        for word in remove_words:
            result = result.replace(word, "")
        return result.strip()[:100] or question[:100]

    def _detect_ambiguities(self, question: str) -> list[Ambiguity]:
        """曖昧な点を検出."""
        ambiguities = []

        for pattern_info in self.AMBIGUITY_PATTERNS:
            if pattern_info["pattern"] in question:
                ambiguities.append(Ambiguity(
                    point=f"「{pattern_info['pattern']}」の定義",
                    clarification_needed=pattern_info["question"],
                ))

        # 時間軸の欠如をチェック
        time_keywords = ["いつ", "期限", "月", "週", "年"]
        if not any(kw in question for kw in time_keywords):
            ambiguities.append(Ambiguity(
                point="時間軸の不明確さ",
                clarification_needed="この判断はいつまでに必要ですか？成果をいつまでに期待しますか？",
            ))

        return ambiguities[:3]

    def _detect_hidden_assumptions(
        self, question: str, constraints: list[str]
    ) -> list[HiddenAssumption]:
        """暗黙の仮定を検出."""
        assumptions = []

        # 二者択一の仮定
        if "か" in question and ("AとB" in question or "どちら" in question):
            assumptions.append(HiddenAssumption(
                assumption="選択肢はAとBの二つだけと仮定",
                validity_question="第三の選択肢や、両方を部分的に実行する可能性は検討しましたか？",
            ))

        # リソース固定の仮定
        if constraints:
            assumptions.append(HiddenAssumption(
                assumption="現在のリソース制約は固定と仮定",
                validity_question="追加の資金調達や外部パートナーの活用は検討しましたか？",
            ))

        # チーム固定の仮定
        if any("名" in c or "人" in c for c in constraints):
            assumptions.append(HiddenAssumption(
                assumption="チーム構成は変えられないと仮定",
                validity_question="採用、業務委託、パートナーシップの可能性は？",
            ))

        # 市場環境固定の仮定
        assumptions.append(HiddenAssumption(
            assumption="現在の市場環境が継続すると仮定",
            validity_question="市場環境が急変した場合のシナリオは検討しましたか？",
        ))

        return assumptions[:3]

    def _detect_cognitive_biases(self, question: str) -> list[CognitiveBias]:
        """認知バイアスを検出."""
        biases = []

        for _bias_id, bias_info in self.COGNITIVE_BIAS_PATTERNS.items():
            if any(kw in question for kw in bias_info["keywords"]):
                biases.append(CognitiveBias(
                    bias=bias_info["name"],
                    manifestation=f"質問に「{'」「'.join(kw for kw in bias_info['keywords'] if kw in question)}」が含まれており、{bias_info['description']}の可能性。",
                ))

        return biases[:2]

    def _refine_question(
        self,
        question: str,
        ambiguities: list[Ambiguity],
        hidden_assumptions: list[HiddenAssumption],
    ) -> str:
        """精緻化された質問を生成."""
        # 曖昧な点を明示的に含める
        clarifications = []
        for amb in ambiguities[:2]:
            clarifications.append(f"【{amb.point}を明確にした上で】")

        # 暗黙の仮定を明示化
        assumptions_text = []
        for assumption in hidden_assumptions[:2]:
            assumptions_text.append(f"（{assumption.assumption}という前提のもと）")

        refined = question
        if clarifications:
            refined = "".join(clarifications) + " " + refined
        if assumptions_text:
            refined += " " + "".join(assumptions_text)

        return refined

    def _calculate_confidence(
        self,
        ambiguities: list[Ambiguity],
        hidden_assumptions: list[HiddenAssumption],
        cognitive_biases: list[CognitiveBias],
    ) -> float:
        """診断確信度を計算."""
        # 基本スコア
        base = 0.7

        # 検出項目が多いほど確信度が高い（しっかり診断できている）
        if len(ambiguities) >= 2:
            base += 0.1
        if len(hidden_assumptions) >= 2:
            base += 0.1
        if len(cognitive_biases) >= 1:
            base += 0.05

        return min(base, 0.95)

    def validate_output(self, output: ClarificationOutput) -> bool:
        """出力検証."""
        # 復唱と精緻化は必須
        return not (not output.restated_question or not output.refined_question)

