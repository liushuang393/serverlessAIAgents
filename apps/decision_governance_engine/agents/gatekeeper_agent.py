# -*- coding: utf-8 -*-
"""GatekeeperAgent - 入口検証Agent.

不適格な問題を門前払いし、決策系の質問のみを後続Agentに渡す。
このAgentはDaoAgentの前に必ず実行される。
"""

import re
from typing import Any

from apps.decision_governance_engine.agents.base_agent import BaseDecisionAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    GatekeeperInput,
    GatekeeperOutput,
    QuestionCategory,
)


class GatekeeperAgent(BaseDecisionAgent[GatekeeperInput, GatekeeperOutput]):
    """入口検証Agent.

    職責:
    - 問題の適格性を判断
    - 不適格な問題を即座に拒否
    - 境界ケースに言い換え提案を提供
    """

    name = "GatekeeperAgent"
    max_tokens = 300
    temperature = 0.1  # 極めて低い＝一貫した判定

    # 即座に拒否するパターン
    INSTANT_REJECT_PATTERNS: list[tuple[str, QuestionCategory, str]] = [
        # (正規表現, カテゴリ, 拒否メッセージ)
        (r"(天気|気温|weather|何時|today)", QuestionCategory.FACTUAL_LOOKUP,
         "天気や時刻の情報にはお答えできません。"),
        (r"(このシステム|このAI|どうやって作|仕組み|how.*work)", QuestionCategory.SYSTEM_INQUIRY,
         "システム自体への質問にはお答えできません。"),
        (r"(.+)(とは何|って何|とは？|what is)", QuestionCategory.GENERAL_KNOWLEDGE,
         "用語や概念の説明にはお答えできません。"),
        (r"^(こんにちは|hello|hi|ありがとう|thank)", QuestionCategory.CASUAL_CHAT,
         "雑談には対応していません。"),
        (r"(コード.*書いて|write.*code|プログラム.*作成)", QuestionCategory.TECHNICAL_HOWTO,
         "コード生成には対応していません。"),
        (r"(物語|story|poem|詩|小説|作文)", QuestionCategory.CREATIVE_REQUEST,
         "創作・コンテンツ生成には対応していません。"),
        (r"(計算して|convert|換算|translate)", QuestionCategory.FACTUAL_LOOKUP,
         "計算や変換には対応していません。"),
    ]

    # 受理可能なパターン
    ACCEPTANCE_PATTERNS: list[tuple[str, QuestionCategory]] = [
        (r"(どちら|どっち|AとB|選ぶべき|選択)", QuestionCategory.TRADE_OFF_CHOICE),
        (r"(投資|予算|配分|リソース)", QuestionCategory.RESOURCE_ALLOCATION),
        (r"(いつ|タイミング|時期|着手)", QuestionCategory.TIMING_JUDGMENT),
        (r"(リスク|危険|懸念|問題点)", QuestionCategory.RISK_EVALUATION),
        (r"(優先|順位|重要度)", QuestionCategory.PRIORITY_SETTING),
        (r"(続行|中止|やめる|Go.*No)", QuestionCategory.GO_NOGO_DECISION),
        (r"(戦略|方針|方向性|ビジョン)", QuestionCategory.STRATEGIC_DECISION),
    ]

    # 拒否時の定型メッセージ
    REJECTION_MESSAGES: dict[QuestionCategory, dict[str, str | None]] = {
        QuestionCategory.GENERAL_KNOWLEDGE: {
            "reason": "一般的な知識・情報の質問です",
            "message": "このシステムは企業の意思決定支援に特化しています。",
            "suggest": "意思決定が必要な形に言い換えてください。例：「○○を導入すべきか判断したい」",
        },
        QuestionCategory.TECHNICAL_HOWTO: {
            "reason": "技術的なHow-to質問です",
            "message": "技術的な実装方法の質問にはお答えできません。",
            "suggest": "技術選定の意思決定として言い換えてください。",
        },
        QuestionCategory.SYSTEM_INQUIRY: {
            "reason": "システム自体に関する質問です",
            "message": "このシステムの仕組みについてはお答えできません。",
            "suggest": None,
        },
        QuestionCategory.CASUAL_CHAT: {
            "reason": "雑談・挨拶です",
            "message": "このシステムは雑談には対応していません。",
            "suggest": None,
        },
        QuestionCategory.FACTUAL_LOOKUP: {
            "reason": "事実確認の質問です",
            "message": "事実や情報の検索にはお答えできません。",
            "suggest": "その事実を踏まえた意思決定として言い換えてください。",
        },
        QuestionCategory.OPINION_REQUEST: {
            "reason": "意見・感想を求める質問です",
            "message": "個人的な意見や感想の提供は行っていません。",
            "suggest": "客観的な判断基準に基づく意思決定として言い換えてください。",
        },
        QuestionCategory.CREATIVE_REQUEST: {
            "reason": "創作・コンテンツ生成の依頼です",
            "message": "文章や創作物の生成には対応していません。",
            "suggest": None,
        },
    }

    def _parse_input(self, input_data: dict[str, Any]) -> GatekeeperInput:
        """入力をパース."""
        return GatekeeperInput(**input_data)

    async def process(self, input_data: GatekeeperInput) -> GatekeeperOutput:
        """入口検証を実行."""
        question = input_data.raw_question.strip()

        # Step 1: 即座拒否パターンチェック
        for pattern, category, message in self.INSTANT_REJECT_PATTERNS:
            if re.search(pattern, question, re.IGNORECASE):
                rejection_info = self.REJECTION_MESSAGES.get(category, {})
                return GatekeeperOutput(
                    is_acceptable=False,
                    category=category,
                    confidence=0.95,
                    rejection_reason=rejection_info.get("reason", "不適格な質問"),
                    rejection_message=message,
                    suggested_rephrase=rejection_info.get("suggest"),
                )

        # Step 2: 受理可能パターンチェック
        for pattern, category in self.ACCEPTANCE_PATTERNS:
            if re.search(pattern, question, re.IGNORECASE):
                return GatekeeperOutput(
                    is_acceptable=True,
                    category=category,
                    confidence=0.85,
                    rejection_reason=None,
                    rejection_message=None,
                    suggested_rephrase=None,
                )

        # Step 3: 長さと構造チェック
        if len(question) < 20:
            return GatekeeperOutput(
                is_acceptable=False,
                category=QuestionCategory.CASUAL_CHAT,
                confidence=0.7,
                rejection_reason="質問が短すぎます",
                rejection_message="より具体的な意思決定課題を入力してください。",
                suggested_rephrase="背景、制約、選択肢を含めて詳しく記述してください。",
            )

        # Step 4: デフォルト（長い質問は戦略的決策として仮受理）
        return GatekeeperOutput(
            is_acceptable=True,
            category=QuestionCategory.STRATEGIC_DECISION,
            confidence=0.6,
            rejection_reason=None,
            rejection_message=None,
            suggested_rephrase=None,
        )

    def validate_output(self, output: GatekeeperOutput) -> bool:
        """出力検証."""
        # 拒否の場合は理由が必須
        if not output.is_acceptable and not output.rejection_reason:
            return False
        return True

