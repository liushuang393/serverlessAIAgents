# -*- coding: utf-8 -*-
"""GatekeeperAgent - 入口検証Agent.

不適格な問題を門前払いし、企業新事業・新製品/サービス投入に関する
意思決定の質問のみを後続Agentに渡す。
このAgentはDaoAgentの前に必ず実行される。
"""

import json
import logging
import re
from typing import Any

from agentflow import ResilientAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    GatekeeperInput,
    GatekeeperOutput,
    QuestionCategory,
)


class GatekeeperAgent(ResilientAgent[GatekeeperInput, GatekeeperOutput]):
    """入口検証Agent.

    職責:
    - 問題の適格性を判断（企業新事業・新製品/サービス投入に関連するか）
    - 不適格な問題を即座に拒否
    - LLMによる詳細な関連性判定
    """

    name = "GatekeeperAgent"
    max_tokens = 500
    temperature = 0.1  # 極めて低い＝一貫した判定

    # 企業新事業関連性チェック用システムプロンプト
    RELEVANCE_CHECK_PROMPT = """あなたは質問の関連性を判定するゲートキーパーです。

【このシステムの対象範囲】
企業における以下のテーマに関する意思決定：
- 新規事業の立ち上げ・参入判断
- 新製品・新サービスの企画・投入判断
- 既存事業の拡大・縮小・撤退判断
- 戦略的投資・リソース配分判断
- 市場参入・技術選定の判断

【判定タスク】
ユーザーの質問が上記の対象範囲に該当するか判定してください。

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "is_relevant": true または false,
    "reason": "判定理由（1文で簡潔に）",
    "category": "NEW_BUSINESS" | "NEW_PRODUCT" | "EXPANSION" | "INVESTMENT" | "MARKET_ENTRY" | "IRRELEVANT"
}

【判定基準】
- is_relevant=true: 企業の新事業・製品/サービス投入に関連する意思決定
- is_relevant=false: 日常会話、一般質問、個人的な相談、技術How-to、雑談など"""

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    # 即座に拒否するパターン（日本語・中国語・英語対応）
    INSTANT_REJECT_PATTERNS: list[tuple[str, QuestionCategory, str]] = [
        # (正規表現, カテゴリ, 拒否メッセージ)
        # 天気・事実検索系（日本語/簡体中文/英語）
        (r"(天気|天气|気温|气温|weather|何時|几点|today|現在|现在)", QuestionCategory.FACTUAL_LOOKUP,
         "天気や時刻の情報にはお答えできません。"),
        # 「〜怎么样/どうですか」パターン（雑談・意見要求）
        (r"(怎么样|怎麼樣|どうですか|どう\?|如何\?)", QuestionCategory.CASUAL_CHAT,
         "意見や感想を求める質問には対応していません。意思決定課題を入力してください。"),
        # システム質問
        (r"(このシステム|このAI|どうやって作|仕組み|这个系统|这个AI|how.*work)", QuestionCategory.SYSTEM_INQUIRY,
         "システム自体への質問にはお答えできません。"),
        # 知識質問
        (r"(.+)(とは何|って何|とは？|what is|是什么|是啥)", QuestionCategory.GENERAL_KNOWLEDGE,
         "用語や概念の説明にはお答えできません。"),
        # 挨拶・雑談
        (r"^(こんにちは|hello|hi|ありがとう|thank|你好|谢谢|哈喽)", QuestionCategory.CASUAL_CHAT,
         "雑談には対応していません。"),
        # コード生成
        (r"(コード.*書いて|write.*code|プログラム.*作成|写代码|编程)", QuestionCategory.TECHNICAL_HOWTO,
         "コード生成には対応していません。"),
        # 創作
        (r"(物語|story|poem|詩|小説|作文|故事|诗|小说)", QuestionCategory.CREATIVE_REQUEST,
         "創作・コンテンツ生成には対応していません。"),
        # 計算・翻訳
        (r"(計算して|convert|換算|translate|计算|翻译|换算)", QuestionCategory.FACTUAL_LOOKUP,
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

        # Step 1: 即座拒否パターンチェック（明らかに無関係な質問）
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

        # Step 2: 長さチェック（短すぎる質問は拒否）
        if len(question) < 15:
            return GatekeeperOutput(
                is_acceptable=False,
                category=QuestionCategory.CASUAL_CHAT,
                confidence=0.9,
                rejection_reason="質問が短すぎます",
                rejection_message="企業の新事業・新製品/サービス投入に関する具体的な意思決定課題を入力してください。",
                suggested_rephrase="例：「SaaS市場に新規参入すべきか？予算5000万円、12ヶ月以内の判断が必要」",
            )

        # Step 3: LLMによる企業新事業関連性チェック
        relevance_result = await self._check_business_relevance(question)

        if not relevance_result.get("is_relevant", False):
            return GatekeeperOutput(
                is_acceptable=False,
                category=QuestionCategory.GENERAL_KNOWLEDGE,
                confidence=0.85,
                rejection_reason=relevance_result.get("reason", "企業新事業に無関係"),
                rejection_message=(
                    "このシステムは企業の新事業・新製品/サービス投入に関する意思決定支援専用です。\n"
                    "入力された質問は対象外と判定されました。"
                ),
                suggested_rephrase=(
                    "企業の新規事業立ち上げ、新製品/サービス投入、市場参入などに関する"
                    "意思決定課題を入力してください。"
                ),
            )

        # Step 4: 関連性確認済み → カテゴリマッピング
        category_map = {
            "NEW_BUSINESS": QuestionCategory.STRATEGIC_DECISION,
            "NEW_PRODUCT": QuestionCategory.STRATEGIC_DECISION,
            "EXPANSION": QuestionCategory.RESOURCE_ALLOCATION,
            "INVESTMENT": QuestionCategory.RESOURCE_ALLOCATION,
            "MARKET_ENTRY": QuestionCategory.STRATEGIC_DECISION,
        }
        llm_category = relevance_result.get("category", "NEW_BUSINESS")
        final_category = category_map.get(llm_category, QuestionCategory.STRATEGIC_DECISION)

        return GatekeeperOutput(
            is_acceptable=True,
            category=final_category,
            confidence=0.85,
            rejection_reason=None,
            rejection_message=None,
            suggested_rephrase=None,
        )

    async def _check_business_relevance(self, question: str) -> dict[str, Any]:
        """LLMで企業新事業関連性をチェック."""
        if not self._llm:
            # LLMがない場合はデフォルトで受理（フォールバック）
            self._logger.warning("LLM not available, falling back to accept")
            return {"is_relevant": True, "reason": "LLM unavailable", "category": "NEW_BUSINESS"}

        try:
            messages = [
                {"role": "system", "content": self.RELEVANCE_CHECK_PROMPT},
                {"role": "user", "content": f"質問: {question}"},
            ]

            response = await self._llm.chat(
                messages=messages,
                max_tokens=200,
                temperature=0.1,
            )

            # JSON パース
            content = response.content if hasattr(response, "content") else str(response)
            # JSON部分を抽出
            json_match = re.search(r"\{[^}]+\}", content, re.DOTALL)
            if json_match:
                return json.loads(json_match.group())

            self._logger.warning(f"Failed to parse LLM response: {content}")
            return {"is_relevant": True, "reason": "Parse failed", "category": "NEW_BUSINESS"}

        except Exception as e:
            self._logger.error(f"LLM relevance check failed: {e}")
            return {"is_relevant": True, "reason": str(e), "category": "NEW_BUSINESS"}

    def validate_output(self, output: GatekeeperOutput) -> bool:
        """出力検証."""
        # 拒否の場合は理由が必須
        if not output.is_acceptable and not output.rejection_reason:
            return False
        return True

