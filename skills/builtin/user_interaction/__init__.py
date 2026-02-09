"""User Interaction Skill - ユーザー交互能力.

Agent がユーザーに情報補足を求めるための能力。

使用例:
    >>> from agentflow.skills.builtin.user_interaction import UserInteractionSkill
    >>> interaction = UserInteractionSkill()
    >>> missing = await interaction.detect_missing_info("新サービスを始めたい")
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any


# ロガー設定
logger = logging.getLogger("user_interaction")


@dataclass
class ClarificationQuestion:
    """補足質問."""

    id: str
    text: str
    type: str = "text"  # text, number, select, multiselect, date
    required: bool = True
    options: list[str] = field(default_factory=list)
    placeholder: str = ""
    validation: str = ""  # regex pattern


@dataclass
class ClarificationRequest:
    """補足要求."""

    original_question: str
    questions: list[ClarificationQuestion]
    context: dict[str, Any] = field(default_factory=dict)
    timeout_seconds: int = 300  # 5分


@dataclass
class EnrichedQuestion:
    """補足後の質問."""

    original: str
    enriched: str
    supplements: dict[str, Any]


class UserInteractionSkill:
    """User Interaction Skill - ユーザー交互能力.

    情報不足を検出し、ユーザーに補足を求める。
    """

    # 一般的な不足情報パターン
    COMMON_MISSING_PATTERNS = {
        "budget": ["予算", "費用", "コスト", "投資"],
        "timeline": ["期限", "期間", "スケジュール", "いつまで"],
        "scope": ["範囲", "規模", "対象"],
        "goal": ["目標", "目的", "ゴール", "達成"],
        "constraints": ["制約", "条件", "制限"],
        "stakeholders": ["関係者", "ステークホルダー", "担当"],
        "context": ["背景", "経緯", "理由"],
        "metrics": ["指標", "KPI", "成功基準"],
    }

    # 質問タイプマッピング
    QUESTION_TYPES = {
        "budget": ("number", "予算規模を教えてください（万円）"),
        "timeline": ("text", "希望する期間やスケジュールを教えてください"),
        "scope": ("text", "対象範囲や規模を具体的に教えてください"),
        "goal": ("text", "達成したい目標は何ですか？"),
        "constraints": ("text", "制約条件があれば教えてください"),
        "stakeholders": ("text", "関係者や担当者は誰ですか？"),
        "context": ("text", "背景や経緯を教えてください"),
        "metrics": ("text", "成功をどう測定しますか？"),
    }

    def __init__(self, llm_client: Any = None) -> None:
        """初期化.

        Args:
            llm_client: LLM クライアント（高度な検出用）
        """
        self._llm = llm_client
        self._logger = logger

    async def detect_missing_info(
        self,
        question: str,
        context: dict[str, Any] | None = None,
        required_fields: list[str] | None = None,
    ) -> list[str]:
        """情報不足を検出.

        Args:
            question: ユーザーの質問
            context: 追加コンテキスト
            required_fields: 必須フィールド（指定時はそれのみチェック）

        Returns:
            不足情報のリスト
        """
        missing: list[str] = []

        # 必須フィールドが指定された場合
        if required_fields:
            for field_name in required_fields:
                if not self._is_mentioned(question, field_name):
                    missing.append(field_name)
            return missing

        # 一般的なパターンでチェック
        for field_name, keywords in self.COMMON_MISSING_PATTERNS.items():
            if not self._is_mentioned(question, field_name, keywords):
                # 質問の性質に応じて必要な情報を判定
                if self._is_relevant(question, field_name):
                    missing.append(field_name)

        return missing

    def _is_mentioned(
        self, question: str, field_name: str, keywords: list[str] | None = None
    ) -> bool:
        """キーワードが言及されているかチェック."""
        check_words = keywords or self.COMMON_MISSING_PATTERNS.get(field_name, [])
        question_lower = question.lower()
        return any(kw in question_lower for kw in check_words)

    def _is_relevant(self, question: str, field_name: str) -> bool:
        """質問に対してそのフィールドが関連するかチェック."""
        # ビジネス/戦略系の質問には予算・期間が重要
        business_keywords = ["始め", "開始", "計画", "戦略", "投資", "プロジェクト", "サービス"]
        if any(kw in question for kw in business_keywords):
            return field_name in ["budget", "timeline", "goal", "scope"]

        # 技術系の質問には制約・範囲が重要
        tech_keywords = ["実装", "開発", "システム", "アーキテクチャ", "設計"]
        if any(kw in question for kw in tech_keywords):
            return field_name in ["scope", "constraints", "timeline"]

        return False

    async def create_clarification_request(
        self,
        original_question: str,
        missing_info: list[str],
        context: dict[str, Any] | None = None,
    ) -> ClarificationRequest:
        """補足要求を生成.

        Args:
            original_question: 元の質問
            missing_info: 不足情報リスト
            context: 追加コンテキスト

        Returns:
            補足要求
        """
        questions: list[ClarificationQuestion] = []

        for info in missing_info:
            q_type, q_text = self.QUESTION_TYPES.get(info, ("text", f"{info}について教えてください"))
            questions.append(
                ClarificationQuestion(
                    id=info,
                    text=q_text,
                    type=q_type,
                    required=True,
                )
            )

        return ClarificationRequest(
            original_question=original_question,
            questions=questions,
            context=context or {},
        )

    async def integrate_answers(
        self,
        original_question: str,
        answers: dict[str, Any],
    ) -> EnrichedQuestion:
        """回答を統合して質問を強化.

        Args:
            original_question: 元の質問
            answers: ユーザー回答

        Returns:
            強化された質問
        """
        # 回答を文脈として追加
        supplements_text = "\n".join([f"- {k}: {v}" for k, v in answers.items()])
        enriched = f"{original_question}\n\n【補足情報】\n{supplements_text}"

        return EnrichedQuestion(
            original=original_question,
            enriched=enriched,
            supplements=answers,
        )


# エクスポート
__all__ = [
    "ClarificationQuestion",
    "ClarificationRequest",
    "EnrichedQuestion",
    "UserInteractionSkill",
]

