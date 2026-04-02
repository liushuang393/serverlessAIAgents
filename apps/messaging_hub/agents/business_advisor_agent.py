"""ビジネスアドバイザーエージェント.

Minimalist Entrepreneur フレームワークに基づき、
起業・ビジネス関連の質問に対して適切なスキルを選択・実行し、
構造化されたアドバイスを返す。

使用例:
    >>> agent = BusinessAdvisorAgent(gateway=skill_gateway)
    >>> result = await agent.process(
    ...     BusinessAdvisorInput(question="新しいSaaSの価格設定を考えたい")
    ... )
"""

from __future__ import annotations

import json
import logging
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from infrastructure.llm.providers import get_llm
from kernel.agents.resilient_agent import ResilientAgent


if TYPE_CHECKING:
    from kernel.skills.gateway import SkillGateway


# スキル名 → 説明のマッピング（スキル選択プロンプト用）
_SKILL_CATALOG: dict[str, str] = {
    "biz_find_community": "ビジネスを構築するコミュニティの発見・評価",
    "biz_validate_idea": "ビジネスアイデアの構築前検証",
    "biz_mvp": "最小限の実用製品（MVP）定義",
    "biz_processize": "製品アイデアの手動ファーストプロセス化",
    "biz_first_customers": "最初の 100 人の顧客獲得戦略",
    "biz_pricing": "コストベース・バリューベースの価格戦略",
    "biz_marketing_plan": "コンテンツマーケティングによるオーディエンス構築",
    "biz_grow_sustainably": "収益性を維持しながらの持続可能な成長",
    "biz_company_values": "企業文化と価値観の定義",
    "biz_minimalist_review": "ミニマリスト原則によるビジネス判断レビュー",
}

_SELECTOR_PROMPT = """あなたはビジネスアドバイスの専門ルーターです。
ユーザーの質問を分析し、最も適切なスキルを 1〜3 個選択してください。

## 利用可能なスキル

{skill_list}

## ルール
- 質問に最も関連するスキルのみを選択
- 最大 3 つまで
- JSON 配列で返す: ["skill_name_1", "skill_name_2"]
- スキル名は上記リストの名前を正確に使用

## ユーザーの質問
{question}

JSON 配列のみを出力してください。"""


class BusinessAdvisorInput(BaseModel):
    """ビジネスアドバイザー入力."""

    question: str = Field(description="ユーザーのビジネス関連の質問")
    context: str = Field(default="", description="追加コンテキスト")
    execution_context: dict[str, Any] = Field(default_factory=dict, description="実行基盤コンテキスト")


class BusinessAdvisorOutput(BaseModel):
    """ビジネスアドバイザー出力."""

    advice: list[dict[str, Any]] = Field(
        default_factory=list,
        description="各スキルからのアドバイス結果",
    )
    selected_skills: list[str] = Field(
        default_factory=list,
        description="選択されたスキル名",
    )
    summary: str = Field(default="", description="全体の要約")
    error: str | None = Field(default=None, description="エラーメッセージ")


class BusinessAdvisorAgent(ResilientAgent[BusinessAdvisorInput, BusinessAdvisorOutput]):
    """ビジネスアドバイザーエージェント.

    Minimalist Entrepreneur フレームワークに基づくビジネスアドバイス。
    質問を分析し、適切なスキルを自動選択して実行する。
    """

    name = "BusinessAdvisorAgent"
    timeout_seconds = 120
    temperature = 0.3

    def __init__(
        self,
        gateway: SkillGateway | None = None,
    ) -> None:
        """初期化.

        Args:
            gateway: スキルゲートウェイ
        """
        super().__init__()
        self._gateway = gateway
        self._logger = logging.getLogger(__name__)

    async def process(
        self,
        input_data: BusinessAdvisorInput,
    ) -> BusinessAdvisorOutput:
        """ビジネスアドバイスを実行する.

        Args:
            input_data: 入力データ

        Returns:
            アドバイス結果
        """
        question = input_data.question
        context = input_data.context

        if not question.strip():
            return BusinessAdvisorOutput(
                error="質問が空です。ビジネスに関する質問を入力してください。",
            )

        # 1. 適切なスキルを選択
        selected_skills = await self._select_skills(question)
        if not selected_skills:
            # フォールバック: minimalist_review を使用
            selected_skills = ["biz_minimalist_review"]

        self._logger.info(
            "スキル選択完了: question=%s, skills=%s",
            question[:50],
            selected_skills,
        )

        # 2. 各スキルを実行
        advice_results: list[dict[str, Any]] = []
        accumulated_context = context

        for skill_name in selected_skills:
            result = await self._invoke_skill(
                skill_name,
                question,
                accumulated_context,
            )
            advice_results.append(result)

            # 次のスキルのコンテキストに前の結果を追加
            if result.get("success"):
                advice_text = result.get("advice", "")
                accumulated_context = (f"{accumulated_context}\n\n## {skill_name} の結果\n{advice_text}").strip()

        # 3. 要約を生成
        summary = await self._generate_summary(question, advice_results)

        return BusinessAdvisorOutput(
            advice=advice_results,
            selected_skills=selected_skills,
            summary=summary,
        )

    def _parse_input(self, input_data: dict[str, Any]) -> BusinessAdvisorInput:
        """入力を Pydantic モデルへ変換する."""
        return BusinessAdvisorInput.model_validate(input_data)

    async def _select_skills(self, question: str) -> list[str]:
        """質問を分析し、適切なスキルを選択する.

        Args:
            question: ユーザーの質問

        Returns:
            選択されたスキル名リスト（最大 3 つ）
        """
        skill_list = "\n".join(f"- **{name}**: {desc}" for name, desc in _SKILL_CATALOG.items())
        prompt = _SELECTOR_PROMPT.format(skill_list=skill_list, question=question)

        llm = get_llm(temperature=0.1)
        response = await llm.generate(
            role="reasoning",
            messages=[
                {
                    "role": "system",
                    "content": "スキル選択ルーターです。JSON 配列のみを出力します。",
                },
                {"role": "user", "content": prompt},
            ],
        )

        content = str(response.get("content", "[]"))

        # JSON を抽出
        try:
            # コードブロックがある場合は除去
            if "```" in content:
                content = content.split("```")[1]
                if content.startswith("json"):
                    content = content[4:]
                content = content.split("```")[0]

            skills = json.loads(content.strip())
            if not isinstance(skills, list):
                return ["biz_minimalist_review"]

            # 有効なスキル名のみフィルタリング
            valid_skills = [s for s in skills if s in _SKILL_CATALOG]
            return valid_skills[:3]

        except (json.JSONDecodeError, IndexError):
            self._logger.warning("スキル選択の解析に失敗: %s", content[:100])
            return ["biz_minimalist_review"]

    async def _invoke_skill(
        self,
        skill_name: str,
        user_input: str,
        context: str,
    ) -> dict[str, Any]:
        """SkillGateway 経由でスキルを実行する.

        Args:
            skill_name: スキル名
            user_input: ユーザー入力
            context: コンテキスト

        Returns:
            実行結果
        """
        if self._gateway is None:
            return {
                "skill_name": skill_name,
                "success": False,
                "error": "SkillGateway が設定されていません",
            }

        try:
            result = await self._gateway.call(
                skill_name,
                {"user_input": user_input, "context": context},
            )
            return {
                "skill_name": skill_name,
                "success": result.success,
                "advice": result.result.get("advice", "") if result.result else "",
                "error": result.error,
            }
        except Exception as e:
            self._logger.exception("スキル実行エラー: %s", skill_name)
            return {
                "skill_name": skill_name,
                "success": False,
                "error": str(e),
            }

    async def _generate_summary(
        self,
        question: str,
        results: list[dict[str, Any]],
    ) -> str:
        """複数スキルの結果を要約する.

        Args:
            question: 元の質問
            results: 各スキルの実行結果

        Returns:
            要約文
        """
        successful = [r for r in results if r.get("success")]
        if not successful:
            return "アドバイスの生成に失敗しました。別の質問をお試しください。"

        if len(successful) == 1:
            return successful[0].get("advice", "")

        # 複数結果の統合要約
        advice_texts = "\n\n---\n\n".join(f"## {r['skill_name']}\n{r.get('advice', '')}" for r in successful)

        llm = get_llm(temperature=0.3)
        response = await llm.generate(
            role="reasoning",
            messages=[
                {
                    "role": "system",
                    "content": ("複数のビジネスアドバイスを統合し、簡潔で実行可能な要約を日本語で作成してください。"),
                },
                {
                    "role": "user",
                    "content": (
                        f"## 元の質問\n{question}\n\n"
                        f"## 各スキルからのアドバイス\n{advice_texts}\n\n"
                        "上記を統合した実行可能な要約を作成してください。"
                    ),
                },
            ],
        )
        return str(response.get("content", ""))
