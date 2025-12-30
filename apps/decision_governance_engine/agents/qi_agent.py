# -*- coding: utf-8 -*-
"""QiAgent - 技術実装Agent（器）.

実行計画を技術的な実装方針に変換する。
RAG使用許可（technical_docs, compliance）。

Skills統合: RAGSkillで技術ドキュメント・コンプライアンス情報を参照。
"""

import json
import logging
from typing import Any

from agentflow.skills.rag import RAGConfig, RAGSkill

from apps.decision_governance_engine.agents.base_agent import BaseDecisionAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    Implementation,
    QiInput,
    QiOutput,
    ShuOutput,
)


class QiAgent(BaseDecisionAgent[QiInput, QiOutput]):
    """技術実装Agent.

    職責:
    - 技術実装方針の策定
    - ツール推奨
    - 統合ポイントの特定
    - 技術負債警告

    禁止:
    - 抽象的提案
    - スコープ拡大
    - 升维思考（問題を大きくしない）

    許可:
    - RAG使用可（technical_docs, compliance）

    Skills統合:
    - RAGSkillで技術ドキュメントを参照
    """

    name = "QiAgent"
    max_tokens = 1200
    temperature = 0.6

    # RAG使用許可
    USE_RAG = True
    RAG_SOURCES = ["technical_docs", "compliance"]

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

        # RAGスキル初期化（技術ドキュメント向け）
        self._rag: RAGSkill | None = None
        self._rag_config = RAGConfig(
            top_k=3,
            min_similarity=0.4,
            system_prompt="技術実装の参考情報を提供します。",
            context_template="技術参考:\n{context}\n\n実装対象: {query}",
        )

    async def initialize_rag(self) -> None:
        """RAGスキルを初期化・開始."""
        if self._rag is None:
            self._rag = RAGSkill(rag_config=self._rag_config)
            await self._rag.start()
            await self._load_technical_docs()
            self._logger.info("RAG Skill initialized for QiAgent")

    async def _load_technical_docs(self) -> None:
        """技術ドキュメントをRAGに登録."""
        if not self._rag:
            return
        docs = [
            "FastAPIは非同期Webフレームワーク。Pydanticによる型検証とOpenAPI自動生成が特徴。",
            "Reactはコンポーネントベースのフロントエンドライブラリ。TypeScriptとの組み合わせが推奨。",
            "Docker化により環境差分を排除。CI/CDパイプラインとの統合が容易になる。",
            "AWS Lambdaはサーバーレス実行環境。コールドスタートに注意が必要。",
            "セキュリティでは認証（JWT）と認可（RBAC）の分離が重要。OWASP Top 10を参照。",
        ]
        for doc in docs:
            await self._rag.add_document(doc, topic="technical_docs")

    SYSTEM_PROMPT = """あなたはQiAgent（器）です。
実行計画を具体的な技術実装方針に変換します。

【あなたの唯一の責任】
各フェーズの行動を技術的にどう実現するかを具体化すること。

【禁止事項】
- 抽象的な提案は禁止
- スコープを拡大してはいけない
- 問題を大きくしてはいけない（升维思考禁止）

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "implementations": [
        {
            "component": "コンポーネント名",
            "technology": "使用技術",
            "estimated_effort": "見積もり工数",
            "risks": ["技術リスク1"]
        }
    ],
    "tool_recommendations": ["ツール1", "ツール2"],
    "integration_points": ["統合ポイント1"],
    "technical_debt_warnings": ["技術負債警告1"]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> QiInput:
        """入力をパース."""
        if "shu_result" in input_data and isinstance(input_data["shu_result"], dict):
            input_data["shu_result"] = ShuOutput(**input_data["shu_result"])
        return QiInput(**input_data)

    async def process(self, input_data: QiInput) -> QiOutput:
        """技術実装方針を策定."""
        shu_result = input_data.shu_result
        tech_constraints = input_data.tech_constraints

        if self._llm:
            return await self._plan_with_llm(shu_result, tech_constraints)

        return self._plan_rule_based(shu_result, tech_constraints)

    async def _plan_with_llm(
        self,
        shu_result: ShuOutput,
        tech_constraints: list[str],
    ) -> QiOutput:
        """LLMを使用した技術計画（RAG参照付き）."""
        phases_info = "\n".join(
            f"Phase {p.phase_number}: {p.name} ({p.duration})\n  Actions: {', '.join(p.actions)}"
            for p in shu_result.phases
        )

        # RAGから技術参考情報を取得
        rag_context = ""
        if self._rag and self.USE_RAG:
            try:
                actions_str = " ".join(
                    action for p in shu_result.phases for action in p.actions[:2]
                )
                rag_result = await self._rag.query(actions_str, topic="technical_docs")
                if rag_result.sources:
                    rag_context = f"\n\n【参考: 技術ドキュメント】\n{rag_result.context_used}"
                    self._logger.info(f"RAG retrieved {len(rag_result.sources)} sources")
            except Exception as e:
                self._logger.warning(f"RAG query failed: {e}")

        user_prompt = f"""【実行フェーズ】
{phases_info}

【技術制約】{', '.join(tech_constraints) if tech_constraints else "特になし"}{rag_context}

上記を技術実装方針に変換し、JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        try:
            json_match = response[response.find("{"):response.rfind("}") + 1]
            data = json.loads(json_match)

            implementations = [Implementation(**i) for i in data.get("implementations", [])]
            if not implementations:
                implementations = self._generate_default_implementations(shu_result)

            return QiOutput(
                implementations=implementations,
                tool_recommendations=data.get("tool_recommendations", []),
                integration_points=data.get("integration_points", []),
                technical_debt_warnings=data.get("technical_debt_warnings", []),
            )
        except json.JSONDecodeError:
            return self._plan_rule_based(shu_result, tech_constraints)

    def _plan_rule_based(
        self,
        shu_result: ShuOutput,
        tech_constraints: list[str],
    ) -> QiOutput:
        """ルールベース技術計画."""
        implementations = self._generate_default_implementations(shu_result)

        return QiOutput(
            implementations=implementations,
            tool_recommendations=[
                "プロジェクト管理: Jira / Linear",
                "コミュニケーション: Slack",
                "ドキュメント: Notion / Confluence",
            ],
            integration_points=[
                "既存システムとのAPI連携",
                "データベース移行",
                "認証・認可システム統合",
            ],
            technical_debt_warnings=[
                "短期間開発によるテストカバレッジ低下リスク",
                "ドキュメント不足による属人化リスク",
            ],
        )

    def _generate_default_implementations(self, shu_result: ShuOutput) -> list[Implementation]:
        """デフォルト実装要素を生成."""
        return [
            Implementation(
                component="コアシステム",
                technology="Python / FastAPI",
                estimated_effort="2人月",
                risks=["技術選定の妥当性", "スケーラビリティ"],
            ),
            Implementation(
                component="フロントエンド",
                technology="React / TypeScript",
                estimated_effort="1.5人月",
                risks=["UX設計の不確実性"],
            ),
            Implementation(
                component="インフラ",
                technology="AWS / Docker",
                estimated_effort="0.5人月",
                risks=["コスト見積もり精度"],
            ),
        ]

