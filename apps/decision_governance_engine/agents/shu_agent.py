# -*- coding: utf-8 -*-
"""ShuAgent - 実行計画Agent（術）.

選択されたパスを具体的な実行フェーズに分解する。
RAG使用許可（industry_practices, case_studies）。

Skills統合: RAGSkillを使用して業界プラクティスとケーススタディを参照。
"""

import json
import logging
from typing import Any

from agentflow.skills.rag import RAGConfig, RAGSkill

from apps.decision_governance_engine.agents.base_agent import BaseDecisionAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    ActionPhase,
    FaOutput,
    PathOption,
    ShuInput,
    ShuOutput,
)


class ShuAgent(BaseDecisionAgent[ShuInput, ShuOutput]):
    """実行計画Agent.

    職責:
    - フェーズ分解（3-5フェーズ）
    - 具体的行動の定義
    - 「最初の一歩」の明確化
    - 前提条件の整理

    許可:
    - RAG使用可（industry_practices, case_studies）

    Skills統合:
    - RAGSkillで業界プラクティスを参照
    """

    name = "ShuAgent"
    max_tokens = 1000
    temperature = 0.5

    # RAG使用許可
    USE_RAG = True
    RAG_SOURCES = ["industry_practices", "case_studies"]

    def __init__(self, llm_client: Any = None) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（オプション）
        """
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

        # RAGスキル初期化（業界プラクティス・ケーススタディ向け）
        self._rag: RAGSkill | None = None
        self._rag_config = RAGConfig(
            top_k=3,
            min_similarity=0.4,
            system_prompt="あなたは実行計画策定の参考情報を提供するアシスタントです。",
            context_template="参考事例:\n{context}\n\n計画対象: {query}",
        )

    async def initialize_rag(self) -> None:
        """RAGスキルを初期化・開始."""
        if self._rag is None:
            self._rag = RAGSkill(rag_config=self._rag_config)
            await self._rag.start()
            # 業界プラクティスを登録（初期データ）
            await self._load_industry_practices()
            self._logger.info("RAG Skill initialized for ShuAgent")

    async def _load_industry_practices(self) -> None:
        """業界プラクティスをRAGに登録."""
        if not self._rag:
            return
        practices = [
            "アジャイル開発では2週間のスプリントが推奨される。計画・実行・振り返りのサイクルを回す。",
            "新規事業立ち上げではリーンスタートアップ手法が効果的。MVP→検証→ピボットのサイクル。",
            "プロジェクト管理ではWBSによるタスク分解と、マイルストーン設定が重要。",
            "チーム編成では、初期メンバーは3-5名のスモールチームから始めるのが効果的。",
            "リスク管理では、週次でリスクレビューを実施し、早期発見・早期対応を心がける。",
        ]
        for practice in practices:
            await self._rag.add_document(practice, topic="industry_practices")

    SYSTEM_PROMPT = """あなたはShuAgent（術）です。
選択された戦略を具体的な実行計画に落とし込みます。

【あなたの唯一の責任】
戦略パスを3-5つのフェーズに分解し、明日から実行可能な「最初の一歩」を明確にすること。

【制約】
- フェーズは3-5個
- 各フェーズには具体的な行動（max 5）を含める
- 「最初の一歩」は明日実行可能なこと

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "phases": [
        {
            "phase_number": 1,
            "name": "準備",
            "duration": "2週間",
            "actions": ["行動1", "行動2"],
            "deliverables": ["成果物1"],
            "success_criteria": ["完了条件1"]
        }
    ],
    "first_action": "明日できる具体的な一歩",
    "dependencies": ["前提条件1", "前提条件2"]
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> ShuInput:
        """入力をパース."""
        if "fa_result" in input_data and isinstance(input_data["fa_result"], dict):
            input_data["fa_result"] = FaOutput(**input_data["fa_result"])
        return ShuInput(**input_data)

    async def process(self, input_data: ShuInput) -> ShuOutput:
        """実行計画を策定."""
        fa_result = input_data.fa_result
        selected_path_id = input_data.selected_path_id

        # 選択されたパスを取得
        selected_path = self._find_selected_path(fa_result, selected_path_id)

        if self._llm:
            return await self._plan_with_llm(selected_path, fa_result)

        return self._plan_rule_based(selected_path)

    def _find_selected_path(self, fa_result: FaOutput, path_id: str) -> PathOption | None:
        """選択されたパスを検索."""
        for path in fa_result.recommended_paths:
            if path.path_id == path_id:
                return path
        # 見つからない場合は最初の推奨パス
        return fa_result.recommended_paths[0] if fa_result.recommended_paths else None

    async def _plan_with_llm(self, selected_path: PathOption | None, fa_result: FaOutput) -> ShuOutput:
        """LLMを使用した計画策定（RAG参照付き）."""
        path_info = ""
        if selected_path:
            path_info = f"""【選択パス】{selected_path.name}
【説明】{selected_path.description}
【メリット】{', '.join(selected_path.pros)}
【デメリット】{', '.join(selected_path.cons)}"""

        # RAGから業界プラクティスを取得
        rag_context = ""
        if self._rag and self.USE_RAG:
            try:
                query = f"{selected_path.name if selected_path else '実行計画'} 実装方法"
                rag_result = await self._rag.query(query, topic="industry_practices")
                if rag_result.sources:
                    rag_context = f"\n\n【参考: 業界プラクティス】\n{rag_result.context_used}"
                    self._logger.info(f"RAG retrieved {len(rag_result.sources)} sources")
            except Exception as e:
                self._logger.warning(f"RAG query failed: {e}")

        user_prompt = f"""{path_info}

【判断基準】{', '.join(fa_result.decision_criteria)}{rag_context}

上記のパスを実行計画に落とし込み、JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        try:
            json_match = response[response.find("{"):response.rfind("}") + 1]
            data = json.loads(json_match)

            phases = [ActionPhase(**p) for p in data.get("phases", [])]
            if len(phases) < 3:
                phases = self._generate_default_phases()

            return ShuOutput(
                phases=phases[:5],
                first_action=data.get("first_action", "関係者とのキックオフMTGを設定する"),
                dependencies=data.get("dependencies", []),
            )
        except json.JSONDecodeError:
            return self._plan_rule_based(selected_path)

    def _plan_rule_based(self, selected_path: PathOption | None) -> ShuOutput:
        """ルールベース計画策定."""
        phases = self._generate_default_phases()

        return ShuOutput(
            phases=phases,
            first_action="チームメンバーとの30分キックオフMTGを明日設定する",
            dependencies=[
                "経営層の承認",
                "必要な人員の確保",
                "予算の確定",
            ],
        )

    def _generate_default_phases(self) -> list[ActionPhase]:
        """デフォルトフェーズを生成."""
        return [
            ActionPhase(
                phase_number=1,
                name="準備・計画",
                duration="2週間",
                actions=["チーム編成", "詳細要件定義", "リスク洗い出し"],
                deliverables=["プロジェクト計画書"],
                success_criteria=["計画承認"],
            ),
            ActionPhase(
                phase_number=2,
                name="実行・開発",
                duration="2ヶ月",
                actions=["コア機能実装", "テスト", "レビュー"],
                deliverables=["MVP/成果物"],
                success_criteria=["品質基準クリア"],
            ),
            ActionPhase(
                phase_number=3,
                name="検証・評価",
                duration="1ヶ月",
                actions=["パイロット運用", "フィードバック収集", "改善"],
                deliverables=["評価レポート"],
                success_criteria=["目標KPI達成"],
            ),
            ActionPhase(
                phase_number=4,
                name="判断・次ステップ",
                duration="2週間",
                actions=["Go/No-Go判定", "スケール計画策定"],
                deliverables=["判定結果", "次期計画"],
                success_criteria=["経営判断完了"],
            ),
        ]

