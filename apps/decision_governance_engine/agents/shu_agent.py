"""ShuAgent - 実行計画Agent（術）v3.0.

選択されたパスを具体的な実行フェーズに分解する。
RAG使用許可（industry_practices, case_studies）。
v2.0: 30天行動節奏控制（rhythm_control）を追加。
v3.0: 文脈特化行動・切り捨てリスト・単一検証ポイント・撤退基準を追加。

Skills統合: RAGSkillを使用して業界プラクティスとケーススタディを参照。

【術層の核心】
「準備→実行→検証→判断」は教科書。
真の術層は「何を捨てるか」「どこで止めるか」を明確にすること。
"""

import json
import logging
from typing import Any

from apps.decision_governance_engine.schemas.agent_schemas import (
    ActionPhase,
    ContextSpecificAction,
    ExitCriteria,
    FaOutput,
    FocusArea,
    PathOption,
    RhythmControl,
    RhythmPeriod,
    ShuInput,
    ShuOutput,
    SingleValidationPoint,
)

from agentflow import ResilientAgent
from agentflow.core.exceptions import AgentOutputValidationError
from agentflow.skills import RAGConfig, RAGSkill


class ShuAgent(ResilientAgent[ShuInput, ShuOutput]):
    """実行計画Agent v3.0（文脈特化・撤退基準対応）.

    職責:
    - フェーズ分解（3-5フェーズ）
    - 具体的行動の定義
    - 「最初の一歩」の明確化
    - 前提条件の整理
    - 30天行動節奏控制（rhythm_control）
    - **NEW: 文脈特化行動（この問題固有の行動）**
    - **NEW: 切り捨てリスト（最初の30日でやらないこと）**
    - **NEW: 単一検証ポイント（PoCで絶対に検証すべき1点）**
    - **NEW: 撤退基準（どこで止めるか）**

    【術層の核心】
    「準備→実行→検証→判断」は誰でも書ける教科書。
    真の術層は以下を明確にすること:
    - 最初の2週間で何を「捨てる」か
    - PoCで絶対に検証すべき「1点」
    - 失敗した場合どこで「止める」か

    許可:
    - RAG使用可（industry_practices, case_studies）
    """

    name = "ShuAgent"
    # timeout_seconds, max_retries, max_tokens は ResilientAgent のデフォルト値を使用
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
        # 設定は routers/config.py から動的に適用される
        self._rag: RAGSkill | None = None
        self._rag_config = RAGConfig(
            top_k=3,
            min_similarity=0.4,
            system_prompt="あなたは実行計画策定の参考情報を提供するアシスタントです。",
            context_template="参考事例:\n{context}\n\n計画対象: {query}",
        )

    async def initialize_rag(self) -> None:
        """RAGスキルを初期化・開始.

        routers/config.py の設定に基づいてRAGを初期化。
        use_rag=False の場合は初期化をスキップ。
        """
        from apps.decision_governance_engine.agents.rag_config_helper import (
            initialize_agent_rag,
        )

        if self._rag is None:
            self._rag = await initialize_agent_rag(self, "shu", self._rag_config)
            if self._rag:
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

    SYSTEM_PROMPT = """あなたはShuAgent（術）v3.0です。
選択された戦略を具体的な実行計画に落とし込みます。

【術層の核心原則】
「準備→実行→検証→判断」は誰でも書ける教科書。
真の術層は以下を明確にすること:
1. 最初の2週間で何を「捨てる」か（cut_list）
2. PoCで絶対に検証すべき「1点」（single_validation_point）
3. 失敗した場合どこで「止める」か（exit_criteria）
4. この問題「固有」の行動は何か（context_specific_actions）

【文脈特化の原則】
「チーム編成」「要件定義」は教科書的。以下のように文脈特化すること:
× 「要件定義」
○ 「WebRTC SFUベンダー3社の制約比較」（国際音声会議の場合）
× 「パイロット運用」
○ 「中国-EU間レイテンシ実測」

【切り捨てリストの原則】
最初の30日間で「やらないこと」を明確にする。これが最も重要。
- スコープクリープを防ぐ
- チームのフォーカスを保つ
- リソースの分散を避ける

【出力形式】
必ず以下のJSON形式で出力してください：

**重要な制約**:
- phases: 3〜5個のフェーズ
- 各フェーズの actions: 最大5個まで（6個以上は禁止）
- 各フェーズの deliverables: 最大5個まで
- 各フェーズの success_criteria: 最大5個まで

{
    "phases": [
        {
            "phase_number": 1,
            "name": "調査・検証",
            "duration": "2週間",
            "actions": ["行動1", "行動2", "行動3"],
            "deliverables": ["成果物1"],
            "success_criteria": ["完了条件1"]
        }
    ],
    "first_action": "明日できる具体的な一歩",
    "dependencies": ["前提条件1", "前提条件2"],
    "rhythm_control": {
        "period": "MONTH_1",
        "focus": {
            "name": "聚焦名称（20字以内）",
            "description": "具体的に何をするか（100字以内）",
            "success_metric": "数値で測定可能な指標",
            "avoid_list": ["この期間やらないこと1", "やらないこと2"]
        },
        "checkpoint_date": "30天後",
        "checkpoint_criteria": ["評価基準1", "評価基準2"],
        "next_decision_point": "30日後に継続/ピボット/撤退を判断"
    },
    "cut_list": [
        "最初の30日間で明示的にやらないこと1",
        "やらないこと2",
        "やらないこと3"
    ],
    "context_specific_actions": [
        {
            "action": "この問題固有の具体的行動（教科書にない）",
            "why_this_context": "なぜこの文脈で必要か",
            "expected_output": "期待するアウトプット"
        }
    ],
    "single_validation_point": {
        "validation_target": "PoCで絶対に検証すべき1点",
        "success_criteria": "成功基準（数値で）",
        "failure_action": "失敗した場合の行動"
    },
    "exit_criteria": {
        "checkpoint": "チェックポイント（例: 30日後）",
        "exit_trigger": "撤退トリガー（数値で）",
        "exit_action": "撤退時の行動"
    }
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> ShuInput:
        """入力をパース."""
        if "fa_result" in input_data and isinstance(input_data["fa_result"], dict):
            input_data["fa_result"] = FaOutput(**input_data["fa_result"])

        # selected_path_id が未指定の場合、推奨パスの最初を自動選択
        if "selected_path_id" not in input_data:
            fa_result = input_data.get("fa_result")
            if fa_result and hasattr(fa_result, "recommended_paths") and fa_result.recommended_paths:
                input_data["selected_path_id"] = fa_result.recommended_paths[0].path_id
            else:
                # フォールバック
                input_data["selected_path_id"] = "path_auto"

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

    def validate_output(self, output: ShuOutput) -> bool:
        """出力の品質検証.

        Args:
            output: ShuAgent出力

        Returns:
            検証結果（True = 有効）
        """
        # フェーズ数の検証
        if len(output.phases) < 3:
            self._logger.warning(f"Output has only {len(output.phases)} phases (min 3)")
            return False

        # 各フェーズのactions数を検証
        for phase in output.phases:
            if len(phase.actions) == 0:
                self._logger.warning(f"Phase {phase.phase_number} has no actions")
                return False
            if len(phase.actions) > 5:
                self._logger.warning(f"Phase {phase.phase_number} has {len(phase.actions)} actions (max 5)")
                return False

        # first_action の検証
        if not output.first_action or not output.first_action.strip():
            self._logger.warning("first_action is empty")
            return False

        return True

    def _find_selected_path(self, fa_result: FaOutput, path_id: str) -> PathOption | None:
        """選択されたパスを検索."""
        for path in fa_result.recommended_paths:
            if path.path_id == path_id:
                return path
        # 見つからない場合は最初の推奨パス
        return fa_result.recommended_paths[0] if fa_result.recommended_paths else None

    async def _plan_with_llm(self, selected_path: PathOption | None, fa_result: FaOutput) -> ShuOutput:
        """LLMを使用した計画策定（v3.0: 文脈特化対応）."""
        path_info = ""
        strategy_type = "BALANCED"
        if selected_path:
            strategy_type = getattr(selected_path, "strategy_type", "BALANCED")
            path_info = f"""【選択パス】{selected_path.name}
【戦略タイプ】{strategy_type}
【説明】{selected_path.description}
【メリット】{', '.join(selected_path.pros)}
【デメリット】{', '.join(selected_path.cons)}"""

        # v3.0: 戦略的禁止事項を取得
        prohibitions_info = ""
        if fa_result.strategic_prohibitions:
            prohibitions_info = "\n【戦略的禁止事項】\n"
            for sp in fa_result.strategic_prohibitions:
                prohibitions_info += f"- ⛔ {sp.prohibition}\n"

        # v3.0: 差別化軸を取得
        differentiation_info = ""
        if fa_result.differentiation_axis:
            differentiation_info = f"""
【差別化軸】
- 勝負する軸: {fa_result.differentiation_axis.axis_name}
- 勝負しない軸: {fa_result.differentiation_axis.not_this_axis}"""

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
{prohibitions_info}
{differentiation_info}

【判断基準】{', '.join(fa_result.decision_criteria)}{rag_context}

上記のパスを実行計画に落とし込んでください。

【重要】以下を必ず含めること:
1. cut_list: 最初の30日間で明示的に「やらない」こと3つ
2. context_specific_actions: この問題「固有」の行動（教科書にない行動）
3. single_validation_point: PoCで絶対に検証すべき「1点」
4. exit_criteria: 失敗した場合の撤退基準

JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        # 詳細ログ: LLM生出力を記録
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

            # 業務ロジック検証・正規化（Pydantic検証前）
            self._validate_and_normalize_phases(data)

            phases = [ActionPhase(**p) for p in data.get("phases", [])]
            if len(phases) < 3:
                phases = self._generate_default_phases()

            # rhythm_controlをパース
            rhythm_control = self._parse_rhythm_control(
                data.get("rhythm_control", {}),
                selected_path,
            )

            # v3.0: 切り捨てリストをパース
            cut_list = data.get("cut_list", [])[:3]

            # v3.0: 文脈特化行動をパース
            context_specific_actions = []
            for csa in data.get("context_specific_actions", [])[:5]:
                context_specific_actions.append(ContextSpecificAction(
                    action=csa.get("action", "")[:50],
                    why_this_context=csa.get("why_this_context", "")[:50],
                    expected_output=csa.get("expected_output", "")[:30],
                ))

            # v3.0: 単一検証ポイントをパース
            single_validation_point = None
            if "single_validation_point" in data:
                svp = data["single_validation_point"]
                single_validation_point = SingleValidationPoint(
                    validation_target=svp.get("validation_target", "")[:50],
                    success_criteria=svp.get("success_criteria", "")[:50],
                    failure_action=svp.get("failure_action", "")[:50],
                )

            # v3.0: 撤退基準をパース
            exit_criteria = None
            if "exit_criteria" in data:
                ec = data["exit_criteria"]
                exit_criteria = ExitCriteria(
                    checkpoint=ec.get("checkpoint", "")[:30],
                    exit_trigger=ec.get("exit_trigger", "")[:50],
                    exit_action=ec.get("exit_action", "")[:50],
                )

            return ShuOutput(
                phases=phases[:5],
                first_action=data.get("first_action", "関係者とのキックオフMTGを設定する"),
                dependencies=data.get("dependencies", []),
                rhythm_control=rhythm_control,
                cut_list=cut_list,
                context_specific_actions=context_specific_actions,
                single_validation_point=single_validation_point,
                exit_criteria=exit_criteria,
            )
        except json.JSONDecodeError as e:
            self._logger.warning(f"LLM response parse failed: {e}")
            return self._plan_rule_based(selected_path)

    def _validate_and_normalize_phases(self, data: dict[str, Any]) -> None:
        """LLM出力のphasesを検証・正規化（Pydantic検証前）.

        - 長すぎるリストを截断
        - 必須フィールドの欠落をチェック
        - 空のフェーズを除去

        Args:
            data: 抽出されたJSONデータ（in-place で修正）

        Raises:
            AgentOutputValidationError: 修復不可能な問題がある場合
        """
        phases = data.get("phases", [])

        if not phases or not isinstance(phases, list):
            self._logger.warning(f"LLM returned empty or invalid phases: {phases}")
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="phases",
                expected="list with at least 3 phases",
                actual=f"empty or invalid: {phases}",
            )

        # 各フェーズを正規化
        normalized_phases = []
        for i, phase in enumerate(phases[:5]):  # 最大5フェーズ
            if not isinstance(phase, dict):
                self._logger.warning(f"Phase {i} is not a dict: {phase}")
                continue

            # actions の截断（max_length=5）
            actions = phase.get("actions", [])
            if isinstance(actions, list) and len(actions) > 5:
                self._logger.warning(
                    f"Phase {i} has {len(actions)} actions (max 5), truncating: {actions}"
                )
                phase["actions"] = actions[:5]

            # deliverables の截断
            deliverables = phase.get("deliverables", [])
            if isinstance(deliverables, list) and len(deliverables) > 5:
                phase["deliverables"] = deliverables[:5]

            # success_criteria の截断
            success_criteria = phase.get("success_criteria", [])
            if isinstance(success_criteria, list) and len(success_criteria) > 5:
                phase["success_criteria"] = success_criteria[:5]

            # 必須フィールドの存在チェック
            if not phase.get("phase_number"):
                phase["phase_number"] = i + 1
            if not phase.get("name"):
                phase["name"] = f"フェーズ{i + 1}"
            if not phase.get("duration"):
                phase["duration"] = "要定義"
            if not phase.get("actions"):
                phase["actions"] = ["要定義"]

            normalized_phases.append(phase)

        if len(normalized_phases) < 3:
            self._logger.warning(f"Only {len(normalized_phases)} valid phases after normalization")
            # 3フェーズ未満でも続行（_plan_with_llm でデフォルトフェーズが使用される）

        data["phases"] = normalized_phases

    def _plan_rule_based(self, selected_path: PathOption | None) -> ShuOutput:
        """ルールベース計画策定（v3.0: 文脈特化対応）."""
        phases = self._generate_default_phases()
        rhythm_control = self._generate_default_rhythm_control(selected_path)

        # v3.0: デフォルトの切り捨てリスト
        cut_list = self._generate_default_cut_list(selected_path)

        # v3.0: デフォルトの文脈特化行動（プレースホルダー）
        context_specific_actions = self._generate_default_context_specific_actions(selected_path)

        # v3.0: デフォルトの単一検証ポイント
        single_validation_point = SingleValidationPoint(
            validation_target="コア仮説の検証",
            success_criteria="成功基準を定量化して測定",
            failure_action="ピボットまたは撤退の判断会議を開催",
        )

        # v3.0: デフォルトの撤退基準
        exit_criteria = ExitCriteria(
            checkpoint="30日後",
            exit_trigger="検証ポイントが成功基準を50%以上下回る場合",
            exit_action="プロジェクト中断、振り返りレポート作成",
        )

        return ShuOutput(
            phases=phases,
            first_action="チームメンバーとの30分キックオフMTGを明日設定する",
            dependencies=[
                "経営層の承認",
                "必要な人員の確保",
                "予算の確定",
            ],
            rhythm_control=rhythm_control,
            cut_list=cut_list,
            context_specific_actions=context_specific_actions,
            single_validation_point=single_validation_point,
            exit_criteria=exit_criteria,
        )

    def _generate_default_cut_list(self, selected_path: PathOption | None) -> list[str]:
        """デフォルトの切り捨てリストを生成（v3.0）."""
        strategy_type = getattr(selected_path, "strategy_type", "BALANCED") if selected_path else "BALANCED"

        if strategy_type == "AGGRESSIVE":
            return [
                "過度なドキュメント作成",
                "完璧な設計の追求",
                "全ステークホルダーの100%合意待ち",
            ]
        if strategy_type == "CONSERVATIVE":
            return [
                "性急な本格実装",
                "検証不足でのスケール",
                "リスク評価なしの意思決定",
            ]
        return [
            "スコープクリープ（範囲拡大）",
            "並行作業による分散",
            "優先順位の頻繁な変更",
        ]

    def _generate_default_context_specific_actions(
        self, selected_path: PathOption | None
    ) -> list[ContextSpecificAction]:
        """デフォルトの文脈特化行動を生成（v3.0）."""
        # これはプレースホルダー。LLMがない場合の最低限の出力。
        # 実際の文脈特化はLLMに依存する。
        return [
            ContextSpecificAction(
                action="対象ドメインの制約マッピング",
                why_this_context="この問題固有の制約を把握するため",
                expected_output="制約一覧表",
            ),
            ContextSpecificAction(
                action="既存解との差分分析",
                why_this_context="なぜ既存解が使えないかを明確化",
                expected_output="比較分析レポート",
            ),
        ]

    def _parse_rhythm_control(
        self,
        data: dict[str, Any],
        selected_path: PathOption | None,
    ) -> RhythmControl:
        """LLM出力からRhythmControlをパース."""
        if not data:
            return self._generate_default_rhythm_control(selected_path)

        try:
            # focusをパース
            focus_data = data.get("focus", {})
            focus = FocusArea(
                name=focus_data.get("name", "初期目標達成")[:20],
                description=focus_data.get("description", "最初の30日間で達成すべき目標")[:100],
                success_metric=focus_data.get("success_metric", "目標達成率80%以上"),
                avoid_list=focus_data.get("avoid_list", [])[:3],
            )

            # periodをパース
            period_str = data.get("period", "MONTH_1")
            period = RhythmPeriod(period_str) if period_str in RhythmPeriod.__members__ else RhythmPeriod.MONTH_1

            return RhythmControl(
                period=period,
                focus=focus,
                checkpoint_date=data.get("checkpoint_date", "30天後"),
                checkpoint_criteria=data.get("checkpoint_criteria", [])[:3],
                next_decision_point=data.get("next_decision_point", "30日後に継続/ピボット/撤退を判断")[:100],
            )
        except Exception as e:
            self._logger.warning(f"RhythmControl parse failed: {e}")
            return self._generate_default_rhythm_control(selected_path)

    def _generate_default_rhythm_control(self, selected_path: PathOption | None) -> RhythmControl:
        """デフォルトのRhythmControlを生成."""
        path_name = selected_path.name if selected_path else "プロジェクト"
        strategy_type = getattr(selected_path, "strategy_type", "BALANCED") if selected_path else "BALANCED"

        # 戦略タイプに応じたデフォルト設定
        if strategy_type == "AGGRESSIVE":
            avoid_list = [
                "過度なリスク回避による遅延",
                "完璧主義による納期延長",
                "承認プロセスの複雑化",
            ]
            focus_name = "速攻MVP検証"
        elif strategy_type == "CONSERVATIVE":
            avoid_list = [
                "性急な判断による手戻り",
                "検証不足での本格投資",
                "ステークホルダー合意なしの実行",
            ]
            focus_name = "基盤固め"
        else:
            avoid_list = [
                "スコープクリープ（範囲拡大）",
                "並行作業による分散",
                "優先順位の頻繁な変更",
            ]
            focus_name = "第一フェーズ完了"

        return RhythmControl(
            period=RhythmPeriod.MONTH_1,
            focus=FocusArea(
                name=focus_name,
                description=f"{path_name}の初期フェーズを完了し、次の判断に必要なデータを収集する",
                success_metric="フェーズ1完了率100%、重大な阻害要因の特定",
                avoid_list=avoid_list,
            ),
            checkpoint_date="30天後",
            checkpoint_criteria=[
                "フェーズ1の完了度",
                "発見された課題の数と深刻度",
                "チームの実行能力評価",
            ],
            next_decision_point="30日後のレビューで、継続投資/計画修正/撤退を経営判断",
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

