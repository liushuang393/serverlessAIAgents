"""ShuAgent - 実行計画Agent（術）v3.1.

選択されたパスを具体的な実行計画に落とし込む。
RAG使用許可（industry_practices, case_studies）。

v3.1改修方針:
- 術は「提案（実行計画）」であり、レビューではない。
- 「指摘」「警告」「修正提案」などの文体を禁止。
- 前提が整ったものとして「どう実施するか」「注意点」「分岐」を提示する。
- DoD→2段ロケット→分岐付きフェーズの構造で出力する。

Skills統合: RAGSkillを使用して業界プラクティスとケーススタディを参照。
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
    PhaseBranch,
    PoCDefinitionOfDone,
    PoCSuccessMetric,
    ProposalPhase,
    RhythmControl,
    RhythmPeriod,
    ShuInput,
    ShuOutput,
    SingleValidationPoint,
    StagePlan,
    TwoStageRocket,
)

from agentflow import ResilientAgent
from agentflow.core.exceptions import AgentOutputValidationError
from agentflow.core.type_safe import safe_enum
from agentflow.skills import RAGConfig, RAGSkill


class ShuAgent(ResilientAgent[ShuInput, ShuOutput]):
    """実行計画Agent v3.1（提案モード: DoD→2段ロケット→分岐付きフェーズ）.

    設計思想: 術は「提案（実行計画）」であり、レビューではない。
    前提が整ったものとして「どう実施するか」「注意点」「分岐」を提示する。

    職責:
    - PoC完成定義（Definition of Done）を先に固定する
    - 2段ロケット: Stage1(最小パイプライン) → Stage2(統制強化)
    - 各フェーズ: 目的/作業/成果物/計測/注意点/分岐（最低2つ）
    - 「最初の一歩」の明確化
    - 会議/承認は最小限にしテンプレ/チェックリスト/自動化で代替

    文体ルール:
    - 禁止: "〜が不足" "〜が未定義" "修正が必要"
    - 使用: "〜する。〜を行う。注意点は〜。"

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

    SYSTEM_PROMPT = """あなたはShuAgent（術）v3.1です。
術Agentは「提案（実行計画）」であり、レビューではない。
出力では「指摘」「警告」「修正提案」などの文体を禁止し、
前提が整ったものとして「どう実施するか」「注意点」「分岐」を提示する。

【必須の出力要件（v3.1）】
1) まず「PoCの完成定義（Definition of Done）」を固定する：
   - 体験条件（字幕/翻訳/TTSのどこまでを提供するか）
   - 成功指標（遅延・安定性・運用手間など、3～5個）
   - フォールバック（失敗時に何を落として成立させるか）

2) 実行計画は「学び最短」順に並べる（2段ロケット）：
   - Stage1：最小パイプライン成立を最短で確認
   - Stage2：統制（監査ログ/保持削除/権限）を段階的に厚くする
   ※重い作業は"ゲート通過後に追加"として扱う

3) 各フェーズは必ずこの形で書く：
   - 目的 / 作業 / 成果物 / 計測（どう測る） / 注意点 / 分岐（詰まった場合の代替）
   ※分岐は最低2つ用意する（例：字幕先行、翻訳後追い、準リアルタイム化等）

4) 会議や承認者の話は最小限にし、テンプレ・チェックリスト・自動化で代替する。

【文体ルール】
- "〜が不足" "〜が未定義" "修正が必要" のようなレビュー口調を使わない。
- "〜する。〜を行う。注意点は〜。" の実装提案口調で書く。

【出力形式】必ず以下のJSON形式で出力してください：
{
    "poc_definition_of_done": {
        "experience_conditions": ["体験条件1", "体験条件2"],
        "success_metrics": [
            {"metric_name": "指標名", "target_value": "目標値", "measurement_method": "計測方法"}
        ],
        "fallback_strategy": "失敗時に何を落として成立させるか"
    },
    "two_stage_rocket": {
        "stage1_minimal_pipeline": {
            "stage_name": "Stage1: 最小パイプライン成立",
            "objective": "最短で動く最小構成を確認する",
            "phases": [
                {
                    "phase_number": 1, "name": "フェーズ名", "duration": "期間",
                    "purpose": "このフェーズの目的",
                    "tasks": ["作業1", "作業2"],
                    "deliverables": ["成果物1"],
                    "measurement": "計測方法",
                    "notes": ["注意点1"],
                    "branches": [
                        {"branch_name": "代替案名", "trigger_condition": "この分岐に入る条件", "description": "内容"}
                    ]
                }
            ],
            "gate_criteria": ["次ステージへのゲート基準1"]
        },
        "stage2_governance": {
            "stage_name": "Stage2: 統制強化",
            "objective": "監査ログ/保持削除/権限を段階的に厚くする",
            "phases": [],
            "gate_criteria": []
        }
    },
    "phases": [
        {"phase_number": 1, "name": "名前", "duration": "期間",
         "actions": ["行動1"], "deliverables": ["成果物"], "success_criteria": ["条件"]}
    ],
    "first_action": "明日できる具体的な一歩",
    "dependencies": ["前提条件"],
    "cut_list": ["やらないこと1", "やらないこと2"],
    "exit_criteria": {
        "checkpoint": "30日後",
        "exit_trigger": "撤退トリガー",
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
            if (
                fa_result
                and hasattr(fa_result, "recommended_paths")
                and fa_result.recommended_paths
            ):
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
                self._logger.warning(
                    f"Phase {phase.phase_number} has {len(phase.actions)} actions (max 5)"
                )
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

    async def _plan_with_llm(
        self, selected_path: PathOption | None, fa_result: FaOutput
    ) -> ShuOutput:
        """LLMを使用した計画策定（v3.1: 提案モード）."""
        path_info = ""
        strategy_type = "BALANCED"
        if selected_path:
            strategy_type = getattr(selected_path, "strategy_type", "BALANCED")
            path_info = f"""【選択パス】{selected_path.name}
【戦略タイプ】{strategy_type}
【説明】{selected_path.description}
【メリット】{", ".join(selected_path.pros)}
【デメリット】{", ".join(selected_path.cons)}"""

        # 戦略的禁止事項を取得
        prohibitions_info = ""
        if fa_result.strategic_prohibitions:
            prohibitions_info = "\n【戦略的禁止事項（守る前提）】\n"
            for sp in fa_result.strategic_prohibitions:
                prohibitions_info += f"- {sp.prohibition}\n"

        # 競争優位仮説 / 差別化軸
        differentiation_info = ""
        comp_hyp = getattr(fa_result, "competitive_hypothesis", None)
        if comp_hyp:
            differentiation_info = f"\n【競争優位仮説】{comp_hyp.axis_name}"
        elif fa_result.differentiation_axis:
            differentiation_info = f"\n【差別化軸】{fa_result.differentiation_axis.axis_name}"

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

【判断基準】{", ".join(fa_result.decision_criteria)}{rag_context}

上記のパスを実行計画に落とし込む。提案口調で書くこと。

【v3.1 必須出力】
1. poc_definition_of_done: PoC完成定義（体験条件 + 成功指標3-5個 + フォールバック）
2. two_stage_rocket: 2段ロケット（Stage1: 最小パイプライン成立 → Stage2: 統制強化）
   - 各ステージにphases（目的/作業/成果物/計測/注意点/分岐）を含める
   - 各フェーズのbranchesは最低2つ用意する
3. phases: 全体フェーズ一覧（3-5個）
4. cut_list: 最初の30日間でやらないこと
5. exit_criteria: 撤退基準

JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        self._logger.debug(
            f"LLM raw response (first 500 chars): {response[:500] if response else 'EMPTY'}"
        )

        try:
            from agentflow.utils import extract_json

            data = extract_json(response)

            if data is None:
                self._logger.error(f"JSON extraction failed. Raw response: {response[:1000]}")
                msg = "No valid JSON found"
                raise json.JSONDecodeError(msg, response, 0)

            self._logger.debug(f"Extracted JSON keys: {list(data.keys())}")

            # phases の検証・正規化
            self._validate_and_normalize_phases(data)

            phases: list[ActionPhase] = []
            for i, p in enumerate(data.get("phases", [])[:5]):
                phase = self._parse_action_phase(p, default_phase_number=i + 1)
                if phase is not None:
                    phases.append(phase)
            if len(phases) < 3:
                phases = self._generate_default_phases()

            # rhythm_controlをパース
            rhythm_control = self._parse_rhythm_control(
                data.get("rhythm_control", {}),
                selected_path,
            )

            # v3.0: 切り捨てリスト
            cut_list = data.get("cut_list", [])[:3]

            # v3.0: 文脈特化行動
            context_specific_actions = []
            for csa in data.get("context_specific_actions", [])[:5]:
                if not isinstance(csa, dict):
                    continue
                context_specific_actions.append(
                    ContextSpecificAction(
                        action=str(csa.get("action", ""))[:50],
                        why_this_context=str(csa.get("why_this_context", ""))[:50],
                        expected_output=str(csa.get("expected_output", ""))[:30],
                    )
                )

            # v3.0: 単一検証ポイント
            single_validation_point = None
            if "single_validation_point" in data and isinstance(
                data["single_validation_point"], dict
            ):
                svp = data["single_validation_point"]
                single_validation_point = SingleValidationPoint(
                    validation_target=str(svp.get("validation_target", ""))[:50],
                    success_criteria=str(svp.get("success_criteria", ""))[:50],
                    failure_action=str(svp.get("failure_action", ""))[:50],
                )

            # v3.0: 撤退基準
            exit_criteria = None
            if "exit_criteria" in data and isinstance(data["exit_criteria"], dict):
                ec = data["exit_criteria"]
                exit_criteria = ExitCriteria(
                    checkpoint=str(ec.get("checkpoint", ""))[:30],
                    exit_trigger=str(ec.get("exit_trigger", ""))[:50],
                    exit_action=str(ec.get("exit_action", ""))[:50],
                )

            # v3.1: PoC完成定義をパース
            poc_dod = self._parse_poc_dod(data.get("poc_definition_of_done", {}))

            # v3.1: 2段ロケットをパース
            two_stage = self._parse_two_stage_rocket(data.get("two_stage_rocket", {}))

            # v3.1: 提案フェーズを抽出（2段ロケットのphasesから集約）
            proposal_phases = self._extract_proposal_phases(data, two_stage)

            return ShuOutput(
                phases=phases[:5],
                first_action=data.get("first_action", "最小パイプラインの技術選定を開始する"),
                dependencies=data.get("dependencies", []),
                rhythm_control=rhythm_control,
                cut_list=cut_list,
                context_specific_actions=context_specific_actions,
                single_validation_point=single_validation_point,
                exit_criteria=exit_criteria,
                poc_definition_of_done=poc_dod,
                two_stage_rocket=two_stage,
                proposal_phases=proposal_phases,
            )
        except (json.JSONDecodeError, ValueError, TypeError) as e:
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
                self._logger.warning(f"Phase {i} has {len(actions)} actions (max 5), truncating: {actions}")
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
        """ルールベース計画策定（v3.1: 提案モード対応）."""
        phases = self._generate_default_phases()
        rhythm_control = self._generate_default_rhythm_control(selected_path)
        cut_list = self._generate_default_cut_list(selected_path)
        context_specific_actions = self._generate_default_context_specific_actions(selected_path)

        single_validation_point = SingleValidationPoint(
            validation_target="コア仮説の検証",
            success_criteria="成功基準を定量化して測定",
            failure_action="ピボットまたは撤退の判断を実施する",
        )

        exit_criteria = ExitCriteria(
            checkpoint="30日後",
            exit_trigger="検証ポイントが成功基準を50%以上下回る場合",
            exit_action="プロジェクト中断、振り返りレポートを作成する",
        )

        # v3.1: デフォルトPoC完成定義
        poc_dod = self._generate_default_poc_dod()

        # v3.1: デフォルト2段ロケット
        two_stage = self._generate_default_two_stage_rocket()

        # v3.1: 提案フェーズ
        proposal_phases = self._generate_default_proposal_phases()

        return ShuOutput(
            phases=phases,
            first_action="最小パイプラインの技術選定を開始する",
            dependencies=["対象ドメインの基本要件確認", "開発環境の準備"],
            rhythm_control=rhythm_control,
            cut_list=cut_list,
            context_specific_actions=context_specific_actions,
            single_validation_point=single_validation_point,
            exit_criteria=exit_criteria,
            poc_definition_of_done=poc_dod,
            two_stage_rocket=two_stage,
            proposal_phases=proposal_phases,
        )

    def _generate_default_cut_list(self, selected_path: PathOption | None) -> list[str]:
        """デフォルトの切り捨てリストを生成（v3.0）."""
        strategy_type = (
            getattr(selected_path, "strategy_type", "BALANCED") if selected_path else "BALANCED"
        )

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
            period = safe_enum(
                RhythmPeriod,
                data.get("period", "MONTH_1"),
                RhythmPeriod.MONTH_1,
            )

            return RhythmControl(
                period=period,
                focus=focus,
                checkpoint_date=data.get("checkpoint_date", "30天後"),
                checkpoint_criteria=data.get("checkpoint_criteria", [])[:3],
                next_decision_point=data.get(
                    "next_decision_point", "30日後に継続/ピボット/撤退を判断"
                )[:100],
            )
        except Exception as e:
            self._logger.warning(f"RhythmControl parse failed: {e}")
            return self._generate_default_rhythm_control(selected_path)

    def _generate_default_rhythm_control(self, selected_path: PathOption | None) -> RhythmControl:
        """デフォルトのRhythmControlを生成."""
        path_name = selected_path.name if selected_path else "プロジェクト"
        strategy_type = (
            getattr(selected_path, "strategy_type", "BALANCED") if selected_path else "BALANCED"
        )

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

    def _parse_action_phase(self, phase_data: Any, default_phase_number: int) -> ActionPhase | None:
        """LLM出力の1フェーズを安全にActionPhaseへ変換."""
        if not isinstance(phase_data, dict):
            return None

        try:
            phase_number = int(phase_data.get("phase_number", default_phase_number))
        except (TypeError, ValueError):
            phase_number = default_phase_number

        actions_raw = phase_data.get("actions", [])
        actions = [str(a)[:50] for a in actions_raw if a is not None][:5] if isinstance(actions_raw, list) else []
        if not actions:
            actions = ["要定義"]

        deliverables_raw = phase_data.get("deliverables", [])
        deliverables = (
            [str(d)[:50] for d in deliverables_raw if d is not None][:5] if isinstance(deliverables_raw, list) else []
        )

        success_criteria_raw = phase_data.get("success_criteria", [])
        success_criteria = (
            [str(c)[:50] for c in success_criteria_raw if c is not None][:5]
            if isinstance(success_criteria_raw, list)
            else []
        )

        try:
            return ActionPhase(
                phase_number=phase_number,
                name=str(phase_data.get("name", f"フェーズ{default_phase_number}"))[:20],
                duration=str(phase_data.get("duration", "2週間"))[:30],
                actions=actions,
                deliverables=deliverables,
                success_criteria=success_criteria,
            )
        except Exception as e:
            self._logger.warning("ActionPhase parse failed, skip phase: %s", e)
            return None

    # --- v3.1 ヘルパーメソッド ---

    def _parse_poc_dod(self, data: dict[str, Any]) -> PoCDefinitionOfDone | None:
        """LLM出力からPoC完成定義をパースする."""
        if not data or not isinstance(data, dict):
            return None
        try:
            metrics_raw = data.get("success_metrics", [])
            metrics = []
            for m in metrics_raw[:5]:
                if isinstance(m, dict):
                    metrics.append(
                        PoCSuccessMetric(
                            metric_name=str(m.get("metric_name", ""))[:50],
                            target_value=str(m.get("target_value", ""))[:50],
                            measurement_method=str(m.get("measurement_method", ""))[:80],
                        )
                    )
            if len(metrics) < 3:
                # 最低3個を確保するためデフォルトを追加
                defaults = [
                    PoCSuccessMetric(
                        metric_name="主要KPI", target_value="要定義", measurement_method="要定義"
                    ),
                    PoCSuccessMetric(
                        metric_name="安定性", target_value="要定義", measurement_method="要定義"
                    ),
                    PoCSuccessMetric(
                        metric_name="運用負荷", target_value="要定義", measurement_method="要定義"
                    ),
                ]
                metrics.extend(defaults[: (3 - len(metrics))])
            return PoCDefinitionOfDone(
                experience_conditions=data.get("experience_conditions", ["要定義"])[:5],
                success_metrics=metrics[:5],
                fallback_strategy=str(data.get("fallback_strategy", "要定義"))[:200],
            )
        except Exception as e:
            self._logger.warning(f"PoC DoD parse failed: {e}")
            return None

    def _parse_proposal_phase(self, p: dict[str, Any]) -> ProposalPhase:
        """辞書から提案フェーズをパースする."""
        branches = []
        for b in p.get("branches", [])[:5]:
            if isinstance(b, dict):
                branches.append(
                    PhaseBranch(
                        branch_name=str(b.get("branch_name", ""))[:50],
                        trigger_condition=str(b.get("trigger_condition", ""))[:80],
                        description=str(b.get("description", ""))[:150],
                    )
                )
        return ProposalPhase(
            phase_number=int(p.get("phase_number", 1)),
            name=str(p.get("name", ""))[:50],
            duration=str(p.get("duration", ""))[:30],
            purpose=str(p.get("purpose", ""))[:100],
            tasks=p.get("tasks", p.get("actions", ["要定義"]))[:5],
            deliverables=p.get("deliverables", [])[:5],
            measurement=str(p.get("measurement", ""))[:100],
            notes=p.get("notes", [])[:3],
            branches=branches,
        )

    def _parse_stage_plan(self, data: dict[str, Any]) -> StagePlan | None:
        """辞書からステージ計画をパースする."""
        if not data or not isinstance(data, dict):
            return None
        try:
            phases = [self._parse_proposal_phase(p) for p in data.get("phases", [])[:5]]
            if not phases:
                return None
            return StagePlan(
                stage_name=str(data.get("stage_name", ""))[:50],
                objective=str(data.get("objective", ""))[:150],
                phases=phases,
                gate_criteria=data.get("gate_criteria", [])[:3],
            )
        except Exception as e:
            self._logger.warning(f"StagePlan parse failed: {e}")
            return None

    def _parse_two_stage_rocket(self, data: dict[str, Any]) -> TwoStageRocket | None:
        """LLM出力から2段ロケットをパースする."""
        if not data or not isinstance(data, dict):
            return None
        try:
            stage1 = self._parse_stage_plan(data.get("stage1_minimal_pipeline", {}))
            stage2 = self._parse_stage_plan(data.get("stage2_governance", {}))
            if not stage1 or not stage2:
                return None
            return TwoStageRocket(
                stage1_minimal_pipeline=stage1,
                stage2_governance=stage2,
            )
        except Exception as e:
            self._logger.warning(f"TwoStageRocket parse failed: {e}")
            return None

    def _extract_proposal_phases(self, data: dict[str, Any], two_stage: TwoStageRocket | None) -> list[ProposalPhase]:
        """2段ロケットまたはデータから提案フェーズを集約する."""
        # 2段ロケットから抽出
        if two_stage:
            result: list[ProposalPhase] = []
            result.extend(two_stage.stage1_minimal_pipeline.phases)
            result.extend(two_stage.stage2_governance.phases)
            return result[:10]
        # 直接のproposal_phasesフィールドから抽出
        raw_phases = data.get("proposal_phases", [])
        if raw_phases:
            return [self._parse_proposal_phase(p) for p in raw_phases[:10]]
        return []

    def _generate_default_poc_dod(self) -> PoCDefinitionOfDone:
        """デフォルトPoC完成定義を生成する（v3.1）."""
        return PoCDefinitionOfDone(
            experience_conditions=["コア機能が動作すること", "基本的なユーザー体験が成立すること"],
            success_metrics=[
                PoCSuccessMetric(
                    metric_name="主要機能動作率",
                    target_value="100%",
                    measurement_method="手動テスト",
                ),
                PoCSuccessMetric(
                    metric_name="応答遅延",
                    target_value="目標値以内",
                    measurement_method="計測ツール",
                ),
                PoCSuccessMetric(
                    metric_name="安定性",
                    target_value="1時間連続稼働",
                    measurement_method="長時間テスト",
                ),
            ],
            fallback_strategy="品質改善機能を後回しにし、コア体験のみで成立させる",
        )

    def _generate_default_two_stage_rocket(self) -> TwoStageRocket:
        """デフォルト2段ロケットを生成する（v3.1）."""
        stage1 = StagePlan(
            stage_name="Stage1: 最小パイプライン成立",
            objective="コア機能を最短で動かし、体験成立を確認する",
            phases=[
                ProposalPhase(
                    phase_number=1,
                    name="技術選定・環境構築",
                    duration="1週間",
                    purpose="最小構成に必要な技術を選定し開発環境を整える",
                    tasks=[
                        "技術候補のPoC比較",
                        "開発環境セットアップ",
                        "CI/CDパイプライン最小構成",
                    ],
                    deliverables=["技術選定結果", "動作する開発環境"],
                    measurement="全メンバーがローカルで動作確認できること",
                    notes=["完璧な環境を目指さず最小限で進める"],
                    branches=[
                        PhaseBranch(
                            branch_name="クラウドサービス利用",
                            trigger_condition="ローカル構築が複雑な場合",
                            description="マネージドサービスで代替する",
                        ),
                        PhaseBranch(
                            branch_name="既存環境流用",
                            trigger_condition="類似プロジェクトの環境がある場合",
                            description="既存環境をフォークして使用する",
                        ),
                    ],
                ),
                ProposalPhase(
                    phase_number=2,
                    name="コア機能実装・検証",
                    duration="2週間",
                    purpose="最小パイプラインを実装し体験成立を確認する",
                    tasks=["コア機能の実装", "結合テスト", "体験シナリオの確認"],
                    deliverables=["動作するコア機能", "テスト結果"],
                    measurement="PoC完成定義の成功指標を全て満たすこと",
                    notes=["品質よりも動作を優先する"],
                    branches=[
                        PhaseBranch(
                            branch_name="機能縮小",
                            trigger_condition="実装が予定より遅れる場合",
                            description="最小限の機能に絞って先にデモ可能にする",
                        ),
                        PhaseBranch(
                            branch_name="外部サービス活用",
                            trigger_condition="自前実装が困難な場合",
                            description="SaaS/APIを使い最短で動くものを作る",
                        ),
                    ],
                ),
            ],
            gate_criteria=["コア機能が動作すること", "PoC DoD成功指標を満たすこと"],
        )
        stage2 = StagePlan(
            stage_name="Stage2: 統制強化",
            objective="監査ログ/保持削除/権限を段階的に厚くする",
            phases=[
                ProposalPhase(
                    phase_number=3,
                    name="基本統制導入",
                    duration="2週間",
                    purpose="運用に必要な最低限の統制を導入する",
                    tasks=["監査ログ実装", "基本的な権限管理", "データ保持ポリシー適用"],
                    deliverables=["監査ログ出力", "権限設定ドキュメント"],
                    measurement="監査ログが正しく記録されていること",
                    notes=["Stage1の成果を壊さないよう段階的に追加する"],
                    branches=[
                        PhaseBranch(
                            branch_name="ログ最小化",
                            trigger_condition="ログ実装が重い場合",
                            description="相関IDとタイムスタンプのみの最小ログにする",
                        ),
                        PhaseBranch(
                            branch_name="外部ログ基盤",
                            trigger_condition="自前ログ基盤が困難な場合",
                            description="Datadog/CloudWatch等を利用する",
                        ),
                    ],
                ),
            ],
            gate_criteria=["統制要件の最低限が満たされていること"],
        )
        return TwoStageRocket(stage1_minimal_pipeline=stage1, stage2_governance=stage2)

    def _generate_default_proposal_phases(self) -> list[ProposalPhase]:
        """デフォルト提案フェーズを生成する（v3.1: 2段ロケットから集約）."""
        two_stage = self._generate_default_two_stage_rocket()
        result: list[ProposalPhase] = []
        result.extend(two_stage.stage1_minimal_pipeline.phases)
        result.extend(two_stage.stage2_governance.phases)
        return result
