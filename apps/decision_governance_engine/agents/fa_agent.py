# -*- coding: utf-8 -*-
"""FaAgent - 戦略選定Agent（法）v3.0.

稳健型 vs 激进型の対比を含む戦略パス評価。
v3.0: 戦略的禁止事項・差別化軸・既存解失敗理由を追加。

RAG使用禁止。最大2つの推奨パスのみ。

【法層の核心】
「法」とは「何をすべきか」ではなく「何を絶対にしてはいけないか」を定義すること。
"""

import json
import logging
from typing import Any

from agentflow import ResilientAgent
from apps.decision_governance_engine.schemas.agent_schemas import (
    DaoOutput,
    DifferentiationAxis,
    FaInput,
    FaOutput,
    PathComparisonMatrix,
    PathOption,
    ProblemNatureType,
    ProblemType,
    ReversibilityLevel,
    StrategicProhibition,
    StrategyType,
)


class FaAgent(ResilientAgent[FaInput, FaOutput]):
    """戦略選定Agent v3.0（戦略的禁止事項・差別化軸対応）.

    職責:
    - 稳健型（CONSERVATIVE）と激进型（AGGRESSIVE）の対照的なパスを提示
    - 各パスの適用条件・リスク・コスト・可逆性を評価
    - 比較マトリックスで数値化して可視化
    - 不推奨パスの明示
    - **NEW: 戦略的禁止事項（何を絶対にしてはいけないか）**
    - **NEW: 差別化軸（どこで勝負するか）**
    - **NEW: 既存解が使えない理由のサマリー**

    【法層の核心】
    「推奨」ではなく「約束」を定義する。
    「何をすべきか」ではなく「何を絶対にしてはいけないか」が戦略。

    制約:
    - 推奨パスは最大2個（稳健型 + 激进型を推奨）
    - 不推奨を必ず含める
    - 比較マトリックス必須
    - **戦略的禁止事項必須**
    """

    name = "FaAgent"
    # timeout_seconds, max_retries, max_tokens は ResilientAgent のデフォルト値を使用
    temperature = 0.4

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    # RAG使用禁止
    USE_RAG = False
    MAX_RECOMMENDED_PATHS = 2
    MUST_INCLUDE_REJECTED = True

    # 比較マトリックスの評価軸
    COMPARISON_DIMENSIONS = ["ROI", "リスク", "時間", "可逆性", "リソース効率"]

    # v3.0: 問題タイプ別の戦略的禁止事項テンプレート
    STRATEGIC_PROHIBITIONS_TEMPLATES: dict[ProblemType, list[dict[str, str]]] = {
        ProblemType.TRADE_OFF: [
            {
                "prohibition": "両立を試みる中途半端なアプローチ",
                "rationale": "リソースの分散により両方とも失敗するリスク",
                "consequence": "投資した全リソースの無駄",
            },
        ],
        ProblemType.RESOURCE_ALLOCATION: [
            {
                "prohibition": "検証なしの全額一括投資",
                "rationale": "前提が間違っていた場合の回復が不可能",
                "consequence": "事業継続の危機",
            },
        ],
        ProblemType.STRATEGY_DIRECTION: [
            {
                "prohibition": "独自技術・独自規格の開発",
                "rationale": "標準への適合コストと孤立リスク",
                "consequence": "エコシステムからの排除、保守コスト増大",
            },
            {
                "prohibition": "ステークホルダー合意なしの実行",
                "rationale": "実行段階での抵抗、キーパーソン離脱",
                "consequence": "プロジェクト中断、組織分裂",
            },
        ],
        ProblemType.TIMING_DECISION: [
            {
                "prohibition": "競合の動きを無視した遅延",
                "rationale": "市場機会の喪失、先行者優位の損失",
                "consequence": "市場シェア獲得不能",
            },
        ],
        ProblemType.RISK_ASSESSMENT: [
            {
                "prohibition": "最悪ケースを想定せずに進める",
                "rationale": "想定外の事態への対応不能",
                "consequence": "壊滅的ダメージ",
            },
        ],
    }

    SYSTEM_PROMPT = """あなたはFaAgent（法）v3.0です。
稳健型 vs 激进型の対比を含む戦略パス評価を行います。

【法層の核心原則】
「法」とは「何をすべきか」ではなく「何を絶対にしてはいけないか」を定義すること。
戦略的禁止事項がない戦略は、戦略ではなく「選択肢の羅列」に過ぎない。

【あなたの責任】
1. 2つの対照的な戦略パス（稳健型 + 激进型）を提示
2. **戦略的禁止事項（絶対にやってはいけないこと）を定義**
3. **差別化軸（どこで勝負するか/しないか）を明確化**
4. 既存解が使えない理由をサマリー

【戦略的禁止事項の例】
- 「独自音声技術の開発は禁止」（標準準拠が絶対条件）
- 「音声品質での差別化は禁止」（そこでは勝負しない）
- 「検証なしのフルコミットは禁止」（撤退余地を残す）

【差別化軸の例】
- 差別化する軸: 「法規制対応能力」「地域別制御」
- 差別化しない軸: 「音声品質」「基本機能」

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "recommended_paths": [
        {
            "path_id": "A",
            "name": "パス名（10字以内）",
            "description": "説明（100字以内）",
            "strategy_type": "CONSERVATIVE",
            "pros": ["メリット1", "メリット2"],
            "cons": ["デメリット1"],
            "suitable_conditions": ["適用条件1"],
            "risks": ["リスク1"],
            "costs": ["コスト1"],
            "time_to_value": "6ヶ月",
            "reversibility": "HIGH",
            "success_probability": 0.7
        }
    ],
    "rejected_paths": [...],
    "decision_criteria": ["ROI", "リスク", ...],
    "path_comparison": {
        "dimensions": ["ROI", "リスク", "時間", "可逆性", "リソース効率"],
        "scores": {
            "A": [3, 5, 2, 5, 4],
            "B": [5, 2, 5, 2, 3]
        },
        "recommendation_summary": "状況に応じた推奨サマリー"
    },
    "strategic_prohibitions": [
        {
            "prohibition": "禁止事項（50字以内）",
            "rationale": "なぜ禁止するか（戦略的理由）",
            "violation_consequence": "違反した場合の結果"
        }
    ],
    "differentiation_axis": {
        "axis_name": "差別化軸名",
        "why_this_axis": "なぜこの軸で差別化するか",
        "not_this_axis": "差別化しない軸"
    },
    "why_existing_fails": "既存の標準解が使えない理由（一文）"
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> FaInput:
        """入力をパース."""
        # DaoOutputがネストされている場合の処理
        if "dao_result" in input_data and isinstance(input_data["dao_result"], dict):
            input_data["dao_result"] = DaoOutput(**input_data["dao_result"])
        return FaInput(**input_data)

    async def process(self, input_data: FaInput) -> FaOutput:
        """戦略選定を実行."""
        dao_result = input_data.dao_result

        # LLMで分析（ある場合）
        if self._llm:
            return await self._analyze_with_llm(dao_result, input_data)

        # ルールベース分析
        return self._analyze_rule_based(dao_result, input_data)

    async def _analyze_with_llm(
        self, dao_result: DaoOutput, input_data: FaInput
    ) -> FaOutput:
        """LLMを使用した分析（v3.0: 戦略的禁止事項対応）."""
        # 死穴情報を取得
        death_traps_info = ""
        if dao_result.death_traps:
            death_traps_info = "\n【死穴（禁忌）】\n"
            for trap in dao_result.death_traps:
                death_traps_info += f"- ⚠️ {trap.action}（{trap.severity}）\n"

        # v3.0: 既存代替手段情報を取得
        existing_alternatives_info = ""
        if dao_result.existing_alternatives:
            existing_alternatives_info = "\n【既存代替手段とその限界】\n"
            for alt in dao_result.existing_alternatives:
                existing_alternatives_info += f"- {alt.name}: {alt.why_not_viable}\n"

        # v3.0: 本質導出プロセス情報を取得
        essence_derivation_info = ""
        if dao_result.essence_derivation:
            essence_derivation_info = f"""
【本質導出】
- 表面的問題: {dao_result.essence_derivation.surface_problem}
- 深層の理由: {dao_result.essence_derivation.underlying_why}
- 根本制約: {dao_result.essence_derivation.root_constraint}"""

        user_prompt = f"""【問題タイプ】{dao_result.problem_type.value}
【問題の本質的性質】{dao_result.problem_nature.value if dao_result.problem_nature else "不明"}
【本質】{dao_result.essence}
【不可変制約】{', '.join(dao_result.immutable_constraints)}
【時間軸】{input_data.time_horizon or "未指定"}
{essence_derivation_info}
{death_traps_info}
{existing_alternatives_info}

上記に基づき、以下を出力してください:
1. 稳健型と激进型の2つの戦略パス
2. **戦略的禁止事項**（この状況で絶対にやってはいけないこと）
3. **差別化軸**（どこで勝負するか/しないか）
4. 既存解が使えない理由のサマリー

JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        try:
            # JSON部分を抽出してパース（堅牢な抽出）
            from agentflow.utils import extract_json
            data = extract_json(response)
            if data is None:
                raise json.JSONDecodeError("No valid JSON found", response, 0)

            recommended = []
            for p in data.get("recommended_paths", [])[:2]:
                recommended.append(self._parse_path_option(p))

            rejected = []
            for p in data.get("rejected_paths", []):
                rejected.append(self._parse_path_option(p))

            # 比較マトリックス
            comparison = None
            if "path_comparison" in data:
                comparison = PathComparisonMatrix(**data["path_comparison"])

            # v3.0: 戦略的禁止事項をパース
            strategic_prohibitions = []
            for sp in data.get("strategic_prohibitions", [])[:3]:
                strategic_prohibitions.append(StrategicProhibition(
                    prohibition=sp.get("prohibition", "")[:50],
                    rationale=sp.get("rationale", "")[:100],
                    violation_consequence=sp.get("violation_consequence", "")[:50],
                ))

            # v3.0: 差別化軸をパース
            differentiation_axis = None
            if "differentiation_axis" in data:
                da = data["differentiation_axis"]
                differentiation_axis = DifferentiationAxis(
                    axis_name=da.get("axis_name", "")[:30],
                    why_this_axis=da.get("why_this_axis", "")[:100],
                    not_this_axis=da.get("not_this_axis", "")[:50],
                )

            return FaOutput(
                recommended_paths=recommended,
                rejected_paths=rejected,
                decision_criteria=data.get("decision_criteria", []),
                path_comparison=comparison,
                strategic_prohibitions=strategic_prohibitions,
                differentiation_axis=differentiation_axis,
                why_existing_fails=data.get("why_existing_fails", "")[:100],
            )
        except json.JSONDecodeError as e:
            self._logger.warning(f"LLM response parse failed: {e}")
            return self._analyze_rule_based(dao_result, input_data)

    def _parse_path_option(self, data: dict) -> PathOption:
        """パスオプションをパース."""
        return PathOption(
            path_id=data.get("path_id", "X"),
            name=data.get("name", "")[:10],
            description=data.get("description", "")[:100],
            strategy_type=StrategyType(data.get("strategy_type", "BALANCED")),
            pros=data.get("pros", [])[:3],
            cons=data.get("cons", [])[:3],
            suitable_conditions=data.get("suitable_conditions", [])[:3],
            risks=data.get("risks", [])[:3],
            costs=data.get("costs", [])[:3],
            time_to_value=data.get("time_to_value", ""),
            reversibility=ReversibilityLevel(data.get("reversibility", "MEDIUM")),
            success_probability=data.get("success_probability", 0.5),
        )

    def _analyze_rule_based(
        self, dao_result: DaoOutput, input_data: FaInput
    ) -> FaOutput:
        """ルールベース分析（v3.0: 戦略的禁止事項対応）."""
        # 問題タイプに応じたデフォルトパス生成
        recommended, rejected = self._generate_default_paths(
            dao_result.problem_type, dao_result
        )

        # 判断基準生成
        criteria = self._generate_criteria(dao_result)

        # 比較マトリックス生成
        comparison = self._generate_comparison_matrix(recommended, dao_result)

        # v3.0: 戦略的禁止事項を生成
        strategic_prohibitions = self._generate_strategic_prohibitions(
            dao_result.problem_type, dao_result
        )

        # v3.0: 差別化軸を生成
        differentiation_axis = self._generate_differentiation_axis(dao_result)

        # v3.0: 既存解が使えない理由をサマリー
        why_existing_fails = self._summarize_why_existing_fails(dao_result)

        return FaOutput(
            recommended_paths=recommended,
            rejected_paths=rejected,
            decision_criteria=criteria,
            path_comparison=comparison,
            strategic_prohibitions=strategic_prohibitions,
            differentiation_axis=differentiation_axis,
            why_existing_fails=why_existing_fails,
        )

    def _generate_strategic_prohibitions(
        self, problem_type: ProblemType, dao_result: DaoOutput
    ) -> list[StrategicProhibition]:
        """戦略的禁止事項を生成（v3.0）."""
        prohibitions = []

        # 問題タイプ別のテンプレートを取得
        templates = self.STRATEGIC_PROHIBITIONS_TEMPLATES.get(problem_type, [])
        for template in templates[:2]:
            prohibitions.append(StrategicProhibition(
                prohibition=template["prohibition"],
                rationale=template["rationale"],
                violation_consequence=template["consequence"],
            ))

        # 死穴から追加の禁止事項を生成
        if dao_result.death_traps:
            trap = dao_result.death_traps[0]
            if len(prohibitions) < 3:
                prohibitions.append(StrategicProhibition(
                    prohibition=trap.action[:50],
                    rationale=trap.reason[:100],
                    violation_consequence=f"{trap.severity}レベルのダメージ",
                ))

        # 制約主導型の場合、独自開発禁止を追加
        if dao_result.problem_nature == ProblemNatureType.CONSTRAINT_DRIVEN:
            if not any("独自" in p.prohibition for p in prohibitions):
                prohibitions.append(StrategicProhibition(
                    prohibition="独自技術・独自規格の開発",
                    rationale="標準への適合が長期的な保守性と互換性を確保",
                    violation_consequence="エコシステムからの孤立、保守コスト増大",
                ))

        return prohibitions[:3]

    def _generate_differentiation_axis(self, dao_result: DaoOutput) -> DifferentiationAxis:
        """差別化軸を生成（v3.0）."""
        # 問題の本質的性質に基づいて差別化軸を決定
        if dao_result.problem_nature == ProblemNatureType.REGULATORY_COMPLIANCE:
            return DifferentiationAxis(
                axis_name="法規制対応能力",
                why_this_axis="規制対応が本質的な課題であり、ここでの優位性が競合との差別化になる",
                not_this_axis="基本機能・価格（ここでは勝負しない）",
            )
        if dao_result.problem_nature == ProblemNatureType.CONSTRAINT_DRIVEN:
            return DifferentiationAxis(
                axis_name="固有制約への適合能力",
                why_this_axis="既存解が使えない制約に対応できることが唯一の差別化ポイント",
                not_this_axis="一般的な機能・性能（標準解と同じ土俵で戦わない）",
            )

        # デフォルト
        return DifferentiationAxis(
            axis_name="実行速度・柔軟性",
            why_this_axis="市場環境の変化に対応できる俊敏性が競争優位",
            not_this_axis="コスト（価格競争は避ける）",
        )

    def _summarize_why_existing_fails(self, dao_result: DaoOutput) -> str:
        """既存解が使えない理由をサマリー（v3.0）."""
        if dao_result.existing_alternatives:
            # 最初の代替手段の理由を使用
            alt = dao_result.existing_alternatives[0]
            return f"既存の{alt.name}は{alt.specific_constraint}のため使用不可"

        if dao_result.problem_nature == ProblemNatureType.CONSTRAINT_DRIVEN:
            return "既存の標準解が自社固有の制約条件を満たせない"

        if dao_result.problem_nature == ProblemNatureType.REGULATORY_COMPLIANCE:
            return "既存サービスが対象地域の法規制要件を満たしていない"

        return ""

    def _generate_default_paths(
        self,
        problem_type: ProblemType,
        dao_result: DaoOutput,
    ) -> tuple[list[PathOption], list[PathOption]]:
        """稳健型 vs 激进型のデフォルトパスを生成."""

        # 死穴に関する警告
        death_trap_warning = ""
        if dao_result.death_traps:
            death_trap_warning = f"（{dao_result.death_traps[0].action}を避ける）"

        # 稳健型パス（CONSERVATIVE）
        conservative = PathOption(
            path_id="A",
            name="段階投資",
            description=f"段階的に検証しながら投資を拡大{death_trap_warning}",
            strategy_type=StrategyType.CONSERVATIVE,
            pros=["リスク分散", "学習機会の確保", "軌道修正が容易"],
            cons=["成長が遅い", "機会損失リスク", "競合に先行される可能性"],
            suitable_conditions=[
                "市場が成熟していない",
                "試行錯誤の余地がある",
                "キャッシュフロー維持が重要",
            ],
            risks=["市場機会の喪失", "競合の先行", "チームモチベーション低下"],
            costs=["機会コスト", "追加の検証コスト"],
            time_to_value="9-12ヶ月",
            reversibility=ReversibilityLevel.HIGH,
            success_probability=0.75,
        )

        # 激进型パス（AGGRESSIVE）
        aggressive = PathOption(
            path_id="B",
            name="全力投入",
            description="リソースを集中投入し短期間で市場検証",
            strategy_type=StrategyType.AGGRESSIVE,
            pros=["市場先行", "スピード", "組織の活性化"],
            cons=["高リスク", "後戻り困難", "既存事業への影響"],
            suitable_conditions=[
                "市場の窓口期が短い",
                "競合が迫っている",
                "失敗しても回復可能な体力がある",
            ],
            risks=["投資回収失敗", "既存顧客離反", "チーム疲弊"],
            costs=["大規模初期投資", "既存事業の機会コスト"],
            time_to_value="3-6ヶ月",
            reversibility=ReversibilityLevel.LOW,
            success_probability=0.50,
        )

        recommended = [conservative, aggressive]

        # 不推奨パス（現状維持）
        rejected = [
            PathOption(
                path_id="X",
                name="現状維持",
                description="変化を避け現状を維持する選択",
                strategy_type=StrategyType.CONSERVATIVE,
                pros=["短期的な安定"],
                cons=["成長機会喪失", "市場変化への対応遅れ", "人材流出リスク"],
                suitable_conditions=[],
                risks=["長期的な競争力低下"],
                costs=["将来の成長機会の喪失"],
                time_to_value="N/A",
                reversibility=ReversibilityLevel.HIGH,
                success_probability=0.30,
            ),
        ]

        return recommended, rejected

    def _generate_criteria(self, dao_result: DaoOutput) -> list[str]:
        """判断基準を生成."""
        base_criteria = [
            "ROI（投資対効果）",
            "Risk Exposure（リスク露出度）",
            "Time to Value（価値実現時間）",
            "Reversibility（可逆性）",
        ]

        # 問題タイプに応じて追加
        if dao_result.problem_type == ProblemType.TIMING_DECISION:
            base_criteria.append("Market Timing（市場タイミング）")
        elif dao_result.problem_type == ProblemType.RESOURCE_ALLOCATION:
            base_criteria.append("Resource Efficiency（リソース効率）")
        elif dao_result.problem_type == ProblemType.TRADE_OFF:
            base_criteria.append("Stakeholder Alignment（ステークホルダー合意）")

        return base_criteria

    def _generate_comparison_matrix(
        self,
        recommended_paths: list[PathOption],
        dao_result: DaoOutput,
    ) -> PathComparisonMatrix:
        """比較マトリックスを生成."""
        scores: dict[str, list[int]] = {}

        for path in recommended_paths:
            if path.strategy_type == StrategyType.CONSERVATIVE:
                # 稳健型: ROI中、リスク低(高スコア)、時間遅、可逆性高、効率高
                scores[path.path_id] = [3, 5, 2, 5, 4]
            elif path.strategy_type == StrategyType.AGGRESSIVE:
                # 激进型: ROI高、リスク高(低スコア)、時間速、可逆性低、効率中
                scores[path.path_id] = [5, 2, 5, 2, 3]
            else:
                # バランス型
                scores[path.path_id] = [4, 3, 3, 3, 4]

        # 死穴を考慮したサマリー
        death_trap_note = ""
        if dao_result.death_traps:
            trap = dao_result.death_traps[0]
            death_trap_note = f" ⚠️ 注意: 「{trap.action}」は{trap.severity}レベルの禁忌。"

        summary = (
            "リスク許容度が高く市場機会を逃したくない場合は激进型（B）。"
            "安定性を重視し段階的に学習したい場合は稳健型（A）。"
            f"{death_trap_note}"
        )

        return PathComparisonMatrix(
            dimensions=self.COMPARISON_DIMENSIONS,
            scores=scores,
            recommendation_summary=summary[:200],
        )

    def validate_output(self, output: FaOutput) -> bool:
        """出力検証（v3.0: 戦略的禁止事項の検証を追加）."""
        # 推奨パスが存在するか
        if not output.recommended_paths:
            self._logger.warning("Validation failed: no recommended_paths")
            return False

        # 2つの対照的なパスがあるか確認（警告のみ）
        strategy_types = [p.strategy_type for p in output.recommended_paths]
        if len(set(strategy_types)) < 2:
            self._logger.warning(
                f"Validation warning: only {len(set(strategy_types))} strategy types"
            )
            # 警告のみ、通過させる

        # v3.0: 戦略的禁止事項があるか確認
        if not output.strategic_prohibitions:
            self._logger.warning(
                "Validation warning: strategic_prohibitions is empty"
            )
            # 警告のみ、通過させる（段階的移行のため）

        # v3.0: 差別化軸があるか確認
        if not output.differentiation_axis:
            self._logger.warning(
                "Validation warning: differentiation_axis is missing"
            )
            # 警告のみ、通過させる

        return True
