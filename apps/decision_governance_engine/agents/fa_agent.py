"""FaAgent - 戦略選定Agent（法）v3.1.

根拠駆動・可実行な戦略パス評価。
v3.1: 確率禁止→条件付き評価、差別化仮説化、4案以上、Must/Should分離、禁止事項仕組み化。

RAG使用禁止。

【法層の核心】
「法」とは「何をすべきか」ではなく「何を絶対にしてはいけないか」を定義すること。
"""

import json
import logging
from typing import Any

from apps.decision_governance_engine.schemas.agent_schemas import (
    CompetitiveHypothesis,
    ConditionalEvaluation,
    DaoOutput,
    DifferentiationAxis,
    FaInput,
    FaOutput,
    FaSelfCheckResult,
    JudgmentFramework,
    MustGate,
    PathComparisonMatrix,
    PathOption,
    ProblemNatureType,
    ProblemType,
    ReversibilityLevel,
    SelfCheckStatus,
    ShouldCriterion,
    StrategicProhibition,
    StrategyType,
)

from agentflow import ResilientAgent
from agentflow.core.exceptions import AgentOutputValidationError
from agentflow.core.type_safe import safe_enum


class FaAgent(ResilientAgent[FaInput, FaOutput]):
    """戦略選定Agent v3.1（根拠駆動・可実行）.

    職責:
    - 最低4案の戦略オプション（中間案含む）を提示
    - 各案に成立条件/リスク集中点/可逆性を評価
    - **確率%禁止** → 条件付き評価（成立条件/リスク要因/失敗モード）
    - **差別化軸仮説化** → 検証計画付き競争優位仮説
    - **Must/Should分離** → Mustゲート（閾値）＋Should採点（重み）
    - **禁止事項仕組み化** → 防止策・検知指標・責任者付き
    - **セルフチェック** → 根拠不足/中間案漏れ/ゲート不在/数値の見せかけ

    制約:
    - 戦略オプション最低4案（中間案必須）
    - 比較マトリックス必須
    - 戦略的禁止事項必須（仕組み付き）
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
    MIN_STRATEGIC_OPTIONS = 4
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

    SYSTEM_PROMPT = """あなたはFaAgent（法）v3.1です。
根拠駆動・可実行な戦略選定を行います。

【法層の核心原則】
「法」とは「何をすべきか」ではなく「何を絶対にしてはいけないか」を定義すること。
戦略的禁止事項がない戦略は、戦略ではなく「選択肢の羅列」に過ぎない。

【v3.1 改修方針】

方針1: 「成功確率◯%」の表現は禁止
- 数値確率を出さず、代わりに条件付き評価を使う:
  - 成立条件（満たせば成功確率↑）
  - 主要リスク要因（失敗確率↑）
  - 代表的失敗モード（どう壊れるか）
- どうしても数値を出す場合は算定式/前提/根拠を必須化する

方針2: 差別化軸は「主張」で終わらせず、競争優位仮説として書く
  - 具体的な対象顧客/利用シーン
  - 代替が難しい理由（何が障壁か）
  - 勝ち筋指標（何で勝ったと判定するか）
  - 最小検証（誰に何を当ててどう測るか）

方針3: 戦略オプションは最低4案（中間案必須）
  - 内製統合（自前構築）
  - 専用アプライアンス（特化型製品導入）
  - 旁路/プラグイン（既存基盤は維持し一部だけ別系統）
  - ハイブリッド（一部機能のみ受託/管理クラウド等）
  各案に「成立条件/コスト/リスク集中点/可逆性」を書く

方針4: 判断基準はMust/Shouldに分離
  - Must（不可変ゲート）：満たさなければ即却下（閾値を明記）
  - Should（比較評価）：重み（High/Med/Low）と採点方法を付ける
  各案がゲートを通るか、比較で何点かを出す

方針5: 禁止事項は「守る仕組み」まで落とす
  各禁止事項に:
  - 防止策（レビュー/手順/チェックリスト）
  - 検知指標（証跡/ログ/監査項目）
  - 責任者（Roleで可）

【出力形式（JSON）】
{
    "strategic_prohibitions": [
        {
            "prohibition": "禁止事項（50字以内）",
            "rationale": "なぜ禁止するか",
            "violation_consequence": "違反結果",
            "prevention_measure": "防止策",
            "detection_metric": "検知指標",
            "responsible_role": "責任者Role"
        }
    ],
    "competitive_hypothesis": {
        "axis_name": "差別化軸名",
        "target_customer": "対象顧客/利用シーン",
        "substitution_barrier": "代替困難な理由",
        "winning_metric": "勝ち筋指標",
        "minimum_verification": "最小検証方法"
    },
    "recommended_paths": [
        {
            "path_id": "A",
            "name": "パス名（20字以内）",
            "description": "説明（100字以内）",
            "strategy_type": "CONSERVATIVE",
            "pros": ["メリット1"],
            "cons": ["デメリット1"],
            "suitable_conditions": ["成立条件1"],
            "risks": ["リスク1"],
            "costs": ["コスト1"],
            "time_to_value": "6ヶ月",
            "reversibility": "HIGH",
            "risk_concentration": "リスク集中点",
            "conditional_evaluation": {
                "success_conditions": ["条件1"],
                "risk_factors": ["要因1"],
                "failure_modes": ["失敗モード1"],
                "probability_basis": ""
            }
        }
    ],
    "rejected_paths": [...],
    "judgment_framework": {
        "must_gates": [
            {"criterion": "基準名", "threshold": "閾値"}
        ],
        "should_criteria": [
            {"criterion": "基準名", "weight": "High", "scoring_method": "採点方法"}
        ],
        "gate_results": {"A": [true, true], "B": [true, false]},
        "should_scores": {"A": [4, 3], "B": [3, 5]}
    },
    "path_comparison": {
        "dimensions": ["ROI", "リスク", "時間", "可逆性", "リソース効率"],
        "scores": {"A": [3,5,2,5,4], "B": [5,2,5,2,3]},
        "recommendation_summary": "比較サマリー"
    },
    "fa_self_check": {
        "baseless_numbers": [],
        "missing_intermediate": [],
        "missing_gates": [],
        "appearance_precision": [],
        "overall_status": "PASS"
    },
    "why_existing_fails": "既存解が使えない理由"
}

【厳守事項】
- recommended_paths: 最低4案（中間案/ハイブリッド案を含む）
- success_probability フィールドは使わない（conditional_evaluation を使用）
- 判断基準はMust/Should分離（judgment_framework）
- 禁止事項は防止策・検知・責任者付き
- 最後にセルフチェック（fa_self_check）を実行"""

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

    async def _analyze_with_llm(self, dao_result: DaoOutput, input_data: FaInput) -> FaOutput:
        """LLMを使用した分析（v3.1: 根拠駆動・可実行）."""
        # 道Agent結果の情報を収集
        death_traps_info = ""
        if dao_result.death_traps:
            death_traps_info = "\n【死穴（禁忌）】\n"
            for trap in dao_result.death_traps:
                death_traps_info += f"- ⚠️ {trap.action}（{trap.severity}）\n"

        existing_alternatives_info = ""
        if dao_result.existing_alternatives:
            existing_alternatives_info = "\n【既存代替手段とその限界】\n"
            for alt in dao_result.existing_alternatives:
                existing_alternatives_info += f"- {alt.name}: {alt.why_not_viable}\n"

        essence_derivation_info = ""
        if dao_result.essence_derivation:
            essence_derivation_info = (
                f"\n【本質導出】\n"
                f"- 表面的問題: {dao_result.essence_derivation.surface_problem}\n"
                f"- 深層の理由: {dao_result.essence_derivation.underlying_why}\n"
                f"- 根本制約: {dao_result.essence_derivation.root_constraint}"
            )

        # v3.1: 制約境界条件・成立ルート・定量指標の情報
        boundaries_info = ""
        if dao_result.constraint_boundaries:
            boundaries_info = "\n【制約境界条件】\n"
            for cb in dao_result.constraint_boundaries:
                boundaries_info += f"- {cb.constraint_name}: {cb.definition}\n"

        routes_info = ""
        if dao_result.solution_routes:
            routes_info = "\n【成立ルート（道Agentの探索結果）】\n"
            for sr in dao_result.solution_routes:
                routes_info += (
                    f"- [{sr.route_type}] {sr.description} (実現可能性: {sr.viability})\n"
                )

        user_prompt = f"""【問題タイプ】{dao_result.problem_type.value}
【問題の本質的性質】{dao_result.problem_nature.value if dao_result.problem_nature else "不明"}
【本質】{dao_result.essence}
【不可変制約】{", ".join(dao_result.immutable_constraints)}
【時間軸】{input_data.time_horizon or "未指定"}
{essence_derivation_info}
{death_traps_info}
{existing_alternatives_info}
{boundaries_info}
{routes_info}

v3.1必須出力（JSON）:
1. 戦略的禁止事項（防止策・検知指標・責任者付き）
2. 競争優位仮説（対象顧客・代替障壁・勝ち筋指標・最小検証）
3. 戦略オプション最低4案（内製/アプライアンス/旁路/ハイブリッド等、条件付き評価付き）
4. 判断フレームワーク（Mustゲート＋Should採点）
5. 比較マトリックス
6. セルフチェック
7. 既存解が使えない理由

JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")
        self._logger.debug(
            "LLM raw response (first 500 chars): %s", response[:500] if response else "EMPTY"
        )

        try:
            from agentflow.utils import extract_json

            data = extract_json(response)

            if data is None:
                self._logger.error("JSON extraction failed. Raw response: %s", response[:1000])
                msg = "No valid JSON found"
                raise json.JSONDecodeError(msg, response, 0)

            self._logger.debug("Extracted JSON keys: %s", list(data.keys()))
            self._validate_and_normalize_llm_output(data)

            # 戦略オプションのパース
            recommended = []
            for p in data.get("recommended_paths", []):
                if not isinstance(p, dict):
                    continue
                recommended.append(self._parse_path_option(p))

            rejected = []
            for p in data.get("rejected_paths", []):
                if not isinstance(p, dict):
                    continue
                rejected.append(self._parse_path_option(p))

            # 比較マトリックス
            comparison = None
            if "path_comparison" in data:
                try:
                    comparison = PathComparisonMatrix(**data["path_comparison"])
                except Exception as e:
                    self._logger.warning("Invalid path_comparison, fallback to None: %s", e)

            # v3.1: 戦略的禁止事項（仕組み化付き）
            strategic_prohibitions = []
            for sp in data.get("strategic_prohibitions", []):
                if not isinstance(sp, dict):
                    continue
                strategic_prohibitions.append(
                    StrategicProhibition(
                        prohibition=sp.get("prohibition", "")[:50],
                        rationale=sp.get("rationale", "")[:100],
                        violation_consequence=sp.get("violation_consequence", "")[:50],
                        prevention_measure=sp.get("prevention_measure", "")[:100],
                        detection_metric=sp.get("detection_metric", "")[:100],
                        responsible_role=sp.get("responsible_role", "")[:30],
                    )
                )

            # v3.1: 競争優位仮説
            competitive_hypothesis = None
            ch_data = data.get("competitive_hypothesis")
            if ch_data and isinstance(ch_data, dict):
                competitive_hypothesis = CompetitiveHypothesis(
                    axis_name=ch_data.get("axis_name", "")[:30],
                    target_customer=ch_data.get("target_customer", "")[:100],
                    substitution_barrier=ch_data.get("substitution_barrier", "")[:100],
                    winning_metric=ch_data.get("winning_metric", "")[:100],
                    minimum_verification=ch_data.get("minimum_verification", "")[:150],
                )

            # v3.1: 判断フレームワーク
            judgment_framework = None
            jf_data = data.get("judgment_framework")
            if jf_data and isinstance(jf_data, dict):
                must_gates = [
                    MustGate(
                        criterion=mg.get("criterion", "")[:50],
                        threshold=mg.get("threshold", "")[:100],
                    )
                    for mg in jf_data.get("must_gates", [])
                    if isinstance(mg, dict)
                ]
                should_criteria = [
                    ShouldCriterion(
                        criterion=sc.get("criterion", "")[:50],
                        weight=sc.get("weight", "Med"),
                        scoring_method=sc.get("scoring_method", "")[:100],
                    )
                    for sc in jf_data.get("should_criteria", [])
                    if isinstance(sc, dict)
                ]
                judgment_framework = JudgmentFramework(
                    must_gates=must_gates,
                    should_criteria=should_criteria,
                    gate_results=jf_data.get("gate_results", {}),
                    should_scores=jf_data.get("should_scores", {}),
                )

            # v3.1: セルフチェック
            fa_self_check = None
            sc_data = data.get("fa_self_check")
            if sc_data and isinstance(sc_data, dict):
                fa_self_check = FaSelfCheckResult(
                    baseless_numbers=sc_data.get("baseless_numbers", []),
                    missing_intermediate=sc_data.get("missing_intermediate", []),
                    missing_gates=sc_data.get("missing_gates", []),
                    appearance_precision=sc_data.get("appearance_precision", []),
                    overall_status=safe_enum(
                        SelfCheckStatus,
                        sc_data.get("overall_status", "WARNING"),
                        SelfCheckStatus.WARNING,
                    ),
                )

            # v3.0互換: 差別化軸
            differentiation_axis = None
            da_data = data.get("differentiation_axis")
            if da_data and isinstance(da_data, dict):
                differentiation_axis = DifferentiationAxis(
                    axis_name=da_data.get("axis_name", "")[:30],
                    why_this_axis=da_data.get("why_this_axis", "")[:100],
                    not_this_axis=da_data.get("not_this_axis", "")[:50],
                )
            elif competitive_hypothesis:
                differentiation_axis = DifferentiationAxis(
                    axis_name=competitive_hypothesis.axis_name,
                    why_this_axis=competitive_hypothesis.substitution_barrier[:100],
                    not_this_axis="",
                )

            return FaOutput(
                recommended_paths=recommended,
                rejected_paths=rejected,
                decision_criteria=data.get("decision_criteria", []),
                path_comparison=comparison,
                strategic_prohibitions=strategic_prohibitions,
                differentiation_axis=differentiation_axis,
                competitive_hypothesis=competitive_hypothesis,
                judgment_framework=judgment_framework,
                fa_self_check=fa_self_check,
                why_existing_fails=data.get("why_existing_fails", "")[:100],
            )
        except (json.JSONDecodeError, ValueError, TypeError) as e:
            self._logger.warning("LLM response parse failed: %s", e)
            return self._analyze_rule_based(dao_result, input_data)

    def _validate_and_normalize_llm_output(self, data: dict[str, Any]) -> None:
        """LLM出力の業務ロジック検証と正規化（v3.1: 最低4案検証）.

        Args:
            data: 抽出されたJSONデータ

        Raises:
            AgentOutputValidationError: 必須フィールドが空の場合
        """
        # recommended_paths の検証（v3.1: 最低4案）
        recommended = data.get("recommended_paths")
        if not recommended or not isinstance(recommended, list) or len(recommended) == 0:
            self._logger.warning(
                "LLM returned empty recommended_paths. Keys: %s", list(data.keys())
            )
            raise AgentOutputValidationError(
                agent_name=self.name,
                field_name="recommended_paths",
                expected=f"list with at least {self.MIN_STRATEGIC_OPTIONS} items",
                actual=f"empty or invalid: {type(recommended)}",
            )

        if len(recommended) < self.MIN_STRATEGIC_OPTIONS:
            self._logger.warning(
                "recommended_paths has %d items (min %d). Proceeding with available options.",
                len(recommended),
                self.MIN_STRATEGIC_OPTIONS,
            )
            # 警告のみ。LLMが3案しか返さなくても続行する

        # 各パスのpros/cons/risks/costs のリスト長を正規化（max 3）
        for i, path in enumerate(data.get("recommended_paths", [])):
            if isinstance(path, dict):
                for field in ["pros", "cons", "suitable_conditions", "risks", "costs"]:
                    items = path.get(field, [])
                    if isinstance(items, list) and len(items) > 3:
                        self._logger.warning(
                            "Path %d %s has %d items (max 3), truncating", i, field, len(items)
                        )
                        path[field] = items[:3]

        # v3.1: judgment_framework の存在チェック（警告のみ）
        if not data.get("judgment_framework"):
            self._logger.warning("judgment_framework is missing in LLM output")

        # v3.1: fa_self_check の存在チェック（警告のみ）
        if not data.get("fa_self_check"):
            self._logger.warning("fa_self_check is missing in LLM output")

    def _parse_path_option(self, data: dict[str, Any]) -> PathOption:
        """パスオプションをパース（v3.1: 条件付き評価対応）."""
        # v3.1: conditional_evaluation のパース
        conditional_eval = None
        ce_data = data.get("conditional_evaluation")
        if ce_data and isinstance(ce_data, dict):
            conditional_eval = ConditionalEvaluation(
                success_conditions=ce_data.get("success_conditions", [])[:3],
                risk_factors=ce_data.get("risk_factors", [])[:3],
                failure_modes=ce_data.get("failure_modes", [])[:3],
                probability_basis=ce_data.get("probability_basis", "")[:200],
            )

        return PathOption(
            path_id=data.get("path_id", "X"),
            name=data.get("name", "")[:20],
            description=data.get("description", "")[:100],
            strategy_type=safe_enum(
                StrategyType,
                data.get("strategy_type", "BALANCED"),
                StrategyType.BALANCED,
            ),
            pros=data.get("pros", [])[:3],
            cons=data.get("cons", [])[:3],
            suitable_conditions=data.get("suitable_conditions", [])[:3],
            risks=data.get("risks", [])[:3],
            costs=data.get("costs", [])[:3],
            time_to_value=data.get("time_to_value", ""),
            reversibility=safe_enum(
                ReversibilityLevel,
                data.get("reversibility", "MEDIUM"),
                ReversibilityLevel.MEDIUM,
            ),
            success_probability=0.0,
            conditional_evaluation=conditional_eval,
            risk_concentration=data.get("risk_concentration", "")[:100],
        )

    def _analyze_rule_based(self, dao_result: DaoOutput, input_data: FaInput) -> FaOutput:
        """ルールベース分析（v3.1: 根拠駆動・可実行）."""
        # v3.1: 最低4案のデフォルトパス生成
        recommended, rejected = self._generate_default_paths(dao_result.problem_type, dao_result)

        criteria = self._generate_criteria(dao_result)
        comparison = self._generate_comparison_matrix(recommended, dao_result)

        # v3.1: 戦略的禁止事項（仕組み化付き）
        strategic_prohibitions = self._generate_strategic_prohibitions(
            dao_result.problem_type, dao_result
        )

        # v3.1: 競争優位仮説
        competitive_hypothesis = self._generate_default_competitive_hypothesis(dao_result)

        # v3.0互換: 差別化軸
        differentiation_axis = DifferentiationAxis(
            axis_name=competitive_hypothesis.axis_name,
            why_this_axis=competitive_hypothesis.substitution_barrier[:100],
            not_this_axis="",
        )

        # v3.1: 判断フレームワーク
        judgment_framework = self._generate_default_judgment_framework(recommended)

        # v3.1: セルフチェック
        fa_self_check = self._generate_default_fa_self_check()

        why_existing_fails = self._summarize_why_existing_fails(dao_result)

        return FaOutput(
            recommended_paths=recommended,
            rejected_paths=rejected,
            decision_criteria=criteria,
            path_comparison=comparison,
            strategic_prohibitions=strategic_prohibitions,
            differentiation_axis=differentiation_axis,
            competitive_hypothesis=competitive_hypothesis,
            judgment_framework=judgment_framework,
            fa_self_check=fa_self_check,
            why_existing_fails=why_existing_fails,
        )

    def _generate_strategic_prohibitions(
        self, problem_type: ProblemType, dao_result: DaoOutput
    ) -> list[StrategicProhibition]:
        """戦略的禁止事項を生成（v3.1: 仕組み化付き）."""
        prohibitions: list[StrategicProhibition] = []

        # 問題タイプ別テンプレート
        templates = self.STRATEGIC_PROHIBITIONS_TEMPLATES.get(problem_type, [])
        for template in templates[:2]:
            prohibitions.append(
                StrategicProhibition(
                    prohibition=template["prohibition"],
                    rationale=template["rationale"],
                    violation_consequence=template["consequence"],
                    prevention_measure="設計レビュー時のチェックリスト確認",
                    detection_metric="レビュー記録・承認ログ",
                    responsible_role="テックリード",
                )
            )

        # 死穴から追加の禁止事項を生成
        if dao_result.death_traps:
            trap = dao_result.death_traps[0]
            if len(prohibitions) < 3:
                prohibitions.append(
                    StrategicProhibition(
                        prohibition=trap.action[:50],
                        rationale=trap.reason[:100],
                        violation_consequence=f"{trap.severity}レベルのダメージ",
                        prevention_measure="リスクレビュー会議での確認",
                        detection_metric="リスク台帳の更新状況",
                        responsible_role="プロジェクトマネージャー",
                    )
                )

        # 制約主導型の場合
        if dao_result.problem_nature == ProblemNatureType.CONSTRAINT_DRIVEN:
            if not any("独自" in p.prohibition for p in prohibitions):
                prohibitions.append(
                    StrategicProhibition(
                        prohibition="独自技術・独自規格の開発",
                        rationale="標準への適合が長期的な保守性と互換性を確保",
                        violation_consequence="エコシステムからの孤立、保守コスト増大",
                        prevention_measure="アーキテクチャレビューでの標準準拠チェック",
                        detection_metric="非標準コンポーネント数の監視",
                        responsible_role="アーキテクト",
                    )
                )

        return prohibitions[:3]

    def _generate_default_competitive_hypothesis(
        self, dao_result: DaoOutput
    ) -> CompetitiveHypothesis:
        """競争優位仮説のデフォルト生成（v3.1）."""
        if dao_result.problem_nature == ProblemNatureType.REGULATORY_COMPLIANCE:
            return CompetitiveHypothesis(
                axis_name="法規制対応能力",
                target_customer="規制産業の中堅企業（金融・医療・公共）",
                substitution_barrier="規制要件は地域・業種固有のため汎用SaaSでは対応不可",
                winning_metric="規制監査パス率（目標: 初回合格率90%以上）",
                minimum_verification="対象業種3社でPoC実施、監査模擬テストを実行",
            )
        if dao_result.problem_nature == ProblemNatureType.CONSTRAINT_DRIVEN:
            return CompetitiveHypothesis(
                axis_name="固有制約への適合能力",
                target_customer="標準SaaSが使えない固有制約を持つ組織",
                substitution_barrier="既存解が制約を満たせないため代替が困難",
                winning_metric="制約充足率（目標: 全不可変制約の100%充足）",
                minimum_verification="主要制約3件でフィージビリティ検証を実施",
            )
        return CompetitiveHypothesis(
            axis_name="実行速度・柔軟性",
            target_customer="市場変化への迅速な対応を必要とする成長企業",
            substitution_barrier="組織内の知見と実行プロセスの蓄積",
            winning_metric="価値実現時間（目標: 競合比50%短縮）",
            minimum_verification="MVP構築→初期顧客3社でフィードバック収集",
        )

    def _generate_default_judgment_framework(
        self, recommended: list[PathOption]
    ) -> JudgmentFramework:
        """判断フレームワークのデフォルト生成（v3.1）."""
        must_gates = [
            MustGate(criterion="予算上限", threshold="初期投資が承認済み予算の120%以内"),
            MustGate(criterion="法規制準拠", threshold="対象地域の規制要件を全て充足"),
            MustGate(criterion="既存業務への影響", threshold="既存サービスのSLA劣化なし"),
        ]
        should_criteria = [
            ShouldCriterion(criterion="ROI", weight="High", scoring_method="3年NPV換算で1-5点"),
            ShouldCriterion(
                criterion="リスク分散度",
                weight="High",
                scoring_method="単一障害点の数で1-5点（少ない方が高得点）",
            ),
            ShouldCriterion(
                criterion="可逆性", weight="Med", scoring_method="撤退コスト/期間で1-5点"
            ),
            ShouldCriterion(
                criterion="実行速度", weight="Med", scoring_method="価値実現までの期間で1-5点"
            ),
        ]
        # 各案のゲート通過結果とスコア
        gate_results: dict[str, list[bool]] = {}
        should_scores: dict[str, list[int]] = {}
        for path in recommended:
            gate_results[path.path_id] = [True, True, True]
            if path.strategy_type == StrategyType.CONSERVATIVE:
                should_scores[path.path_id] = [3, 5, 5, 2]
            elif path.strategy_type == StrategyType.AGGRESSIVE:
                should_scores[path.path_id] = [5, 2, 2, 5]
            else:
                should_scores[path.path_id] = [4, 3, 3, 3]

        return JudgmentFramework(
            must_gates=must_gates,
            should_criteria=should_criteria,
            gate_results=gate_results,
            should_scores=should_scores,
        )

    def _generate_default_fa_self_check(self) -> FaSelfCheckResult:
        """セルフチェックのデフォルト生成（v3.1）."""
        return FaSelfCheckResult(
            baseless_numbers=[],
            missing_intermediate=[],
            missing_gates=[],
            appearance_precision=["ルールベース生成のため精度は参考値"],
            overall_status=SelfCheckStatus.WARNING,
        )

    def _generate_differentiation_axis(self, dao_result: DaoOutput) -> DifferentiationAxis:
        """差別化軸を生成（v3.0互換）."""
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
        """v3.1: 最低4案のデフォルトパスを生成（内製/アプライアンス/旁路/ハイブリッド）."""

        death_trap_warning = ""
        if dao_result.death_traps:
            death_trap_warning = f"（{dao_result.death_traps[0].action}を避ける）"

        # A: 内製統合（自前構築）
        option_a = PathOption(
            path_id="A",
            name="内製統合",
            description=f"自社チームで段階的に構築・統合{death_trap_warning}",
            strategy_type=StrategyType.CONSERVATIVE,
            pros=["完全な制御", "カスタマイズ自由度", "知見の蓄積"],
            cons=["開発期間長い", "人材確保が必要", "保守負荷"],
            suitable_conditions=["技術力が十分", "制約が複雑で外部に委託困難"],
            risks=["開発遅延", "人材流出", "技術負債の蓄積"],
            costs=["開発チーム人件費", "インフラ運用コスト"],
            time_to_value="9-12ヶ月",
            reversibility=ReversibilityLevel.MEDIUM,
            success_probability=0.0,
            conditional_evaluation=ConditionalEvaluation(
                success_conditions=["コアチーム3名以上確保", "類似実績あり"],
                risk_factors=["人材離脱", "要件膨張"],
                failure_modes=["開発長期化→予算超過→凍結"],
            ),
            risk_concentration="開発チームの属人性",
        )

        # B: 専用アプライアンス（特化型製品導入）
        option_b = PathOption(
            path_id="B",
            name="専用アプライアンス",
            description="特化型の専用製品・ソリューションを導入",
            strategy_type=StrategyType.AGGRESSIVE,
            pros=["導入速度", "専門ベンダーの知見", "保守の外部化"],
            cons=["ベンダー依存", "カスタマイズ制限", "ライセンスコスト"],
            suitable_conditions=["市場に適合製品が存在", "スピード優先"],
            risks=["ベンダーロックイン", "機能不足発覚", "コスト増加"],
            costs=["ライセンス費", "カスタマイズ費用", "連携開発費"],
            time_to_value="3-6ヶ月",
            reversibility=ReversibilityLevel.LOW,
            success_probability=0.0,
            conditional_evaluation=ConditionalEvaluation(
                success_conditions=["要件の80%以上を製品がカバー", "予算内"],
                risk_factors=["制約との不一致", "ベンダーの継続性"],
                failure_modes=["要件ギャップ拡大→大規模カスタマイズ→コスト爆発"],
            ),
            risk_concentration="ベンダー単一依存",
        )

        # C: 旁路/プラグイン（既存基盤は維持、一部だけ別系統）
        option_c = PathOption(
            path_id="C",
            name="旁路/プラグイン",
            description="既存基盤は維持し、不足部分のみ別系統で補完",
            strategy_type=StrategyType.BALANCED,
            pros=["既存投資の保全", "影響範囲限定", "段階的拡張可"],
            cons=["統合の複雑さ", "二重運用負荷", "一貫性の欠如"],
            suitable_conditions=["既存基盤が概ね有効", "変更範囲を最小化したい"],
            risks=["インターフェース障害", "二重管理", "技術負債"],
            costs=["連携開発費", "追加運用コスト"],
            time_to_value="4-8ヶ月",
            reversibility=ReversibilityLevel.HIGH,
            success_probability=0.0,
            conditional_evaluation=ConditionalEvaluation(
                success_conditions=["APIまたは連携IFが利用可能", "差分が明確"],
                risk_factors=["連携の複雑化", "既存システムの変更制約"],
                failure_modes=["連携障害の連鎖→全体停止"],
            ),
            risk_concentration="連携インターフェースの品質",
        )

        # D: ハイブリッド（一部機能のみ受託/管理クラウド等）
        option_d = PathOption(
            path_id="D",
            name="ハイブリッド",
            description="コア機能は内製、周辺機能は外部サービスを活用",
            strategy_type=StrategyType.BALANCED,
            pros=["リスク分散", "コアの制御維持", "コスト最適化"],
            cons=["管理の複雑化", "責任分界点の曖昧さ"],
            suitable_conditions=["コアと周辺の切り分けが明確", "段階移行が可能"],
            risks=["責任分界の不明確化", "連携障害"],
            costs=["内製部分の開発費", "外部サービス利用料"],
            time_to_value="6-9ヶ月",
            reversibility=ReversibilityLevel.MEDIUM,
            success_probability=0.0,
            conditional_evaluation=ConditionalEvaluation(
                success_conditions=["コアと周辺の境界が明確", "外部サービスのSLA十分"],
                risk_factors=["境界の曖昧化", "外部サービスの仕様変更"],
                failure_modes=["責任不明→障害対応遅延→信頼喪失"],
            ),
            risk_concentration="コア/周辺の境界設計",
        )

        recommended = [option_a, option_b, option_c, option_d]

        # 不推奨パス（現状維持）
        rejected = [
            PathOption(
                path_id="X",
                name="現状維持",
                description="変化を避け現状を維持する選択",
                strategy_type=StrategyType.CONSERVATIVE,
                pros=["短期的な安定"],
                cons=["成長機会喪失", "市場変化への対応遅れ"],
                suitable_conditions=[],
                risks=["長期的な競争力低下"],
                costs=["将来の成長機会の喪失"],
                time_to_value="N/A",
                reversibility=ReversibilityLevel.HIGH,
                success_probability=0.0,
                conditional_evaluation=ConditionalEvaluation(
                    success_conditions=["外部環境が全く変化しない"],
                    risk_factors=["市場変化", "競合の台頭"],
                    failure_modes=["競争力喪失→市場退出"],
                ),
                risk_concentration="外部環境変化への無防備",
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
        """比較マトリックスを生成（v3.1: 4案対応）."""
        scores: dict[str, list[int]] = {}

        for path in recommended_paths:
            if path.strategy_type == StrategyType.CONSERVATIVE:
                scores[path.path_id] = [3, 5, 2, 5, 4]
            elif path.strategy_type == StrategyType.AGGRESSIVE:
                scores[path.path_id] = [5, 2, 5, 2, 3]
            else:
                scores[path.path_id] = [4, 3, 3, 3, 4]

        death_trap_note = ""
        if dao_result.death_traps:
            trap = dao_result.death_traps[0]
            death_trap_note = f" ⚠️ 注意: 「{trap.action}」は{trap.severity}レベルの禁忌。"

        path_ids = [p.path_id for p in recommended_paths]
        summary = (
            f"4案比較（{'/'.join(path_ids)}）: Must判断基準を通過した案のうち、"
            f"Should採点が最も高い案を推奨。{death_trap_note}"
        )

        return PathComparisonMatrix(
            dimensions=self.COMPARISON_DIMENSIONS,
            scores=scores,
            recommendation_summary=summary[:200],
        )

    def validate_output(self, output: FaOutput) -> bool:
        """出力検証（v3.1: 根拠駆動・可実行の検証）."""
        if not output.recommended_paths:
            self._logger.warning("Validation failed: no recommended_paths")
            return False

        # v3.1: 最低4案の確認（警告のみ）
        if len(output.recommended_paths) < self.MIN_STRATEGIC_OPTIONS:
            self._logger.warning(
                "Validation warning: recommended_paths has %d items (min %d)",
                len(output.recommended_paths),
                self.MIN_STRATEGIC_OPTIONS,
            )

        # v3.1: 戦略的禁止事項（仕組み化）の確認
        if not output.strategic_prohibitions:
            self._logger.warning("Validation warning: strategic_prohibitions is empty")

        # v3.1: 競争優位仮説の確認
        if not output.competitive_hypothesis:
            self._logger.warning("Validation warning: competitive_hypothesis is missing")

        # v3.1: 判断フレームワークの確認
        if not output.judgment_framework:
            self._logger.warning("Validation warning: judgment_framework is missing")
        elif not output.judgment_framework.must_gates:
            self._logger.warning("Validation warning: Must gates are empty")

        # v3.1: セルフチェックの確認
        if not output.fa_self_check:
            self._logger.warning("Validation warning: fa_self_check is missing")

        # v3.1: 条件付き評価の確認（各パスにconditional_evaluationがあるか）
        for path in output.recommended_paths:
            if not path.conditional_evaluation:
                self._logger.warning(
                    "Validation warning: path %s missing conditional_evaluation",
                    path.path_id,
                )

        return True
