"""ReviewAgent - 検証Agent v3.1（差分パッチ型UX）.

全層の結果を検証し、高レバレッジ欠陥のみを提示。
最小入力（checkbox＋注釈）でスコアを自動再計算し、再分析コストを最小化する。
"""

import json
import logging
from typing import Any

from apps.decision_governance_engine.schemas.agent_schemas import (
    ActionType,
    CheckpointItem,
    ConfidenceBreakdown,
    ConfidenceComponent,
    DaoOutput,
    FaOutput,
    FindingCategory,
    FindingSeverity,
    MinimalPatch,
    QiOutput,
    ReviewFinding,
    ReviewInput,
    ReviewOutput,
    ReviewVerdict,
    ScoreImprovement,
    ShuOutput,
)

from agentflow import ResilientAgent
from agentflow.core.type_safe import safe_enum, safe_float


class ReviewAgent(ResilientAgent[ReviewInput, ReviewOutput]):
    """検証Agent.

    職責:
    - 全層の整合性検証
    - リスク評価
    - 最終判定（PASS/REVISE/COACH）
    - 警告の提示（COACH: コーチング型改善指導）

    必須チェック項目:
    - 責任者が明確か
    - 最悪ケースの想定があるか
    - 撤退条件が定義されているか
    - 最初の一歩が明日実行可能か
    """

    name = "ReviewAgent"
    # timeout_seconds, max_retries, max_tokens は ResilientAgent のデフォルト値を使用
    temperature = 0.7  # やや高め＝多角的視点

    MANDATORY_CHECKS = [
        "責任者が明確か",
        "最悪ケースの想定があるか",
        "撤退条件が定義されているか",
        "最初の一歩が明日実行可能か",
    ]
    MAX_CHECKBOX_LABEL_LEN = 80
    MAX_ANNOTATION_HINT_LEN = 30
    MAX_DEFAULT_VALUE_LEN = 50
    MAX_FAILURE_POINT_LEN = 200
    MAX_IMPACT_SCOPE_LEN = 200
    MAX_TARGET_SCORE_LEN = 50
    MAX_COMPONENT_NAME_LEN = 30
    MAX_COMPONENT_DESC_LEN = 100

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    SYSTEM_PROMPT = """あなたはReviewAgent v3.1（差分パッチ型検証）です。
ユーザーが"考えにくい/見落としやすい"高レバレッジ欠陥だけを提示し、
最小入力（checkbox＋短い注釈）でスコアを自動再計算する。再分析コストを最小化する。

【目的】
道・法・術・器の結果を検証し、"見落としやすい破綻点"のみを最大3件に統合して提示する。
一般論やコピペ再掲は厳禁。ユーザーはチェックボックスと短い注釈だけで確認・補完できる。

【絶対禁止】
- 一般論（RACIが必要、会議で決めるべき等）だけで終わる指摘
- 「再分析してください」をデフォルトにする
- 同じ指摘の重複（最終警告でのコピペ再掲も禁止）
- ユーザーに"分かりきった作業"を要求して終了すること

【重要な境界条件】
- 入力質問が対象範囲かどうかの判定は GatekeeperAgent の責務。ReviewAgent は「対象外」判定をしてはいけない。
- 情報不足・不確実性がある場合は、PATCH/RECALCで補完可能な差分パッチを提示する。

【指摘ルール（最大3件、重複除去）】
各指摘は必ず以下の形式で出す：
- failure_point: 破綻点（このままだとどこで失敗するか）
- impact_scope: 影響範囲（どのAgent/どの成果物が無効になるか）
- minimal_patch: 最小パッチ（checkbox_label＋annotation_hint＋default_value）
- score_improvements: パッチ適用後のスコア改善見込み [{target_score, current_estimate, improved_estimate, delta}]
- action_type: "PATCH"（追記だけ）/ "RECALC"（追記→再計算）/ "RERUN"（全体再走、原則40点未満のみ）

【修正アクション3段階】
- PATCH: 追記だけでOK（再走不要）
- RECALC: 追記→自動再計算（再走不要）
- RERUN: 全体再走が必要（原則40点未満のときだけ）

【信頼度分解】
信頼度は4項目に分解する：
- input_sufficiency（入力充足度）
- logic_consistency（論理整合）
- implementation_feasibility（実装可能性）
- risk_coverage（リスク網羅）
各項目に「チェックで+何点」を付ける。

【デフォルト案】
「誰が判断するか」が未確定な場合、ユーザーに会議を要求するのではなく、
デフォルト案（暫定責任者ロール）を提示し、ユーザーはチェックで確定できるようにする。

【文字数制約（厳守）】
- minimal_patch.checkbox_label: 80文字以内
- minimal_patch.annotation_hint: 30文字以内
- minimal_patch.default_value: 50文字以内
- failure_point / impact_scope: 200文字以内
- score_improvements.target_score: 50文字以内
- 超過しそうなら要約して短くする（例文の長文は出さない）

【出力形式（JSON）】
{
    "overall_verdict": "PASS" | "REVISE" | "COACH",
    "findings": [
        {
            "severity": "CRITICAL" | "WARNING" | "INFO",
            "category": "LOGIC_FLAW" | "OVER_OPTIMISM" | "RESPONSIBILITY_GAP" | "RESOURCE_MISMATCH" | "TIMELINE_UNREALISTIC",
            "description": "説明",
            "affected_agent": "DaoAgent",
            "suggested_revision": "修正提案",
            "failure_point": "破綻点の具体的説明",
            "impact_scope": "影響範囲",
            "minimal_patch": {"checkbox_label": "ラベル", "annotation_hint": "ヒント", "default_value": "デフォルト案"},
            "score_improvements": [{"target_score": "入力充足度", "current_estimate": 60, "improved_estimate": 80, "delta": 20}],
            "action_type": "PATCH" | "RECALC" | "RERUN"
        }
    ],
    "confidence_score": 0.65,
    "confidence_breakdown": {
        "input_sufficiency": {"name": "入力充足度", "score": 70, "checkbox_boost": 10, "description": "説明"},
        "logic_consistency": {"name": "論理整合", "score": 80, "checkbox_boost": 5, "description": "説明"},
        "implementation_feasibility": {"name": "実装可能性", "score": 60, "checkbox_boost": 15, "description": "説明"},
        "risk_coverage": {"name": "リスク網羅", "score": 50, "checkbox_boost": 10, "description": "説明"}
    },
    "final_warnings": []
}"""

    def _parse_input(self, input_data: dict[str, Any]) -> ReviewInput:
        """入力をパース."""
        if "dao_result" in input_data and isinstance(input_data["dao_result"], dict):
            input_data["dao_result"] = DaoOutput(**input_data["dao_result"])
        if "fa_result" in input_data and isinstance(input_data["fa_result"], dict):
            input_data["fa_result"] = FaOutput(**input_data["fa_result"])
        if "shu_result" in input_data and isinstance(input_data["shu_result"], dict):
            input_data["shu_result"] = ShuOutput(**input_data["shu_result"])
        if "qi_result" in input_data and isinstance(input_data["qi_result"], dict):
            input_data["qi_result"] = QiOutput(**input_data["qi_result"])
        return ReviewInput(**input_data)

    async def process(self, input_data: ReviewInput) -> ReviewOutput:
        """検証を実行."""
        if self._llm:
            return await self._review_with_llm(input_data)

        return self._review_rule_based(input_data)

    async def _review_with_llm(self, input_data: ReviewInput) -> ReviewOutput:
        """LLMを使用した差分パッチ型検証 v3.1."""
        summary = self._create_summary(input_data)

        user_prompt = f"""【分析結果サマリー】
{summary}

上記を差分パッチ型で検証してください。
指摘は最大3件に統合し、各指摘にfailure_point/impact_scope/minimal_patch/score_improvements/action_typeを付けてください。
信頼度はconfidence_breakdownで4項目に分解してください。
minimal_patch は checkbox_label<=80文字, annotation_hint<=30文字, default_value<=50文字 を厳守してください。
JSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        self._logger.debug(f"LLM raw response (first 500 chars): {response[:500] if response else 'EMPTY'}")

        try:
            from agentflow.utils import extract_json

            data = extract_json(response)

            if data is None:
                self._logger.error(f"JSON extraction failed. Raw response: {response[:1000]}")
                msg = "No valid JSON found"
                raise json.JSONDecodeError(msg, response, 0)

            self._logger.debug(f"Extracted JSON: {data}")

            # v3.1: findingsパース（最大3件に制限、重複除去）
            raw_findings_data = data.get("findings", [])
            raw_findings = raw_findings_data[:3] if isinstance(raw_findings_data, list) else []
            findings = self._parse_findings_v31(raw_findings)

            llm_verdict = self._parse_verdict(data.get("overall_verdict"))
            verdict, confidence = self.derive_verdict_and_confidence(
                findings=findings,
                llm_verdict=llm_verdict,
            )

            # v3.1: 信頼度分解パース
            breakdown = self._parse_confidence_breakdown(data.get("confidence_breakdown", {}))

            # v3.1: チェックポイント項目を生成
            checkpoint_items = self._generate_checkpoint_items(input_data, findings)

            return ReviewOutput(
                overall_verdict=verdict,
                findings=findings,
                confidence_score=confidence,
                final_warnings=self._build_final_warnings(
                    findings=findings,
                    llm_warnings=data.get("final_warnings", []) if isinstance(data.get("final_warnings"), list) else [],
                ),
                confidence_breakdown=breakdown,
                checkpoint_items=checkpoint_items,
                auto_recalc_enabled=True,
            )
        except json.JSONDecodeError:
            return self._review_rule_based(input_data)

    def _review_rule_based(self, input_data: ReviewInput) -> ReviewOutput:
        """ルールベース差分パッチ型検証 v3.1."""
        findings = self._check_all(input_data)
        # v3.1: 最大3件に制限
        findings = findings[:3]
        verdict, confidence = self.derive_verdict_and_confidence(findings=findings)

        # v3.1: 信頼度分解を生成
        breakdown = self._calculate_confidence_breakdown(input_data)

        # v3.1: チェックポイント項目を生成
        checkpoint_items = self._generate_checkpoint_items(input_data, findings)

        return ReviewOutput(
            overall_verdict=verdict,
            findings=findings,
            confidence_score=confidence,
            final_warnings=self._generate_warnings(findings),
            confidence_breakdown=breakdown,
            checkpoint_items=checkpoint_items,
            auto_recalc_enabled=True,
        )

    @classmethod
    def derive_verdict_and_confidence(
        cls,
        findings: list[ReviewFinding],
        mandatory_check_failures: int = 0,
        llm_verdict: ReviewVerdict | None = None,
    ) -> tuple[ReviewVerdict, float]:
        """所見から判定と信頼度を一貫して算出."""
        has_critical = any(f.severity == FindingSeverity.CRITICAL for f in findings)
        warning_count = sum(1 for f in findings if f.severity == FindingSeverity.WARNING)

        # SKILL.md の算出規則を基準に信頼度を統一
        confidence = 0.85
        if has_critical:
            confidence -= 0.25
        confidence -= 0.10 * warning_count
        confidence -= 0.15 * max(0, mandatory_check_failures)
        confidence = max(0.0, min(1.0, confidence))

        if has_critical:
            verdict = ReviewVerdict.COACH  # コーチング型改善指導（即終了しない）
        elif warning_count > 0 or confidence < 0.70:
            verdict = ReviewVerdict.REVISE
        else:
            verdict = ReviewVerdict.PASS

        # LLM がより厳しい判定を返した場合のみ採用
        if llm_verdict and cls._verdict_severity(llm_verdict) > cls._verdict_severity(verdict):
            verdict = llm_verdict

        # 判定と信頼度の不整合を防止
        if verdict == ReviewVerdict.REVISE:
            confidence = min(confidence, 0.79)
        elif verdict == ReviewVerdict.COACH:
            confidence = min(confidence, 0.59)

        return verdict, round(confidence, 2)

    @staticmethod
    def _verdict_severity(verdict: ReviewVerdict) -> int:
        """判定の厳しさを数値化."""
        order = {
            ReviewVerdict.PASS: 0,
            ReviewVerdict.REVISE: 1,
            ReviewVerdict.COACH: 2,
        }
        return order[verdict]

    @staticmethod
    def _parse_verdict(value: Any) -> ReviewVerdict | None:
        """文字列を ReviewVerdict に変換（後方互換: REJECT → COACH）."""
        if not isinstance(value, str):
            return None
        # LLM が旧 REJECT を返した場合は COACH にマッピング
        if value.upper() == "REJECT":
            return ReviewVerdict.COACH
        try:
            return ReviewVerdict(value)
        except ValueError:
            return None

    @staticmethod
    def _truncate_text(value: Any, max_length: int) -> str:
        """文字列を安全に正規化して切り詰め."""
        if value is None:
            return ""
        text = str(value).strip()
        if len(text) <= max_length:
            return text
        return text[:max_length]

    def _build_final_warnings(
        self,
        findings: list[ReviewFinding],
        llm_warnings: list[Any] | None = None,
    ) -> list[str]:
        """LLM警告と所見警告を統合."""
        warnings: list[str] = []
        if llm_warnings:
            warnings.extend(str(item) for item in llm_warnings if isinstance(item, str))
        warnings.extend(self._generate_warnings(findings))
        return list(dict.fromkeys(warnings))

    def _create_summary(self, input_data: ReviewInput) -> str:
        """検証用サマリーを作成."""
        return f"""【道】本質: {input_data.dao_result.essence}
【法】推奨: {input_data.fa_result.recommended_paths[0].name if input_data.fa_result.recommended_paths else "なし"}
【術】最初の一歩: {input_data.shu_result.first_action}
【器】技術負債警告: {", ".join(input_data.qi_result.technical_debt_warnings) or "なし"}"""

    def _check_all(self, input_data: ReviewInput) -> list[ReviewFinding]:
        """全チェックを実行（v3.1差分パッチ型）."""
        findings: list[ReviewFinding] = []

        # チェック1: タイムライン破綻
        total_months = sum(self._parse_duration(p.duration) for p in input_data.shu_result.phases)
        if total_months > 12:
            findings.append(
                ReviewFinding(
                    severity=FindingSeverity.WARNING,
                    category=FindingCategory.TIMELINE_UNREALISTIC,
                    description=f"計画期間が{total_months}ヶ月と長期",
                    affected_agent="ShuAgent",
                    suggested_revision="フェーズ期間の短縮またはPoC範囲の限定",
                    failure_point=f"計画が{total_months}ヶ月に及び、環境変化で前提が崩れるリスク",
                    impact_scope="術（実行計画）全フェーズ、器（技術実装）の技術選定",
                    minimal_patch=MinimalPatch(
                        checkbox_label="PoC期間を3ヶ月以内に限定する",
                        annotation_hint="対象フェーズ番号",
                    ),
                    score_improvements=[
                        ScoreImprovement(
                            target_score="実装可能性",
                            current_estimate=55.0,
                            improved_estimate=75.0,
                            delta=20.0,
                        )
                    ],
                    action_type=ActionType.RECALC,
                )
            )

        # チェック2: 技術負債集中
        debt_count = len(input_data.qi_result.technical_debt_warnings)
        if debt_count > 2:
            findings.append(
                ReviewFinding(
                    severity=FindingSeverity.WARNING,
                    category=FindingCategory.OVER_OPTIMISM,
                    description=f"技術負債警告が{debt_count}件。PoC段階での対応優先度が未定義",
                    affected_agent="QiAgent",
                    suggested_revision="PoC段階で対応すべき負債と後回しにする負債を分類",
                    failure_point=f"技術負債{debt_count}件が未整理のまま実装開始すると手戻りが発生",
                    impact_scope="器（技術実装）のコンポーネント選定、術（実行計画）の工数見積",
                    minimal_patch=MinimalPatch(
                        checkbox_label="PoC対応必須の負債を特定済み",
                        annotation_hint="対応する負債名",
                    ),
                    score_improvements=[
                        ScoreImprovement(
                            target_score="リスク網羅",
                            current_estimate=50.0,
                            improved_estimate=70.0,
                            delta=20.0,
                        )
                    ],
                    action_type=ActionType.PATCH,
                )
            )

        # チェック3: 撤退基準未定義
        exit_criteria = input_data.shu_result.exit_criteria
        has_exit = bool(exit_criteria and getattr(exit_criteria, "exit_trigger", ""))
        if not has_exit:
            findings.append(
                ReviewFinding(
                    severity=FindingSeverity.CRITICAL,
                    category=FindingCategory.RESPONSIBILITY_GAP,
                    description="撤退基準が未定義。損切りポイントが不明確",
                    affected_agent="ShuAgent",
                    suggested_revision="撤退トリガー条件と撤退時行動を定義",
                    failure_point="損切り判断が遅れ、リソースを浪費する",
                    impact_scope="術（実行計画）全体、法（戦略選定）の推奨戦略",
                    minimal_patch=MinimalPatch(
                        checkbox_label="撤退基準を定義済み",
                        annotation_hint="撤退トリガー条件",
                        default_value="PoC3ヶ月時点でKPI未達なら中止",
                    ),
                    score_improvements=[
                        ScoreImprovement(
                            target_score="リスク網羅",
                            current_estimate=40.0,
                            improved_estimate=65.0,
                            delta=25.0,
                        )
                    ],
                    action_type=ActionType.RECALC,
                )
            )

        return findings

    def _parse_duration(self, duration: str) -> int:
        """期間文字列を月数に変換."""
        import re

        if match := re.search(r"(\d+)\s*(週|week)", duration, re.IGNORECASE):
            return int(match.group(1)) // 4
        if match := re.search(r"(\d+)\s*(ヶ月|月|month)", duration, re.IGNORECASE):
            return int(match.group(1))
        return 1

    def _generate_warnings(self, findings: list[ReviewFinding]) -> list[str]:
        """最終警告を生成."""
        return [f.description for f in findings if f.severity in [FindingSeverity.CRITICAL, FindingSeverity.WARNING]]

    def validate_output(self, output: ReviewOutput) -> bool:
        """出力検証.

        Args:
            output: ReviewAgent出力

        Returns:
            検証結果
        """
        # overall_verdict が有効な値か
        if output.overall_verdict not in ReviewVerdict:
            self._logger.warning(f"Validation failed: invalid verdict {output.overall_verdict}")
            return False

        # confidence_score が範囲内か
        if not (0.0 <= output.confidence_score <= 1.0):
            self._logger.warning(f"Validation failed: confidence_score {output.confidence_score} out of range")
            return False

        return True

    # =========================================================================
    # v3.1 差分パッチ型ヘルパーメソッド
    # =========================================================================

    def _parse_findings_v31(self, raw_findings: list[dict[str, Any]]) -> list[ReviewFinding]:
        """LLM出力からv3.1形式のfindingsをパース."""
        findings: list[ReviewFinding] = []
        seen_descriptions: set[str] = set()

        for f in raw_findings:
            if not isinstance(f, dict):
                continue
            desc = self._truncate_text(f.get("description", ""), self.MAX_IMPACT_SCOPE_LEN)
            # 重複除去
            if desc in seen_descriptions:
                continue
            seen_descriptions.add(desc)

            # minimal_patchのパース
            mp_data = f.get("minimal_patch")
            minimal_patch = None
            if isinstance(mp_data, dict):
                checkbox_label = (
                    self._truncate_text(
                        mp_data.get("checkbox_label", "確認済み"),
                        self.MAX_CHECKBOX_LABEL_LEN,
                    )
                    or "確認済み"
                )
                annotation_hint = self._truncate_text(
                    mp_data.get("annotation_hint", ""),
                    self.MAX_ANNOTATION_HINT_LEN,
                )
                default_value = self._truncate_text(
                    mp_data.get("default_value", ""),
                    self.MAX_DEFAULT_VALUE_LEN,
                )
                minimal_patch = MinimalPatch(
                    checkbox_label=checkbox_label,
                    annotation_hint=annotation_hint,
                    default_value=default_value,
                )

            # score_improvementsのパース
            si_data = f.get("score_improvements", [])
            score_improvements: list[ScoreImprovement] = []
            for si in si_data:
                if isinstance(si, dict):
                    score_improvements.append(
                        ScoreImprovement(
                            target_score=self._truncate_text(si.get("target_score", ""), self.MAX_TARGET_SCORE_LEN),
                            current_estimate=safe_float(si.get("current_estimate", 50), 50.0),
                            improved_estimate=safe_float(si.get("improved_estimate", 70), 70.0),
                            delta=safe_float(si.get("delta", 20), 20.0),
                        )
                    )

            # action_typeのパース
            action_type = safe_enum(
                ActionType,
                f.get("action_type", "RECALC"),
                ActionType.RECALC,
            )
            suggested_revision = self._truncate_text(
                f.get("suggested_revision", ""),
                self.MAX_IMPACT_SCOPE_LEN,
            )
            if not suggested_revision:
                if minimal_patch and minimal_patch.checkbox_label:
                    suggested_revision = self._truncate_text(
                        f"{minimal_patch.checkbox_label} を実施",
                        self.MAX_IMPACT_SCOPE_LEN,
                    )
                else:
                    suggested_revision = "指摘事項を解消する具体策を追記"

            findings.append(
                ReviewFinding(
                    severity=safe_enum(
                        FindingSeverity,
                        f.get("severity", "WARNING"),
                        FindingSeverity.WARNING,
                    ),
                    category=safe_enum(
                        FindingCategory,
                        f.get("category", "LOGIC_FLAW"),
                        FindingCategory.LOGIC_FLAW,
                    ),
                    description=desc,
                    affected_agent=self._truncate_text(f.get("affected_agent", ""), 30),
                    suggested_revision=suggested_revision,
                    failure_point=self._truncate_text(f.get("failure_point", ""), self.MAX_FAILURE_POINT_LEN),
                    impact_scope=self._truncate_text(f.get("impact_scope", ""), self.MAX_IMPACT_SCOPE_LEN),
                    minimal_patch=minimal_patch,
                    score_improvements=score_improvements,
                    action_type=action_type,
                )
            )

        return findings[:3]

    def _parse_confidence_breakdown(self, data: dict[str, Any]) -> ConfidenceBreakdown | None:
        """LLM出力から信頼度分解をパース."""
        if not data or not isinstance(data, dict):
            return None

        def _parse_component(comp_data: Any, default_name: str) -> ConfidenceComponent:
            normalized = comp_data if isinstance(comp_data, dict) else {}
            return ConfidenceComponent(
                name=self._truncate_text(normalized.get("name", default_name), self.MAX_COMPONENT_NAME_LEN),
                score=safe_float(normalized.get("score", 50), 50.0),
                checkbox_boost=safe_float(normalized.get("checkbox_boost", 5), 5.0),
                description=self._truncate_text(normalized.get("description", ""), self.MAX_COMPONENT_DESC_LEN),
            )

        return ConfidenceBreakdown(
            input_sufficiency=_parse_component(data.get("input_sufficiency", {}), "入力充足度"),
            logic_consistency=_parse_component(data.get("logic_consistency", {}), "論理整合"),
            implementation_feasibility=_parse_component(data.get("implementation_feasibility", {}), "実装可能性"),
            risk_coverage=_parse_component(data.get("risk_coverage", {}), "リスク網羅"),
        )

    def _calculate_confidence_breakdown(self, input_data: ReviewInput) -> ConfidenceBreakdown:
        """ルールベースで信頼度分解を計算."""
        # 入力充足度: 各Agent結果の充実度を評価
        has_essence = bool(input_data.dao_result.essence)
        has_paths = bool(input_data.fa_result.recommended_paths)
        has_first_action = bool(input_data.shu_result.first_action)
        has_impls = bool(input_data.qi_result.implementations)
        input_score = 25.0 * sum([has_essence, has_paths, has_first_action, has_impls])

        # 論理整合: 各層間の一貫性
        logic_score = 70.0  # ルールベースではデフォルト

        # 実装可能性: 器の充実度
        impl_count = len(input_data.qi_result.implementations)
        impl_score = min(90.0, 40.0 + impl_count * 10.0)

        # リスク網羅: 撤退基準・警告の有無
        has_exit = bool(input_data.shu_result.exit_criteria)
        has_debt_warnings = bool(input_data.qi_result.technical_debt_warnings)
        risk_score = 30.0 + (has_exit * 30.0) + (has_debt_warnings * 20.0)

        return ConfidenceBreakdown(
            input_sufficiency=ConfidenceComponent(
                name="入力充足度",
                score=input_score,
                checkbox_boost=10.0,
                description="全Agent結果の充実度",
            ),
            logic_consistency=ConfidenceComponent(
                name="論理整合",
                score=logic_score,
                checkbox_boost=5.0,
                description="道→法→術→器の一貫性",
            ),
            implementation_feasibility=ConfidenceComponent(
                name="実装可能性",
                score=impl_score,
                checkbox_boost=15.0,
                description="技術実装の具体性",
            ),
            risk_coverage=ConfidenceComponent(
                name="リスク網羅",
                score=risk_score,
                checkbox_boost=10.0,
                description="撤退基準・リスク対策の網羅",
            ),
        )

    def _generate_checkpoint_items(
        self, input_data: ReviewInput, findings: list[ReviewFinding]
    ) -> list[CheckpointItem]:
        """v3.1チェックボックス項目を生成."""
        items: list[CheckpointItem] = []

        # 承認者（ロール）確認済み
        items.append(
            CheckpointItem(
                item_id="approver_confirmed",
                label="承認者（ロール）確認済み",
                checked=False,
                score_boost=8.0,
                target_component="input_sufficiency",
                default_suggestion="暫定: プロジェクトオーナー（PO）が承認責任者",
            )
        )

        # Gate0〜Gate3 定義済み
        items.append(
            CheckpointItem(
                item_id="gates_defined",
                label="Gate0〜Gate3 定義済み",
                checked=False,
                score_boost=10.0,
                target_component="logic_consistency",
                default_suggestion="Gate0=PoC開始, Gate1=MVP判断, Gate2=スケール判断, Gate3=本番移行",
            )
        )

        # 撤退基準 定義済み
        has_exit = bool(
            input_data.shu_result.exit_criteria and getattr(input_data.shu_result.exit_criteria, "exit_trigger", "")
        )
        items.append(
            CheckpointItem(
                item_id="exit_criteria_defined",
                label="撤退基準 定義済み",
                checked=has_exit,
                score_boost=12.0,
                target_component="risk_coverage",
                default_suggestion="PoC3ヶ月時点でKPI50%未達なら撤退",
            )
        )

        # 最悪ケース一次対応 定義済み
        items.append(
            CheckpointItem(
                item_id="worst_case_response",
                label="最悪ケース一次対応 定義済み",
                checked=False,
                score_boost=10.0,
                target_component="risk_coverage",
                default_suggestion="障害発生時: サービス停止→ログ保全→PO報告→24h以内に原因分析",
            )
        )

        return items
