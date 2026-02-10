"""ReviewAgent - 検証Agent.

全層の結果を検証し、最終判定を下す。
最も重要なAgent。
"""

import json
import logging
from typing import Any

from apps.decision_governance_engine.schemas.agent_schemas import (
    DaoOutput,
    FaOutput,
    FindingCategory,
    FindingSeverity,
    QiOutput,
    ReviewFinding,
    ReviewInput,
    ReviewOutput,
    ReviewVerdict,
    ShuOutput,
)

from agentflow import ResilientAgent


class ReviewAgent(ResilientAgent[ReviewInput, ReviewOutput]):
    """検証Agent.

    職責:
    - 全層の整合性検証
    - リスク評価
    - 最終判定（PASS/REVISE/REJECT）
    - 警告の提示

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

    def __init__(self, llm_client: Any = None) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._logger = logging.getLogger(self.name)

    SYSTEM_PROMPT = """あなたはReviewAgent（検証）です。
全ての分析結果を俯瞰し、最終的な判定を下します。

【あなたの唯一の責任】
道・法・術・器の結果を検証し、実行可能性と整合性を評価すること。

【必須チェック項目】
1. 責任者が明確か
2. 最悪ケースの想定があるか
3. 撤退条件が定義されているか
4. 最初の一歩が明日実行可能か

【判定基準】
- PASS: 全チェッククリア、CRITICALなし
- REVISE: WARNINGあり、修正推奨
- REJECT: CRITICALあり、再検討必要

【出力形式】
必ず以下のJSON形式で出力してください：
{
    "overall_verdict": "PASS" | "REVISE" | "REJECT",
    "findings": [
        {
            "severity": "CRITICAL" | "WARNING" | "INFO",
            "category": "LOGIC_FLAW" | "OVER_OPTIMISM" | "RESPONSIBILITY_GAP" | "RESOURCE_MISMATCH" | "TIMELINE_UNREALISTIC",
            "description": "説明",
            "affected_agent": "DaoAgent",
            "suggested_revision": "修正提案"
        }
    ],
    "confidence_score": 0.85,
    "final_warnings": ["最終警告1"]
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
        """LLMを使用した検証."""
        summary = self._create_summary(input_data)

        user_prompt = f"""【分析結果サマリー】
{summary}

上記を検証し、最終判定をJSON形式で出力してください。"""

        response = await self._call_llm(f"{self.SYSTEM_PROMPT}\n\n{user_prompt}")

        # 詳細ログ: LLM生出力を記録（デバッグ用）
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

            findings = [ReviewFinding(**f) for f in data.get("findings", [])]
            llm_verdict = self._parse_verdict(data.get("overall_verdict"))
            verdict, confidence = self.derive_verdict_and_confidence(
                findings=findings,
                llm_verdict=llm_verdict,
            )

            return ReviewOutput(
                overall_verdict=verdict,
                findings=findings,
                confidence_score=confidence,
                final_warnings=self._build_final_warnings(
                    findings=findings,
                    llm_warnings=data.get("final_warnings", []),
                ),
            )
        except json.JSONDecodeError:
            return self._review_rule_based(input_data)

    def _review_rule_based(self, input_data: ReviewInput) -> ReviewOutput:
        """ルールベース検証."""
        findings = self._check_all(input_data)
        verdict, confidence = self.derive_verdict_and_confidence(findings=findings)

        return ReviewOutput(
            overall_verdict=verdict,
            findings=findings,
            confidence_score=confidence,
            final_warnings=self._generate_warnings(findings),
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
            verdict = ReviewVerdict.REJECT
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
        elif verdict == ReviewVerdict.REJECT:
            confidence = min(confidence, 0.59)

        return verdict, round(confidence, 2)

    @staticmethod
    def _verdict_severity(verdict: ReviewVerdict) -> int:
        """判定の厳しさを数値化."""
        order = {
            ReviewVerdict.PASS: 0,
            ReviewVerdict.REVISE: 1,
            ReviewVerdict.REJECT: 2,
        }
        return order[verdict]

    @staticmethod
    def _parse_verdict(value: Any) -> ReviewVerdict | None:
        """文字列を ReviewVerdict に変換."""
        if not isinstance(value, str):
            return None
        try:
            return ReviewVerdict(value)
        except ValueError:
            return None

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
【器】技術負債警告: {', '.join(input_data.qi_result.technical_debt_warnings) or "なし"}"""

    def _check_all(self, input_data: ReviewInput) -> list[ReviewFinding]:
        """全チェックを実行."""
        findings = []

        # タイムラインチェック
        total_months = sum(
            self._parse_duration(p.duration)
            for p in input_data.shu_result.phases
        )
        if total_months > 12:
            findings.append(ReviewFinding(
                severity=FindingSeverity.WARNING,
                category=FindingCategory.TIMELINE_UNREALISTIC,
                description=f"計画期間が{total_months}ヶ月と長期。バッファを推奨。",
                affected_agent="ShuAgent",
                suggested_revision="フェーズの期間を見直し、バッファを追加",
            ))

        # 技術負債警告チェック
        if len(input_data.qi_result.technical_debt_warnings) > 2:
            findings.append(ReviewFinding(
                severity=FindingSeverity.WARNING,
                category=FindingCategory.OVER_OPTIMISM,
                description="技術負債警告が多い。リスク対策を強化推奨。",
                affected_agent="QiAgent",
                suggested_revision="技術負債の優先対応計画を策定",
            ))

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
