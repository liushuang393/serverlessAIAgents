# -*- coding: utf-8 -*-
"""ReviewAgent - 検証Agent.

全層の結果を検証し、最終判定を下す。
最も重要なAgent。
"""

import json
from typing import Any

from agentflow import ResilientAgent
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
    max_tokens = 1500
    temperature = 0.7  # やや高め＝多角的視点

    MANDATORY_CHECKS = [
        "責任者が明確か",
        "最悪ケースの想定があるか",
        "撤退条件が定義されているか",
        "最初の一歩が明日実行可能か",
    ]

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

        try:
            json_match = response[response.find("{"):response.rfind("}") + 1]
            data = json.loads(json_match)

            findings = [ReviewFinding(**f) for f in data.get("findings", [])]
            verdict = ReviewVerdict(data.get("overall_verdict", "REVISE"))

            return ReviewOutput(
                overall_verdict=verdict,
                findings=findings,
                confidence_score=data.get("confidence_score", 0.7),
                final_warnings=data.get("final_warnings", []),
            )
        except json.JSONDecodeError:
            return self._review_rule_based(input_data)

    def _review_rule_based(self, input_data: ReviewInput) -> ReviewOutput:
        """ルールベース検証."""
        findings = self._check_all(input_data)

        # 判定ロジック
        has_critical = any(f.severity == FindingSeverity.CRITICAL for f in findings)
        has_warning = any(f.severity == FindingSeverity.WARNING for f in findings)

        if has_critical:
            verdict = ReviewVerdict.REJECT
            confidence = 0.6
        elif has_warning:
            verdict = ReviewVerdict.REVISE
            confidence = 0.75
        else:
            verdict = ReviewVerdict.PASS
            confidence = 0.85

        return ReviewOutput(
            overall_verdict=verdict,
            findings=findings,
            confidence_score=confidence,
            final_warnings=self._generate_warnings(findings),
        )

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

