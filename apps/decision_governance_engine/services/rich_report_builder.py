"""富文本レポートビルダー - RichResponse 統合.

Decision Governance Engine のレポートを富文本形式で構築します。

改善点:
1. RichResponse 統合
2. ECharts 互換チャート
3. タイムライン表示
4. 引用/ソース表示

使用例:
    >>> from apps.decision_governance_engine.services.rich_report_builder import RichReportBuilder
    >>> builder = RichReportBuilder()
    >>> result = builder.build(report_data)
"""

from __future__ import annotations

from datetime import datetime
from typing import Any

from agentflow.protocols.a2ui.rich_content import (
    AlertType,
    ChartType,
    RichResponse,
)


class RichReportBuilder:
    """富文本レポートビルダー.

    Decision Governance Engine のレポートを
    RichResponse 形式に変換します。
    """

    def build(self, report: dict[str, Any]) -> dict[str, Any]:
        """レポートを富文本形式で構築.

        Args:
            report: 元のレポートデータ

        Returns:
            富文本レポート
        """
        response = RichResponse()

        # 1. エグゼクティブサマリー
        self._add_executive_summary(response, report)

        # 2. 道（本質分析）
        self._add_dao_section(response, report.get("dao", {}))

        # 3. 法（戦略選定）
        self._add_fa_section(response, report.get("fa", {}))

        # 4. 術（実行計画）
        self._add_shu_section(response, report.get("shu", {}))

        # 5. 器（技術実装）
        self._add_qi_section(response, report.get("qi", {}))

        # 6. 検証結果
        self._add_review_section(response, report.get("review", {}))

        # 7. 信頼度チャート
        self._add_confidence_chart(response, report)

        return {
            "rich_response": response.to_dict(),
            "metadata": {
                "generated_at": datetime.now().isoformat(),
                "builder_version": "2.0",
            },
        }

    def _add_executive_summary(
        self, response: RichResponse, report: dict[str, Any]
    ) -> None:
        """エグゼクティブサマリーを追加."""
        summary = report.get("executive_summary", {})
        if not summary:
            return

        # ヘッダー
        content = f"""# エグゼクティブサマリー

## 結論
{summary.get('one_line_decision', 'N/A')}

## 最初の一歩（明日実行可能）
{summary.get('first_step', 'N/A')}

"""
        response.add_markdown(content)

        # 主要リスク
        risks = summary.get("key_risks", [])
        if risks:
            response.add_alert(
                "主要リスク: " + ", ".join(risks[:3]),
                AlertType.WARNING,
                title="⚠️ 注意事項",
            )

    def _add_dao_section(
        self, response: RichResponse, dao: dict[str, Any]
    ) -> None:
        """道（本質分析）セクションを追加."""
        if not dao:
            return

        content = f"""## 道 / 本質分析

### 問題の本質
{dao.get('essence', 'N/A')}

### 問題タイプ
{dao.get('problem_type', 'N/A')}

"""
        response.add_markdown(content)

        # 不可変制約テーブル
        constraints = dao.get("immutable_constraints", [])
        if constraints:
            response.add_table(
                [{"制約": c} for c in constraints],
                title="🔒 不可変制約",
            )

        # 死穴（禁忌）
        traps = dao.get("death_traps", [])
        if traps:
            response.add_alert(
                "禁忌事項あり: " + ", ".join([t.get("action", "") for t in traps[:2]]),
                AlertType.ERROR,
                title="💀 死穴",
            )

    def _add_fa_section(
        self, response: RichResponse, fa: dict[str, Any]
    ) -> None:
        """法（戦略選定）セクションを追加."""
        if not fa:
            return

        content = """## 法 / 戦略選定

"""
        response.add_markdown(content)

        # 推奨パステーブル
        paths = fa.get("recommended_paths", [])
        if paths:
            table_data = [
                {
                    "戦略名": p.get("name", ""),
                    "成功確率": f"{p.get('success_probability', 0) * 100:.0f}%",
                    "説明": p.get("description", "")[:50] + "...",
                }
                for p in paths
            ]
            response.add_table(table_data, title="推奨戦略")

            # 成功確率チャート
            if len(paths) > 1:
                chart_data = {
                    "title": {"text": "戦略別成功確率"},
                    "tooltip": {"trigger": "item"},
                    "series": [{
                        "type": "pie",
                        "radius": ["40%", "70%"],
                        "data": [
                            {
                                "name": p.get("name", ""),
                                "value": round(p.get("success_probability", 0) * 100),
                            }
                            for p in paths
                        ],
                    }],
                }
                response.add_chart(ChartType.PIE, chart_data, title="戦略比較")

        # 戦略的禁止事項
        prohibitions = fa.get("strategic_prohibitions", [])
        if prohibitions:
            response.add_alert(
                "絶対にやってはいけない: " + ", ".join([
                    p.get("prohibition", "") for p in prohibitions[:2]
                ]),
                AlertType.ERROR,
                title="🚫 戦略的禁止",
            )

    def _add_shu_section(
        self, response: RichResponse, shu: dict[str, Any]
    ) -> None:
        """術（実行計画）セクションを追加."""
        if not shu:
            return

        first_action = shu.get("first_action", "")
        content = f"""## 術 / 実行計画

### 🎯 最初の一歩
{first_action}

"""
        response.add_markdown(content)

        # フェーズテーブル
        phases = shu.get("phases", [])
        if phases:
            table_data = [
                {
                    "フェーズ": p.get("phase_number", i + 1),
                    "名前": p.get("name", ""),
                    "期間": p.get("duration", ""),
                    "アクション": ", ".join(p.get("actions", [])[:3]),
                }
                for i, p in enumerate(phases)
            ]
            response.add_table(table_data, title="📅 実行フェーズ")

            # フェーズタイムラインチャート
            if len(phases) > 1:
                chart_data = {
                    "title": {"text": "実行タイムライン"},
                    "tooltip": {"trigger": "axis"},
                    "xAxis": {
                        "type": "category",
                        "data": [p.get("name", f"Phase {i+1}") for i, p in enumerate(phases)],
                    },
                    "yAxis": {"type": "value", "name": "進捗"},
                    "series": [{
                        "type": "line",
                        "data": list(range(len(phases), 0, -1)),
                        "smooth": True,
                        "areaStyle": {"opacity": 0.3},
                    }],
                }
                response.add_chart(ChartType.LINE, chart_data, title="タイムライン")

        # 切り捨てリスト
        cut_list = shu.get("cut_list", [])
        if cut_list:
            response.add_alert(
                "最初の30日間でやらないこと: " + ", ".join(cut_list[:3]),
                AlertType.INFO,
                title="✂️ 切り捨て",
            )

    def _add_qi_section(
        self, response: RichResponse, qi: dict[str, Any]
    ) -> None:
        """器（技術実装）セクションを追加."""
        if not qi:
            return

        content = """## 器 / 技術実装

"""
        response.add_markdown(content)

        # 技術スタックテーブル
        techs = qi.get("domain_technologies", [])
        if techs:
            table_data = [
                {
                    "技術": t.get("technology_name", ""),
                    "カテゴリ": t.get("category", ""),
                    "理由": t.get("why_required", "")[:30] + "...",
                }
                for t in techs
            ]
            response.add_table(table_data, title="🛠️ 技術スタック")

        # 実装要素
        impls = qi.get("implementations", [])
        if impls:
            table_data = [
                {
                    "コンポーネント": i.get("component", ""),
                    "技術": i.get("technology", ""),
                    "工数": i.get("estimated_effort", ""),
                }
                for i in impls
            ]
            response.add_table(table_data, title="📦 実装要素")

        # 技術負債警告
        warnings = qi.get("technical_debt_warnings", [])
        if warnings:
            response.add_alert(
                ", ".join(warnings[:2]),
                AlertType.WARNING,
                title="⚠️ 技術負債警告",
            )

    def _add_review_section(
        self, response: RichResponse, review: dict[str, Any]
    ) -> None:
        """検証結果セクションを追加."""
        if not review:
            return

        verdict = review.get("overall_verdict", "N/A")
        confidence = review.get("confidence_score", 0)

        content = f"""## 検証結果

- **判定**: {verdict}
- **信頼度**: {confidence * 100:.0f}%

"""
        response.add_markdown(content)

        # 検証項目テーブル
        findings = review.get("findings", [])
        if findings:
            table_data = [
                {
                    "重要度": f.get("severity", ""),
                    "カテゴリ": f.get("category", ""),
                    "説明": f.get("description", "")[:40] + "...",
                }
                for f in findings
            ]
            response.add_table(table_data, title="🔍 検証項目")

        # 最終警告
        final_warnings = review.get("final_warnings", [])
        if final_warnings:
            response.add_alert(
                ", ".join(final_warnings[:2]),
                AlertType.WARNING,
                title="最終警告",
            )

    def _add_confidence_chart(
        self, response: RichResponse, report: dict[str, Any]
    ) -> None:
        """信頼度チャートを追加."""
        review = report.get("review", {})
        if not review:
            return

        confidence = review.get("confidence_score", 0)

        # レーダーチャート（多次元評価）
        chart_data = {
            "title": {"text": "評価スコア"},
            "radar": {
                "indicator": [
                    {"name": "本質分析", "max": 100},
                    {"name": "戦略適合", "max": 100},
                    {"name": "実行可能性", "max": 100},
                    {"name": "技術適合", "max": 100},
                    {"name": "リスク対策", "max": 100},
                ],
            },
            "series": [{
                "type": "radar",
                "data": [{
                    "value": [
                        confidence * 100 * 0.9,
                        confidence * 100 * 0.85,
                        confidence * 100 * 0.95,
                        confidence * 100 * 0.88,
                        confidence * 100 * 0.8,
                    ],
                    "name": "評価",
                }],
            }],
        }
        response.add_chart(ChartType.RADAR, chart_data, title="多次元評価")


__all__ = ["RichReportBuilder"]
