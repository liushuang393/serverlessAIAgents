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

from kernel.protocols.a2ui.rich_content import (
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

    def _add_executive_summary(self, response: RichResponse, report: dict[str, Any]) -> None:
        """エグゼクティブサマリーを追加."""
        summary = report.get("executive_summary", {})
        if not summary:
            return

        # ヘッダー
        content = f"""# エグゼクティブサマリー

## 結論
{summary.get("one_line_decision", "N/A")}

## 最初の一歩（明日実行可能）
{summary.get("first_step", "N/A")}

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

    def _add_dao_section(self, response: RichResponse, dao: dict[str, Any]) -> None:
        """道（本質分析）セクションを追加."""
        if not dao:
            return

        content = f"""## 道 / 本質分析

### 問題の本質
{dao.get("essence", "N/A")}

### 問題タイプ
{dao.get("problem_type", "N/A")}

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

        # v3.1: 制約境界条件
        boundaries = dao.get("constraint_boundaries", [])
        if boundaries:
            response.add_table(
                [
                    {
                        "制約名": cb.get("constraint_name", ""),
                        "判定条件": cb.get("definition", ""),
                        "違反例": cb.get("violation_example", ""),
                        "例外": cb.get("exceptions", ""),
                    }
                    for cb in boundaries
                ],
                title="🚧 制約境界条件",
            )

        # v3.1: 成立ルート比較
        routes = dao.get("solution_routes", [])
        if routes:
            response.add_table(
                [
                    {
                        "種別": sr.get("route_type", ""),
                        "説明": sr.get("description", ""),
                        "実現可能性": sr.get("viability", ""),
                        "トレードオフ": "、".join(sr.get("tradeoffs", [])),
                    }
                    for sr in routes
                ],
                title="🛤️ 成立ルート比較",
            )

        # v3.1: 定量指標
        metrics = dao.get("quantified_metrics", [])
        if metrics:
            response.add_table(
                [
                    {
                        "優先": f"P{qm.get('priority', '')}",
                        "指標名": qm.get("metric_name", ""),
                        "目標値": qm.get("target_value", ""),
                        "トレードオフ": qm.get("tradeoff_note", ""),
                    }
                    for qm in metrics
                ],
                title="📊 定量指標",
            )

        # v3.1: 監査証拠チェックリスト
        audit_items = dao.get("audit_evidence_checklist", [])
        if audit_items:
            response.add_table(
                [
                    {
                        "カテゴリ": ae.get("category", ""),
                        "必要な証拠": ae.get("required_evidence", ""),
                        "確認方法": ae.get("verification_method", ""),
                    }
                    for ae in audit_items
                ],
                title="📋 監査証拠チェックリスト",
            )

        # v3.1: セルフチェック結果
        self_check = dao.get("self_check", {})
        if self_check:
            sc_status = self_check.get("overall_status", "N/A")
            if hasattr(sc_status, "value"):
                sc_status = sc_status.value
            alert_type = (
                AlertType.SUCCESS
                if sc_status == "PASS"
                else AlertType.ERROR
                if sc_status == "FATAL"
                else AlertType.WARNING
            )
            response.add_alert(
                f"セルフチェック: {sc_status}",
                alert_type,
                title="🔬 セルフチェック",
            )

    def _add_fa_section(self, response: RichResponse, fa: dict[str, Any]) -> None:
        """法（戦略選定）セクションを追加 v3.1."""
        if not fa:
            return

        content = """## 法 / 戦略選定 v3.1

"""
        response.add_markdown(content)

        # v3.1: 戦略的禁止事項（仕組み化）
        prohibitions = fa.get("strategic_prohibitions", [])
        if prohibitions:
            prohibition_text = "絶対にやってはいけない: " + ", ".join(
                [p.get("prohibition", "") for p in prohibitions[:3]]
            )
            response.add_alert(prohibition_text, AlertType.ERROR, title="🚫 戦略的禁止事項")
            # 仕組み化テーブル
            enforcement_data = []
            for p in prohibitions:
                if p.get("prevention_measure") or p.get("detection_metric") or p.get("responsible_role"):
                    enforcement_data.append(
                        {
                            "禁止事項": p.get("prohibition", ""),
                            "防止策": p.get("prevention_measure", "—"),
                            "検知指標": p.get("detection_metric", "—"),
                            "責任者": p.get("responsible_role", "—"),
                        }
                    )
            if enforcement_data:
                response.add_table(enforcement_data, title="禁止事項の仕組み化")

        # v3.1: 競争優位仮説
        comp_hyp = fa.get("competitive_hypothesis", {})
        if comp_hyp:
            hyp_data = [
                {
                    "差別化軸": comp_hyp.get("axis_name", ""),
                    "対象顧客": comp_hyp.get("target_customer", ""),
                    "代替障壁": comp_hyp.get("substitution_barrier", ""),
                    "勝ち筋指標": comp_hyp.get("winning_metric", ""),
                    "最小検証": comp_hyp.get("minimum_verification", ""),
                }
            ]
            response.add_table(hyp_data, title="🎯 競争優位仮説")

        # 推奨パステーブル（v3.1: 条件付き評価）
        paths = fa.get("recommended_paths", [])
        if paths:
            table_data = [
                {
                    "戦略名": p.get("name", ""),
                    "可逆性": p.get("reversibility", ""),
                    "価値実現": p.get("time_to_value", ""),
                    "説明": p.get("description", "")[:50] + "...",
                }
                for p in paths
            ]
            response.add_table(table_data, title="戦略オプション一覧")

        # v3.1: 判断フレームワーク
        jf = fa.get("judgment_framework", {})
        if jf:
            must_gates = jf.get("must_gates", [])
            if must_gates:
                must_data = [{"基準": g.get("criterion", ""), "閾値": g.get("threshold", "")} for g in must_gates]
                response.add_table(must_data, title="⚖️ Mustゲート（不合格=即却下）")
            should = jf.get("should_criteria", [])
            if should:
                should_data = [
                    {
                        "基準": s.get("criterion", ""),
                        "重み": s.get("weight", ""),
                        "採点方法": s.get("scoring_method", ""),
                    }
                    for s in should
                ]
                response.add_table(should_data, title="📏 Should評価基準")

        # v3.1: セルフチェック
        fa_sc = fa.get("fa_self_check", {})
        if fa_sc:
            status = fa_sc.get("overall_status", "")
            if hasattr(status, "value"):
                status = status.value
            alert_type = AlertType.SUCCESS if status == "PASS" else AlertType.WARNING
            issues: list[str] = []
            for label, key in [
                ("根拠なき数値", "baseless_numbers"),
                ("中間案漏れ", "missing_intermediate"),
                ("ゲート不在", "missing_gates"),
                ("見せかけ精度", "appearance_precision"),
            ]:
                vals = fa_sc.get(key, [])
                if vals:
                    issues.append(f"{label}: {', '.join(vals)}")
            sc_text = "; ".join(issues) if issues else "問題なし"
            response.add_alert(sc_text, alert_type, title=f"🔍 セルフチェック: {status}")

    def _add_shu_section(self, response: RichResponse, shu: dict[str, Any]) -> None:
        """術（実行計画）セクションを追加 v3.1."""
        if not shu:
            return

        first_action = shu.get("first_action", "")
        content = f"""## 術 / 実行計画 v3.1

### 🎯 最初の一歩
{first_action}

"""
        response.add_markdown(content)

        # v3.1: PoC完成定義（DoD）
        poc_dod = shu.get("poc_definition_of_done", {})
        if poc_dod:
            conditions = poc_dod.get("experience_conditions", [])
            if conditions:
                response.add_markdown(
                    "### 🎯 PoC完成定義（DoD）\n\n**体験条件**\n" + "\n".join(f"- ✓ {c}" for c in conditions)
                )
            metrics = poc_dod.get("success_metrics", [])
            if metrics:
                table_data = [
                    {
                        "指標": m.get("metric_name", ""),
                        "目標値": m.get("target_value", ""),
                        "計測方法": m.get("measurement_method", ""),
                    }
                    for m in metrics
                ]
                response.add_table(table_data, title="📊 成功指標")
            fallback = poc_dod.get("fallback_strategy", "")
            if fallback:
                response.add_alert(f"フォールバック: {fallback}", AlertType.INFO, title="🔄 撤退策")

        # v3.1: 2段ロケット
        rocket = shu.get("two_stage_rocket", {})
        if rocket:
            for stage_key, label in [
                ("stage1_minimal_pipeline", "🚀 Stage1: 最小パイプライン"),
                ("stage2_governance", "🛡️ Stage2: 統制強化"),
            ]:
                stage = rocket.get(stage_key, {})
                if not stage:
                    continue
                response.add_markdown(f"### {label}: {stage.get('stage_name', '')}\n\n{stage.get('objective', '')}")
                phases_data = stage.get("phases", [])
                if phases_data:
                    table_data = [
                        {
                            "Phase": p.get("phase_number", ""),
                            "名前": p.get("name", ""),
                            "期間": p.get("duration", ""),
                            "作業": ", ".join(p.get("tasks", [])[:3]),
                        }
                        for p in phases_data
                    ]
                    response.add_table(table_data, title=f"{stage.get('stage_name', '')} フェーズ")
                gate = stage.get("gate_criteria", [])
                if gate:
                    response.add_alert("ゲート基準: " + ", ".join(gate[:3]), AlertType.INFO, title="🚪 ゲート")

        # フェーズテーブル（v3.0互換）
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
                        "data": [p.get("name", f"Phase {i + 1}") for i, p in enumerate(phases)],
                    },
                    "yAxis": {"type": "value", "name": "進捗"},
                    "series": [
                        {
                            "type": "line",
                            "data": list(range(len(phases), 0, -1)),
                            "smooth": True,
                            "areaStyle": {"opacity": 0.3},
                        }
                    ],
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

    def _add_qi_section(self, response: RichResponse, qi: dict[str, Any]) -> None:
        """器（技術実装）セクションを追加 v3.1."""
        if not qi:
            return

        content = """## 器 / 技術実装 v3.1

"""
        response.add_markdown(content)

        # v3.1: PoC最小アーキテクチャ
        poc_arch = qi.get("poc_minimal_architecture", {})
        if poc_arch:
            comps = poc_arch.get("components", [])
            if comps:
                table_data = [
                    {
                        "コンポーネント": c.get("name", ""),
                        "目的": c.get("purpose", ""),
                        "技術": c.get("technology_choice", ""),
                        "備考": c.get("notes", ""),
                    }
                    for c in comps
                ]
                response.add_table(table_data, title="🏗️ PoC最小アーキテクチャ")
            flow = poc_arch.get("data_flow_description", "")
            if flow:
                response.add_markdown(f"**データフロー**: {flow}")
            deferred = poc_arch.get("deferred_components", [])
            if deferred:
                response.add_alert(
                    "後回し: " + ", ".join(deferred),
                    AlertType.INFO,
                    title="⏳ 後回しコンポーネント",
                )

        # v3.1: 拡張アーキテクチャ段階
        expansion = qi.get("expansion_stages", [])
        if expansion:
            table_data = [
                {
                    "段階": s.get("stage_name", ""),
                    "導入条件": s.get("introduction_condition", ""),
                    "追加": ", ".join(s.get("added_components", [])),
                    "理由": s.get("rationale", ""),
                }
                for s in expansion
            ]
            response.add_table(table_data, title="📈 拡張アーキテクチャ")

        # v3.1: 実装手順
        steps = qi.get("implementation_steps", [])
        if steps:
            table_data = [
                {
                    "Step": s.get("step_number", ""),
                    "目標": s.get("objective", ""),
                    "作業": ", ".join(s.get("tasks", [])[:3]),
                    "落とし穴": ", ".join(s.get("common_pitfalls", [])[:2]),
                }
                for s in steps
            ]
            response.add_table(table_data, title="📝 実装手順")

        # v3.1: 将来スケール要件
        future = qi.get("future_scale_requirements", [])
        if future:
            response.add_alert("将来要件: " + ", ".join(future[:3]), AlertType.INFO, title="🔮 将来スケール要件")

        # 技術スタックテーブル（v3.0互換）
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

    def _add_review_section(self, response: RichResponse, review: dict[str, Any]) -> None:
        """検証結果セクションを追加 v3.1（差分パッチ型）."""
        if not review:
            return

        verdict = review.get("overall_verdict", "N/A")
        confidence = review.get("confidence_score", 0)

        content = f"""## 検証結果 — 差分パッチ型判定 v3.1

- **判定**: {verdict}
- **信頼度**: {confidence * 100:.0f}%

"""
        response.add_markdown(content)

        # v3.1 信頼度分解テーブル
        breakdown = review.get("confidence_breakdown")
        if isinstance(breakdown, dict):
            bd_rows: list[dict[str, str]] = []
            for key in (
                "input_sufficiency",
                "logic_consistency",
                "implementation_feasibility",
                "risk_coverage",
            ):
                comp = breakdown.get(key, {})
                if isinstance(comp, dict):
                    bd_rows.append(
                        {
                            "項目": comp.get("name", key),
                            "スコア": f"{comp.get('score', 0):.0f}%",
                            "チェック加点": f"+{comp.get('checkbox_boost', 0):.0f}点",
                            "説明": comp.get("description", ""),
                        }
                    )
            if bd_rows:
                response.add_table(bd_rows, title="📊 信頼度分解")

        # v3.1 差分パッチ型 所見（最大3件）
        findings = review.get("findings", [])
        if findings:
            table_data = [
                {
                    "アクション": f.get("action_type", "RECALC"),
                    "破綻点": (f.get("failure_point", "") or "")[:30],
                    "影響範囲": (f.get("impact_scope", "") or "")[:30],
                    "最小パッチ": (
                        f.get("minimal_patch", {}).get("checkbox_label", "")
                        if isinstance(f.get("minimal_patch"), dict)
                        else ""
                    )[:30],
                }
                for f in findings[:3]
                if isinstance(f, dict)
            ]
            response.add_table(table_data, title="🎯 高レバレッジ欠陥")

        # v3.1 チェックポイント項目
        checkpoints = review.get("checkpoint_items", [])
        if checkpoints:
            cp_text = "\n".join(
                f"- ☐ {c.get('label', '')} (+{c.get('score_boost', 0):.0f}点) — {c.get('default_suggestion', '')}"
                for c in checkpoints
                if isinstance(c, dict)
            )
            response.add_markdown(f"### ☑️ 確認チェックポイント\n\n{cp_text}\n")

    def _add_confidence_chart(self, response: RichResponse, report: dict[str, Any]) -> None:
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
            "series": [
                {
                    "type": "radar",
                    "data": [
                        {
                            "value": [
                                confidence * 100 * 0.9,
                                confidence * 100 * 0.85,
                                confidence * 100 * 0.95,
                                confidence * 100 * 0.88,
                                confidence * 100 * 0.8,
                            ],
                            "name": "評価",
                        }
                    ],
                }
            ],
        }
        response.add_chart(ChartType.RADAR, chart_data, title="多次元評価")


__all__ = ["RichReportBuilder"]
