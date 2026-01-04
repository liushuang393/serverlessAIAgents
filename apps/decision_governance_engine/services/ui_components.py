# -*- coding: utf-8 -*-
"""A2UI コンポーネント生成サービス v3.0.

Decision Governance Engine のレポートを A2UI コンポーネントに変換。
フロントエンドはこれらの宣言的コンポーネントを受け取ってレンダリングする。
v3.0: 本質導出・戦略的禁止事項・撤退基準など全フィールド対応。
"""

from typing import Any

from agentflow.protocols.a2ui.components import (
    A2UIComponent,
    ButtonComponent,
    CardComponent,
    ListComponent,
    TextComponent,
)

from apps.decision_governance_engine.schemas.output_schemas import DecisionReport


class DecisionUIComponentBuilder:
    """Decision レポート用 A2UI コンポーネントビルダー v3.0.

    DecisionReportから画面表示用のA2UIコンポーネントツリーを構築。
    v3.0: 全ての道・法・術・器フィールドをコンポーネント化。
    """

    def _to_dict(self, obj: Any) -> dict:
        """Pydanticオブジェクトまたはdictをdictに変換."""
        if hasattr(obj, "model_dump"):
            return obj.model_dump()
        if isinstance(obj, dict):
            return obj
        return {}

    def build_report_view(self, report: DecisionReport) -> list[A2UIComponent]:
        """レポート全体のUIコンポーネントを構築.

        Args:
            report: 決策レポート

        Returns:
            A2UIコンポーネントリスト
        """
        components: list[A2UIComponent] = []

        # エグゼクティブサマリーカード
        components.append(self._build_summary_card(report))

        # 道（Dao）セクション
        components.append(self._build_dao_card(report.dao))

        # 法（Fa）セクション
        components.append(self._build_fa_card(report.fa))

        # 術（Shu）セクション
        components.append(self._build_shu_card(report.shu))

        # 器（Qi）セクション
        components.append(self._build_qi_card(report.qi))

        # 検証（Review）セクション
        components.append(self._build_review_card(report.review))

        # アクションボタン
        components.append(self._build_action_buttons(report.report_id))

        return components

    def _build_summary_card(self, report: DecisionReport) -> CardComponent:
        """エグゼクティブサマリーカードを構築 v3.0."""
        summary = report.executive_summary
        children = [
            TextComponent(summary.one_line_decision, variant="headline"),
        ]

        # v3.0: 本質の一文
        if hasattr(summary, "essence_statement") and summary.essence_statement:
            children.append(TextComponent(f"📍 本質: {summary.essence_statement}", variant="quote"))

        children.append(TextComponent(f"推奨アクション: {summary.recommended_action}"))
        children.append(TextComponent(f"🎯 最初の一歩: {summary.first_step}", variant="highlight"))

        # v3.0: 戦略的禁止事項サマリー
        if hasattr(summary, "strategic_prohibition_summary") and summary.strategic_prohibition_summary:
            children.append(TextComponent(f"⛔ 戦略的禁止: {summary.strategic_prohibition_summary}", variant="warning"))

        # v3.0: 撤退基準サマリー
        if hasattr(summary, "exit_criteria_summary") and summary.exit_criteria_summary:
            children.append(TextComponent(f"🚪 撤退基準: {summary.exit_criteria_summary}", variant="warning"))

        # リスク一覧
        if summary.key_risks:
            risk_items = [TextComponent(f"⚠️ {r}") for r in summary.key_risks]
            children.append(ListComponent(items=risk_items, title="主要リスク"))

        return CardComponent(title="📊 エグゼクティブサマリー", children=children)

    def _build_dao_card(self, dao: Any) -> CardComponent:
        """道セクションカードを構築 v3.0."""
        dao = self._to_dict(dao)

        problem_type = dao.get("problem_type", "N/A")
        if hasattr(problem_type, "value"):
            problem_type = problem_type.value

        problem_nature = dao.get("problem_nature", "")
        if hasattr(problem_nature, "value"):
            problem_nature = problem_nature.value

        children: list[A2UIComponent] = [
            TextComponent(f"問題タイプ: {problem_type}"),
            TextComponent(f"問題の本質的性質: {problem_nature}"),
            TextComponent(f"本質: {dao.get('essence', 'N/A')}", variant="quote"),
        ]

        # v3.0: 本質導出プロセス
        ed = dao.get("essence_derivation", {})
        if ed:
            ed_children = [
                TextComponent(f"表面的問題: {ed.get('surface_problem', '')}"),
                TextComponent(f"一段深い理由: {ed.get('underlying_why', '')}"),
                TextComponent(f"根本制約: {ed.get('root_constraint', '')}"),
                TextComponent(f"本質の一文: {ed.get('essence_statement', '')}", variant="highlight"),
            ]
            children.append(CardComponent(title="🔍 本質導出プロセス", children=ed_children))

        # v3.0: 既存代替手段
        alternatives = dao.get("existing_alternatives", [])
        if alternatives:
            alt_items = [
                TextComponent(f"• {a.get('name', '')}: {a.get('why_not_viable', '')} (制約: {a.get('specific_constraint', '')})")
                for a in alternatives
            ]
            children.append(ListComponent(items=alt_items, title="🔄 既存代替手段（なぜ使えないか）"))

        # 不可変制約
        constraints = dao.get("immutable_constraints", [])
        if constraints:
            constraint_items = [TextComponent(f"• {c}") for c in constraints]
            children.append(ListComponent(items=constraint_items, title="🔒 不可変制約"))

        # 隠れた前提
        assumptions = dao.get("hidden_assumptions", [])
        if assumptions:
            assumption_items = [TextComponent(f"• {a}") for a in assumptions]
            children.append(ListComponent(items=assumption_items, title="💭 隠れた前提"))

        # 因果齿轮
        gears = dao.get("causal_gears", [])
        if gears:
            gear_items = [
                TextComponent(f"⚙️ {g.get('name', '')} (Leverage: {g.get('leverage', '')}): {g.get('description', '')}")
                for g in gears
            ]
            bottleneck = dao.get("bottleneck_gear", "")
            gear_items.append(TextComponent(f"🎯 ボトルネック: Gear {bottleneck}", variant="highlight"))
            children.append(ListComponent(items=gear_items, title="⚙️ 因果齿轮"))

        # 死穴
        traps = dao.get("death_traps", [])
        if traps:
            trap_items = [
                TextComponent(f"⚠️ {t.get('action', '')} ({t.get('severity', '')}): {t.get('reason', '')}", variant="warning")
                for t in traps
            ]
            children.append(ListComponent(items=trap_items, title="💀 死穴（禁忌）"))

        return CardComponent(title="🎯 道 - 本質分析", children=children)

    def _build_fa_card(self, fa: Any) -> CardComponent:
        """法セクションカードを構築 v3.0."""
        fa = self._to_dict(fa)
        children: list[A2UIComponent] = []

        # v3.0: 戦略的禁止事項
        prohibitions = fa.get("strategic_prohibitions", [])
        if prohibitions:
            prohibition_items = [
                TextComponent(f"⛔ {p.get('prohibition', '')}: {p.get('rationale', '')} → {p.get('violation_consequence', '')}", variant="warning")
                for p in prohibitions
            ]
            children.append(ListComponent(items=prohibition_items, title="🚫 戦略的禁止事項（絶対にやってはいけない）"))

        # v3.0: 差別化軸
        diff_axis = fa.get("differentiation_axis", {})
        if diff_axis:
            diff_children = [
                TextComponent(f"🎯 勝負する軸: {diff_axis.get('axis_name', '')}", variant="highlight"),
                TextComponent(f"理由: {diff_axis.get('why_this_axis', '')}"),
                TextComponent(f"❌ 勝負しない軸: {diff_axis.get('not_this_axis', '')}"),
            ]
            children.append(CardComponent(title="🎯 差別化軸", children=diff_children))

        # v3.0: 既存解が使えない理由
        why_existing = fa.get("why_existing_fails", "")
        if why_existing:
            children.append(TextComponent(f"⚠️ 既存解が使えない理由: {why_existing}", variant="warning"))

        # 推奨パス
        for path in fa.get("recommended_paths", []):
            strategy_type = path.get("strategy_type", "")
            if hasattr(strategy_type, "value"):
                strategy_type = strategy_type.value

            path_children = [
                TextComponent(path.get("description", "")),
                TextComponent(f"成功確率: {path.get('success_probability', 0)*100:.0f}%"),
                TextComponent(f"価値実現時間: {path.get('time_to_value', '')}"),
                TextComponent(f"可逆性: {path.get('reversibility', '')}"),
            ]

            pros = path.get("pros", [])
            if pros:
                pros_items = [TextComponent(f"✅ {p}") for p in pros]
                path_children.append(ListComponent(items=pros_items, title="メリット"))

            cons = path.get("cons", [])
            if cons:
                cons_items = [TextComponent(f"❌ {c}") for c in cons]
                path_children.append(ListComponent(items=cons_items, title="デメリット"))

            path_card = CardComponent(
                title=f"📌 {path.get('name', '推奨案')} ({strategy_type})",
                children=path_children,
            )
            children.append(path_card)

        return CardComponent(title="⚖️ 法 - 戦略選定", children=children)

    def _build_shu_card(self, shu: Any) -> CardComponent:
        """術セクションカードを構築 v3.0."""
        shu = self._to_dict(shu)
        children: list[A2UIComponent] = []

        # 最初の一歩
        first_action = shu.get("first_action", "")
        if first_action:
            children.append(TextComponent(f"🎯 最初の一歩: {first_action}", variant="highlight"))

        # v3.0: 切り捨てリスト
        cut_list = shu.get("cut_list", [])
        if cut_list:
            cut_items = [TextComponent(f"❌ {c}", variant="warning") for c in cut_list]
            children.append(ListComponent(items=cut_items, title="✂️ 切り捨てリスト（最初の30日間でやらないこと）"))

        # v3.0: 文脈特化行動
        context_actions = shu.get("context_specific_actions", [])
        if context_actions:
            context_items = [
                TextComponent(f"🎯 {a.get('action', '')}: {a.get('why_this_context', '')} → {a.get('expected_output', '')}")
                for a in context_actions
            ]
            children.append(ListComponent(items=context_items, title="🎯 文脈特化行動（この問題固有）"))

        # v3.0: 単一検証ポイント
        validation = shu.get("single_validation_point", {})
        if validation:
            validation_children = [
                TextComponent(f"検証対象: {validation.get('validation_target', '')}"),
                TextComponent(f"成功基準: {validation.get('success_criteria', '')}"),
                TextComponent(f"失敗時行動: {validation.get('failure_action', '')}", variant="warning"),
            ]
            children.append(CardComponent(title="🔬 単一検証ポイント（PoCで絶対に検証すべき1点）", children=validation_children))

        # v3.0: 撤退基準
        exit_criteria = shu.get("exit_criteria", {})
        if exit_criteria:
            exit_children = [
                TextComponent(f"チェックポイント: {exit_criteria.get('checkpoint', '')}"),
                TextComponent(f"撤退トリガー: {exit_criteria.get('exit_trigger', '')}"),
                TextComponent(f"撤退時行動: {exit_criteria.get('exit_action', '')}", variant="warning"),
            ]
            children.append(CardComponent(title="🚪 撤退基準（どこで止めるか）", children=exit_children))

        # フェーズ
        phases_children = []
        for phase in shu.get("phases", []):
            phase_text = TextComponent(
                f"Phase {phase.get('phase_number', '?')}: {phase.get('name', '')} ({phase.get('duration', '')})"
            )
            phases_children.append(phase_text)

            actions = phase.get("actions", [])
            if actions:
                for action in actions[:3]:
                    phases_children.append(TextComponent(f"    • {action}"))

        if phases_children:
            children.append(CardComponent(title="📅 フェーズ", children=phases_children))

        # 30天节奏控制
        rhythm = shu.get("rhythm_control", {})
        if rhythm:
            focus = rhythm.get("focus", {})
            rhythm_children = [
                TextComponent(f"聚焦: {focus.get('name', '')}", variant="highlight"),
                TextComponent(focus.get("description", "")),
                TextComponent(f"成功指標: {focus.get('success_metric', '')}"),
            ]
            avoid_list = focus.get("avoid_list", [])
            if avoid_list:
                avoid_items = [TextComponent(f"❌ {a}") for a in avoid_list]
                rhythm_children.append(ListComponent(items=avoid_items, title="この期間やらないこと"))
            rhythm_children.append(TextComponent(f"チェックポイント: {rhythm.get('checkpoint_date', '')}"))
            rhythm_children.append(TextComponent(f"次の判断: {rhythm.get('next_decision_point', '')}"))
            children.append(CardComponent(title="⏱️ 30天行動節奏", children=rhythm_children))

        return CardComponent(title="📋 術 - 実行計画", children=children)

    def _build_qi_card(self, qi: Any) -> CardComponent:
        """器セクションカードを構築 v3.0."""
        qi = self._to_dict(qi)
        children: list[A2UIComponent] = []

        # v3.0: ドメイン固有技術
        domain_techs = qi.get("domain_technologies", [])
        if domain_techs:
            tech_items = [
                TextComponent(f"🛠️ {t.get('technology_name', '')} ({t.get('category', '')}): {t.get('why_required', '')}", variant="highlight")
                for t in domain_techs
            ]
            children.append(ListComponent(items=tech_items, title="🛠️ ドメイン固有技術（具体名詞）"))

        # v3.0: 規制対応
        regulations = qi.get("regulatory_considerations", [])
        if regulations:
            reg_items = [
                TextComponent(f"📜 {r.get('region', '')} / {r.get('regulation', '')}: {r.get('requirement', '')} → {r.get('implementation_impact', '')}", variant="warning")
                for r in regulations
            ]
            children.append(ListComponent(items=reg_items, title="📜 規制対応事項"))

        # v3.0: 地理的考慮
        geographics = qi.get("geographic_considerations", [])
        if geographics:
            geo_items = [
                TextComponent(f"🌍 {g.get('region', '')}: {g.get('latency_requirement', '')} | {g.get('infrastructure_need', '')}")
                for g in geographics
            ]
            children.append(ListComponent(items=geo_items, title="🌍 地理的考慮事項"))

        # 実装要素
        impls = qi.get("implementations", [])
        if impls:
            impl_items = [
                TextComponent(f"🔧 {impl.get('component', '')}: {impl.get('technology', '')} ({impl.get('estimated_effort', '')})")
                for impl in impls
            ]
            children.append(ListComponent(items=impl_items, title="🔧 実装要素"))

        # ツール推奨
        tools = qi.get("tool_recommendations", [])
        if tools:
            children.append(TextComponent(f"🧰 推奨ツール: {', '.join(tools)}"))

        # 統合ポイント
        integration = qi.get("integration_points", [])
        if integration:
            int_items = [TextComponent(f"🔗 {i}") for i in integration]
            children.append(ListComponent(items=int_items, title="🔗 統合ポイント"))

        # 技術負債警告
        warnings = qi.get("technical_debt_warnings", [])
        if warnings:
            warn_items = [TextComponent(f"⚠️ {w}", variant="warning") for w in warnings]
            children.append(ListComponent(items=warn_items, title="⚠️ 技術負債警告"))

        return CardComponent(title="🔧 器 - 技術実装", children=children)

    def _build_review_card(self, review: Any) -> CardComponent:
        """検証セクションカードを構築 v3.0."""
        review = self._to_dict(review)
        verdict = review.get("overall_verdict", "N/A")
        if hasattr(verdict, "value"):
            verdict = verdict.value
        confidence = review.get("confidence_score", 0)

        verdict_variant = "highlight" if verdict == "PASS" else "warning"
        children: list[A2UIComponent] = [
            TextComponent(f"判定: {verdict}", variant="headline"),
            TextComponent(f"信頼度: {confidence*100:.0f}%", variant=verdict_variant),
        ]

        # 所見
        findings = review.get("findings", [])
        if findings:
            finding_items = [
                TextComponent(f"• {f.get('severity', '')}: {f.get('description', '')} (影響: {f.get('affected_agent', '')})")
                for f in findings
            ]
            children.append(ListComponent(items=finding_items, title="📝 検証所見"))

        # 最終警告
        warnings = review.get("final_warnings", [])
        if warnings:
            warn_items = [TextComponent(f"⚠️ {w}", variant="warning") for w in warnings]
            children.append(ListComponent(items=warn_items, title="⚠️ 最終警告"))

        return CardComponent(title="✅ 検証 - 最終判定", children=children)

    def _build_action_buttons(self, report_id: str) -> CardComponent:
        """アクションボタンを構築."""
        return CardComponent(
            title="",
            children=[
                ButtonComponent(label="📄 PDF出力", action=f"/api/report/{report_id}/pdf"),
                ButtonComponent(label="✍️ 署名", action=f"/api/report/{report_id}/sign"),
            ],
        )

