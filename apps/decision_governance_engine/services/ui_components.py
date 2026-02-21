"""A2UI コンポーネント生成サービス v3.0.

Decision Governance Engine のレポートを A2UI コンポーネントに変換。
フロントエンドはこれらの宣言的コンポーネントを受け取ってレンダリングする。
v3.0: 本質導出・戦略的禁止事項・撤退基準など全フィールド対応。
"""

from typing import Any

from apps.decision_governance_engine.schemas.output_schemas import DecisionReport

from agentflow.protocols.a2ui.components import (
    A2UIComponent,
    ButtonComponent,
    CardComponent,
    ListComponent,
    TextComponent,
)


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
                TextComponent(
                    f"• {a.get('name', '')}: {a.get('why_not_viable', '')} (制約: {a.get('specific_constraint', '')})"
                )
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
                TextComponent(
                    f"⚠️ {t.get('action', '')} ({t.get('severity', '')}): {t.get('reason', '')}",
                    variant="warning",
                )
                for t in traps
            ]
            children.append(ListComponent(items=trap_items, title="💀 死穴（禁忌）"))

        # v3.1: 制約境界条件
        boundaries = dao.get("constraint_boundaries", [])
        if boundaries:
            b_items = [
                TextComponent(
                    f"🚧 {cb.get('constraint_name', '')}: {cb.get('definition', '')} "
                    f"(違反例: {cb.get('violation_example', '')}、例外: {cb.get('exceptions', '')})"
                )
                for cb in boundaries
            ]
            children.append(ListComponent(items=b_items, title="🚧 制約境界条件"))

        # v3.1: 成立ルート比較
        routes = dao.get("solution_routes", [])
        if routes:
            r_items = [
                TextComponent(
                    f"🛤️ [{sr.get('route_type', '')}] {sr.get('description', '')} "
                    f"(実現可能性: {sr.get('viability', '')})"
                )
                for sr in routes
            ]
            children.append(ListComponent(items=r_items, title="🛤️ 成立ルート比較"))

        # v3.1: 定量指標
        metrics = dao.get("quantified_metrics", [])
        if metrics:
            m_items = [
                TextComponent(
                    f"📊 P{qm.get('priority', '')} {qm.get('metric_name', '')}: "
                    f"目標 {qm.get('target_value', '')} ({qm.get('tradeoff_note', '')})"
                )
                for qm in metrics
            ]
            children.append(ListComponent(items=m_items, title="📊 定量指標"))

        # v3.1: 監査証拠チェックリスト
        audit_items = dao.get("audit_evidence_checklist", [])
        if audit_items:
            a_items = [
                TextComponent(
                    f"📋 [{ae.get('category', '')}] {ae.get('required_evidence', '')} "
                    f"→ {ae.get('verification_method', '')}"
                )
                for ae in audit_items
            ]
            children.append(ListComponent(items=a_items, title="📋 監査証拠チェックリスト"))

        # v3.1: セルフチェック結果
        self_check = dao.get("self_check", {})
        if self_check:
            sc_status = self_check.get("overall_status", "N/A")
            if hasattr(sc_status, "value"):
                sc_status = sc_status.value
            sc_text = f"総合ステータス: {sc_status}"
            sc_variant = "highlight" if sc_status == "PASS" else "warning"
            children.append(TextComponent(f"🔬 セルフチェック: {sc_text}", variant=sc_variant))

        return CardComponent(title="🎯 道 - 本質分析 v3.1", children=children)

    def _build_fa_card(self, fa: Any) -> CardComponent:
        """法セクションカードを構築 v3.1."""
        fa = self._to_dict(fa)
        children: list[A2UIComponent] = []

        # v3.1: 戦略的禁止事項（仕組み化）
        prohibitions = fa.get("strategic_prohibitions", [])
        if prohibitions:
            prohibition_items = []
            for p in prohibitions:
                text = f"⛔ {p.get('prohibition', '')}: {p.get('rationale', '')} → {p.get('violation_consequence', '')}"
                if p.get("prevention_measure"):
                    text += f"\n  🛡️ 防止策: {p['prevention_measure']}"
                if p.get("detection_metric"):
                    text += f"\n  📊 検知指標: {p['detection_metric']}"
                if p.get("responsible_role"):
                    text += f"\n  👤 責任者: {p['responsible_role']}"
                prohibition_items.append(TextComponent(text, variant="warning"))
            children.append(ListComponent(items=prohibition_items, title="🚫 戦略的禁止事項（仕組み化）"))

        # v3.1: 競争優位仮説
        comp_hyp = fa.get("competitive_hypothesis", {})
        if comp_hyp:
            hyp_children = [
                TextComponent(f"🎯 差別化軸: {comp_hyp.get('axis_name', '')}", variant="highlight"),
                TextComponent(f"対象顧客: {comp_hyp.get('target_customer', '')}"),
                TextComponent(f"代替障壁: {comp_hyp.get('substitution_barrier', '')}"),
                TextComponent(f"勝ち筋指標: {comp_hyp.get('winning_metric', '')}"),
                TextComponent(f"最小検証: {comp_hyp.get('minimum_verification', '')}"),
            ]
            children.append(CardComponent(title="🎯 競争優位仮説", children=hyp_children))
        else:
            # v3.0フォールバック
            diff_axis = fa.get("differentiation_axis", {})
            if diff_axis:
                diff_children = [
                    TextComponent(f"🎯 勝負する軸: {diff_axis.get('axis_name', '')}", variant="highlight"),
                    TextComponent(f"理由: {diff_axis.get('why_this_axis', '')}"),
                    TextComponent(f"❌ 勝負しない軸: {diff_axis.get('not_this_axis', '')}"),
                ]
                children.append(CardComponent(title="🎯 差別化軸", children=diff_children))

        # 既存解が使えない理由
        why_existing = fa.get("why_existing_fails", "")
        if why_existing:
            children.append(TextComponent(f"⚠️ 既存解が使えない理由: {why_existing}", variant="warning"))

        # 推奨パス（v3.1: 条件付き評価）
        for path in fa.get("recommended_paths", []):
            strategy_type = path.get("strategy_type", "")
            if hasattr(strategy_type, "value"):
                strategy_type = strategy_type.value
            path_children = [
                TextComponent(path.get("description", "")),
                TextComponent(f"価値実現: {path.get('time_to_value', '')} | 可逆性: {path.get('reversibility', '')}"),
            ]
            # v3.1: 条件付き評価
            cond_eval = path.get("conditional_evaluation", {})
            if cond_eval:
                sc = cond_eval.get("success_conditions", [])
                if sc:
                    path_children.append(TextComponent(f"成立条件: {', '.join(sc)}", variant="highlight"))
                rf = cond_eval.get("risk_factors", [])
                if rf:
                    path_children.append(TextComponent(f"リスク要因: {', '.join(rf)}", variant="warning"))
                fm = cond_eval.get("failure_modes", [])
                if fm:
                    path_children.append(TextComponent(f"失敗モード: {', '.join(fm)}", variant="warning"))
            risk_conc = path.get("risk_concentration", "")
            if risk_conc:
                path_children.append(TextComponent(f"⚡ リスク集中点: {risk_conc}", variant="warning"))
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

        # v3.1: 判断フレームワーク
        jf = fa.get("judgment_framework", {})
        if jf:
            jf_children: list[A2UIComponent] = []
            must_gates = jf.get("must_gates", [])
            if must_gates:
                must_items = [
                    TextComponent(
                        f"🚪 {g.get('criterion', '')} — 閾値: {g.get('threshold', '')}",
                        variant="warning",
                    )
                    for g in must_gates
                ]
                jf_children.append(ListComponent(items=must_items, title="Must（不可変ゲート）"))
            should = jf.get("should_criteria", [])
            if should:
                should_items = [
                    TextComponent(
                        f"📏 {s.get('criterion', '')} [{s.get('weight', '')}] — {s.get('scoring_method', '')}"
                    )
                    for s in should
                ]
                jf_children.append(ListComponent(items=should_items, title="Should（比較評価）"))
            children.append(CardComponent(title="⚖️ 判断フレームワーク", children=jf_children))

        # v3.1: セルフチェック
        fa_sc = fa.get("fa_self_check", {})
        if fa_sc:
            status = fa_sc.get("overall_status", "")
            if hasattr(status, "value"):
                status = status.value
            variant = "highlight" if status == "PASS" else "warning"
            sc_items: list[A2UIComponent] = [TextComponent(f"結果: {status}", variant=variant)]
            for label, key in [
                ("根拠なき数値", "baseless_numbers"),
                ("中間案漏れ", "missing_intermediate"),
                ("ゲート不在", "missing_gates"),
                ("見せかけ精度", "appearance_precision"),
            ]:
                vals = fa_sc.get(key, [])
                if vals:
                    sc_items.append(TextComponent(f"{label}: {', '.join(vals)}", variant="warning"))
            children.append(CardComponent(title="🔍 セルフチェック", children=sc_items))

        return CardComponent(title="⚖️ 法 - 戦略選定 v3.1", children=children)

    def _build_shu_card(self, shu: Any) -> CardComponent:
        """術セクションカードを構築 v3.1."""
        shu = self._to_dict(shu)
        children: list[A2UIComponent] = []

        # v3.1: PoC完成定義
        poc_dod = shu.get("poc_definition_of_done", {})
        if poc_dod:
            dod_children: list[A2UIComponent] = []
            for c in poc_dod.get("experience_conditions", []):
                dod_children.append(TextComponent(f"✓ {c}"))
            for m in poc_dod.get("success_metrics", []):
                dod_children.append(
                    TextComponent(
                        f"📊 {m.get('metric_name', '')}: {m.get('target_value', '')} ({m.get('measurement_method', '')})"
                    )
                )
            fallback = poc_dod.get("fallback_strategy", "")
            if fallback:
                dod_children.append(TextComponent(f"🔄 フォールバック: {fallback}", variant="warning"))
            children.append(CardComponent(title="🎯 PoC完成定義（DoD）", children=dod_children))

        # v3.1: 2段ロケット
        rocket = shu.get("two_stage_rocket", {})
        if rocket:
            for stage_key, emoji in [("stage1_minimal_pipeline", "🚀"), ("stage2_governance", "🛡️")]:
                stage = rocket.get(stage_key, {})
                if not stage:
                    continue
                stage_children: list[A2UIComponent] = [TextComponent(stage.get("objective", ""))]
                for p in stage.get("phases", []):
                    tasks = ", ".join(p.get("tasks", []))
                    stage_children.append(
                        TextComponent(
                            f"Phase {p.get('phase_number', '')}: {p.get('name', '')} ({p.get('duration', '')}) → {tasks}"
                        )
                    )
                    for b in p.get("branches", []):
                        stage_children.append(
                            TextComponent(f"  ↳ 分岐: {b.get('branch_name', '')} ({b.get('trigger_condition', '')})")
                        )
                children.append(CardComponent(title=f"{emoji} {stage.get('stage_name', '')}", children=stage_children))

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
                TextComponent(
                    f"🎯 {a.get('action', '')}: {a.get('why_this_context', '')} → {a.get('expected_output', '')}"
                )
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
            children.append(
                CardComponent(
                    title="🔬 単一検証ポイント（PoCで絶対に検証すべき1点）",
                    children=validation_children,
                )
            )

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
        """器セクションカードを構築 v3.1."""
        qi = self._to_dict(qi)
        children: list[A2UIComponent] = []

        # v3.1: PoC最小アーキテクチャ
        poc_arch = qi.get("poc_minimal_architecture", {})
        if poc_arch:
            arch_children: list[A2UIComponent] = []
            for c in poc_arch.get("components", []):
                arch_children.append(
                    TextComponent(f"🏗️ {c.get('name', '')}: {c.get('purpose', '')} → {c.get('technology_choice', '')}")
                )
            flow = poc_arch.get("data_flow_description", "")
            if flow:
                arch_children.append(TextComponent(f"📊 データフロー: {flow}"))
            logging_info = poc_arch.get("minimal_logging", {})
            if logging_info:
                arch_children.append(TextComponent(f"📝 ID戦略: {logging_info.get('correlation_id_strategy', '')}"))
            deferred = poc_arch.get("deferred_components", [])
            if deferred:
                arch_children.append(TextComponent(f"⏳ 後回し: {', '.join(deferred)}"))
            children.append(CardComponent(title="🏗️ PoC最小アーキテクチャ", children=arch_children))

        # v3.1: 拡張アーキテクチャ段階
        expansion = qi.get("expansion_stages", [])
        if expansion:
            exp_items = [
                TextComponent(
                    f"📈 {s.get('stage_name', '')}: {s.get('introduction_condition', '')} → +{', '.join(s.get('added_components', []))}"
                )
                for s in expansion
            ]
            children.append(ListComponent(items=exp_items, title="📈 拡張アーキテクチャ（導入条件付き）"))

        # v3.1: 実装手順
        steps = qi.get("implementation_steps", [])
        if steps:
            step_items: list[A2UIComponent] = []
            for s in steps:
                tasks = ", ".join(s.get("tasks", []))
                step_items.append(TextComponent(f"Step {s.get('step_number', '')}: {s.get('objective', '')} → {tasks}"))
                for p in s.get("common_pitfalls", []):
                    step_items.append(TextComponent(f"  ⚠️ {p}", variant="warning"))
            children.append(ListComponent(items=step_items, title="📝 実装手順"))

        # v3.1: 将来スケール要件
        future = qi.get("future_scale_requirements", [])
        if future:
            future_items = [TextComponent(f"🔮 {r}") for r in future]
            children.append(ListComponent(items=future_items, title="🔮 将来スケール要件（PoC範囲外）"))

        # v3.0: ドメイン固有技術
        domain_techs = qi.get("domain_technologies", [])
        if domain_techs:
            tech_items = [
                TextComponent(
                    f"🛠️ {t.get('technology_name', '')} ({t.get('category', '')}): {t.get('why_required', '')}",
                    variant="highlight",
                )
                for t in domain_techs
            ]
            children.append(ListComponent(items=tech_items, title="🛠️ ドメイン固有技術（具体名詞）"))

        # v3.0: 規制対応
        regulations = qi.get("regulatory_considerations", [])
        if regulations:
            reg_items = [
                TextComponent(
                    f"📜 {r.get('region', '')} / {r.get('regulation', '')}: {r.get('requirement', '')} → {r.get('implementation_impact', '')}",
                    variant="warning",
                )
                for r in regulations
            ]
            children.append(ListComponent(items=reg_items, title="📜 規制対応事項"))

        # v3.0: 地理的考慮
        geographics = qi.get("geographic_considerations", [])
        if geographics:
            geo_items = [
                TextComponent(
                    f"🌍 {g.get('region', '')}: {g.get('latency_requirement', '')} | {g.get('infrastructure_need', '')}"
                )
                for g in geographics
            ]
            children.append(ListComponent(items=geo_items, title="🌍 地理的考慮事項"))

        # 実装要素
        impls = qi.get("implementations", [])
        if impls:
            impl_items = [
                TextComponent(
                    f"🔧 {impl.get('component', '')}: {impl.get('technology', '')} ({impl.get('estimated_effort', '')})"
                )
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
        """検証セクションカードを構築 v3.1（差分パッチ型）."""
        review = self._to_dict(review)
        verdict = review.get("overall_verdict", "N/A")
        if hasattr(verdict, "value"):
            verdict = verdict.value
        confidence = review.get("confidence_score", 0)

        verdict_variant = "highlight" if verdict == "PASS" else "warning"
        children: list[A2UIComponent] = [
            TextComponent(f"判定: {verdict}", variant="headline"),
            TextComponent(f"信頼度: {confidence * 100:.0f}%", variant=verdict_variant),
        ]

        # v3.1 信頼度分解
        breakdown = review.get("confidence_breakdown")
        if isinstance(breakdown, dict):
            bd_items: list[A2UIComponent] = []
            for key in (
                "input_sufficiency",
                "logic_consistency",
                "implementation_feasibility",
                "risk_coverage",
            ):
                comp = breakdown.get(key, {})
                if isinstance(comp, dict):
                    bd_items.append(
                        TextComponent(
                            f"• {comp.get('name', key)}: {comp.get('score', 0):.0f}% (✓で+{comp.get('checkbox_boost', 0):.0f}点)"
                        )
                    )
            if bd_items:
                children.append(ListComponent(items=bd_items, title="📊 信頼度分解"))

        # v3.1 差分パッチ型 所見（最大3件）
        findings = review.get("findings", [])
        if findings:
            finding_items: list[A2UIComponent] = []
            for f in findings[:3]:
                action = f.get("action_type", "RECALC")
                fp = f.get("failure_point", "")
                desc = f.get("description", "")
                patch = f.get("minimal_patch", {})
                cb = patch.get("checkbox_label", "") if isinstance(patch, dict) else ""
                line = f"[{action}] {desc}"
                if fp:
                    line += f" | 破綻点: {fp}"
                if cb:
                    line += f" | パッチ: ☐ {cb}"
                finding_items.append(TextComponent(f"• {line}"))
            children.append(ListComponent(items=finding_items, title="🎯 高レバレッジ欠陥"))

        # v3.1 チェックポイント
        checkpoints = review.get("checkpoint_items", [])
        if checkpoints:
            cp_items: list[A2UIComponent] = [
                TextComponent(
                    f"☐ {c.get('label', '')} (+{c.get('score_boost', 0):.0f}点) — {c.get('default_suggestion', '')}"
                )
                for c in checkpoints
                if isinstance(c, dict)
            ]
            children.append(ListComponent(items=cp_items, title="☑️ 確認チェックポイント"))

        return CardComponent(title="✅ 検証 - 差分パッチ型判定 v3.1", children=children)

    def _build_action_buttons(self, report_id: str) -> CardComponent:
        """アクションボタンを構築."""
        return CardComponent(
            title="",
            children=[
                ButtonComponent(label="📄 PDF出力", action=f"/api/report/{report_id}/pdf"),
                ButtonComponent(label="✍️ 署名", action=f"/api/report/{report_id}/sign"),
            ],
        )
