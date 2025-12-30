# -*- coding: utf-8 -*-
"""A2UI コンポーネント生成サービス.

Decision Governance Engine のレポートを A2UI コンポーネントに変換。
フロントエンドはこれらの宣言的コンポーネントを受け取ってレンダリングする。
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
    """Decision レポート用 A2UI コンポーネントビルダー.

    DecisionReportから画面表示用のA2UIコンポーネントツリーを構築。
    """

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
        """エグゼクティブサマリーカードを構築."""
        summary = report.executive_summary
        children = [
            TextComponent(summary.one_line_decision, variant="headline"),
            TextComponent(f"推奨アクション: {summary.recommended_action}"),
            TextComponent(f"最初の一歩: {summary.first_step}", variant="highlight"),
        ]
        # リスク一覧
        if summary.key_risks:
            risk_items = [TextComponent(f"⚠️ {r}") for r in summary.key_risks]
            children.append(ListComponent(items=risk_items, title="主要リスク"))

        return CardComponent(title="📊 エグゼクティブサマリー", children=children)

    def _build_dao_card(self, dao: Any) -> CardComponent:
        """道セクションカードを構築."""
        # dictまたはPydanticオブジェクトに対応
        if hasattr(dao, "model_dump"):
            dao = dao.model_dump()
        problem_type = dao.get("problem_type", "N/A")
        if hasattr(problem_type, "value"):
            problem_type = problem_type.value
        children = [
            TextComponent(f"問題タイプ: {problem_type}"),
            TextComponent(f"本質: {dao.get('essence', 'N/A')}", variant="quote"),
        ]
        constraints = dao.get("immutable_constraints", [])
        if constraints:
            constraint_items = [TextComponent(f"• {c}") for c in constraints]
            children.append(ListComponent(items=constraint_items, title="不可変制約"))

        return CardComponent(title="道 - 本質分析", children=children)

    def _build_fa_card(self, fa: Any) -> CardComponent:
        """法セクションカードを構築."""
        if hasattr(fa, "model_dump"):
            fa = fa.model_dump()
        children = []
        for path in fa.get("recommended_paths", []):
            if hasattr(path, "model_dump"):
                path = path.model_dump()
            path_card = CardComponent(
                title=f"✅ {path.get('name', '推奨案')}",
                children=[
                    TextComponent(path.get("description", "")),
                    TextComponent(f"成功確率: {path.get('success_probability', 0)*100:.0f}%"),
                ],
            )
            children.append(path_card)

        return CardComponent(title="法 - 戦略選定", children=children)

    def _build_shu_card(self, shu: Any) -> CardComponent:
        """術セクションカードを構築."""
        if hasattr(shu, "model_dump"):
            shu = shu.model_dump()
        children = []
        for phase in shu.get("phases", []):
            if hasattr(phase, "model_dump"):
                phase = phase.model_dump()
            phase_text = TextComponent(
                f"Phase {phase.get('phase_number', '?')}: {phase.get('name', '')} "
                f"({phase.get('duration', '')})"
            )
            children.append(phase_text)

        first_action = shu.get("first_action", "")
        if first_action:
            children.append(TextComponent(f"🎯 最初の一歩: {first_action}", variant="highlight"))

        return CardComponent(title="術 - 実行計画", children=children)

    def _build_qi_card(self, qi: Any) -> CardComponent:
        """器セクションカードを構築."""
        if hasattr(qi, "model_dump"):
            qi = qi.model_dump()
        children: list[A2UIComponent] = []
        for impl in qi.get("implementations", []):
            if hasattr(impl, "model_dump"):
                impl = impl.model_dump()
            impl_text = TextComponent(
                f"• {impl.get('component', '')}: {impl.get('technology', '')}"
            )
            children.append(impl_text)

        tools = qi.get("tool_recommendations", [])
        if tools:
            children.append(TextComponent(f"推奨ツール: {', '.join(tools)}"))

        return CardComponent(title="器 - 技術実装", children=children)

    def _build_review_card(self, review: Any) -> CardComponent:
        """検証セクションカードを構築."""
        if hasattr(review, "model_dump"):
            review = review.model_dump()
        verdict = review.get("overall_verdict", "N/A")
        if hasattr(verdict, "value"):
            verdict = verdict.value
        confidence = review.get("confidence_score", 0)
        children: list[A2UIComponent] = [
            TextComponent(f"判定: {verdict}", variant="headline"),
            TextComponent(f"信頼度: {confidence*100:.0f}%"),
        ]
        return CardComponent(title="検証 - 最終判定", children=children)

    def _build_action_buttons(self, report_id: str) -> CardComponent:
        """アクションボタンを構築."""
        return CardComponent(
            title="",
            children=[
                ButtonComponent(label="📄 PDF出力", action=f"/api/report/{report_id}/pdf"),
                ButtonComponent(label="✍️ 署名", action=f"/api/report/{report_id}/sign"),
            ],
        )

