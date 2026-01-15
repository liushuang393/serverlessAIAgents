# -*- coding: utf-8 -*-
"""DecisionReportBuilder - 決策レポート生成.

AgentFlow の SectionedReportBuilder を継承し、DGE 専用のレポート生成を実装。
既存の ReportGenerator との互換性を維持しつつ、フレームワークの機能を活用。

使用例:
    >>> from apps.decision_governance_engine.services.decision_report_builder import (
    ...     DecisionReportBuilder
    ... )
    >>>
    >>> builder = DecisionReportBuilder()
    >>> report = builder.build(results, inputs={"question": "..."})
"""

from typing import Any

from agentflow.engines.report_builder import (
    ExecutiveSummary,
    ReportSection,
    SectionedReportBuilder,
)

from apps.decision_governance_engine.services.report_generator import ReportGenerator


class DecisionReportBuilder(SectionedReportBuilder):
    """DGE 専用レポートビルダー.

    SectionedReportBuilder を継承し、決策エンジン専用のレポート構造を生成。
    既存の ReportGenerator と連携して動作。
    """

    id_prefix = "PROP-"

    def __init__(self) -> None:
        """初期化."""
        self._generator = ReportGenerator()

    def build(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """決策レポートを生成.

        Args:
            results: 各ステージの実行結果
            inputs: 元の入力データ
            **kwargs: 追加パラメータ

        Returns:
            JSON シリアライズ可能なレポート辞書
        """
        inputs = inputs or {}
        question = inputs.get("question", inputs.get("raw_question", ""))
        clarification = results.get("clarification", {})

        # 既存の ReportGenerator を使用して DecisionReport を生成
        report = self._generator.generate(
            results,
            original_question=question,
            clarification_result=clarification,
        )

        # Pydantic モデルを JSON シリアライズ可能な辞書に変換
        return self.to_json_serializable(report)

    def build_sections(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
    ) -> list[ReportSection]:
        """DGE 専用セクションを生成.

        Args:
            results: 実行結果
            inputs: 入力データ

        Returns:
            セクションリスト
        """
        sections = []

        # 道（本質分析）セクション
        dao = results.get("dao", {})
        if dao:
            sections.append(ReportSection(
                title="道（本質分析）",
                content=dao.get("essence", ""),
                metadata={"stage": "dao", "problem_type": dao.get("problem_type", "")},
            ))

        # 法（戦略選定）セクション
        fa = results.get("fa", {})
        if fa:
            paths = fa.get("recommended_paths", [])
            content = "\n".join([
                f"- {p.get('name', '')}: {p.get('description', '')}"
                for p in paths[:3]
            ])
            sections.append(ReportSection(
                title="法（戦略選定）",
                content=content,
                metadata={"stage": "fa", "paths_count": len(paths)},
            ))

        # 術（実行計画）セクション
        shu = results.get("shu", {})
        if shu:
            sections.append(ReportSection(
                title="術（実行計画）",
                content=shu.get("first_action", ""),
                metadata={"stage": "shu"},
            ))

        # 器（リソース評価）セクション
        qi = results.get("qi", {})
        if qi:
            sections.append(ReportSection(
                title="器（リソース評価）",
                content=str(qi.get("resource_assessment", "")),
                metadata={"stage": "qi"},
            ))

        return sections

    def build_executive_summary(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
    ) -> ExecutiveSummary | None:
        """エグゼクティブサマリーを生成.

        Args:
            results: 実行結果
            inputs: 入力データ

        Returns:
            ExecutiveSummary
        """
        dao = results.get("dao", {})
        fa = results.get("fa", {})
        shu = results.get("shu", {})

        # 推奨パスから結論を生成
        recommended = fa.get("recommended_paths", [{}])[0] if fa else {}
        one_line = f"{recommended.get('name', '推奨案')}を選択すべき"

        # リスク抽出
        risks = list(recommended.get("cons", [])[:3])
        death_traps = dao.get("death_traps", [])
        if death_traps:
            risks.append(f"⚠️ {death_traps[0].get('action', '禁忌行動あり')}")

        return ExecutiveSummary(
            one_line=one_line[:50],
            recommendation=recommended.get("description", ""),
            key_points=list(recommended.get("pros", [])[:3]),
            risks=risks[:3],
            next_step=shu.get("first_action", "キックオフMTG設定"),
        )


__all__ = ["DecisionReportBuilder"]

