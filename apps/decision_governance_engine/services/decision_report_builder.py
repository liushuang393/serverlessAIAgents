# -*- coding: utf-8 -*-
"""DecisionReportBuilder - 決策レポート生成.

AgentFlow の ReportBuilder を継承し、DGE 専用のレポート生成を実装。
ReportGenerator のロジックを ReportBuilder インターフェースに適合。

使用例:
    >>> from apps.decision_governance_engine.services.decision_report_builder import (
    ...     DecisionReportBuilder
    ... )
    >>>
    >>> builder = DecisionReportBuilder()
    >>> report = builder.build(results, inputs={"question": "..."})
"""

from typing import Any

from agentflow.engines.report_builder import ReportBuilder

from apps.decision_governance_engine.services.report_generator import ReportGenerator


class DecisionReportBuilder(ReportBuilder):
    """DGE 専用レポートビルダー.
    
    ReportBuilder インターフェースを実装し、
    既存の ReportGenerator を内部で使用。
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
        
        report = self._generator.generate(
            results,
            original_question=question,
            clarification_result=clarification,
        )
        
        # Pydantic モデルを JSON シリアライズ可能な辞書に変換
        return self.to_json_serializable(report)


__all__ = ["DecisionReportBuilder"]

