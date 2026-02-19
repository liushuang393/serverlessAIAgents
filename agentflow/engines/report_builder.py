"""ReportBuilder - パイプライン結果レポート生成インターフェース.

PipelineEngine の最終出力を標準化するための抽象基底クラス。
各アプリケーションは独自の ReportBuilder を実装し、
結果のマージ・要約生成・ID付与などを行う。

機能:
- 基本的なレポート構造生成（SimpleReportBuilder）
- セクション構造のサポート（SectionedReportBuilder）
- グラフ・チャートデータのサポート
- エグゼクティブサマリー生成
- 複数出力フォーマット（JSON/Markdown/HTML）

使用例:
    >>> from agentflow.engines.report_builder import SectionedReportBuilder, ReportSection
    >>>
    >>> class MyReportBuilder(SectionedReportBuilder):
    ...     def build_sections(self, results: dict) -> list[ReportSection]:
    ...         return [
    ...             ReportSection(title="概要", content=results.get("summary", "")),
    ...             ReportSection(title="詳細", content=results.get("details", "")),
    ...         ]
"""

from __future__ import annotations

import logging
import uuid
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


# ロガー設定
_logger = logging.getLogger(__name__)


class OutputFormat(Enum):
    """レポート出力フォーマット."""

    JSON = "json"
    MARKDOWN = "markdown"
    HTML = "html"


@dataclass
class ChartData:
    """グラフ・チャートデータ.

    Attributes:
        chart_type: グラフ種別（bar, line, pie, etc.）
        title: グラフタイトル
        data: グラフデータ（形式はchart_typeによる）
        options: 追加オプション
    """

    chart_type: str
    title: str
    data: list[dict[str, Any]]
    options: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "type": self.chart_type,
            "title": self.title,
            "data": self.data,
            "options": self.options,
        }


@dataclass
class ReportSection:
    """レポートセクション.

    Attributes:
        title: セクションタイトル
        content: セクション内容（テキスト）
        charts: グラフデータリスト
        metadata: メタデータ
        subsections: サブセクション（入れ子可）
    """

    title: str
    content: str = ""
    charts: list[ChartData] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)
    subsections: list[ReportSection] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "title": self.title,
            "content": self.content,
            "charts": [c.to_dict() for c in self.charts],
            "metadata": self.metadata,
            "subsections": [s.to_dict() for s in self.subsections],
        }

    def to_markdown(self, level: int = 2) -> str:
        """Markdown 形式に変換.

        Args:
            level: 見出しレベル（2=##, 3=###）
        """
        lines = [f"{'#' * level} {self.title}", "", self.content, ""]
        for sub in self.subsections:
            lines.append(sub.to_markdown(level + 1))
        return "\n".join(lines)


@dataclass
class ExecutiveSummary:
    """エグゼクティブサマリー.

    Attributes:
        one_line: 一行要約
        recommendation: 推奨アクション
        key_points: 重要ポイント
        risks: 主要リスク
        next_step: 次のステップ
    """

    one_line: str
    recommendation: str = ""
    key_points: list[str] = field(default_factory=list)
    risks: list[str] = field(default_factory=list)
    next_step: str = ""

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "one_line": self.one_line,
            "recommendation": self.recommendation,
            "key_points": self.key_points,
            "risks": self.risks,
            "next_step": self.next_step,
        }

    def to_markdown(self) -> str:
        """Markdown 形式に変換."""
        lines = [
            "## エグゼクティブサマリー",
            "",
            f"**結論**: {self.one_line}",
            "",
        ]
        if self.recommendation:
            lines.extend([f"**推奨**: {self.recommendation}", ""])
        if self.key_points:
            lines.append("**重要ポイント**:")
            for point in self.key_points:
                lines.append(f"- {point}")
            lines.append("")
        if self.risks:
            lines.append("**リスク**:")
            for risk in self.risks:
                lines.append(f"- ⚠️ {risk}")
            lines.append("")
        if self.next_step:
            lines.append(f"**次のステップ**: {self.next_step}")
        return "\n".join(lines)


class ReportBuilder(ABC):
    """レポート生成の抽象基底クラス.

    パイプライン実行結果から最終レポートを生成する。
    各アプリケーションは build() を実装してカスタマイズ。

    Attributes:
        id_prefix: レポートID接頭辞（例: "PROP-", "RPT-"）
    """

    id_prefix: str = "RPT-"

    @abstractmethod
    def build(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """結果からレポートを生成.

        Args:
            results: 各ステージの実行結果 {stage_name: result}
            inputs: 元の入力データ
            **kwargs: 追加パラメータ

        Returns:
            JSON シリアライズ可能なレポート辞書
        """

    def generate_id(self, prefix: str | None = None) -> str:
        """レポートIDを生成.

        Args:
            prefix: ID接頭辞（省略時は self.id_prefix）

        Returns:
            一意のレポートID（例: "RPT-202601-ABC123"）
        """
        prefix = prefix or self.id_prefix
        date_part = datetime.now().strftime("%Y%m")
        unique_part = uuid.uuid4().hex[:6].upper()
        return f"{prefix}{date_part}-{unique_part}"

    def generate_summary(
        self,
        results: dict[str, Any],
        max_length: int = 200,
    ) -> str:
        """結果から要約を生成.

        Args:
            results: 実行結果
            max_length: 要約最大文字数

        Returns:
            要約文字列
        """
        stages = list(results.keys())
        summary = f"Completed stages: {', '.join(stages)}"
        return summary[:max_length]

    def to_json_serializable(self, data: Any) -> Any:
        """データをJSONシリアライズ可能な形式に変換.

        datetime、Pydantic モデル、dataclass 等を処理。

        Args:
            data: 変換対象データ

        Returns:
            JSONシリアライズ可能なデータ
        """
        if hasattr(data, "model_dump"):
            return data.model_dump(mode="json")
        if hasattr(data, "to_dict"):
            return data.to_dict()
        if isinstance(data, datetime):
            return data.isoformat()
        if isinstance(data, Enum):
            return data.value
        if isinstance(data, dict):
            return {k: self.to_json_serializable(v) for k, v in data.items()}
        if isinstance(data, (list, tuple)):
            return [self.to_json_serializable(item) for item in data]
        return data


class SimpleReportBuilder(ReportBuilder):
    """シンプルなレポートビルダー.

    基本的なレポート構造を生成。
    カスタマイズ不要な場合のデフォルト実装。
    """

    def build(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """結果からシンプルなレポートを生成."""
        return {
            "report_id": self.generate_id(),
            "created_at": datetime.now().isoformat(),
            "status": "success",
            "summary": self.generate_summary(results),
            "inputs": self.to_json_serializable(inputs) if inputs else {},
            "results": self.to_json_serializable(results),
        }


class SectionedReportBuilder(ReportBuilder):
    """セクション構造レポートビルダー.

    複数セクション・グラフ・エグゼクティブサマリーを持つ
    構造化されたレポートを生成。

    使用例:
        >>> class MyReportBuilder(SectionedReportBuilder):
        ...     def build_sections(self, results: dict) -> list[ReportSection]:
        ...         return [ReportSection(title="概要", content="...")]
        ...     def build_executive_summary(self, results: dict) -> ExecutiveSummary:
        ...         return ExecutiveSummary(one_line="成功")
    """

    # サポートする出力フォーマット
    supported_formats: list[OutputFormat] = [
        OutputFormat.JSON,
        OutputFormat.MARKDOWN,
    ]

    def build(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """セクション構造レポートを生成."""
        report_id = self.generate_id()
        created_at = datetime.now()

        # セクション生成（サブクラスで実装）
        sections = self.build_sections(results, inputs)

        # エグゼクティブサマリー生成（オプション）
        executive_summary = self.build_executive_summary(results, inputs)

        # メタデータ
        metadata = self.build_metadata(results, inputs, **kwargs)

        report = {
            "report_id": report_id,
            "created_at": created_at.isoformat(),
            "status": "success",
            "formats": [f.value for f in self.supported_formats],
            "inputs": self.to_json_serializable(inputs) if inputs else {},
            "metadata": metadata,
        }

        # エグゼクティブサマリーがあれば追加
        if executive_summary:
            report["executive_summary"] = self.to_json_serializable(executive_summary)

        # セクション追加
        report["sections"] = [self.to_json_serializable(s) for s in sections]

        return report

    def build_sections(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
    ) -> list[ReportSection]:
        """セクションを生成（サブクラスでオーバーライド）.

        Args:
            results: 実行結果
            inputs: 入力データ

        Returns:
            セクションリスト
        """
        # デフォルト: 各ステージを1セクションに
        sections = []
        for stage_name, stage_result in results.items():
            content = str(stage_result)[:500] if stage_result else ""
            sections.append(
                ReportSection(
                    title=stage_name,
                    content=content,
                    metadata={"stage": stage_name},
                )
            )
        return sections

    def build_executive_summary(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
    ) -> ExecutiveSummary | None:
        """エグゼクティブサマリーを生成（サブクラスでオーバーライド）.

        Args:
            results: 実行結果
            inputs: 入力データ

        Returns:
            エグゼクティブサマリー、または None
        """
        return None

    def build_metadata(
        self,
        results: dict[str, Any],
        inputs: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """メタデータを生成.

        Args:
            results: 実行結果
            inputs: 入力データ
            **kwargs: 追加パラメータ

        Returns:
            メタデータ辞書
        """
        return {
            "stages_count": len(results),
            "stages": list(results.keys()),
            **kwargs,
        }

    def to_markdown(
        self,
        report: dict[str, Any],
        title: str = "レポート",
    ) -> str:
        """レポートをMarkdown形式に変換.

        Args:
            report: レポート辞書
            title: レポートタイトル

        Returns:
            Markdown文字列
        """
        lines = [f"# {title}", "", f"レポートID: {report.get('report_id', 'N/A')}", ""]

        # エグゼクティブサマリー
        if "executive_summary" in report:
            es = report["executive_summary"]
            summary = ExecutiveSummary(
                one_line=es.get("one_line", ""),
                recommendation=es.get("recommendation", ""),
                key_points=es.get("key_points", []),
                risks=es.get("risks", []),
                next_step=es.get("next_step", ""),
            )
            lines.append(summary.to_markdown())
            lines.append("")

        # セクション
        for section_data in report.get("sections", []):
            section = ReportSection(
                title=section_data.get("title", ""),
                content=section_data.get("content", ""),
                metadata=section_data.get("metadata", {}),
            )
            lines.append(section.to_markdown())

        return "\n".join(lines)


# ============================================================
# ユーティリティ: グラフデータ生成ヘルパー
# ============================================================


def create_bar_chart(
    title: str,
    data: list[dict[str, Any]],
    x_key: str = "label",
    y_key: str = "value",
) -> ChartData:
    """棒グラフデータを生成.

    Args:
        title: グラフタイトル
        data: データリスト
        x_key: X軸キー
        y_key: Y軸キー

    Returns:
        ChartData
    """
    return ChartData(
        chart_type="bar",
        title=title,
        data=data,
        options={"x_key": x_key, "y_key": y_key},
    )


def create_line_chart(
    title: str,
    data: list[dict[str, Any]],
    x_key: str = "x",
    y_key: str = "y",
) -> ChartData:
    """折れ線グラフデータを生成.

    Args:
        title: グラフタイトル
        data: データリスト
        x_key: X軸キー
        y_key: Y軸キー

    Returns:
        ChartData
    """
    return ChartData(
        chart_type="line",
        title=title,
        data=data,
        options={"x_key": x_key, "y_key": y_key},
    )


def create_pie_chart(
    title: str,
    data: list[dict[str, Any]],
    label_key: str = "label",
    value_key: str = "value",
) -> ChartData:
    """円グラフデータを生成.

    Args:
        title: グラフタイトル
        data: データリスト
        label_key: ラベルキー
        value_key: 値キー

    Returns:
        ChartData
    """
    return ChartData(
        chart_type="pie",
        title=title,
        data=data,
        options={"label_key": label_key, "value_key": value_key},
    )


__all__ = [
    "ChartData",
    "ExecutiveSummary",
    "OutputFormat",
    "ReportBuilder",
    "ReportSection",
    "SectionedReportBuilder",
    "SimpleReportBuilder",
    "create_bar_chart",
    "create_line_chart",
    "create_pie_chart",
]
