# -*- coding: utf-8 -*-
"""可視化コンポーネント.

チャート生成、ダッシュボード、レポート出力を提供。
"""

from __future__ import annotations

import json
import logging
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any

from agentflow.skills.builtin.bi_analytics.connector import DataFrame
from agentflow.skills.builtin.bi_analytics.analyzer import AnalysisResult

logger = logging.getLogger(__name__)


class ChartType(str, Enum):
    """チャート種別."""

    BAR = "bar"  # 棒グラフ
    LINE = "line"  # 折れ線グラフ
    SCATTER = "scatter"  # 散布図
    PIE = "pie"  # 円グラフ
    HISTOGRAM = "histogram"  # ヒストグラム
    TABLE = "table"  # テーブル


@dataclass
class Chart:
    """チャート定義.

    フロントエンドで描画可能なJSONフォーマット。
    ECharts/Chart.js互換の形式を出力。
    """

    chart_type: ChartType
    title: str = ""
    data: dict[str, Any] = field(default_factory=dict)
    options: dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.now)

    def to_echarts(self) -> dict[str, Any]:
        """ECharts形式に変換."""
        return {
            "title": {"text": self.title},
            "xAxis": self.data.get("xAxis", {}),
            "yAxis": self.data.get("yAxis", {}),
            "series": self.data.get("series", []),
            **self.options,
        }

    def to_chartjs(self) -> dict[str, Any]:
        """Chart.js形式に変換."""
        return {
            "type": self.chart_type.value,
            "data": {
                "labels": self.data.get("labels", []),
                "datasets": self.data.get("datasets", []),
            },
            "options": {
                "title": {"display": True, "text": self.title},
                **self.options,
            },
        }

    def to_json(self) -> str:
        """JSON文字列に変換."""
        return json.dumps(self.to_echarts(), indent=2, ensure_ascii=False)


class ChartGenerator:
    """チャート生成器.

    データフレームや分析結果からチャートを生成。

    Example:
        >>> generator = ChartGenerator()
        >>> chart = await generator.generate(df, ChartType.BAR, x="category", y="amount")
    """

    def __init__(self) -> None:
        """初期化."""
        self._logger = logging.getLogger(__name__)

    async def generate(
        self,
        data: DataFrame | AnalysisResult,
        chart_type: ChartType | str,
        x: str | None = None,
        y: str | None = None,
        title: str = "",
        **kwargs: Any,
    ) -> Chart:
        """チャートを生成.

        Args:
            data: データフレームまたは分析結果
            chart_type: チャート種別
            x: X軸カラム
            y: Y軸カラム
            title: タイトル
            **kwargs: 追加オプション

        Returns:
            Chart インスタンス
        """
        if isinstance(chart_type, str):
            chart_type = ChartType(chart_type)

        # 分析結果の場合はデータを抽出
        if isinstance(data, AnalysisResult):
            return await self._from_analysis_result(data, chart_type, title, **kwargs)

        # データフレームの場合
        if chart_type == ChartType.BAR:
            return await self._bar_chart(data, x, y, title, **kwargs)
        elif chart_type == ChartType.LINE:
            return await self._line_chart(data, x, y, title, **kwargs)
        elif chart_type == ChartType.SCATTER:
            return await self._scatter_chart(data, x, y, title, **kwargs)
        elif chart_type == ChartType.PIE:
            return await self._pie_chart(data, x, y, title, **kwargs)
        elif chart_type == ChartType.HISTOGRAM:
            return await self._histogram_chart(data, x, title, **kwargs)
        elif chart_type == ChartType.TABLE:
            return await self._table_chart(data, title, **kwargs)
        else:
            raise ValueError(f"不明なチャート種別: {chart_type}")

    async def _bar_chart(
        self,
        data: DataFrame,
        x: str | None,
        y: str | None,
        title: str,
        **kwargs: Any,
    ) -> Chart:
        """棒グラフを生成."""
        if not x:
            x = data.columns[0] if data.columns else ""
        if not y:
            y = data.columns[1] if len(data.columns) > 1 else ""

        x_data = [row.get(x) for row in data.rows]
        y_data = [row.get(y) for row in data.rows]

        return Chart(
            chart_type=ChartType.BAR,
            title=title or f"{y} by {x}",
            data={
                "labels": x_data,
                "datasets": [
                    {
                        "label": y,
                        "data": y_data,
                        "backgroundColor": "rgba(54, 162, 235, 0.5)",
                    }
                ],
                "xAxis": {"type": "category", "data": x_data},
                "yAxis": {"type": "value"},
                "series": [{"name": y, "type": "bar", "data": y_data}],
            },
        )

    async def _line_chart(
        self,
        data: DataFrame,
        x: str | None,
        y: str | None,
        title: str,
        **kwargs: Any,
    ) -> Chart:
        """折れ線グラフを生成."""
        if not x:
            x = data.columns[0] if data.columns else ""
        if not y:
            y = data.columns[1] if len(data.columns) > 1 else ""

        x_data = [row.get(x) for row in data.rows]
        y_data = [row.get(y) for row in data.rows]

        return Chart(
            chart_type=ChartType.LINE,
            title=title or f"{y} over {x}",
            data={
                "labels": x_data,
                "datasets": [
                    {
                        "label": y,
                        "data": y_data,
                        "borderColor": "rgba(75, 192, 192, 1)",
                        "fill": False,
                    }
                ],
                "xAxis": {"type": "category", "data": x_data},
                "yAxis": {"type": "value"},
                "series": [{"name": y, "type": "line", "data": y_data}],
            },
        )

    async def _scatter_chart(
        self,
        data: DataFrame,
        x: str | None,
        y: str | None,
        title: str,
        **kwargs: Any,
    ) -> Chart:
        """散布図を生成."""
        if not x:
            x = data.columns[0] if data.columns else ""
        if not y:
            y = data.columns[1] if len(data.columns) > 1 else ""

        points = [
            [row.get(x), row.get(y)]
            for row in data.rows
            if row.get(x) is not None and row.get(y) is not None
        ]

        return Chart(
            chart_type=ChartType.SCATTER,
            title=title or f"{y} vs {x}",
            data={
                "datasets": [
                    {
                        "label": f"{x} vs {y}",
                        "data": [{"x": p[0], "y": p[1]} for p in points],
                        "backgroundColor": "rgba(255, 99, 132, 0.5)",
                    }
                ],
                "xAxis": {"type": "value", "name": x},
                "yAxis": {"type": "value", "name": y},
                "series": [{"type": "scatter", "data": points}],
            },
        )

    async def _pie_chart(
        self,
        data: DataFrame,
        x: str | None,
        y: str | None,
        title: str,
        **kwargs: Any,
    ) -> Chart:
        """円グラフを生成."""
        if not x:
            x = data.columns[0] if data.columns else ""
        if not y:
            y = data.columns[1] if len(data.columns) > 1 else ""

        labels = [row.get(x) for row in data.rows]
        values = [row.get(y) for row in data.rows]

        return Chart(
            chart_type=ChartType.PIE,
            title=title or f"{y} Distribution",
            data={
                "labels": labels,
                "datasets": [
                    {
                        "data": values,
                        "backgroundColor": [
                            "rgba(255, 99, 132, 0.5)",
                            "rgba(54, 162, 235, 0.5)",
                            "rgba(255, 206, 86, 0.5)",
                            "rgba(75, 192, 192, 0.5)",
                            "rgba(153, 102, 255, 0.5)",
                        ],
                    }
                ],
                "series": [
                    {
                        "type": "pie",
                        "data": [{"name": l, "value": v} for l, v in zip(labels, values)],
                    }
                ],
            },
        )

    async def _histogram_chart(
        self,
        data: DataFrame,
        column: str | None,
        title: str,
        **kwargs: Any,
    ) -> Chart:
        """ヒストグラムを生成."""
        if not column:
            column = data.columns[0] if data.columns else ""

        values = [
            row.get(column) for row in data.rows
            if isinstance(row.get(column), (int, float))
        ]

        if not values:
            return Chart(chart_type=ChartType.HISTOGRAM, title=title, data={})

        # ビン計算
        min_val = min(values)
        max_val = max(values)
        bin_count = min(10, len(values))
        bin_width = (max_val - min_val) / bin_count if max_val != min_val else 1

        bins: list[int] = [0] * bin_count
        labels: list[str] = []

        for i in range(bin_count):
            bin_start = min_val + i * bin_width
            bin_end = bin_start + bin_width
            labels.append(f"{round(bin_start, 1)}-{round(bin_end, 1)}")
            bins[i] = sum(1 for v in values if bin_start <= v < bin_end)

        return Chart(
            chart_type=ChartType.HISTOGRAM,
            title=title or f"{column} Distribution",
            data={
                "labels": labels,
                "datasets": [
                    {
                        "label": "Frequency",
                        "data": bins,
                        "backgroundColor": "rgba(54, 162, 235, 0.5)",
                    }
                ],
                "xAxis": {"type": "category", "data": labels},
                "yAxis": {"type": "value"},
                "series": [{"type": "bar", "data": bins}],
            },
        )

    async def _table_chart(
        self,
        data: DataFrame,
        title: str,
        **kwargs: Any,
    ) -> Chart:
        """テーブルを生成."""
        return Chart(
            chart_type=ChartType.TABLE,
            title=title or "Data Table",
            data={
                "columns": data.columns,
                "rows": data.to_dict()[:kwargs.get("max_rows", 100)],
            },
        )

    async def _from_analysis_result(
        self,
        result: AnalysisResult,
        chart_type: ChartType,
        title: str,
        **kwargs: Any,
    ) -> Chart:
        """分析結果からチャートを生成."""
        # ヒストグラムデータがある場合
        if "histogram" in result.data:
            histogram = result.data["histogram"]
            labels = [f"{b['bin_start']}-{b['bin_end']}" for b in histogram]
            values = [b["count"] for b in histogram]

            return Chart(
                chart_type=ChartType.BAR,
                title=title or f"Distribution of {result.data.get('column', 'data')}",
                data={
                    "labels": labels,
                    "datasets": [{"label": "Frequency", "data": values}],
                    "xAxis": {"type": "category", "data": labels},
                    "yAxis": {"type": "value"},
                    "series": [{"type": "bar", "data": values}],
                },
            )

        # 相関データがある場合
        if "correlations" in result.data:
            correlations = result.data["correlations"]
            # ヒートマップ形式に変換
            return Chart(
                chart_type=ChartType.TABLE,
                title=title or "Correlation Matrix",
                data={"correlations": correlations},
            )

        # デフォルト
        return Chart(
            chart_type=chart_type,
            title=title or result.summary,
            data=result.data,
        )
