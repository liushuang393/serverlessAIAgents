"""Chart Service - フレームワーク級チャート生成サービス.

データからチャートを自動生成する再利用可能なサービス。
ECharts/Chart.js互換フォーマットを出力。

機能:
- データ分析によるチャートタイプ自動推薦
- 複数チャートフォーマット対応（ECharts, Chart.js）
- カスタマイズ可能なスタイル

使用例:
    >>> from agentflow.services import ChartService
    >>>
    >>> service = ChartService()
    >>> result = await service.execute(
    ...     action="generate",
    ...     data=[{"name": "A", "value": 100}, ...],
    ...     columns=["name", "value"],
    ... )
"""

from __future__ import annotations

import logging
import time
from dataclasses import dataclass, field
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow.services.base import (
    ServiceBase,
    ServiceEvent,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


# =============================================================================
# 設定・型定義
# =============================================================================


class ChartType(str, Enum):
    """チャートタイプ."""

    BAR = "bar"
    LINE = "line"
    PIE = "pie"
    SCATTER = "scatter"
    AREA = "area"
    RADAR = "radar"
    HEATMAP = "heatmap"
    TABLE = "table"
    # 増強: Redash/Superset 参照追加タイプ
    STACKED_BAR = "stacked_bar"
    GROUPED_BAR = "grouped_bar"
    DONUT = "donut"
    FUNNEL = "funnel"
    TREEMAP = "treemap"
    PIVOT = "pivot"
    COMBO = "combo"  # 棒グラフ + 折れ線グラフ


class ChartFormat(str, Enum):
    """出力フォーマット."""

    ECHARTS = "echarts"
    CHARTJS = "chartjs"
    BOTH = "both"


# =============================================================================
# ダッシュボード・ドリルダウン設定（Redash/Superset 参照）
# =============================================================================


@dataclass
class DrillDownConfig:
    """ドリルダウン設定 - クリック時の詳細表示設定."""

    enabled: bool = True
    drill_fields: list[str] = field(default_factory=list)  # ドリルダウン可能なフィールド
    drill_query_template: str = ""  # ドリルダウン時のクエリテンプレート
    max_drill_depth: int = 3  # 最大ドリルダウン階層


@dataclass
class DashboardPanel:
    """ダッシュボードパネル - 1つのチャート設定."""

    panel_id: str
    title: str
    chart_type: ChartType
    data_query: str = ""
    width: int = 6  # 12カラムグリッドで幅（1-12）
    height: int = 4  # 高さ単位
    x: int = 0  # グリッド位置 X
    y: int = 0  # グリッド位置 Y
    drill_down: DrillDownConfig = field(default_factory=DrillDownConfig)
    filters: dict[str, Any] = field(default_factory=dict)


@dataclass
class DashboardConfig:
    """ダッシュボード設定."""

    dashboard_id: str
    title: str
    panels: list[DashboardPanel] = field(default_factory=list)
    global_filters: list[str] = field(default_factory=list)  # ダッシュボード全体フィルタ
    refresh_interval: int = 0  # 自動更新間隔（秒、0=無効）
    theme: str = "light"


@dataclass
class ChartRecommendation:
    """チャート推薦結果 - より詳細な分析結果."""

    primary_type: ChartType
    primary_reason: str
    confidence: float
    alternatives: list[tuple[ChartType, str, float]]  # (type, reason, confidence)
    data_characteristics: dict[str, Any]  # データ特性分析結果


@dataclass
class ChartConfig:
    """チャート設定."""

    default_type: ChartType = ChartType.BAR
    format: ChartFormat = ChartFormat.ECHARTS
    theme: str = "default"
    max_data_points: int = 100
    auto_recommend: bool = True
    # 増強: ドリルダウン・ダッシュボード対応
    enable_drill_down: bool = True
    enable_interactivity: bool = True
    color_palette: list[str] = field(
        default_factory=lambda: [
            "#5470c6",
            "#91cc75",
            "#fac858",
            "#ee6666",
            "#73c0de",
            "#3ba272",
            "#fc8452",
            "#9a60b4",
            "#ea7ccc",
        ]
    )
    pivot_aggregation: str = "sum"  # sum, count, avg, min, max

    @classmethod
    def get_config_fields(cls) -> list[dict[str, Any]]:
        """Studio 設定フィールド定義."""
        return [
            {
                "name": "default_type",
                "type": "select",
                "label": "デフォルトチャート",
                "options": [e.value for e in ChartType],
                "default": "bar",
            },
            {
                "name": "format",
                "type": "select",
                "label": "出力フォーマット",
                "options": [e.value for e in ChartFormat],
                "default": "echarts",
            },
            {
                "name": "max_data_points",
                "type": "number",
                "label": "最大データポイント",
                "default": 100,
                "min": 10,
                "max": 1000,
            },
            {
                "name": "auto_recommend",
                "type": "boolean",
                "label": "自動タイプ推薦",
                "default": True,
            },
            {
                "name": "enable_drill_down",
                "type": "boolean",
                "label": "ドリルダウン有効",
                "default": True,
            },
            {
                "name": "pivot_aggregation",
                "type": "select",
                "label": "ピボット集計方法",
                "options": ["sum", "count", "avg", "min", "max"],
                "default": "sum",
            },
        ]


@dataclass
class ChartOutput:
    """チャート出力."""

    chart_type: ChartType
    title: str
    echarts: dict[str, Any] | None = None
    chartjs: dict[str, Any] | None = None
    recommendation: str = ""
    # 増強: ドリルダウン・分析情報
    drill_down_config: DrillDownConfig | None = None
    data_characteristics: dict[str, Any] = field(default_factory=dict)


# =============================================================================
# Chart Service 実装
# =============================================================================


class ChartService(ServiceBase[dict[str, Any]]):
    """Chart Service - フレームワーク級サービス.

    Redash/Superset を参照した増強版可視化サービス。

    Actions:
    - generate: データからチャート生成
    - recommend: 最適なチャートタイプを推薦（詳細分析付き）
    - analyze: データ特性を分析（チャート推薦の根拠を提供）
    - pivot: ピボットテーブル生成
    - drill_down: ドリルダウンデータ取得
    """

    def __init__(self, config: ChartConfig | None = None) -> None:
        """初期化."""
        super().__init__()
        self._config = config or ChartConfig()

    async def _execute_internal(
        self,
        execution_id: str,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """内部実行ロジック."""
        action = kwargs.get("action", "generate")

        if action == "generate":
            async for event in self._do_generate(execution_id, **kwargs):
                yield event
        elif action == "recommend":
            async for event in self._do_recommend(execution_id, **kwargs):
                yield event
        elif action == "analyze":
            async for event in self._do_analyze(execution_id, **kwargs):
                yield event
        elif action == "pivot":
            async for event in self._do_pivot(execution_id, **kwargs):
                yield event
        elif action == "drill_down":
            async for event in self._do_drill_down(execution_id, **kwargs):
                yield event
        else:
            yield self._emit_error(execution_id, "invalid_action", f"不明なアクション: {action}")

    async def _do_generate(
        self,
        execution_id: str,
        data: list[dict[str, Any]] | None = None,
        columns: list[str] | None = None,
        chart_type: str | None = None,
        title: str = "",
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """チャート生成."""
        start_time = time.time()

        if not data:
            yield self._emit_error(execution_id, "no_data", "データがありません")
            return

        data = data[: self._config.max_data_points]
        columns = columns or (list(data[0].keys()) if data else [])

        yield self._emit_progress(execution_id, 20, "チャートタイプを分析中...", phase="analyze")

        if chart_type:
            selected_type = ChartType(chart_type)
            recommendation = ""
        elif self._config.auto_recommend:
            selected_type, recommendation = self._recommend_chart_type(data, columns)
        else:
            selected_type = self._config.default_type
            recommendation = ""

        yield self._emit_progress(execution_id, 60, "チャートを生成中...", phase="generate")

        echarts_config = None
        chartjs_config = None

        if self._config.format in (ChartFormat.ECHARTS, ChartFormat.BOTH):
            echarts_config = self._build_echarts(data, columns, selected_type, title)

        if self._config.format in (ChartFormat.CHARTJS, ChartFormat.BOTH):
            chartjs_config = self._build_chartjs(data, columns, selected_type, title)

        yield self._emit_result(
            execution_id,
            {
                "chart_type": selected_type.value,
                "title": title,
                "echarts": echarts_config,
                "chartjs": chartjs_config,
                "recommendation": recommendation,
                "data_points": len(data),
            },
            (time.time() - start_time) * 1000,
        )

    async def _do_recommend(
        self,
        execution_id: str,
        data: list[dict[str, Any]] | None = None,
        columns: list[str] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """チャートタイプ推薦."""
        start_time = time.time()

        if not data:
            yield self._emit_error(execution_id, "no_data", "データがありません")
            return

        columns = columns or (list(data[0].keys()) if data else [])
        selected_type, recommendation = self._recommend_chart_type(data, columns)

        yield self._emit_result(
            execution_id,
            {
                "recommended_type": selected_type.value,
                "recommendation": recommendation,
                "alternatives": self._get_alternatives(data, columns),
            },
            (time.time() - start_time) * 1000,
        )

    def _recommend_chart_type(
        self,
        data: list[dict[str, Any]],
        columns: list[str],
    ) -> tuple[ChartType, str]:
        """チャートタイプを推薦."""
        if not data or not columns:
            return ChartType.TABLE, "データが少ないためテーブル表示を推奨"

        row_count = len(data)
        len(columns)

        numeric_cols = []
        text_cols = []
        date_cols = []

        for col in columns:
            sample_values = [r.get(col) for r in data[:10] if r.get(col) is not None]
            if not sample_values:
                continue

            first_val = sample_values[0]
            if isinstance(first_val, (int, float)):
                numeric_cols.append(col)
            elif self._is_date_like(first_val):
                date_cols.append(col)
            else:
                text_cols.append(col)

        if date_cols and numeric_cols:
            return ChartType.LINE, "時系列データのため折れ線グラフを推奨"

        if len(text_cols) == 1 and len(numeric_cols) == 1 and row_count <= 8:
            return ChartType.PIE, "カテゴリ別の割合表示のため円グラフを推奨"

        if text_cols and numeric_cols:
            return ChartType.BAR, "カテゴリ別比較のため棒グラフを推奨"

        if len(numeric_cols) >= 2:
            return ChartType.SCATTER, "数値間の相関分析のため散布図を推奨"

        return ChartType.TABLE, "データ構造が複雑なためテーブル表示を推奨"

    def _get_alternatives(
        self,
        data: list[dict[str, Any]],
        columns: list[str],
    ) -> list[dict[str, str]]:
        """代替チャートタイプを取得."""
        alternatives = []

        if len(data) <= 20:
            alternatives.append({"type": "pie", "reason": "少量データに適している"})

        if len(data) > 5:
            alternatives.append({"type": "bar", "reason": "比較分析に適している"})
            alternatives.append({"type": "line", "reason": "トレンド表示に適している"})

        alternatives.append({"type": "table", "reason": "詳細データの確認に適している"})

        return alternatives[:3]

    def _build_echarts(
        self,
        data: list[dict[str, Any]],
        columns: list[str],
        chart_type: ChartType,
        title: str,
    ) -> dict[str, Any]:
        """ECharts設定を構築."""
        x_col = columns[0]
        y_col = columns[1] if len(columns) > 1 else columns[0]

        labels = [str(r.get(x_col, "")) for r in data]
        values = [r.get(y_col, 0) for r in data]

        config: dict[str, Any] = {
            "title": {"text": title, "left": "center"},
            "tooltip": {"trigger": "item" if chart_type == ChartType.PIE else "axis"},
        }

        if chart_type == ChartType.PIE:
            config["series"] = [
                {
                    "type": "pie",
                    "radius": "50%",
                    "data": [{"name": l, "value": v} for l, v in zip(labels, values, strict=False)],
                }
            ]
        elif chart_type == ChartType.SCATTER:
            if len(columns) >= 2:
                scatter_data = [[r.get(columns[0], 0), r.get(columns[1], 0)] for r in data]
                config["xAxis"] = {"type": "value"}
                config["yAxis"] = {"type": "value"}
                config["series"] = [{"type": "scatter", "data": scatter_data}]
        else:
            config["xAxis"] = {"type": "category", "data": labels}
            config["yAxis"] = {"type": "value"}
            config["series"] = [
                {
                    "type": chart_type.value,
                    "data": values,
                }
            ]

        return config

    def _build_chartjs(
        self,
        data: list[dict[str, Any]],
        columns: list[str],
        chart_type: ChartType,
        title: str,
    ) -> dict[str, Any]:
        """Chart.js設定を構築."""
        x_col = columns[0]
        y_col = columns[1] if len(columns) > 1 else columns[0]

        labels = [str(r.get(x_col, "")) for r in data]
        values = [r.get(y_col, 0) for r in data]

        chartjs_type = chart_type.value
        if chartjs_type == "area":
            chartjs_type = "line"

        return {
            "type": chartjs_type,
            "data": {
                "labels": labels,
                "datasets": [
                    {
                        "label": y_col,
                        "data": values,
                        "fill": chart_type == ChartType.AREA,
                    }
                ],
            },
            "options": {
                "responsive": True,
                "plugins": {
                    "title": {"display": True, "text": title},
                },
            },
        }

    def _is_date_like(self, value: Any) -> bool:
        """値が日付っぽいか判定."""
        import re

        if value is None:
            return False
        s = str(value)
        date_patterns = [r"\d{4}-\d{2}-\d{2}", r"\d{2}/\d{2}/\d{4}", r"\d{4}/\d{2}/\d{2}"]
        return any(re.match(p, s) for p in date_patterns)

    # =========================================================================
    # 増強: データ分析・ピボット・ドリルダウン機能
    # =========================================================================

    async def _do_analyze(
        self,
        execution_id: str,
        data: list[dict[str, Any]] | None = None,
        columns: list[str] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """データ特性を詳細分析してチャート推薦根拠を提供."""
        start_time = time.time()

        if not data:
            yield self._emit_error(execution_id, "no_data", "データがありません")
            return

        columns = columns or (list(data[0].keys()) if data else [])
        characteristics = self._analyze_data_characteristics(data, columns)
        recommendation = self._recommend_chart_with_details(data, columns, characteristics)

        yield self._emit_result(
            execution_id,
            {
                "characteristics": characteristics,
                "recommendation": {
                    "primary_type": recommendation.primary_type.value,
                    "primary_reason": recommendation.primary_reason,
                    "confidence": recommendation.confidence,
                    "alternatives": [
                        {"type": t.value, "reason": r, "confidence": c} for t, r, c in recommendation.alternatives
                    ],
                },
            },
            (time.time() - start_time) * 1000,
        )

    async def _do_pivot(
        self,
        execution_id: str,
        data: list[dict[str, Any]] | None = None,
        rows: list[str] | None = None,
        cols: list[str] | None = None,
        values: str | None = None,
        aggregation: str | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """ピボットテーブル生成."""
        start_time = time.time()

        if not data:
            yield self._emit_error(execution_id, "no_data", "データがありません")
            return

        rows = rows or []
        cols = cols or []
        aggregation = aggregation or self._config.pivot_aggregation

        yield self._emit_progress(execution_id, 50, "ピボットテーブルを生成中...", phase="pivot")

        pivot_result = self._build_pivot_table(data, rows, cols, values, aggregation)

        yield self._emit_result(
            execution_id,
            {
                "pivot_data": pivot_result["data"],
                "row_headers": pivot_result["row_headers"],
                "col_headers": pivot_result["col_headers"],
                "aggregation": aggregation,
            },
            (time.time() - start_time) * 1000,
        )

    async def _do_drill_down(
        self,
        execution_id: str,
        data: list[dict[str, Any]] | None = None,
        drill_field: str | None = None,
        drill_value: Any = None,
        parent_filters: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """ドリルダウンデータ取得."""
        start_time = time.time()

        if not data:
            yield self._emit_error(execution_id, "no_data", "データがありません")
            return

        if not drill_field:
            yield self._emit_error(execution_id, "no_drill_field", "ドリルダウンフィールドを指定してください")
            return

        parent_filters = parent_filters or {}

        # フィルタリング
        filtered_data = data
        for key, val in parent_filters.items():
            filtered_data = [r for r in filtered_data if r.get(key) == val]

        if drill_value is not None:
            filtered_data = [r for r in filtered_data if r.get(drill_field) == drill_value]

        # ドリルダウン可能なフィールドを検出
        next_drill_fields = self._detect_drill_fields(filtered_data, drill_field)

        yield self._emit_result(
            execution_id,
            {
                "filtered_data": filtered_data[: self._config.max_data_points],
                "total_count": len(filtered_data),
                "drill_field": drill_field,
                "drill_value": drill_value,
                "next_drill_fields": next_drill_fields,
            },
            (time.time() - start_time) * 1000,
        )

    def _analyze_data_characteristics(
        self,
        data: list[dict[str, Any]],
        columns: list[str],
    ) -> dict[str, Any]:
        """データ特性を詳細分析."""
        characteristics: dict[str, Any] = {
            "row_count": len(data),
            "column_count": len(columns),
            "columns": {},
        }

        for col in columns:
            col_data = [r.get(col) for r in data if r.get(col) is not None]
            if not col_data:
                continue

            col_info: dict[str, Any] = {
                "non_null_count": len(col_data),
                "null_count": len(data) - len(col_data),
            }

            sample = col_data[0]
            if isinstance(sample, (int, float)):
                numeric_values = [float(v) for v in col_data if isinstance(v, (int, float))]
                if not numeric_values:
                    continue
                col_info["type"] = "numeric"
                col_info["min"] = min(numeric_values)
                col_info["max"] = max(numeric_values)
                col_info["avg"] = sum(numeric_values) / len(numeric_values)
            elif self._is_date_like(sample):
                col_info["type"] = "date"
                col_info["is_time_series"] = True
            else:
                col_info["type"] = "text"
                unique_vals = {str(v) for v in col_data}
                col_info["unique_count"] = len(unique_vals)
                col_info["is_categorical"] = len(unique_vals) <= 20

            characteristics["columns"][col] = col_info

        # 時系列判定
        characteristics["has_time_series"] = any(c.get("type") == "date" for c in characteristics["columns"].values())
        # カテゴリカル判定
        characteristics["has_categorical"] = any(
            c.get("is_categorical", False) for c in characteristics["columns"].values()
        )

        return characteristics

    def _recommend_chart_with_details(
        self,
        data: list[dict[str, Any]],
        columns: list[str],
        characteristics: dict[str, Any],
    ) -> ChartRecommendation:
        """詳細なチャート推薦."""
        row_count = characteristics["row_count"]
        has_time_series = characteristics.get("has_time_series", False)
        has_categorical = characteristics.get("has_categorical", False)

        numeric_cols = [c for c, info in characteristics["columns"].items() if info.get("type") == "numeric"]
        text_cols = [c for c, info in characteristics["columns"].items() if info.get("type") == "text"]
        [c for c, info in characteristics["columns"].items() if info.get("type") == "date"]

        alternatives: list[tuple[ChartType, str, float]] = []

        # 推薦ロジック（Redash/Superset 参照）
        if has_time_series and numeric_cols:
            primary = ChartType.LINE
            reason = "時系列データのためトレンド表示に最適"
            confidence = 0.95
            alternatives.append((ChartType.AREA, "塗りつぶしで累積効果を表示", 0.85))
            alternatives.append((ChartType.BAR, "期間別比較にも適している", 0.70))
        elif len(text_cols) == 1 and len(numeric_cols) == 1 and row_count <= 8:
            primary = ChartType.PIE
            reason = "カテゴリ別の割合表示に最適"
            confidence = 0.90
            alternatives.append((ChartType.DONUT, "中央にKPI表示可能", 0.85))
            alternatives.append((ChartType.BAR, "比較分析にも適している", 0.75))
        elif has_categorical and numeric_cols:
            primary = ChartType.BAR
            reason = "カテゴリ別比較に最適"
            confidence = 0.88
            alternatives.append((ChartType.STACKED_BAR, "構成比表示にも適している", 0.80))
            alternatives.append((ChartType.PIE, "割合表示にも適している", 0.65))
        elif len(numeric_cols) >= 2:
            primary = ChartType.SCATTER
            reason = "数値間の相関分析に最適"
            confidence = 0.85
            alternatives.append((ChartType.HEATMAP, "密度分布の表示に適している", 0.70))
        else:
            primary = ChartType.TABLE
            reason = "データ構造が複雑なため詳細表示を推奨"
            confidence = 0.60
            alternatives.append((ChartType.BAR, "数値があれば棒グラフも可能", 0.50))

        return ChartRecommendation(
            primary_type=primary,
            primary_reason=reason,
            confidence=confidence,
            alternatives=alternatives,
            data_characteristics=characteristics,
        )

    def _build_pivot_table(
        self,
        data: list[dict[str, Any]],
        rows: list[str],
        cols: list[str],
        values: str | None,
        aggregation: str,
    ) -> dict[str, Any]:
        """ピボットテーブルを構築."""
        from collections import defaultdict

        # 行・列ヘッダーを収集
        row_headers: list[tuple[Any, ...]] = []
        col_headers: list[tuple[Any, ...]] = []
        pivot_data: dict[tuple[Any, ...], dict[tuple[Any, ...], list[Any]]] = defaultdict(lambda: defaultdict(list))

        for record in data:
            row_key = tuple(record.get(r, "") for r in rows) if rows else ("",)
            col_key = tuple(record.get(c, "") for c in cols) if cols else ("",)
            val = record.get(values, 0) if values else 1

            if row_key not in row_headers:
                row_headers.append(row_key)
            if col_key not in col_headers:
                col_headers.append(col_key)

            pivot_data[row_key][col_key].append(val)

        # 集計実行
        result_data: list[list[Any]] = []
        for row_key in row_headers:
            row: list[Any] = list(row_key)
            for col_key in col_headers:
                cell_values = pivot_data[row_key][col_key]
                if not cell_values:
                    aggregated = 0
                elif aggregation == "sum":
                    aggregated = sum(cell_values)
                elif aggregation == "count":
                    aggregated = len(cell_values)
                elif aggregation == "avg":
                    aggregated = sum(cell_values) / len(cell_values)
                elif aggregation == "min":
                    aggregated = min(cell_values)
                elif aggregation == "max":
                    aggregated = max(cell_values)
                else:
                    aggregated = sum(cell_values)
                row.append(aggregated)
            result_data.append(row)

        return {
            "data": result_data,
            "row_headers": [list(r) for r in row_headers],
            "col_headers": [list(c) for c in col_headers],
        }

    def _detect_drill_fields(
        self,
        data: list[dict[str, Any]],
        current_field: str,
    ) -> list[str]:
        """ドリルダウン可能なフィールドを検出."""
        if not data:
            return []

        all_fields = list(data[0].keys())
        drill_fields = []

        for field in all_fields:
            if field == current_field:
                continue
            # カテゴリカルなフィールドのみドリルダウン対象
            unique_vals = {str(r.get(field, "")) for r in data[:100]}
            if 2 <= len(unique_vals) <= 50:
                drill_fields.append(field)

        return drill_fields[:5]  # 最大5フィールド

    # =========================================================================
    # Studio 統合用メソッド
    # =========================================================================

    @classmethod
    def get_node_definition(cls) -> dict[str, Any]:
        """Studio ノード定義."""
        return {
            "type": "chart",
            "label": "チャート生成",
            "category": "visualization",
            "icon": "chart-bar",
            "description": "データからチャートを自動生成",
            "inputs": [
                {"name": "data", "type": "array", "label": "データ"},
                {"name": "columns", "type": "array", "label": "カラム", "optional": True},
                {"name": "title", "type": "string", "label": "タイトル", "optional": True},
            ],
            "outputs": [
                {"name": "echarts", "type": "object", "label": "ECharts設定"},
                {"name": "chartjs", "type": "object", "label": "Chart.js設定"},
                {"name": "chart_type", "type": "string", "label": "チャートタイプ"},
            ],
            "config": ChartConfig.get_config_fields(),
        }


__all__ = [
    "ChartConfig",
    "ChartFormat",
    "ChartOutput",
    "ChartRecommendation",
    "ChartService",
    "ChartType",
    "DashboardConfig",
    "DashboardPanel",
    # 増強: ダッシュボード・ドリルダウン
    "DrillDownConfig",
]
