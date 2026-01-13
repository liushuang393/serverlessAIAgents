# -*- coding: utf-8 -*-
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
from collections.abc import AsyncIterator
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

from agentflow.services.base import (
    ServiceBase,
    ServiceEvent,
    ResultEvent,
)

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


class ChartFormat(str, Enum):
    """出力フォーマット."""
    ECHARTS = "echarts"
    CHARTJS = "chartjs"
    BOTH = "both"


@dataclass
class ChartConfig:
    """チャート設定."""
    
    default_type: ChartType = ChartType.BAR
    format: ChartFormat = ChartFormat.ECHARTS
    theme: str = "default"
    max_data_points: int = 100
    auto_recommend: bool = True
    
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
        ]


@dataclass
class ChartOutput:
    """チャート出力."""
    chart_type: ChartType
    title: str
    echarts: dict[str, Any] | None = None
    chartjs: dict[str, Any] | None = None
    recommendation: str = ""


# =============================================================================
# Chart Service 実装
# =============================================================================


class ChartService(ServiceBase):
    """Chart Service - フレームワーク級サービス.
    
    Actions:
    - generate: データからチャート生成
    - recommend: 最適なチャートタイプを推薦
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
        
        data = data[:self._config.max_data_points]
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
        
        yield self._emit_result(execution_id, {
            "chart_type": selected_type.value,
            "title": title,
            "echarts": echarts_config,
            "chartjs": chartjs_config,
            "recommendation": recommendation,
            "data_points": len(data),
        }, (time.time() - start_time) * 1000)

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
        
        yield self._emit_result(execution_id, {
            "recommended_type": selected_type.value,
            "recommendation": recommendation,
            "alternatives": self._get_alternatives(data, columns),
        }, (time.time() - start_time) * 1000)

    def _recommend_chart_type(
        self,
        data: list[dict[str, Any]],
        columns: list[str],
    ) -> tuple[ChartType, str]:
        """チャートタイプを推薦."""
        if not data or not columns:
            return ChartType.TABLE, "データが少ないためテーブル表示を推奨"
        
        row_count = len(data)
        col_count = len(columns)
        
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
            config["series"] = [{
                "type": "pie",
                "radius": "50%",
                "data": [{"name": l, "value": v} for l, v in zip(labels, values)],
            }]
        elif chart_type == ChartType.SCATTER:
            if len(columns) >= 2:
                scatter_data = [[r.get(columns[0], 0), r.get(columns[1], 0)] for r in data]
                config["xAxis"] = {"type": "value"}
                config["yAxis"] = {"type": "value"}
                config["series"] = [{"type": "scatter", "data": scatter_data}]
        else:
            config["xAxis"] = {"type": "category", "data": labels}
            config["yAxis"] = {"type": "value"}
            config["series"] = [{
                "type": chart_type.value,
                "data": values,
            }]
        
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
                "datasets": [{
                    "label": y_col,
                    "data": values,
                    "fill": chart_type == ChartType.AREA,
                }],
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
        date_patterns = [r'\d{4}-\d{2}-\d{2}', r'\d{2}/\d{2}/\d{4}', r'\d{4}/\d{2}/\d{2}']
        return any(re.match(p, s) for p in date_patterns)

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
    "ChartService",
    "ChartConfig",
    "ChartOutput",
    "ChartType",
    "ChartFormat",
]
