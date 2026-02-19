"""分析エンジン.

統計分析、トレンド分析、異常検出を提供。
"""

from __future__ import annotations

import logging
import math
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.skills.builtin.bi_analytics.connector import DataFrame


logger = logging.getLogger(__name__)


class AnalysisType(str, Enum):
    """分析種別."""

    STATISTICAL = "statistical"  # 基本統計
    TREND = "trend"  # トレンド分析
    ANOMALY = "anomaly"  # 異常検出
    CORRELATION = "correlation"  # 相関分析
    DISTRIBUTION = "distribution"  # 分布分析


@dataclass
class AnalysisResult:
    """分析結果."""

    analysis_type: AnalysisType
    success: bool = True
    data: dict[str, Any] = field(default_factory=dict)
    summary: str = ""
    insights: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    analyzed_at: datetime = field(default_factory=datetime.now)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "analysis_type": self.analysis_type.value,
            "success": self.success,
            "data": self.data,
            "summary": self.summary,
            "insights": self.insights,
            "warnings": self.warnings,
            "analyzed_at": self.analyzed_at.isoformat(),
        }


class BIAnalyzer:
    """BI分析エンジン.

    データフレームに対して各種分析を実行。

    Example:
        >>> analyzer = BIAnalyzer()
        >>> result = await analyzer.analyze(df, AnalysisType.STATISTICAL, column="amount")
    """

    def __init__(self, llm_client: Any = None) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（インサイト生成用）
        """
        self._llm = llm_client
        self._logger = logging.getLogger(__name__)

    async def analyze(
        self,
        data: DataFrame,
        analysis_type: AnalysisType | str,
        column: str | None = None,
        **kwargs: Any,
    ) -> AnalysisResult:
        """分析を実行.

        Args:
            data: データフレーム
            analysis_type: 分析種別
            column: 対象カラム（オプション）
            **kwargs: 追加パラメータ

        Returns:
            分析結果
        """
        if isinstance(analysis_type, str):
            analysis_type = AnalysisType(analysis_type)

        self._logger.info(f"分析開始: type={analysis_type.value}, rows={len(data)}")

        if analysis_type == AnalysisType.STATISTICAL:
            return await self._statistical_analysis(data, column)
        if analysis_type == AnalysisType.TREND:
            return await self._trend_analysis(data, column, kwargs.get("date_column"))
        if analysis_type == AnalysisType.ANOMALY:
            return await self._anomaly_analysis(data, column)
        if analysis_type == AnalysisType.CORRELATION:
            return await self._correlation_analysis(data)
        if analysis_type == AnalysisType.DISTRIBUTION:
            return await self._distribution_analysis(data, column)
        return AnalysisResult(
            analysis_type=analysis_type,
            success=False,
            summary=f"不明な分析タイプ: {analysis_type}",
        )

    async def _statistical_analysis(
        self,
        data: DataFrame,
        column: str | None = None,
    ) -> AnalysisResult:
        """基本統計分析."""
        stats: dict[str, Any] = {"row_count": len(data)}
        insights: list[str] = []

        # 対象カラムを決定
        columns = [column] if column else data.columns

        for col in columns:
            values = [row.get(col) for row in data.rows if isinstance(row.get(col), (int, float))]

            if not values:
                continue

            n = len(values)
            mean = sum(values) / n
            variance = sum((x - mean) ** 2 for x in values) / n
            std_dev = math.sqrt(variance)
            sorted_vals = sorted(values)

            col_stats = {
                "count": n,
                "mean": round(mean, 2),
                "std": round(std_dev, 2),
                "min": min(values),
                "max": max(values),
                "median": sorted_vals[n // 2],
                "sum": sum(values),
            }
            stats[col] = col_stats

            # インサイト生成
            if std_dev / mean > 0.5 if mean != 0 else False:
                insights.append(
                    f"{col}: データのばらつきが大きい (CV={round(std_dev / mean * 100, 1)}%)"
                )

        return AnalysisResult(
            analysis_type=AnalysisType.STATISTICAL,
            data=stats,
            summary=f"{len(columns)}カラムの基本統計を算出しました",
            insights=insights,
        )

    async def _trend_analysis(
        self,
        data: DataFrame,
        value_column: str | None = None,
        date_column: str | None = None,
    ) -> AnalysisResult:
        """トレンド分析."""
        if not value_column:
            # 数値カラムを自動検出
            for col in data.columns:
                if any(isinstance(row.get(col), (int, float)) for row in data.rows):
                    value_column = col
                    break

        if not value_column:
            return AnalysisResult(
                analysis_type=AnalysisType.TREND,
                success=False,
                summary="数値カラムが見つかりませんでした",
            )

        values = [
            row.get(value_column)
            for row in data.rows
            if isinstance(row.get(value_column), (int, float))
        ]

        if len(values) < 2:
            return AnalysisResult(
                analysis_type=AnalysisType.TREND,
                success=False,
                summary="トレンド分析には2点以上のデータが必要です",
            )

        # 単純な線形トレンドを計算
        n = len(values)
        x_mean = (n - 1) / 2
        y_mean = sum(values) / n

        numerator = sum((i - x_mean) * (values[i] - y_mean) for i in range(n))
        denominator = sum((i - x_mean) ** 2 for i in range(n))

        slope = numerator / denominator if denominator != 0 else 0
        intercept = y_mean - slope * x_mean

        # トレンド判定
        if slope > 0:
            trend = "上昇"
        elif slope < 0:
            trend = "下降"
        else:
            trend = "横ばい"

        # 変化率
        change_rate = (values[-1] - values[0]) / values[0] * 100 if values[0] != 0 else 0

        insights: list[str] = []
        if abs(change_rate) > 20:
            insights.append(f"期間全体で{round(change_rate, 1)}%の変化がありました")

        return AnalysisResult(
            analysis_type=AnalysisType.TREND,
            data={
                "column": value_column,
                "trend": trend,
                "slope": round(slope, 4),
                "intercept": round(intercept, 2),
                "change_rate": round(change_rate, 2),
                "first_value": values[0],
                "last_value": values[-1],
            },
            summary=f"{value_column}は{trend}傾向です（変化率: {round(change_rate, 1)}%）",
            insights=insights,
        )

    async def _anomaly_analysis(
        self,
        data: DataFrame,
        column: str | None = None,
    ) -> AnalysisResult:
        """異常検出."""
        if not column:
            for col in data.columns:
                if any(isinstance(row.get(col), (int, float)) for row in data.rows):
                    column = col
                    break

        if not column:
            return AnalysisResult(
                analysis_type=AnalysisType.ANOMALY,
                success=False,
                summary="数値カラムが見つかりませんでした",
            )

        values = [
            (i, row.get(column))
            for i, row in enumerate(data.rows)
            if isinstance(row.get(column), (int, float))
        ]

        if len(values) < 3:
            return AnalysisResult(
                analysis_type=AnalysisType.ANOMALY,
                success=False,
                summary="異常検出には3点以上のデータが必要です",
            )

        # IQR法による外れ値検出
        sorted_values = sorted(v for _, v in values)
        n = len(sorted_values)
        q1 = sorted_values[n // 4]
        q3 = sorted_values[3 * n // 4]
        iqr = q3 - q1

        lower_bound = q1 - 1.5 * iqr
        upper_bound = q3 + 1.5 * iqr

        anomalies = [
            {"index": idx, "value": val}
            for idx, val in values
            if val < lower_bound or val > upper_bound
        ]

        insights: list[str] = []
        if anomalies:
            insights.append(f"{len(anomalies)}件の異常値を検出しました")

        return AnalysisResult(
            analysis_type=AnalysisType.ANOMALY,
            data={
                "column": column,
                "method": "IQR",
                "lower_bound": round(lower_bound, 2),
                "upper_bound": round(upper_bound, 2),
                "anomaly_count": len(anomalies),
                "anomalies": anomalies[:10],  # 最大10件
            },
            summary=f"{column}で{len(anomalies)}件の異常値を検出しました",
            insights=insights,
        )

    async def _correlation_analysis(self, data: DataFrame) -> AnalysisResult:
        """相関分析."""
        # 数値カラムを抽出
        numeric_columns = [
            col
            for col in data.columns
            if any(isinstance(row.get(col), (int, float)) for row in data.rows)
        ]

        if len(numeric_columns) < 2:
            return AnalysisResult(
                analysis_type=AnalysisType.CORRELATION,
                success=False,
                summary="相関分析には2つ以上の数値カラムが必要です",
            )

        # ピアソン相関係数を計算
        correlations: dict[str, dict[str, float]] = {}
        insights: list[str] = []

        for i, col1 in enumerate(numeric_columns):
            correlations[col1] = {}
            for col2 in numeric_columns[i + 1 :]:
                corr = self._pearson_correlation(data, col1, col2)
                correlations[col1][col2] = round(corr, 3)

                if abs(corr) > 0.7:
                    direction = "正" if corr > 0 else "負"
                    insights.append(
                        f"{col1}と{col2}に強い{direction}の相関があります (r={round(corr, 2)})"
                    )

        return AnalysisResult(
            analysis_type=AnalysisType.CORRELATION,
            data={"correlations": correlations},
            summary=f"{len(numeric_columns)}カラム間の相関を分析しました",
            insights=insights,
        )

    def _pearson_correlation(
        self,
        data: DataFrame,
        col1: str,
        col2: str,
    ) -> float:
        """ピアソン相関係数を計算."""
        pairs = [
            (row.get(col1), row.get(col2))
            for row in data.rows
            if isinstance(row.get(col1), (int, float)) and isinstance(row.get(col2), (int, float))
        ]

        if len(pairs) < 2:
            return 0.0

        n = len(pairs)
        x_vals = [p[0] for p in pairs]
        y_vals = [p[1] for p in pairs]

        x_mean = sum(x_vals) / n
        y_mean = sum(y_vals) / n

        numerator = sum((x - x_mean) * (y - y_mean) for x, y in pairs)
        denom_x = math.sqrt(sum((x - x_mean) ** 2 for x in x_vals))
        denom_y = math.sqrt(sum((y - y_mean) ** 2 for y in y_vals))

        if denom_x == 0 or denom_y == 0:
            return 0.0

        return numerator / (denom_x * denom_y)

    async def _distribution_analysis(
        self,
        data: DataFrame,
        column: str | None = None,
    ) -> AnalysisResult:
        """分布分析."""
        if not column:
            for col in data.columns:
                if any(isinstance(row.get(col), (int, float)) for row in data.rows):
                    column = col
                    break

        if not column:
            return AnalysisResult(
                analysis_type=AnalysisType.DISTRIBUTION,
                success=False,
                summary="数値カラムが見つかりませんでした",
            )

        values = [row.get(column) for row in data.rows if isinstance(row.get(column), (int, float))]

        if not values:
            return AnalysisResult(
                analysis_type=AnalysisType.DISTRIBUTION,
                success=False,
                summary="データがありません",
            )

        n = len(values)
        mean = sum(values) / n
        variance = sum((x - mean) ** 2 for x in values) / n
        std_dev = math.sqrt(variance)

        # 歪度（Skewness）
        skewness = sum((x - mean) ** 3 for x in values) / (n * std_dev**3) if std_dev != 0 else 0

        # 尖度（Kurtosis）
        kurtosis = (
            sum((x - mean) ** 4 for x in values) / (n * std_dev**4) - 3 if std_dev != 0 else 0
        )

        # ヒストグラムデータ
        min_val = min(values)
        max_val = max(values)
        bin_count = min(10, n)
        bin_width = (max_val - min_val) / bin_count if max_val != min_val else 1

        histogram: list[dict[str, Any]] = []
        for i in range(bin_count):
            bin_start = min_val + i * bin_width
            bin_end = bin_start + bin_width
            count = sum(1 for v in values if bin_start <= v < bin_end)
            histogram.append(
                {
                    "bin_start": round(bin_start, 2),
                    "bin_end": round(bin_end, 2),
                    "count": count,
                }
            )

        insights: list[str] = []
        if abs(skewness) > 1:
            direction = "右" if skewness > 0 else "左"
            insights.append(f"分布は{direction}に偏っています")
        if kurtosis > 1:
            insights.append("分布は尖っています（外れ値が多い可能性）")

        return AnalysisResult(
            analysis_type=AnalysisType.DISTRIBUTION,
            data={
                "column": column,
                "skewness": round(skewness, 3),
                "kurtosis": round(kurtosis, 3),
                "histogram": histogram,
            },
            summary=f"{column}の分布を分析しました（歪度: {round(skewness, 2)}）",
            insights=insights,
        )
