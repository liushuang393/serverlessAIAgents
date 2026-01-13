# -*- coding: utf-8 -*-
"""BI Analytics Skill 単体テスト."""

from __future__ import annotations

import pytest

from agentflow.skills.builtin.bi_analytics.connector import (
    DataFrame,
    DataConnector,
    MemoryConnector,
    ConnectorType,
)
from agentflow.skills.builtin.bi_analytics.analyzer import (
    BIAnalyzer,
    AnalysisType,
    AnalysisResult,
)
from agentflow.skills.builtin.bi_analytics.visualizer import (
    ChartGenerator,
    ChartType,
    Chart,
)


class TestDataFrame:
    """DataFrame テスト."""

    def test_create_empty(self) -> None:
        """空のDataFrame."""
        df = DataFrame()
        assert len(df) == 0
        assert df.columns == []

    def test_create_with_data(self) -> None:
        """データ付きDataFrame."""
        data = [
            {"a": 1, "b": 2},
            {"a": 3, "b": 4},
        ]
        df = DataFrame(data)
        assert len(df) == 2
        assert "a" in df.columns
        assert "b" in df.columns

    def test_head(self) -> None:
        """先頭N行."""
        data = [{"x": i} for i in range(10)]
        df = DataFrame(data)
        head = df.head(3)
        assert len(head) == 3

    def test_describe(self) -> None:
        """基本統計."""
        data = [{"value": i} for i in [1, 2, 3, 4, 5]]
        df = DataFrame(data)
        stats = df.describe()
        assert stats["count"] == 5
        assert "value" in stats
        assert stats["value"]["min"] == 1
        assert stats["value"]["max"] == 5

    def test_to_csv(self) -> None:
        """CSV変換."""
        data = [{"a": 1, "b": 2}]
        df = DataFrame(data, columns=["a", "b"])
        csv = df.to_csv()
        assert "a,b" in csv
        assert "1,2" in csv


class TestMemoryConnector:
    """MemoryConnector テスト."""

    @pytest.fixture
    def connector(self) -> MemoryConnector:
        """コネクタのフィクスチャ."""
        conn = MemoryConnector()
        conn.add_sample_data()
        return conn

    @pytest.mark.asyncio
    async def test_connect(self, connector: MemoryConnector) -> None:
        """接続."""
        await connector.connect()
        assert connector.is_connected is True

    @pytest.mark.asyncio
    async def test_list_tables(self, connector: MemoryConnector) -> None:
        """テーブル一覧."""
        tables = await connector.list_tables()
        assert "sales" in tables
        assert "users" in tables

    @pytest.mark.asyncio
    async def test_query(self, connector: MemoryConnector) -> None:
        """クエリ."""
        result = await connector.query("SELECT * FROM sales")
        assert len(result) > 0

    def test_from_url(self) -> None:
        """URLから作成."""
        conn = DataConnector.from_url("memory://")
        assert isinstance(conn, MemoryConnector)


class TestBIAnalyzer:
    """BIAnalyzer テスト."""

    @pytest.fixture
    def analyzer(self) -> BIAnalyzer:
        """分析器のフィクスチャ."""
        return BIAnalyzer()

    @pytest.fixture
    def sample_data(self) -> DataFrame:
        """サンプルデータ."""
        return DataFrame([
            {"date": "2024-01-01", "amount": 100, "quantity": 10},
            {"date": "2024-01-02", "amount": 150, "quantity": 15},
            {"date": "2024-01-03", "amount": 120, "quantity": 12},
            {"date": "2024-01-04", "amount": 180, "quantity": 18},
            {"date": "2024-01-05", "amount": 200, "quantity": 20},
        ])

    @pytest.mark.asyncio
    async def test_statistical_analysis(
        self,
        analyzer: BIAnalyzer,
        sample_data: DataFrame,
    ) -> None:
        """統計分析."""
        result = await analyzer.analyze(sample_data, AnalysisType.STATISTICAL, column="amount")
        assert result.success is True
        assert "amount" in result.data
        assert result.data["amount"]["count"] == 5

    @pytest.mark.asyncio
    async def test_trend_analysis(
        self,
        analyzer: BIAnalyzer,
        sample_data: DataFrame,
    ) -> None:
        """トレンド分析."""
        result = await analyzer.analyze(sample_data, AnalysisType.TREND, column="amount")
        assert result.success is True
        assert result.data["trend"] == "上昇"

    @pytest.mark.asyncio
    async def test_anomaly_analysis(
        self,
        analyzer: BIAnalyzer,
    ) -> None:
        """異常検出."""
        data = DataFrame([
            {"value": 10},
            {"value": 12},
            {"value": 11},
            {"value": 100},  # 異常値
            {"value": 9},
        ])
        result = await analyzer.analyze(data, AnalysisType.ANOMALY, column="value")
        assert result.success is True
        assert result.data["anomaly_count"] >= 1

    @pytest.mark.asyncio
    async def test_correlation_analysis(
        self,
        analyzer: BIAnalyzer,
        sample_data: DataFrame,
    ) -> None:
        """相関分析."""
        result = await analyzer.analyze(sample_data, AnalysisType.CORRELATION)
        assert result.success is True
        assert "correlations" in result.data

    @pytest.mark.asyncio
    async def test_distribution_analysis(
        self,
        analyzer: BIAnalyzer,
        sample_data: DataFrame,
    ) -> None:
        """分布分析."""
        result = await analyzer.analyze(sample_data, AnalysisType.DISTRIBUTION, column="amount")
        assert result.success is True
        assert "histogram" in result.data


class TestChartGenerator:
    """ChartGenerator テスト."""

    @pytest.fixture
    def generator(self) -> ChartGenerator:
        """生成器のフィクスチャ."""
        return ChartGenerator()

    @pytest.fixture
    def sample_data(self) -> DataFrame:
        """サンプルデータ."""
        return DataFrame([
            {"category": "A", "value": 100},
            {"category": "B", "value": 150},
            {"category": "C", "value": 80},
        ])

    @pytest.mark.asyncio
    async def test_bar_chart(
        self,
        generator: ChartGenerator,
        sample_data: DataFrame,
    ) -> None:
        """棒グラフ."""
        chart = await generator.generate(
            sample_data,
            ChartType.BAR,
            x="category",
            y="value",
            title="Test Bar Chart",
        )
        assert chart.chart_type == ChartType.BAR
        assert chart.title == "Test Bar Chart"
        assert "labels" in chart.data

    @pytest.mark.asyncio
    async def test_line_chart(
        self,
        generator: ChartGenerator,
        sample_data: DataFrame,
    ) -> None:
        """折れ線グラフ."""
        chart = await generator.generate(sample_data, ChartType.LINE, x="category", y="value")
        assert chart.chart_type == ChartType.LINE

    @pytest.mark.asyncio
    async def test_pie_chart(
        self,
        generator: ChartGenerator,
        sample_data: DataFrame,
    ) -> None:
        """円グラフ."""
        chart = await generator.generate(sample_data, ChartType.PIE, x="category", y="value")
        assert chart.chart_type == ChartType.PIE

    @pytest.mark.asyncio
    async def test_scatter_chart(
        self,
        generator: ChartGenerator,
    ) -> None:
        """散布図."""
        data = DataFrame([
            {"x": 1, "y": 2},
            {"x": 2, "y": 4},
            {"x": 3, "y": 6},
        ])
        chart = await generator.generate(data, ChartType.SCATTER, x="x", y="y")
        assert chart.chart_type == ChartType.SCATTER

    @pytest.mark.asyncio
    async def test_table_chart(
        self,
        generator: ChartGenerator,
        sample_data: DataFrame,
    ) -> None:
        """テーブル."""
        chart = await generator.generate(sample_data, ChartType.TABLE)
        assert chart.chart_type == ChartType.TABLE
        assert "columns" in chart.data

    def test_chart_to_echarts(self) -> None:
        """ECharts形式変換."""
        chart = Chart(
            chart_type=ChartType.BAR,
            title="Test",
            data={
                "xAxis": {"data": ["A", "B"]},
                "series": [{"type": "bar", "data": [1, 2]}],
            },
        )
        echarts = chart.to_echarts()
        assert echarts["title"]["text"] == "Test"

    def test_chart_to_chartjs(self) -> None:
        """Chart.js形式変換."""
        chart = Chart(
            chart_type=ChartType.BAR,
            title="Test",
            data={
                "labels": ["A", "B"],
                "datasets": [{"data": [1, 2]}],
            },
        )
        chartjs = chart.to_chartjs()
        assert chartjs["type"] == "bar"

    @pytest.mark.asyncio
    async def test_from_analysis_result(
        self,
        generator: ChartGenerator,
    ) -> None:
        """分析結果からチャート."""
        result = AnalysisResult(
            analysis_type=AnalysisType.DISTRIBUTION,
            data={
                "column": "value",
                "histogram": [
                    {"bin_start": 0, "bin_end": 10, "count": 5},
                    {"bin_start": 10, "bin_end": 20, "count": 3},
                ],
            },
        )
        chart = await generator.generate(result, ChartType.BAR, title="Distribution")
        assert chart.title == "Distribution"
