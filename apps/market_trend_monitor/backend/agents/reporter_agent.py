"""レポート生成エージェント.

分析結果から読みやすいレポートを生成します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
- 型安全：Pydantic による I/O 検証
- 健壮性：ResilientAgent によるリトライ・タイムアウト制御
"""

import logging
import uuid
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.models import (
    Report,
    ReporterInput,
    ReporterOutput,
    ReportSchema,
    ReportSection,
    ReportSectionSchema,
    SentimentType,
    Trend,
    TrendSchema,
)

from agentflow import ResilientAgent


class ReporterAgent(ResilientAgent[ReporterInput, ReporterOutput]):
    """レポート生成エージェント（ResilientAgent 継承・型安全）.

    役割:
    - 分析結果からレポート生成
    - Markdown/HTML フォーマット
    - グラフデータ生成

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます（松耦合）。
        ResilientAgent によりリトライ・タイムアウトが自動制御されます。
    """

    # ResilientAgent 設定
    name = "ReporterAgent"
    temperature = 0.7  # レポート生成は高め

    def __init__(self) -> None:
        """初期化.

        Note:
            LLM クライアントは get_llm() により自動取得されます（松耦合原則）。
        """
        super().__init__()  # ResilientAgent が内部で get_llm() を呼び出す
        self._logger = logging.getLogger(self.name)

    def _parse_input(self, input_data: dict[str, Any]) -> ReporterInput:
        """入力データを Pydantic モデルに変換."""
        return ReporterInput(**input_data)

    async def process(self, input_data: ReporterInput) -> ReporterOutput:
        """レポート生成を実行.

        Args:
            input_data: 型付き入力データ

        Returns:
            型付きレポート生成結果
        """
        trends_data = input_data.trends
        summary = input_data.summary
        period = input_data.period or datetime.now().strftime("%Y-W%U")

        self._logger.info(f"Starting report generation: period={period}")

        # TrendSchema から Trend に変換
        trends = [self._schema_to_trend(t) for t in trends_data]

        # レポート生成
        report = await self._generate_report(trends, summary, period)

        self._logger.info(f"Report generated: {report.id}")

        # Report を ReportSchema に変換
        report_schema = ReportSchema(
            id=report.id,
            title=report.title,
            sections=[
                ReportSectionSchema(
                    title=s.title,
                    content=s.content,
                    charts=s.charts,
                    metadata=s.metadata,
                )
                for s in report.sections
            ],
            generated_at=report.generated_at.isoformat(),
            period=report.period,
            metadata=report.metadata,
        )

        return ReporterOutput(
            report=report_schema,
            formats=["markdown", "html"],
        )

    def _schema_to_trend(self, schema: TrendSchema) -> Trend:
        """TrendSchema から Trend オブジェクトを生成."""
        # sentiment を SentimentType に変換
        sentiment = SentimentType.NEUTRAL
        if schema.sentiment in [s.value for s in SentimentType]:
            sentiment = SentimentType(schema.sentiment)

        return Trend(
            id=schema.id,
            topic=schema.topic,
            score=schema.score,
            articles_count=schema.articles_count,
            keywords=schema.keywords,
            sentiment=sentiment,
            growth_rate=schema.growth_rate,
            created_at=datetime.fromisoformat(schema.created_at),
            metadata=schema.metadata,
        )

    async def _generate_report(
        self,
        trends: list[Trend],
        summary: str,
        period: str,
    ) -> Report:
        """レポート生成.

        Args:
            trends: トレンドリスト
            summary: サマリーテキスト
            period: レポート期間

        Returns:
            レポートオブジェクト
        """
        sections: list[ReportSection] = []

        # 1. エグゼクティブサマリー
        executive_summary = await self._generate_executive_summary(summary, trends)
        sections.append(
            ReportSection(
                title="エグゼクティブサマリー",
                content=executive_summary,
                metadata={"section_type": "summary", "llm_generated": True},
            )
        )

        # 2. 主要トレンド
        trends_content = await self._generate_trends_section(trends)
        trends_chart = self._generate_trends_chart(trends)
        sections.append(
            ReportSection(
                title="主要トレンド",
                content=trends_content,
                charts=[trends_chart],
                metadata={"section_type": "trends", "llm_generated": True},
            )
        )

        # 3. 成長率分析
        growth_content = await self._generate_growth_section(trends)
        growth_chart = self._generate_growth_chart(trends)
        sections.append(
            ReportSection(
                title="成長率分析",
                content=growth_content,
                charts=[growth_chart],
                metadata={"section_type": "growth", "llm_generated": True},
            )
        )

        # レポート作成
        return Report(
            id=str(uuid.uuid4()),
            title=f"市場動向レポート ({period})",
            sections=sections,
            generated_at=datetime.now(),
            period=period,
            metadata={"trends_count": len(trends)},
        )

    async def _generate_executive_summary(self, summary: str, trends: list[Trend]) -> str:
        """LLM を使用してエグゼクティブサマリーを生成."""
        trends_text = "\n".join([
            f"- {t.topic}: {t.articles_count}件, 成長率{t.growth_rate:.1%}"
            for t in trends[:5]
        ])

        prompt = f"""Generate an executive summary for a market trend report in Japanese.
Base summary: {summary}

Top trends:
{trends_text}

Write a professional, insightful executive summary (150-200 words).

Executive Summary:"""

        try:
            response = await self._call_llm(prompt)
            return response if response else summary
        except Exception as e:
            self._logger.warning(f"Failed to generate executive summary: {e}")
            return summary

    async def _generate_trends_section(self, trends: list[Trend]) -> str:
        """LLM を使用してトレンドセクションを生成."""
        trends_text = "\n".join([
            f"{i+1}. {t.topic} (スコア: {t.score:.2f}, 記事数: {t.articles_count})"
            for i, t in enumerate(trends[:5])
        ])

        prompt = f"""Analyze and describe the following market trends in Japanese (200-250 words).

Trends:
{trends_text}

Analysis:"""

        try:
            response = await self._call_llm(prompt)
            return response if response else self._format_trends(trends)
        except Exception as e:
            self._logger.warning(f"Failed to generate trends section: {e}")
            return self._format_trends(trends)

    async def _generate_growth_section(self, trends: list[Trend]) -> str:
        """LLM を使用して成長率分析セクションを生成."""
        growing = [t for t in trends if t.growth_rate > 0]
        declining = [t for t in trends if t.growth_rate < 0]

        prompt = f"""Analyze the growth trends in Japanese (150-200 words).

Growing trends ({len(growing)}):
{', '.join([f"{t.topic} (+{t.growth_rate:.1%})" for t in growing[:5]])}

Declining trends ({len(declining)}):
{', '.join([f"{t.topic} ({t.growth_rate:.1%})" for t in declining[:5]])}

Analysis:"""

        try:
            response = await self._call_llm(prompt)
            return response if response else self._format_growth_analysis(trends)
        except Exception as e:
            self._logger.warning(f"Failed to generate growth section: {e}")
            return self._format_growth_analysis(trends)

    def _format_trends(self, trends: list[Trend]) -> str:
        """トレンドをフォーマット（フォールバック用）."""
        lines = ["## トレンド一覧\n"]
        for i, trend in enumerate(trends[:5], 1):
            lines.append(
                f"{i}. **{trend.topic}** "
                f"(スコア: {trend.score:.2f}, "
                f"記事数: {trend.articles_count}, "
                f"センチメント: {trend.sentiment.value})"
            )
        return "\n".join(lines)

    def _generate_trends_chart(self, trends: list[Trend]) -> dict[str, Any]:
        """トレンドグラフデータ生成."""
        return {
            "type": "bar",
            "title": "トレンドスコア",
            "data": [{"topic": t.topic, "score": t.score} for t in trends[:10]],
        }

    def _format_growth_analysis(self, trends: list[Trend]) -> str:
        """成長率分析をフォーマット."""
        growing = [t for t in trends if t.growth_rate > 0]
        declining = [t for t in trends if t.growth_rate < 0]

        lines = [
            f"成長中のトレンド: {len(growing)}件",
            f"減少中のトレンド: {len(declining)}件",
        ]
        return "\n".join(lines)

    def _generate_growth_chart(self, trends: list[Trend]) -> dict[str, Any]:
        """成長率グラフデータ生成."""
        return {
            "type": "line",
            "title": "成長率推移",
            "data": [{"topic": t.topic, "growth_rate": t.growth_rate} for t in trends[:10]],
        }

    def validate_output(self, output: ReporterOutput) -> bool:
        """出力検証.

        Args:
            output: 出力データ

        Returns:
            検証結果（True = 有効）
        """
        # レポートが存在するか
        if not output.report:
            self._logger.warning("Validation warning: report is empty")
            return False
        return True

