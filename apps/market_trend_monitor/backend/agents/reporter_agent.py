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

from kernel import ResilientAgent


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

        if not trends:
            self._logger.info("トレンドが0件のため、軽量レポートを生成します")
            report = self._build_empty_report(summary=summary, period=period)
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

    def _build_empty_report(self, summary: str, period: str) -> Report:
        """トレンド未検出時の軽量レポートを生成."""
        content = summary.strip() if summary.strip() else "今回の収集では有効なトレンドを検出できませんでした。"
        section = ReportSection(
            title="収集結果サマリー",
            content=content,
            charts=[],
            metadata={"section_type": "summary", "empty_result": True},
        )
        return Report(
            id=str(uuid.uuid4()),
            title=f"市場動向レポート ({period})",
            sections=[section],
            generated_at=datetime.now(),
            period=period,
            metadata={"trends_count": 0, "empty_result": True},
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

        # 4. 配布資料設計（PDF/PPT）
        sections.append(
            ReportSection(
                title="配布資料設計（PDF / PPT）",
                content=self._build_delivery_section(trends, period),
                metadata={"section_type": "delivery", "llm_generated": False},
            )
        )

        # レポート作成
        return Report(
            id=str(uuid.uuid4()),
            title=f"市場動向レポート ({period})",
            sections=sections,
            generated_at=datetime.now(),
            period=period,
            metadata={"trends_count": len(trends), "report_version": "2.2"},
        )

    async def _generate_executive_summary(self, summary: str, trends: list[Trend]) -> str:
        """LLM を使用してエグゼクティブサマリーを生成."""
        trends_text = "\n".join(
            [f"- {t.topic}: 記事 {t.articles_count}件 / 成長指標 {self._format_growth_label(t)}" for t in trends[:5]]
        )

        prompt = f"""以下の情報を元に、日本語 Markdown でエグゼクティブサマリーを作成してください。
制約:
- 180文字以内
- 箇条書き 3点
- 先頭に見出し `## 📌 エグゼクティブサマリー`

ベースサマリー:
{summary}

主要トレンド:
{trends_text}
"""

        fallback_summary = self._build_executive_fallback(summary, trends)

        try:
            response = await self._call_llm(prompt)
            return response if response else fallback_summary
        except Exception as e:
            self._logger.warning(f"Failed to generate executive summary: {e}")
            return fallback_summary

    async def _generate_trends_section(self, trends: list[Trend]) -> str:
        """LLM を使用してトレンドセクションを生成."""
        trends_text = "\n".join(
            [
                f"{i + 1}. {t.topic} (スコア: {t.score:.2f}, 記事数: {t.articles_count}, 成長: {self._format_growth_label(t)})"
                for i, t in enumerate(trends[:5])
            ]
        )

        prompt = f"""以下のトレンドを日本語で分析し、Markdownで出力してください。
制約:
- 先頭に `## 📈 主要トレンド`
- 箇条書き3-5点
- 150-220文字

Trends:
{trends_text}
"""

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
        new_topics = [t for t in trends if str(t.metadata.get("growth_state", "")) == "new"]

        prompt = f"""以下の成長分析を日本語 Markdown で出力してください。
制約:
- 先頭に `## 📉 成長率の読み方`
- 「NEW は前期間データなし」を明記
- 120-180文字

Growing trends ({len(growing)}):
{", ".join([f"{t.topic} (+{t.growth_rate:.1%})" for t in growing[:5]])}

Declining trends ({len(declining)}):
{", ".join([f"{t.topic} ({t.growth_rate:.1%})" for t in declining[:5]])}
New topics ({len(new_topics)}):
{", ".join([t.topic for t in new_topics[:5]])}
"""

        try:
            response = await self._call_llm(prompt)
            return response if response else self._format_growth_analysis(trends)
        except Exception as e:
            self._logger.warning(f"Failed to generate growth section: {e}")
            return self._format_growth_analysis(trends)

    def _format_trends(self, trends: list[Trend]) -> str:
        """トレンドをフォーマット（フォールバック用）."""
        lines = [
            "## 📈 主要トレンド",
            "",
            "| トピック | スコア | 記事数 | 成長 |",
            "|---|---:|---:|---|",
        ]
        for i, trend in enumerate(trends[:5], 1):
            lines.append(
                f"| {i}. {trend.topic} | {trend.score:.2f} | {trend.articles_count} | "
                f"{self._format_growth_label(trend)} |"
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
        new_topics = [t for t in trends if str(t.metadata.get("growth_state", "")) == "new"]

        lines = [
            "## 📉 成長率の読み方",
            "",
            f"- 成長中のトレンド: {len(growing)}件",
            f"- 減少中のトレンド: {len(declining)}件",
            f"- NEW（前期間データなし）: {len(new_topics)}件",
            "- 成長率は「(現期間 - 前期間) / (前期間 + 1.0)」で平滑化しています。",
        ]
        return "\n".join(lines)

    def _build_executive_fallback(self, summary: str, trends: list[Trend]) -> str:
        """エグゼクティブサマリーのフォールバックを生成."""
        top = trends[0] if trends else None
        top_line = (
            f"最注目は **{top.topic}**（スコア {top.score:.2f} / 成長 {self._format_growth_label(top)}）"
            if top
            else "現時点で主要トレンドは未検出です。"
        )
        base_summary = summary.strip() if summary.strip() else "収集データから市場動向を要約しました。"
        return "\n".join(
            [
                "## 📌 エグゼクティブサマリー",
                "",
                f"- {base_summary}",
                f"- {top_line}",
                "- NEW は前期間データがない新規検知を意味します。",
            ]
        )

    def _format_growth_label(self, trend: Trend) -> str:
        """トレンドの成長表示ラベルを生成."""
        state = str(trend.metadata.get("growth_state", ""))
        if state == "new":
            return "NEW"
        if state == "insufficient_history":
            return "N/A"
        return f"{trend.growth_rate:.1%}"

    def _build_delivery_section(self, trends: list[Trend], period: str) -> str:
        """PDF/PPT 用の配布設計セクションを生成."""
        top_topics = [trend.topic for trend in trends[:3]]
        topics_text = " / ".join(top_topics) if top_topics else "主要トピックなし"

        return "\n".join(
            [
                "## 🧾 PDF提出レイアウト",
                "",
                "- 表紙: レポート名、対象期間、作成日時、責任者",
                "- 1ページ目: エグゼクティブサマリー（意思決定ポイント3点）",
                "- 2ページ目: トレンド表（スコア・記事数・成長ラベル）",
                "- 3ページ目: 成長率の算定式と注記（NEW定義を明記）",
                "",
                "## 🖥️ PPT構成（提案）",
                "",
                f"- Slide 1: タイトル（期間 {period}）",
                f"- Slide 2: 主要トピック `{topics_text}`",
                "- Slide 3: 成長トピック / 減衰トピック比較",
                "- Slide 4: リスク・アクション・次週フォロー",
            ]
        )

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
