"""レポート生成エージェント.

分析結果から読みやすいレポートを生成します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
"""

import asyncio
import logging
import uuid
from datetime import datetime
from typing import TYPE_CHECKING, Any

from agentflow.core.agent_block import AgentBlock
from agentflow.providers import get_llm

from apps.market_trend_monitor.backend.models import Report, ReportSection, Trend

if TYPE_CHECKING:
    from agentflow.providers.llm_provider import LLMProvider


class ReporterAgent(AgentBlock):
    """レポート生成エージェント（松耦合設計）.

    役割:
    - 分析結果からレポート生成
    - Markdown/HTML フォーマット
    - グラフデータ生成

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます。

    入力:
        {
            "trends": [Trend, ...],
            "summary": "サマリーテキスト",
            "period": "2025-W03"
        }

    出力:
        {
            "report": Report,
            "formats": ["markdown", "html"]
        }
    """

    def __init__(self) -> None:
        """初期化."""
        super().__init__()
        self._logger = logging.getLogger(__name__)

        # LLM プロバイダー（環境変数から自動検出・松耦合）
        # レポート生成には高めの温度を使用
        self._llm: "LLMProvider" = get_llm(temperature=0.7)

    async def initialize(self) -> None:
        """エージェント初期化."""
        await super().initialize()
        self._logger.info("ReporterAgent initialized")

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """レポート生成を実行.

        Args:
            input_data: 入力データ
                - trends: トレンドリスト
                - summary: サマリーテキスト
                - period: レポート期間
                - shared_context: 共有コンテキスト（記憶システム付き）

        Returns:
            レポート生成結果
                - report: レポートオブジェクト
                - formats: 生成フォーマット
        """
        trends_data = input_data.get("trends", [])
        summary = input_data.get("summary", "")
        period = input_data.get("period", datetime.now().strftime("%Y-W%U"))
        shared_context = input_data.get("shared_context")

        self._logger.info(f"Starting report generation: period={period}")

        # Trend オブジェクトに変換
        trends = [self._dict_to_trend(t) for t in trends_data]

        # 過去のレポートを検索（記憶システムから）
        past_reports = await self._recall_past_reports(shared_context) if shared_context else []

        # レポート生成（過去のレポートを考慮）
        report = await self._generate_report(trends, summary, period, past_reports)

        # レポートを記憶システムに保存
        if shared_context:
            await self._save_report_to_memory(report, shared_context)

        self._logger.info(f"Report generated: {report.id}")

        return {
            "report": report.to_dict(),
            "formats": ["markdown", "html"],
        }

    def _dict_to_trend(self, data: dict[str, Any]) -> Trend:
        """辞書からTrendオブジェクトを生成."""
        from agentflow.utils.type_helpers import convert_enum

        from apps.market_trend_monitor.backend.models import SentimentType

        return Trend(
            id=data["id"],
            topic=data["topic"],
            score=data["score"],
            articles_count=data["articles_count"],
            keywords=data["keywords"],
            sentiment=convert_enum(data["sentiment"], SentimentType),
            growth_rate=data["growth_rate"],
            created_at=datetime.fromisoformat(data["created_at"]),
            metadata=data.get("metadata", {}),
        )

    async def _generate_report(
        self,
        trends: list[Trend],
        summary: str,
        period: str,
        past_reports: list[dict[str, Any]] | None = None,
    ) -> Report:
        """レポート生成.

        Args:
            trends: トレンドリスト
            summary: サマリーテキスト
            period: レポート期間
            past_reports: 過去のレポート（記憶システムから取得）

        Returns:
            レポートオブジェクト
        """
        sections: list[ReportSection] = []
        past_reports = past_reports or []

        # 1. エグゼクティブサマリー（LLM生成、リトライ機構付き）
        executive_summary = await self._generate_executive_summary_with_retry(summary, trends, past_reports, max_retries=3)
        sections.append(
            ReportSection(
                title="エグゼクティブサマリー",
                content=executive_summary,
                metadata={"section_type": "summary", "has_past_data": len(past_reports) > 0, "llm_generated": True},
            )
        )

        # 2. 主要トレンド（LLM生成、リトライ機構付き）
        trends_content = await self._generate_section_with_retry(
            self._generate_trends_section, trends, max_retries=3
        )
        trends_chart = self._generate_trends_chart(trends)
        sections.append(
            ReportSection(
                title="主要トレンド",
                content=trends_content,
                charts=[trends_chart],
                metadata={"section_type": "trends", "llm_generated": True},
            )
        )

        # 3. 成長率分析（LLM生成、リトライ機構付き）
        growth_content = await self._generate_section_with_retry(
            self._generate_growth_section, trends, max_retries=3
        )
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
        report = Report(
            id=str(uuid.uuid4()),
            title=f"市場動向レポート ({period})",
            sections=sections,
            generated_at=datetime.now(),
            period=period,
            metadata={"trends_count": len(trends)},
        )

        return report

    async def _generate_section_with_retry(
        self, generator_func: Any, trends: list[Trend], max_retries: int = 3
    ) -> str:
        """汎用リトライ機構付きセクション生成.

        Args:
            generator_func: セクション生成関数
            trends: トレンドリスト
            max_retries: 最大リトライ回数

        Returns:
            生成されたセクションコンテンツ
        """
        for attempt in range(max_retries):
            try:
                return await generator_func(trends)
            except asyncio.TimeoutError:
                self._logger.warning(
                    f"Timeout generating section with {generator_func.__name__} (attempt {attempt + 1}/{max_retries})"
                )
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0 * (2 ** attempt))  # 指数バックオフ
                else:
                    self._logger.error(
                        f"Failed to generate section with {generator_func.__name__} after {max_retries} attempts"
                    )
                    return self._format_trends(trends)  # フォールバック
            except Exception as e:
                self._logger.error(f"Error generating section with {generator_func.__name__}: {e}")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0)
                else:
                    return self._format_trends(trends)  # フォールバック
        return self._format_trends(trends)

    async def _generate_executive_summary_with_retry(
        self, summary: str, trends: list[Trend], past_reports: list[dict[str, Any]], max_retries: int = 3
    ) -> str:
        """リトライ機構付きエグゼクティブサマリー生成.

        Args:
            summary: 基本サマリー
            trends: トレンドリスト
            past_reports: 過去のレポート
            max_retries: 最大リトライ回数

        Returns:
            エグゼクティブサマリー
        """
        for attempt in range(max_retries):
            try:
                return await self._generate_executive_summary(summary, trends, past_reports)
            except asyncio.TimeoutError:
                self._logger.warning(f"Timeout generating executive summary (attempt {attempt + 1}/{max_retries})")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0 * (2 ** attempt))  # 指数バックオフ
                else:
                    self._logger.error(f"Failed to generate executive summary after {max_retries} attempts")
                    comparison_text = f"\n\n過去{len(past_reports)}件のレポートと比較して分析しました。" if past_reports else ""
                    return summary + comparison_text
            except Exception as e:
                self._logger.error(f"Error generating executive summary: {e}")
                if attempt < max_retries - 1:
                    await asyncio.sleep(1.0)
                else:
                    comparison_text = f"\n\n過去{len(past_reports)}件のレポートと比較して分析しました。" if past_reports else ""
                    return summary + comparison_text
        comparison_text = f"\n\n過去{len(past_reports)}件のレポートと比較して分析しました。" if past_reports else ""
        return summary + comparison_text

    async def _generate_executive_summary(
        self, summary: str, trends: list[Trend], past_reports: list[dict[str, Any]]
    ) -> str:
        """LLMを使用してエグゼクティブサマリーを生成.

        Args:
            summary: 基本サマリー
            trends: トレンドリスト
            past_reports: 過去のレポート

        Returns:
            エグゼクティブサマリー

        Raises:
            asyncio.TimeoutError: タイムアウト時
            Exception: その他のエラー時
        """
        trends_text = "\n".join([
            f"- {t.topic}: {t.articles_count}件, 成長率{t.growth_rate:.1%}"
            for t in trends[:5]
        ])

        prompt = f"""Generate an executive summary for a market trend report in Japanese.
Base summary: {summary}
Number of past reports: {len(past_reports)}

Top trends:
{trends_text}

Write a professional, insightful executive summary (150-200 words) that:
1. Highlights key findings
2. Compares with past trends if available
3. Provides actionable insights

Executive Summary:"""

        # LLM でサマリー生成（松耦合：プロバイダー不明）
        response = await self._llm.complete(prompt)
        return response["content"]

    async def _generate_trends_section(self, trends: list[Trend]) -> str:
        """LLMを使用してトレンドセクションを生成.

        Args:
            trends: トレンドリスト

        Returns:
            トレンドセクションコンテンツ
        """
        try:
            trends_text = "\n".join([
                f"{i+1}. {t.topic} (スコア: {t.score:.2f}, 記事数: {t.articles_count}, センチメント: {t.sentiment.value})"
                for i, t in enumerate(trends[:5])
            ])

            prompt = f"""Analyze and describe the following market trends in Japanese.
Provide insights about each trend and their relationships (200-250 words).

Trends:
{trends_text}

Analysis:"""

            # LLM で分析（松耦合：プロバイダー不明）
            response = await self._llm.complete(prompt)
            return response["content"]

        except Exception as e:
            self._logger.warning(f"Failed to generate trends section with LLM: {e}")
            return self._format_trends(trends)

    async def _generate_growth_section(self, trends: list[Trend]) -> str:
        """LLMを使用して成長率分析セクションを生成.

        Args:
            trends: トレンドリスト

        Returns:
            成長率分析セクションコンテンツ
        """
        try:
            growing = [t for t in trends if t.growth_rate > 0]
            declining = [t for t in trends if t.growth_rate < 0]

            prompt = f"""Analyze the growth trends in Japanese (150-200 words).

Growing trends ({len(growing)}):
{', '.join([f"{t.topic} (+{t.growth_rate:.1%})" for t in growing[:5]])}

Declining trends ({len(declining)}):
{', '.join([f"{t.topic} ({t.growth_rate:.1%})" for t in declining[:5]])}

Provide insights about:
1. Why certain trends are growing
2. Why certain trends are declining
3. Future predictions

Analysis:"""

            # LLM で成長率分析（松耦合：プロバイダー不明）
            response = await self._llm.complete(prompt)
            return response["content"]

        except Exception as e:
            self._logger.warning(f"Failed to generate growth section with LLM: {e}")
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

    async def _recall_past_reports(self, shared_context: Any) -> list[dict[str, Any]]:
        """過去のレポートを記憶システムから検索.

        Args:
            shared_context: 共有コンテキスト

        Returns:
            過去のレポートリスト
        """
        try:
            memories = await shared_context.recall(topic="generated_reports", limit=10)
            past_reports = []
            for memory in memories:
                # メタデータからレポート情報を抽出
                metadata = memory.metadata
                if "report_id" in metadata:
                    past_reports.append(metadata)

            self._logger.info(f"Recalled {len(past_reports)} past reports from memory")
            return past_reports
        except Exception as e:
            self._logger.warning(f"Failed to recall past reports: {e}")
            return []

    async def _save_report_to_memory(self, report: Report, shared_context: Any) -> None:
        """レポートを記憶システムに保存.

        Args:
            report: レポートオブジェクト
            shared_context: 共有コンテキスト
        """
        try:
            # レポート情報を記憶
            memory_text = f"Report: {report.title}\nPeriod: {report.period}\nSections: {len(report.sections)}\nGenerated: {report.generated_at.isoformat()}"

            await shared_context.remember(
                memory_text,
                topic="generated_reports",
                metadata={
                    "report_id": report.id,
                    "title": report.title,
                    "period": report.period,
                    "sections_count": len(report.sections),
                    "trends_count": report.metadata.get("trends_count", 0),
                    "generated_at": report.generated_at.isoformat(),
                },
            )

            self._logger.info(f"Saved report {report.id} to memory system")
        except Exception as e:
            self._logger.warning(f"Failed to save report to memory: {e}")

    async def cleanup(self) -> None:
        """クリーンアップ処理."""
        self._logger.info("ReporterAgent cleanup")
        await super().cleanup()

