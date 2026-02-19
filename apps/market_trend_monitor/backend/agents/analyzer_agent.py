"""分析エージェント.

収集データを分析し、トレンドを抽出します。

設計原則：
- 松耦合：LLM プロバイダーを意識しない（環境変数から自動検出）
- 型安全：Pydantic による I/O 検証
- 健壮性：ResilientAgent によるリトライ・タイムアウト制御
- Skills体系：確定性処理をSkillスクリプトに分離（Anthropic Skills準拠）
"""

import logging
import uuid
from collections import Counter
from datetime import datetime, timedelta
from typing import Any

from apps.market_trend_monitor.backend.config import config
from apps.market_trend_monitor.backend.models import (
    AnalyzerInput,
    AnalyzerOutput,
    Article,
    ArticleSchema,
    SentimentType,
    Trend,
    TrendSchema,
)
from apps.market_trend_monitor.backend.services.evidence_service import EvidenceService

from agentflow import ResilientAgent
from agentflow.skills import Skill, SkillRouter, SkillRuntime


class AnalyzerAgent(ResilientAgent[AnalyzerInput, AnalyzerOutput]):
    """分析エージェント（ResilientAgent 継承・型安全）.

    役割:
    - キーワード抽出（TF-IDF）
    - トピック分類
    - センチメント分析（LLM）
    - トレンド計算（成長率、スコア）
    - エンティティ抽出（Phase 7）
    - トピッククラスタリング（Phase 7）
    - 異常検知（Phase 7）

    Note:
        LLM プロバイダー/モデルは環境変数から自動検出されます（松耦合）。
        ResilientAgent によりリトライ・タイムアウトが自動制御されます。
    """

    # ResilientAgent 設定
    name = "AnalyzerAgent"
    temperature = 0.5  # 分析タスクは中程度

    def __init__(
        self,
        *,
        skill_router: SkillRouter | None = None,
        skill_runtime: SkillRuntime | None = None,
        evidence_service: EvidenceService | None = None,
        entity_extraction_service: Any | None = None,
        topic_clustering_service: Any | None = None,
        anomaly_detection_service: Any | None = None,
    ) -> None:
        """初期化.

        Note:
            LLM クライアントは get_llm() により自動取得されます（松耦合原則）。

        引数:
            skill_router: Skillルーター（None の場合は新規作成）
            skill_runtime: Skill実行環境（None の場合は新規作成）
            evidence_service: 証拠管理サービス
            entity_extraction_service: エンティティ抽出サービス（Phase 7）
            topic_clustering_service: トピッククラスタリングサービス（Phase 7）
            anomaly_detection_service: 異常検知サービス（Phase 7）
        """
        super().__init__()  # ResilientAgent が内部で get_llm() を呼び出す
        self._logger = logging.getLogger(self.name)

        # Skill体系初期化（Anthropic Skills準拠）
        self._skill_router = skill_router or SkillRouter()
        self._skill_runtime = skill_runtime or SkillRuntime()
        self._market_trend_skill: Skill | None = None
        self._evidence_service = evidence_service

        # Phase 7: 分析エンジン強化
        self._entity_extraction = entity_extraction_service
        self._topic_clustering = topic_clustering_service
        self._anomaly_detection = anomaly_detection_service

    def _parse_input(self, input_data: dict[str, Any]) -> AnalyzerInput:
        """入力データを Pydantic モデルに変換."""
        return AnalyzerInput(**input_data)

    async def process(self, input_data: AnalyzerInput) -> AnalyzerOutput:
        """分析を実行.

        引数:
            input_data: 型付き入力データ

        戻り値:
            型付き分析結果
        """
        articles_data = input_data.articles
        enable_sentiment = input_data.enable_sentiment

        self._logger.info("分析開始: %s件の記事", len(articles_data))

        # ArticleSchema から Article に変換
        articles = [self._schema_to_article(a) for a in articles_data]

        # キーワード抽出（Evidence 優先）
        keywords_stats = await self._extract_keywords(articles)

        # トレンド抽出
        trend_context = await self._build_trend_context()
        trends = await self._extract_trends(
            articles,
            keywords_stats,
            trend_context,
            enable_sentiment,
        )

        # Phase 7: エンティティ抽出・異常検知
        entities = await self._extract_entities_from_articles(articles)
        anomalies = await self._detect_anomalies(trends)
        for trend in trends:
            trend.entities = entities
            trend.metadata["anomalies"] = anomalies

        # サマリー生成
        summary = await self._generate_summary(trends, keywords_stats)

        self._logger.info("分析完了: %s件のトレンド", len(trends))

        # Trend を TrendSchema に変換
        trend_schemas = [
            TrendSchema(
                id=t.id,
                topic=t.topic,
                score=t.score,
                articles_count=t.articles_count,
                keywords=t.keywords,
                sentiment=t.sentiment.value,
                growth_rate=t.growth_rate,
                first_seen=t.first_seen.isoformat(),
                last_seen=t.last_seen.isoformat(),
                article_count=t.article_count,
                created_at=t.created_at.isoformat(),
                metadata=t.metadata,
            )
            for t in trends
        ]

        return AnalyzerOutput(
            trends=trend_schemas,
            summary=summary,
            keywords_stats=keywords_stats,
        )

    def _schema_to_article(self, schema: ArticleSchema) -> Article:
        """ArticleSchema から Article オブジェクトを生成."""
        from apps.market_trend_monitor.backend.models import SourceType

        # source 文字列を SourceType に変換
        source_type = SourceType.NEWS
        if schema.source in [s.value for s in SourceType]:
            source_type = SourceType(schema.source)

        return Article(
            id=schema.id,
            title=schema.title,
            url=schema.url,
            source=source_type,
            published_at=datetime.fromisoformat(schema.published_at),
            content=schema.content,
            keywords=schema.keywords,
            collected_at=datetime.fromisoformat(schema.collected_at),
            metadata=schema.metadata,
        )

    async def _extract_entities_from_articles(
        self,
        articles: list[Article],
    ) -> list[dict[str, Any]]:
        """Phase 7: 記事からエンティティを抽出."""
        if not self._entity_extraction:
            return []
        try:
            all_entities = []
            for article in articles[:20]:
                text = f"{article.title}\n{article.content}"
                entities = await self._entity_extraction.extract_entities(text)
                all_entities.extend(entities)
            merged = self._entity_extraction.merge_entities(all_entities)
            return [e.to_dict() for e in merged[:20]]
        except Exception as e:
            self._logger.warning("エンティティ抽出失敗: %s", e)
            return []

    async def _detect_anomalies(
        self,
        trends: list[Trend],
    ) -> list[dict[str, Any]]:
        """Phase 7: 異常検知."""
        if not self._anomaly_detection:
            return []
        try:
            anomalies = await self._anomaly_detection.detect_growth_rate_anomalies(trends)
            return [a.to_dict() for a in anomalies[:10]]
        except Exception as e:
            self._logger.warning("異常検知失敗: %s", e)
            return []

    async def _extract_keywords(self, articles: list[Article]) -> dict[str, int]:
        """キーワード抽出.

        Phase 7: EntityExtractionService統合。
        エンティティ名をキーワードとして優先的に使用。

        引数:
            articles: 記事リスト

        戻り値:
            キーワード出現頻度
        """
        if self._evidence_service:
            stats = await self._extract_keywords_from_evidence()
            if stats:
                return stats

        # Phase 7: エンティティベースのキーワード抽出
        if self._entity_extraction:
            try:
                all_entities = []
                for article in articles[:20]:
                    text = f"{article.title}\n{article.content}"
                    entities = await self._entity_extraction.extract_entities(text)
                    all_entities.extend(entities)
                merged = self._entity_extraction.merge_entities(all_entities)
                if merged:
                    return {e.name: e.mentions for e in merged[:20]}
            except Exception as e:
                self._logger.warning("エンティティベースキーワード抽出失敗: %s", e)

        all_keywords: list[str] = []
        for article in articles:
            all_keywords.extend(article.keywords)

        counter = Counter(all_keywords)
        return dict(counter.most_common(20))

    async def _extract_trends(
        self,
        articles: list[Article],
        keywords_stats: dict[str, int],
        trend_context: dict[str, Any],
        enable_sentiment: bool,
    ) -> list[Trend]:
        """トレンド抽出."""
        trends: list[Trend] = []
        weighted_current = trend_context.get("weighted_current", {})
        weighted_previous = trend_context.get("weighted_previous", {})
        count_current = trend_context.get("count_current", {})

        if weighted_current:
            total_weight = sum(weighted_current.values())
            sorted_items = sorted(weighted_current.items(), key=lambda x: x[1], reverse=True)
            for keyword, weight in sorted_items[:10]:
                score = min(weight / max(total_weight, 1.0), 1.0)
                prev_weight = weighted_previous.get(keyword, 0.0)
                growth_rate, growth_state = self._calculate_growth_rate(weight, prev_weight)
                growth_explanation = self._build_growth_explanation(
                    growth_state=growth_state,
                    current=weight,
                    previous=prev_weight,
                    growth_rate=growth_rate,
                )

                sentiment = SentimentType.NEUTRAL
                if enable_sentiment:
                    sentiment = await self._analyze_sentiment(keyword, articles)

                now = datetime.now()
                count = int(count_current.get(keyword, 0))
                trend = Trend(
                    id=str(uuid.uuid4()),
                    topic=keyword,
                    score=score,
                    articles_count=count,
                    keywords=[keyword],
                    sentiment=sentiment,
                    growth_rate=growth_rate,
                    first_seen=now,
                    last_seen=now,
                    article_count=count,
                    created_at=now,
                    metadata={
                        "analysis_version": "3.2",
                        "llm_analyzed": enable_sentiment,
                        "weighted": True,
                        "growth_state": growth_state,
                        "growth_current_weight": round(weight, 4),
                        "growth_previous_weight": round(prev_weight, 4),
                        "growth_explanation": growth_explanation,
                        "growth_baseline_missing": growth_state == "new",
                    },
                )
                trends.append(trend)
            return trends

        # フォールバック: 記事キーワードのみ
        for keyword, count in list(keywords_stats.items())[:10]:
            score = min(count / max(len(articles), 1), 1.0)
            sentiment = SentimentType.NEUTRAL
            if enable_sentiment:
                sentiment = await self._analyze_sentiment(keyword, articles)

            now = datetime.now()
            trend = Trend(
                id=str(uuid.uuid4()),
                topic=keyword,
                score=score,
                articles_count=count,
                keywords=[keyword],
                sentiment=sentiment,
                growth_rate=0.0,
                first_seen=now,
                last_seen=now,
                article_count=count,
                created_at=now,
                metadata={
                    "analysis_version": "3.2",
                    "llm_analyzed": enable_sentiment,
                    "weighted": False,
                    "growth_state": "insufficient_history",
                    "growth_explanation": "履歴データが不足しているため、成長率は比較できません。",
                    "growth_baseline_missing": True,
                },
            )
            trends.append(trend)

        return trends

    async def _extract_keywords_from_evidence(self) -> dict[str, int]:
        """Evidence からキーワード頻度を取得."""
        if not self._evidence_service:
            return {}

        now = datetime.now()
        window_days = config.analyzer.trend_window_days
        since = now - timedelta(days=window_days)
        evidences = await self._evidence_service.list_evidences_in_window(since, now)
        counts: dict[str, int] = {}
        for evidence in evidences:
            for keyword in evidence.extracted_data.get("keywords", []):
                counts[keyword] = counts.get(keyword, 0) + 1
        return dict(sorted(counts.items(), key=lambda x: x[1], reverse=True)[:20])

    async def _build_trend_context(self) -> dict[str, Any]:
        """時系列トレンド算定用コンテキストを構築."""
        if not self._evidence_service:
            return {}

        now = datetime.now()
        window_days = config.analyzer.trend_window_days
        current_since = now - timedelta(days=window_days)
        previous_since = current_since - timedelta(days=window_days)

        current_evidences = await self._evidence_service.list_evidences_in_window(
            current_since, now
        )
        previous_evidences = await self._evidence_service.list_evidences_in_window(
            previous_since, current_since
        )

        weighted_current: dict[str, float] = {}
        weighted_previous: dict[str, float] = {}
        count_current: dict[str, int] = {}

        for evidence in current_evidences:
            for keyword in evidence.extracted_data.get("keywords", []):
                weighted_current[keyword] = (
                    weighted_current.get(keyword, 0.0) + evidence.reliability_score
                )
                count_current[keyword] = count_current.get(keyword, 0) + 1

        for evidence in previous_evidences:
            for keyword in evidence.extracted_data.get("keywords", []):
                weighted_previous[keyword] = (
                    weighted_previous.get(keyword, 0.0) + evidence.reliability_score
                )

        return {
            "weighted_current": weighted_current,
            "weighted_previous": weighted_previous,
            "count_current": count_current,
        }

    def _calculate_growth_rate(self, current: float, previous: float) -> tuple[float, str]:
        """成長率と状態を計算.

        Phase 10修正: 前期=0で新規トピックの場合はgrowth_rate=1.0を返す。
        新興トレンドの重要性を正しく反映するためのラプラス平滑化を適用。
        """
        if current <= 0 and previous <= 0:
            return 0.0, "no_signal"
        if previous <= 0 and current > 0:
            # Phase 10: 新規トピックは最大成長率を付与
            return 1.0, "new"

        # ラプラス平滑化: 分母にε=0.5を加えて微小母数の暴騰を抑制
        epsilon = 0.5
        raw_rate = (current - previous) / (previous + epsilon)
        growth_rate = max(min(raw_rate, 3.0), -1.0)

        if growth_rate > 0.05:
            return growth_rate, "up"
        if growth_rate < -0.05:
            return growth_rate, "down"
        return growth_rate, "flat"

    def _build_growth_explanation(
        self,
        *,
        growth_state: str,
        current: float,
        previous: float,
        growth_rate: float,
    ) -> str:
        """成長率の説明文を生成."""
        if growth_state == "new":
            return (
                "前期間に同一トピックの証拠が存在しないため、比率ではなく新規検知として扱いました。"
            )
        if growth_state == "no_signal":
            return "現期間・前期間ともに有効な証拠がないため、変化は判定できません。"
        if growth_state == "flat":
            return (
                f"現期間 {current:.2f} / 前期間 {previous:.2f} で差分が小さく、"
                "概ね横ばいと判定しました。"
            )
        direction = "増加" if growth_state == "up" else "減少"
        return (
            f"現期間 {current:.2f} / 前期間 {previous:.2f} の比較で、"
            f"{direction}率は {growth_rate:.1%} です。"
        )

    async def _analyze_sentiment(self, keyword: str, articles: list[Article]) -> SentimentType:
        """LLM を使用してセンチメント分析."""
        related_titles = [a.title for a in articles if keyword in a.keywords][:5]
        if not related_titles:
            return SentimentType.NEUTRAL

        titles_text = "\n".join(related_titles)
        prompt = f"""Analyze the sentiment of the following article titles related to "{keyword}".
Respond with only one word: POSITIVE, NEGATIVE, or NEUTRAL.

Titles:
{titles_text}

Sentiment:"""

        try:
            response = await self._call_llm(prompt)
            sentiment_text = response.strip().upper() if response else ""

            if "POSITIVE" in sentiment_text:
                return SentimentType.POSITIVE
            if "NEGATIVE" in sentiment_text:
                return SentimentType.NEGATIVE
            return SentimentType.NEUTRAL
        except Exception as e:
            self._logger.warning("センチメント分析失敗 '%s': %s", keyword, e)
            return SentimentType.NEUTRAL

    async def _generate_summary(
        self,
        trends: list[Trend],
        keywords_stats: dict[str, int],
    ) -> str:
        """分析サマリーを生成."""
        if not trends:
            return "現在、分析対象のトレンドは検出されませんでした。キーワードまたは情報源を見直してください。"

        top_trends = sorted(trends, key=lambda t: t.score, reverse=True)[:5]
        top_topics = ", ".join([trend.topic for trend in top_trends])
        positive_count = sum(1 for trend in trends if trend.sentiment == SentimentType.POSITIVE)
        negative_count = sum(1 for trend in trends if trend.sentiment == SentimentType.NEGATIVE)
        growing_count = sum(1 for trend in trends if trend.growth_rate > 0)
        new_topics_count = sum(1 for trend in trends if trend.metadata.get("growth_state") == "new")
        top_keywords = ", ".join(list(keywords_stats.keys())[:5])

        fallback_summary = (
            f"主要トピック: {top_topics}。"
            f" 成長傾向 {growing_count} 件 / 全 {len(trends)} 件。"
            f" 新規検知 {new_topics_count} 件。"
            f" センチメントは Positive {positive_count} 件、Negative {negative_count} 件。"
            f" 主要キーワード: {top_keywords if top_keywords else 'なし'}。"
        )

        prompt = f"""以下の市場分析結果をもとに、日本語で100-150文字の要約を作成してください。

主要トピック: {top_topics}
トレンド件数: {len(trends)}
成長傾向件数: {growing_count}
新規検知件数: {new_topics_count}
Positive件数: {positive_count}
Negative件数: {negative_count}
主要キーワード: {top_keywords}
"""
        try:
            response = await self._call_llm(prompt)
            summary = response.strip() if response else ""
            return summary or fallback_summary
        except Exception as e:
            self._logger.warning("サマリー生成失敗: %s", e)
            return fallback_summary
