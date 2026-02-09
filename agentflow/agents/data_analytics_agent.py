"""DataAnalyticsAgent - データ分析統一Agent.

NL2SQL + SemanticLayer(DSL) + Chart + Suggestion を統合した
データ分析専門のAgent。

設計原則：
- ResilientAgent 継承による自動リトライ・タイムアウト制御
- Pydantic による型安全な入出力
- 松耦合：LLM プロバイダーを意識しない
- NL2SQL 増強（Schema Linking + Few-shot + Post-Processing）
- DSL 中間層による可読性向上

使用例:
    >>> from agentflow.agents import DataAnalyticsAgent, DataAnalyticsInput
    >>>
    >>> agent = DataAnalyticsAgent(config=DataAnalyticsConfig(
    ...     db_schema={"sales": ["id", "amount", "date", "region"]},
    ... ))
    >>>
    >>> result = await agent.run({"question": "今月の売上TOP10を教えて"})
    >>> print(result["answer"])
    >>> print(result["sql"])
    >>> print(result["chart"])
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

from pydantic import BaseModel, Field

from agentflow.core import ResilientAgent


logger = logging.getLogger(__name__)


# =============================================================================
# Pydantic スキーマ（型安全な入出力）
# =============================================================================


class DataAnalyticsInput(BaseModel):
    """DataAnalyticsAgent 入力スキーマ."""

    question: str = Field(..., description="自然言語クエリ")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")


class DSLOutputSchema(BaseModel):
    """DSL 中間表現スキーマ."""

    metrics: list[str] = Field(default_factory=list)
    dimensions: list[str] = Field(default_factory=list)
    filters: list[dict[str, Any]] = Field(default_factory=list)
    order_by: list[dict[str, Any]] = Field(default_factory=list)
    limit: int | None = None
    time_range: dict[str, Any] | None = None
    confidence: float = 1.0


class ChartDataSchema(BaseModel):
    """チャートデータスキーマ."""

    chart_type: str = "bar"
    title: str = ""
    echarts: dict[str, Any] = Field(default_factory=dict)
    chartjs: dict[str, Any] = Field(default_factory=dict)


class InsightSchema(BaseModel):
    """インサイトスキーマ."""

    text: str = ""
    type: str = "insight"
    confidence: float = 1.0
    priority: str = "medium"
    reason: str = ""


class SuggestionOutputSchema(BaseModel):
    """提案出力スキーマ."""

    text: str = ""
    type: str = "follow_up"
    confidence: float = 1.0
    priority: str = "medium"


class DataAnalyticsOutput(BaseModel):
    """DataAnalyticsAgent 出力スキーマ."""

    question: str = ""
    answer: str = ""
    sql: str = ""
    dsl: DSLOutputSchema | None = None
    data: list[dict[str, Any]] = Field(default_factory=list)
    columns: list[str] = Field(default_factory=list)
    chart: ChartDataSchema | None = None
    insights: list[InsightSchema] = Field(default_factory=list)
    suggestions: list[SuggestionOutputSchema] = Field(default_factory=list)
    schema_link_info: dict[str, Any] = Field(default_factory=dict)
    error: str = ""


# =============================================================================
# 設定
# =============================================================================


@dataclass
class NL2SQLEnhancementConfig:
    """NL2SQL 増強設定."""

    enable_schema_linking: bool = True
    schema_linking_use_llm: bool = False
    enable_fewshot: bool = True
    fewshot_k: int = 3
    enable_postprocess: bool = True


@dataclass
class DataAnalyticsConfig:
    """DataAnalyticsAgent 設定."""

    db_schema: dict[str, list[str]] = field(default_factory=dict)
    sql_dialect: str = "postgresql"
    auto_chart: bool = True
    auto_insights: bool = True
    max_suggestions: int = 5
    nl2sql: NL2SQLEnhancementConfig = field(default_factory=NL2SQLEnhancementConfig)
    enable_dsl_pipeline: bool = True


# =============================================================================
# DataAnalyticsAgent（ResilientAgent 継承）
# =============================================================================


class DataAnalyticsAgent(ResilientAgent[DataAnalyticsInput, DataAnalyticsOutput]):
    """データ分析統一Agent（ResilientAgent 継承・型安全）.

    NL2SQL + SemanticLayer(DSL) + Chart + Suggestion を統合。
    """

    name = "DataAnalyticsAgent"
    temperature = 0.3

    SYSTEM_PROMPT = """あなたはデータ分析アシスタントです。

主な職責:
1. 自然言語クエリをSQLに変換してデータを取得
2. データを視覚化してわかりやすく説明
3. データからインサイトを抽出
4. 次のステップを提案

回答ルール:
- 簡潔で正確な回答を心がける
- データの根拠を明示する
- 不明な点は正直に伝える
- 追加の分析を促す提案を行う"""

    def __init__(
        self,
        config: DataAnalyticsConfig | None = None,
        llm_client: Any = None,
    ) -> None:
        """初期化."""
        super().__init__(llm_client)
        self._config = config or DataAnalyticsConfig()
        self._logger = logging.getLogger(self.name)
        self._services_initialized = False

        # サービスインスタンス（遅延初期化）
        self.__semantic_layer = None
        self.__chart_service = None
        self.__suggestion_service = None
        self.__schema_linker = None
        self.__fewshot_manager = None
        self.__sql_postprocessor = None

    async def _ensure_services(self) -> None:
        """サービスを初期化（遅延初期化パターン）."""
        if self._services_initialized:
            return

        from agentflow.services import (
            ChartConfig,
            ChartService,
            SemanticLayerConfig,
            SemanticLayerService,
            SuggestionConfig,
            SuggestionService,
            SuggestionType,
        )
        from agentflow.services.fewshot_manager import FewshotManager
        from agentflow.services.schema_linker import SchemaLinker, SchemaLinkerConfig
        from agentflow.services.sql_postprocessor import SQLPostProcessor

        # SemanticLayerService
        semantic_config = SemanticLayerConfig(
            metrics={},  # 後で設定可能
            dimensions={},
        )
        self.__semantic_layer = SemanticLayerService(config=semantic_config)

        # ChartService
        chart_config = ChartConfig(
            enable_drill_down=True,
            enable_interactivity=True,
        )
        self.__chart_service = ChartService(config=chart_config)
        await self.__chart_service.start()

        # SuggestionService
        suggestion_config = SuggestionConfig(
            max_suggestions=self._config.max_suggestions,
            types=[
                SuggestionType.FOLLOW_UP,
                SuggestionType.DRILL_DOWN,
                SuggestionType.INSIGHT,
            ],
            enable_dsl_integration=True,
            enable_data_analysis=self._config.auto_insights,
        )
        self.__suggestion_service = SuggestionService(config=suggestion_config)
        await self.__suggestion_service.start()

        # NL2SQL 増強コンポーネント
        if self._config.nl2sql.enable_schema_linking and self._config.db_schema:
            linker_config = SchemaLinkerConfig(
                use_llm=self._config.nl2sql.schema_linking_use_llm,
            )
            self.__schema_linker = SchemaLinker(
                db_schema=self._config.db_schema,
                config=linker_config,
            )

        if self._config.nl2sql.enable_fewshot:
            self.__fewshot_manager = FewshotManager()

        if self._config.nl2sql.enable_postprocess:
            self.__sql_postprocessor = SQLPostProcessor()

        self._services_initialized = True

    async def process(self, input_data: DataAnalyticsInput) -> DataAnalyticsOutput:
        """データ分析処理を実行."""
        await self._ensure_services()

        question = input_data.question
        if not question:
            return DataAnalyticsOutput(error="質問が指定されていません")

        try:
            # 1. Schema Linking（関連テーブル・カラムを特定）
            schema_link_info: dict[str, Any] = {}
            schema_context = ""
            if self.__schema_linker:
                link_result = await self.__schema_linker.link(question)
                schema_link_info = {
                    "tables": list(link_result.linked_tables.keys()),
                    "confidence": link_result.overall_confidence,
                }
                schema_context = link_result.linked_schema

            # 2. Few-shot 例を取得
            query_pattern = ""
            if self.__fewshot_manager:
                similar_examples = self.__fewshot_manager.get_similar_examples(
                    question, k=self._config.nl2sql.fewshot_k
                )
                self.__fewshot_manager.format_examples_prompt(
                    similar_examples
                )
                query_pattern = self.__fewshot_manager._detect_pattern(question)

            # 3. DSL 中間層で NL → DSL → SQL
            dsl_output: DSLOutputSchema | None = None
            sql = ""
            if self._config.enable_dsl_pipeline and self.__semantic_layer:
                try:
                    sql, dsl = await self.__semantic_layer.nl_to_sql_via_dsl(question)
                    dsl_output = DSLOutputSchema(
                        metrics=dsl.metrics,
                        dimensions=dsl.dimensions,
                        filters=[f.to_dict() for f in dsl.filters] if dsl.filters else [],
                        order_by=[o.to_dict() for o in dsl.order_by] if dsl.order_by else [],
                        limit=dsl.limit,
                        time_range=dsl.time_range.to_dict() if dsl.time_range else None,
                        confidence=dsl.confidence,
                    )
                except Exception as e:
                    self._logger.warning(f"DSLパイプライン失敗、フォールバック: {e}")

            # 4. Post-Processing（SQL検証・修正）
            if sql and self.__sql_postprocessor:
                result = await self.__sql_postprocessor.process(
                    sql, question, schema_context
                )
                sql = result.final_sql

            # 5. SQL実行（ここではモック。実際は db_provider を使用）
            data: list[dict[str, Any]] = []
            columns: list[str] = []
            # TODO: 実際のDB接続時は以下を有効化
            # from agentflow.providers import get_db
            # db = get_db()
            # data = await db.execute(sql)

            # 6. チャート生成
            chart_output: ChartDataSchema | None = None
            if self._config.auto_chart and data and self.__chart_service:
                async for event in self.__chart_service.execute(
                    action="generate",
                    data=data,
                    columns=columns,
                    title=question[:50],
                ):
                    if event.get("type") == "result":
                        chart_data = event.get("data", {})
                        chart_output = ChartDataSchema(
                            chart_type=chart_data.get("chart_type", "bar"),
                            title=chart_data.get("title", ""),
                            echarts=chart_data.get("echarts", {}),
                            chartjs=chart_data.get("chartjs", {}),
                        )

            # 7. インサイト・提案生成
            insights: list[InsightSchema] = []
            suggestions: list[SuggestionOutputSchema] = []
            if self.__suggestion_service:
                context = {
                    "query_type": "sql",
                    "data_found": bool(data),
                    "data": data,
                    "columns": columns,
                }
                async for event in self.__suggestion_service.execute(
                    action="generate",
                    question=question,
                    context=context,
                    query_pattern=query_pattern,
                    dsl=dsl_output.model_dump() if dsl_output else None,
                ):
                    if event.get("type") == "result":
                        for s in event.get("data", {}).get("suggestions", []):
                            if s.get("type") == "insight":
                                insights.append(InsightSchema(
                                    text=s.get("text", ""),
                                    type=s.get("type", "insight"),
                                    confidence=s.get("confidence", 1.0),
                                    priority=s.get("priority", "medium"),
                                    reason=s.get("reason", ""),
                                ))
                            else:
                                suggestions.append(SuggestionOutputSchema(
                                    text=s.get("text", ""),
                                    type=s.get("type", "follow_up"),
                                    confidence=s.get("confidence", 1.0),
                                    priority=s.get("priority", "medium"),
                                ))

            # 8. 回答生成
            answer = self._generate_answer(question, sql, data)

            return DataAnalyticsOutput(
                question=question,
                answer=answer,
                sql=sql,
                dsl=dsl_output,
                data=data,
                columns=columns,
                chart=chart_output,
                insights=insights,
                suggestions=suggestions,
                schema_link_info=schema_link_info,
            )

        except Exception as e:
            self._logger.error(f"DataAnalyticsAgent実行エラー: {e}", exc_info=True)
            return DataAnalyticsOutput(
                question=question,
                answer=f"申し訳ありません。エラーが発生しました: {e}",
                error=str(e),
            )

    def _generate_answer(
        self, question: str, sql: str, data: list[dict[str, Any]]
    ) -> str:
        """回答を生成."""
        if not sql:
            return "クエリを生成できませんでした。"
        if not data:
            return f"クエリを実行しましたが、データが見つかりませんでした。\n\n生成SQL:\n```sql\n{sql}\n```"
        return f"データを取得しました（{len(data)}件）。"

    @classmethod
    def get_definition(cls) -> dict[str, Any]:
        """Agent定義（Studio用）."""
        return {
            "type": "data_analytics",
            "name": "DataAnalyticsAgent",
            "label": "データ分析Agent",
            "category": "specialized",
            "icon": "chart-bar",
            "description": "NL2SQL + DSL + Chart + Suggestion を統合したデータ分析Agent",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問", "required": True},
                {"name": "context", "type": "object", "label": "コンテキスト", "required": False},
            ],
            "outputs": [
                {"name": "answer", "type": "string", "label": "回答"},
                {"name": "sql", "type": "string", "label": "生成SQL"},
                {"name": "dsl", "type": "object", "label": "DSL中間表現"},
                {"name": "data", "type": "array", "label": "データ"},
                {"name": "chart", "type": "object", "label": "チャート"},
                {"name": "insights", "type": "array", "label": "インサイト"},
                {"name": "suggestions", "type": "array", "label": "提案"},
            ],
            "config": [
                {"name": "db_schema", "type": "object", "label": "DBスキーマ"},
                {"name": "auto_chart", "type": "boolean", "label": "自動チャート生成", "default": True},
                {"name": "auto_insights", "type": "boolean", "label": "自動インサイト生成", "default": True},
            ],
        }

