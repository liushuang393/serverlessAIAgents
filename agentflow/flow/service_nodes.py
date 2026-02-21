"""サービスノード定義.

フレームワーク級サービスをフローノードとしてラップ。
Studio UIでノーコードで使用可能。

ノードタイプ:
- RAGNode: ナレッジベース検索
- Text2SQLNode: 自然言語→SQL
- ChartNode: チャート生成
- SuggestionNode: 提案生成

使用例:
    >>> from agentflow.flow.service_nodes import RAGNode
    >>>
    >>> # フロー定義内で使用
    >>> node = RAGNode(
    ...     id="rag_search",
    ...     name="ナレッジ検索",
    ...     config=RAGConfig(collection="faq"),
    ... )
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from enum import Enum, auto
from typing import TYPE_CHECKING, Any

from agentflow.flow.types import NextAction, NodeResult


if TYPE_CHECKING:
    from agentflow.flow.context import FlowContext


# =============================================================================
# ノードタイプ拡張
# =============================================================================


class ServiceNodeType(Enum):
    """サービスノードタイプ."""

    RAG = auto()
    TEXT2SQL = auto()
    CHART = auto()
    SUGGESTION = auto()
    FAQ = auto()  # 複合ノード


# =============================================================================
# ベースクラス
# =============================================================================


@dataclass
class ServiceNode:
    """サービスノード基底クラス."""

    id: str
    name: str
    node_type: ServiceNodeType = field(default=ServiceNodeType.RAG)
    label: str = ""
    icon: str = ""
    config: Any = None

    def __post_init__(self) -> None:
        self._logger = logging.getLogger(f"agentflow.flow.service_node.{self.id}")
        if not self.label:
            self.label = self.name

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """ノードを実行."""
        raise NotImplementedError


# =============================================================================
# RAGNode
# =============================================================================


@dataclass
class RAGNode(ServiceNode):
    """RAG検索ノード.

    ナレッジベースを検索し、回答を生成。

    Attributes:
        config: RAGConfig
        input_key: 入力キー（デフォルト: "question"）
        output_key: 出力キー（デフォルト: "rag_result"）
    """

    input_key: str = "question"
    output_key: str = "rag_result"

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", ServiceNodeType.RAG)
        if not self.icon:
            self.icon = "search"

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """RAG検索を実行."""
        from agentflow.services import RAGConfig, RAGService

        try:
            config = self.config if isinstance(self.config, RAGConfig) else RAGConfig()
            service = RAGService(config)

            inputs = ctx.get_inputs()
            question = inputs.get(self.input_key, "")

            if not question:
                return NodeResult(
                    success=False,
                    data={"error": "質問が指定されていません"},
                    action=NextAction.STOP,
                )

            result = await service.execute(
                action="query",
                question=question,
                filters=inputs.get("filters"),
            )

            ctx.set_result(self.id, result.data)
            ctx.set_result(self.output_key, result.data)

            return NodeResult(
                success=result.success,
                data=result.data,
                action=NextAction.CONTINUE if result.success else NextAction.STOP,
            )
        except Exception as e:
            self._logger.exception(f"RAG検索失敗: {e}")
            return NodeResult(
                success=False,
                data={"error": str(e)},
                action=NextAction.STOP,
            )

    @classmethod
    def get_studio_definition(cls) -> dict[str, Any]:
        """Studio UI用ノード定義."""
        from agentflow.services import RAGConfig

        return {
            "type": "rag",
            "label": "RAG検索",
            "category": "knowledge",
            "icon": "search",
            "description": "ナレッジベースをRAGで検索",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問", "required": True},
                {"name": "filters", "type": "object", "label": "フィルター", "required": False},
            ],
            "outputs": [
                {"name": "answer", "type": "string", "label": "回答"},
                {"name": "documents", "type": "array", "label": "ソースドキュメント"},
            ],
            "config": RAGConfig.get_config_fields(),
        }


# =============================================================================
# Text2SQLNode
# =============================================================================


@dataclass
class Text2SQLNode(ServiceNode):
    """Text2SQLノード.

    自然言語からSQLを生成して実行。

    Attributes:
        config: Text2SQLConfig
        input_key: 入力キー
        output_key: 出力キー
    """

    input_key: str = "question"
    output_key: str = "sql_result"

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", ServiceNodeType.TEXT2SQL)
        if not self.icon:
            self.icon = "database"

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """Text2SQLを実行."""
        from agentflow.services import Text2SQLConfig, Text2SQLService

        try:
            config = self.config if isinstance(self.config, Text2SQLConfig) else Text2SQLConfig()
            service = Text2SQLService(config)

            inputs = ctx.get_inputs()
            question = inputs.get(self.input_key, "")

            if not question:
                return NodeResult(
                    success=False,
                    data={"error": "質問が指定されていません"},
                    action=NextAction.STOP,
                )

            result = await service.execute(
                action="query",
                question=question,
            )

            ctx.set_result(self.id, result.data)
            ctx.set_result(self.output_key, result.data)

            return NodeResult(
                success=result.success,
                data=result.data,
                action=NextAction.CONTINUE if result.success else NextAction.STOP,
            )
        except Exception as e:
            self._logger.exception(f"Text2SQL失敗: {e}")
            return NodeResult(
                success=False,
                data={"error": str(e)},
                action=NextAction.STOP,
            )

    @classmethod
    def get_studio_definition(cls) -> dict[str, Any]:
        """Studio UI用ノード定義."""
        from agentflow.services import Text2SQLConfig

        return {
            "type": "text2sql",
            "label": "Text2SQL",
            "category": "data",
            "icon": "database",
            "description": "自然言語からSQLを生成して実行",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問", "required": True},
            ],
            "outputs": [
                {"name": "answer", "type": "string", "label": "回答"},
                {"name": "sql", "type": "string", "label": "生成SQL"},
                {"name": "data", "type": "array", "label": "データ"},
                {"name": "chart", "type": "object", "label": "チャート"},
            ],
            "config": Text2SQLConfig.get_config_fields(),
        }


# =============================================================================
# ChartNode
# =============================================================================


@dataclass
class ChartNode(ServiceNode):
    """チャート生成ノード.

    データからチャートを自動生成。

    Attributes:
        config: ChartConfig
        input_key: データ入力キー
        output_key: 出力キー
    """

    input_key: str = "data"
    output_key: str = "chart"

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", ServiceNodeType.CHART)
        if not self.icon:
            self.icon = "chart-bar"

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """チャートを生成."""
        from agentflow.services import ChartConfig, ChartService

        try:
            config = self.config if isinstance(self.config, ChartConfig) else ChartConfig()
            service = ChartService(config)

            inputs = ctx.get_inputs()
            data = inputs.get(self.input_key, [])
            columns = inputs.get("columns", [])
            title = inputs.get("title", "")

            if not data:
                return NodeResult(
                    success=False,
                    data={"error": "データが指定されていません"},
                    action=NextAction.STOP,
                )

            result = await service.execute(
                action="generate",
                data=data,
                columns=columns,
                title=title,
            )

            ctx.set_result(self.id, result.data)
            ctx.set_result(self.output_key, result.data)

            return NodeResult(
                success=result.success,
                data=result.data,
                action=NextAction.CONTINUE,
            )
        except Exception as e:
            self._logger.exception(f"チャート生成失敗: {e}")
            return NodeResult(
                success=False,
                data={"error": str(e)},
                action=NextAction.STOP,
            )

    @classmethod
    def get_studio_definition(cls) -> dict[str, Any]:
        """Studio UI用ノード定義."""
        from agentflow.services import ChartConfig

        return {
            "type": "chart",
            "label": "チャート生成",
            "category": "visualization",
            "icon": "chart-bar",
            "description": "データからチャートを自動生成",
            "inputs": [
                {"name": "data", "type": "array", "label": "データ", "required": True},
                {"name": "columns", "type": "array", "label": "カラム", "required": False},
                {"name": "title", "type": "string", "label": "タイトル", "required": False},
            ],
            "outputs": [
                {"name": "echarts", "type": "object", "label": "ECharts設定"},
                {"name": "chartjs", "type": "object", "label": "Chart.js設定"},
                {"name": "chart_type", "type": "string", "label": "チャートタイプ"},
            ],
            "config": ChartConfig.get_config_fields(),
        }


# =============================================================================
# SuggestionNode
# =============================================================================


@dataclass
class SuggestionNode(ServiceNode):
    """提案生成ノード.

    フォローアップ質問やアクションを提案。

    Attributes:
        config: SuggestionConfig
        input_key: 質問入力キー
        output_key: 出力キー
    """

    input_key: str = "question"
    output_key: str = "suggestions"

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", ServiceNodeType.SUGGESTION)
        if not self.icon:
            self.icon = "lightbulb"

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """提案を生成."""
        from agentflow.services import SuggestionConfig, SuggestionService

        try:
            config = self.config if isinstance(self.config, SuggestionConfig) else SuggestionConfig()
            service = SuggestionService(config)

            inputs = ctx.get_inputs()
            question = inputs.get(self.input_key, "")
            context = inputs.get("context", {})

            result = await service.execute(
                question=question,
                context=context,
            )

            ctx.set_result(self.id, result.data)
            ctx.set_result(self.output_key, result.data.get("suggestions", []))

            return NodeResult(
                success=result.success,
                data=result.data,
                action=NextAction.CONTINUE,
            )
        except Exception as e:
            self._logger.exception(f"提案生成失敗: {e}")
            return NodeResult(
                success=False,
                data={"error": str(e)},
                action=NextAction.STOP,
            )

    @classmethod
    def get_studio_definition(cls) -> dict[str, Any]:
        """Studio UI用ノード定義."""
        from agentflow.services import SuggestionConfig

        return {
            "type": "suggestion",
            "label": "提案生成",
            "category": "ai",
            "icon": "lightbulb",
            "description": "フォローアップ質問やアクションを提案",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問", "required": True},
                {"name": "context", "type": "object", "label": "コンテキスト", "required": False},
            ],
            "outputs": [
                {"name": "suggestions", "type": "array", "label": "提案リスト"},
            ],
            "config": SuggestionConfig.get_config_fields(),
        }


# =============================================================================
# FAQNode（複合ノード）
# =============================================================================


@dataclass
class FAQNode(ServiceNode):
    """FAQ複合ノード.

    RAG + Text2SQL + Chart + Suggestion を統合。
    質問タイプを自動判定して適切なサービスを実行。

    Attributes:
        rag_config: RAG設定
        sql_config: SQL設定
        chart_config: チャート設定
        suggestion_config: 提案設定
    """

    rag_config: Any = None
    sql_config: Any = None
    chart_config: Any = None
    suggestion_config: Any = None

    def __post_init__(self) -> None:
        super().__post_init__()
        object.__setattr__(self, "node_type", ServiceNodeType.FAQ)
        if not self.icon:
            self.icon = "question-circle"

    async def execute(self, ctx: FlowContext) -> NodeResult:
        """FAQ処理を実行."""
        from agentflow.services import (
            RAGConfig,
            RAGService,
            SuggestionConfig,
            SuggestionService,
            Text2SQLConfig,
            Text2SQLService,
        )

        try:
            inputs = ctx.get_inputs()
            question = inputs.get("question", "")

            if not question:
                return NodeResult(
                    success=False,
                    data={"error": "質問が指定されていません"},
                    action=NextAction.STOP,
                )

            # クエリタイプを判定
            query_type = self._classify_query(question)

            result_data: dict[str, Any] = {
                "question": question,
                "query_type": query_type,
            }

            if query_type == "sql":
                # SQL処理
                sql_config = self.sql_config or Text2SQLConfig()
                sql_service = Text2SQLService(sql_config)
                sql_result = await sql_service.execute(action="query", question=question)

                result_data["answer"] = sql_result.data.get("answer", "")
                result_data["sql"] = sql_result.data.get("sql", "")
                result_data["data"] = sql_result.data.get("data", [])
                result_data["chart"] = sql_result.data.get("chart")

            else:
                # RAG処理
                rag_config = self.rag_config or RAGConfig()
                rag_service = RAGService(rag_config)
                rag_result = await rag_service.execute(action="query", question=question)

                result_data["answer"] = rag_result.data.get("answer", "")
                result_data["documents"] = rag_result.data.get("documents", [])

            # 提案生成
            suggestion_config = self.suggestion_config or SuggestionConfig()
            suggestion_service = SuggestionService(suggestion_config)
            suggestion_result = await suggestion_service.execute(
                question=question,
                context={
                    "query_type": query_type,
                    "data_found": bool(result_data.get("data") or result_data.get("documents")),
                },
            )
            result_data["suggestions"] = suggestion_result.data.get("suggestions", [])

            ctx.set_result(self.id, result_data)

            return NodeResult(
                success=True,
                data=result_data,
                action=NextAction.CONTINUE,
            )
        except Exception as e:
            self._logger.exception(f"FAQ処理失敗: {e}")
            return NodeResult(
                success=False,
                data={"error": str(e)},
                action=NextAction.STOP,
            )

    def _classify_query(self, question: str) -> str:
        """クエリタイプを判定."""
        sql_keywords = [
            "売上",
            "収入",
            "数量",
            "統計",
            "報表",
            "top",
            "排名",
            "トレンド",
            "比較",
            "同比",
            "金額",
            "注文",
            "顧客数",
        ]
        question_lower = question.lower()
        sql_score = sum(1 for k in sql_keywords if k in question_lower)
        return "sql" if sql_score >= 2 else "faq"

    @classmethod
    def get_studio_definition(cls) -> dict[str, Any]:
        """Studio UI用ノード定義."""
        from agentflow.services import RAGConfig, Text2SQLConfig

        return {
            "type": "faq",
            "label": "FAQ",
            "category": "ai",
            "icon": "question-circle",
            "description": "RAG + SQL + チャート統合FAQ",
            "inputs": [
                {"name": "question", "type": "string", "label": "質問", "required": True},
            ],
            "outputs": [
                {"name": "answer", "type": "string", "label": "回答"},
                {"name": "query_type", "type": "string", "label": "クエリタイプ"},
                {"name": "documents", "type": "array", "label": "ソースドキュメント"},
                {"name": "sql", "type": "string", "label": "生成SQL"},
                {"name": "data", "type": "array", "label": "データ"},
                {"name": "chart", "type": "object", "label": "チャート"},
                {"name": "suggestions", "type": "array", "label": "提案"},
            ],
            "config": [
                *RAGConfig.get_config_fields(),
                *Text2SQLConfig.get_config_fields(),
            ],
        }


# =============================================================================
# ノードレジストリ
# =============================================================================


def get_all_service_node_definitions() -> list[dict[str, Any]]:
    """全サービスノード定義を取得.

    Studio UIのノードパレット用。
    """
    return [
        RAGNode.get_studio_definition(),
        Text2SQLNode.get_studio_definition(),
        ChartNode.get_studio_definition(),
        SuggestionNode.get_studio_definition(),
        FAQNode.get_studio_definition(),
    ]


__all__ = [
    "ChartNode",
    "FAQNode",
    "RAGNode",
    "ServiceNode",
    "ServiceNodeType",
    "SuggestionNode",
    "Text2SQLNode",
    "get_all_service_node_definitions",
]
