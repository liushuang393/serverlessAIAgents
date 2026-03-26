"""FAQ Agent - MCP 統合版 App Wrapper.

Agent → MCP → RAG の統一アーキテクチャに基づく FAQ Agent。
Agent は MCP Server を通じて検索ツールのみを呼び出し、
内部の検索実装には一切触れない。

アーキテクチャ:
    FAQAgent
      └── FAQMCPServer.call_tool()
           ├── knowledge_search (伝統的RAG)
           ├── file_search (ファイル検索)
           └── hybrid_search (ハイブリッド)
"""

from __future__ import annotations

import logging
import time
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from kernel.agents.agent_block import AgentBlock
from kernel.agents.specialized.faq_agent import (
    FAQAgentConfig,
    FAQResponse,
)
from shared.services.query_classifier import classify_query


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


# =============================================================================
# 互換スキーマ
# =============================================================================


class FAQInput(BaseModel):
    """FAQAgent 入力スキーマ."""

    question: str = Field(..., description="質問文")
    context: dict[str, Any] = Field(default_factory=dict, description="追加コンテキスト")


class DocumentSchema(BaseModel):
    """ソースドキュメントスキーマ."""

    id: str = ""
    content: str = ""
    source: str = ""
    score: float = 0.0


class ChartSchema(BaseModel):
    """チャートデータスキーマ."""

    chart_type: str = "bar"
    title: str = ""
    data: dict[str, Any] = Field(default_factory=dict)


class SuggestionSchema(BaseModel):
    """提案スキーマ."""

    text: str = ""
    type: str = "followup"


class FAQOutput(BaseModel):
    """FAQAgent 出力スキーマ."""

    question: str = ""
    answer: str = ""
    query_type: str = "faq"
    documents: list[DocumentSchema] = Field(default_factory=list)
    sql: str = ""
    data: list[dict[str, Any]] = Field(default_factory=list)
    columns: list[str] = Field(default_factory=list)
    chart: ChartSchema | None = None
    rich_response: dict[str, Any] | None = None
    artifacts: list[dict[str, Any]] = Field(default_factory=list)
    execution_report: dict[str, Any] = Field(default_factory=dict)
    suggestions: list[SuggestionSchema] = Field(default_factory=list)
    verification: dict[str, Any] = Field(default_factory=dict)
    error: str = ""


# =============================================================================
# FAQ Agent（MCP 統合版）
# =============================================================================


class FAQAgent(AgentBlock):
    """FAQ専門Agent (MCP統合版).

    全ての検索は MCP Server 経由で実行。
    Agent は検索の内部実装を知らない。
    """

    name = "FAQAgent"

    def __init__(
        self,
        config: FAQAgentConfig | None = None,
        llm_client: Any | None = None,  # 理由: LLM プロバイダ型は動的
    ) -> None:
        """初期化.

        Args:
            config: FAQAgent 設定
            llm_client: LLMクライアント（回答生成用）
        """
        super().__init__()
        self._config = config or FAQAgentConfig()
        self._llm_client = llm_client
        self._mcp_server: Any | None = None  # 理由: 循環 import 回避のため遅延型
        self._logger = logging.getLogger(self.name)

    async def _ensure_mcp_server(self) -> Any:
        """MCP Server を遅延初期化.

        Returns:
            FAQMCPServer インスタンス
        """
        if self._mcp_server is None:
            from apps.faq_system.backend.mcp.server import FAQMCPServer

            self._mcp_server = FAQMCPServer.create_default(
                collection=self._config.rag_collection,
                chunk_strategy=self._config.rag_chunk_strategy,
                reranker=self._config.rag_reranker,
                top_k=self._config.rag_top_k,
            )
        return self._mcp_server

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent 実行（MCP 経由）.

        Args:
            input_data: 入力データ（"question" キー必須）

        Returns:
            FAQResponse の dict 表現
        """
        start_time = time.time()
        question = input_data.get("question", "")

        if not question:
            return FAQResponse(error="質問が指定されていません").model_dump()

        try:
            # 1. クエリタイプ判定
            query_type = classify_query(question).value
            agent_trace = [f"Query classified as: {query_type}"]

            # 2. MCP ツール選択・実行
            mcp = await self._ensure_mcp_server()
            tool_name = self._select_tool(query_type)
            agent_trace.append(f"Selected MCP tool: {tool_name}")

            mcp_result = await mcp.call_tool(
                tool_name,
                {
                    "query": question,
                    "top_k": self._config.rag_top_k,
                },
            )

            # 3. 結果を FAQResponse に変換
            if mcp_result.success and mcp_result.output is not None:
                documents = mcp_result.output.get("documents", [])
                answer = self._build_answer_from_docs(question, documents)
                citations = [
                    {
                        "id": d.get("doc_id", ""),
                        "title": d.get("source", ""),
                        "snippet": d.get("content", "")[:200],
                        "score": d.get("score", 0.0),
                    }
                    for d in documents
                ]
                response = FAQResponse(
                    question=question,
                    answer=answer,
                    query_type=query_type,
                    confidence=self._calculate_confidence(documents),
                    citations=citations,
                    agent_trace=agent_trace,
                )
            else:
                errors = mcp_result.errors if mcp_result.errors else ["検索結果なし"]
                response = FAQResponse(
                    question=question,
                    query_type=query_type,
                    error="; ".join(errors),
                    agent_trace=agent_trace,
                )

            elapsed = (time.time() - start_time) * 1000
            response.execution_time_ms = elapsed
            return response.model_dump()

        except Exception as e:
            self._logger.exception("FAQ Agent エラー: %s", e)
            elapsed = (time.time() - start_time) * 1000
            return FAQResponse(
                question=question,
                error=str(e),
                execution_time_ms=elapsed,
            ).model_dump()

    async def run_stream(self, input_data: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """進捗イベントを返しながら FAQ 実行を行う."""
        question = str(input_data.get("question", ""))

        yield {
            "type": "progress",
            "progress": 0,
            "message": f"「{question[:20]}」を処理中...",
        }

        yield {
            "type": "progress",
            "progress": 30,
            "message": "検索経路を選択中...",
        }

        result = await self.run(input_data)

        yield {
            "type": "progress",
            "progress": 100,
            "message": "完了",
        }
        yield {
            "type": "result",
            "data": result,
        }

    def _select_tool(self, query_type: str) -> str:
        """クエリタイプに応じた MCP ツールを選択.

        Args:
            query_type: クエリタイプ（faq / sql / hybrid）

        Returns:
            MCP ツール名
        """
        if query_type == "hybrid":
            return "hybrid_search"
        # faq / sql / その他 → knowledge_search
        return "knowledge_search"

    def _build_answer_from_docs(
        self,
        question: str,
        documents: list[dict[str, Any]],
    ) -> str:
        """検索結果から回答を構築.

        Args:
            question: 質問
            documents: 検索結果ドキュメント

        Returns:
            回答テキスト
        """
        if not documents:
            return "関連する情報が見つかりませんでした。"
        # 上位ドキュメントのコンテンツを結合
        contents = [d.get("content", "") for d in documents[:3] if d.get("content")]
        if not contents:
            return "関連する情報が見つかりませんでした。"
        return "\n\n".join(contents)

    def _calculate_confidence(self, documents: list[dict[str, Any]]) -> float:
        """信頼度を計算.

        Args:
            documents: 検索結果ドキュメント

        Returns:
            信頼度（0.0-1.0）
        """
        if not documents:
            return 0.0
        scores = [float(d.get("score", 0.0)) for d in documents]
        return max(scores) if scores else 0.0

    async def process(self, input_data: FAQInput) -> FAQOutput:
        """互換用プロセスメソッド.

        Args:
            input_data: FAQInput

        Returns:
            FAQOutput
        """
        result_dict = await self.run(input_data.model_dump())
        output = FAQOutput(
            question=result_dict.get("question", ""),
            answer=result_dict.get("answer", ""),
            query_type=result_dict.get("query_type", "faq"),
            sql=result_dict.get("sql", ""),
            data=result_dict.get("data", []),
            columns=result_dict.get("columns", []),
            error=result_dict.get("error", ""),
        )
        if "rich_response" in result_dict:
            output.rich_response = result_dict["rich_response"]
        if "chart" in result_dict and isinstance(result_dict["chart"], dict):
            output.chart = ChartSchema(**result_dict["chart"])
        if "suggestions" in result_dict:
            output.suggestions = [SuggestionSchema(**s) for s in result_dict["suggestions"] if isinstance(s, dict)]
        if "citations" in result_dict:
            output.documents = [
                DocumentSchema(
                    id=c.get("id", ""),
                    content=c.get("snippet", ""),
                    source=c.get("title", ""),
                    score=c.get("score", 0.0),
                )
                for c in result_dict["citations"]
                if isinstance(c, dict)
            ]
        return output

    # -----------------------------------------------------------------------
    # 互換用内部メソッド（テスト用）
    # -----------------------------------------------------------------------

    @property
    def _llm(self) -> Any:  # 理由: LLM プロバイダ型は動的
        """LLM クライアント（互換用プロパティ）."""
        return self._llm_client

    @_llm.setter
    def _llm(self, value: Any) -> None:  # 理由: LLM プロバイダ型は動的
        self._llm_client = value

    async def _classify_query(self, question: str) -> str:
        """テスト互換用のクエリ分類.

        Args:
            question: 質問文

        Returns:
            クエリタイプ文字列
        """
        return classify_query(question).value

    def _classify_query_heuristic(self, question: str) -> str:
        """テスト互換用のヒューリスティック分類.

        Args:
            question: 質問文

        Returns:
            クエリタイプ文字列
        """
        from shared.services.query_classifier import QueryClassifier

        return QueryClassifier().classify(question).value

    async def _handle_chat_query(self, question: str, query_type: str) -> FAQOutput:
        """互換用: MCP 経由で実行.

        Args:
            question: 質問文
            query_type: クエリタイプ

        Returns:
            FAQOutput
        """
        return await self.process(FAQInput(question=question))

    async def _handle_faq_query(self, question: str, query_type: str) -> FAQOutput:
        """互換用 FAQ クエリハンドラ."""
        return await self.process(FAQInput(question=question))

    async def _handle_sql_query(self, question: str, query_type: str) -> FAQOutput:
        """互換用 SQL クエリハンドラ."""
        return await self.process(FAQInput(question=question))

    async def _handle_weather_query(self, question: str, query_type: str) -> FAQOutput:
        """互換用天気クエリハンドラ."""
        return await self.process(FAQInput(question=question))

    async def _handle_external_query(
        self,
        question: str,
        query_type: str,
        context: dict[str, Any] | None = None,
    ) -> FAQOutput:
        """互換用外部クエリハンドラ."""
        return await self.process(FAQInput(question=question, context=context or {}))

    async def _handle_hybrid_query(self, question: str, query_type: str) -> FAQOutput:
        """互換用ハイブリッドクエリハンドラ."""
        return await self.process(FAQInput(question=question))


__all__ = ["FAQAgent", "FAQAgentConfig", "FAQInput", "FAQOutput"]
