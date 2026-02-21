"""Knowledge Discovery Manager.

Orchestrates RAG, SQL, and Maintenance services into a unified experience.
"""

import logging
from typing import Any

from agentflow.services.chart_service import ChartService
from agentflow.services.rag_service import RAGService
from agentflow.services.semantic_layer import SemanticLayerService
from agentflow.services.suggestion_service import SuggestionService
from agentflow.services.text2sql_service import Text2SQLService


logger = logging.getLogger(__name__)


class KnowledgeDiscoveryManager:
    """Unified manager for knowledge and data discovery."""

    def __init__(self, llm_client: Any = None) -> None:
        self.semantic_layer = SemanticLayerService()
        self.text2sql = Text2SQLService()
        self.rag = RAGService()
        self.charting = ChartService()
        self.suggestions = SuggestionService()

    async def discover(self, question: str, user_context: dict[str, Any]) -> dict[str, Any]:
        """Main entry point for unified discovery."""
        # 1. Intent routing (simplified)
        if any(k in question for k in ["売上", "統計", "ランキング"]):
            return await self._handle_analytics(question, user_context)
        return await self._handle_faq(question, user_context)

    async def _handle_analytics(self, question: str, user_context: dict[str, Any]) -> dict[str, Any]:
        resolved = await self.semantic_layer.resolve(question)
        sql_result_resp = await self.text2sql.execute(
            action="query",
            question=str(getattr(resolved, "normalized_query", question)),
        )
        sql_result = sql_result_resp.data if isinstance(sql_result_resp.data, dict) else {}

        # Use centralized charting skill
        chart_resp = await self.charting.execute(
            action="generate",
            data=sql_result.get("data", []),
            title=f"{question}の分析",
        )
        chart = chart_resp.data

        # Use SuggestionService for UI2Agent actions
        suggestions_result = await self.suggestions.execute(
            question=question,
            context={"query_type": "sql", "data_found": True, "has_chart": True},
            action="generate",
        )
        suggestions = (
            suggestions_result.data.get("suggestions", []) if isinstance(suggestions_result.data, dict) else []
        )

        return {
            "type": "analytics",
            "answer": sql_result.get("answer", "分析結果を表示します。"),
            "data": sql_result.get("data"),
            "chart": chart,
            "suggestions": [s for s in suggestions if isinstance(s, dict) and s.get("type") == "action"],
            "next_steps": suggestions,
        }

    async def _handle_faq(self, question: str, user_context: dict[str, Any]) -> dict[str, Any]:
        rag_resp = await self.rag.execute(action="query", question=question)
        rag_result = rag_resp.data if isinstance(rag_resp.data, dict) else {}
        suggestion_resp = await self.suggestions.execute(action="generate", question=question, context=user_context)
        next_steps = suggestion_resp.data.get("suggestions", []) if isinstance(suggestion_resp.data, dict) else []

        return {
            "type": "faq",
            "answer": rag_result.get("answer", ""),
            "citations": rag_result.get("citations", []),
            "next_steps": next_steps,
        }
