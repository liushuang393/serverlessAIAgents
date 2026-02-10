"""Knowledge Discovery Manager.

Orchestrates RAG, SQL, and Maintenance services into a unified experience.
"""

import logging
from typing import Any
from agentflow.services.semantic_layer import SemanticLayerService
from agentflow.services.text2sql_service import Text2SQLService
from agentflow.services.rag_service import RAGService
from agentflow.services.chart_service import ChartService
from agentflow.services.suggestion_service import SuggestionService

logger = logging.getLogger(__name__)

class KnowledgeDiscoveryManager:
    """Unified manager for knowledge and data discovery."""

    def __init__(self, llm_client: Any = None):
        self.semantic_layer = SemanticLayerService()
        self.text2sql = Text2SQLService(llm_client=llm_client)
        self.rag = RAGService()
        self.charting = ChartService()
        self.suggestions = SuggestionService()

    async def discover(self, question: str, user_context: dict[str, Any]) -> dict[str, Any]:
        """Main entry point for unified discovery."""
        # 1. Intent routing (simplified)
        if any(k in question for k in ["売上", "統計", "ランキング"]):
            return await self._handle_analytics(question, user_context)
        else:
            return await self._handle_faq(question, user_context)

    async def _handle_analytics(self, question: str, user_context: dict) -> dict:
        resolved = await self.semantic_layer.resolve(question)
        sql_result = await self.text2sql.generate_and_execute(resolved, user_context)
        
        # Use centralized charting skill
        chart = await self.charting.generate_chart(sql_result["data"], title=f"{question}の分析")
        
        # Use SuggestionService for UI2Agent actions
        suggestions_result = await self.suggestions.execute(
            question=question,
            context={"query_type": "sql", "data_found": True, "has_chart": True},
            action="generate"
        )
        
        return {
            "type": "analytics",
            "answer": sql_result.get("answer", "分析結果を表示します。"),
            "data": sql_result.get("data"),
            "chart": chart,
            "suggestions": [s for s in suggestions_result["suggestions"] if s["type"] == "action"],
            "next_steps": suggestions_result["suggestions"]
        }

    async def _handle_faq(self, question: str, user_context: dict) -> dict:
        rag_result = await self.rag.query(question)
        next_steps = await self.suggestions.get_suggestions(question)
        
        return {
            "type": "faq",
            "answer": rag_result["answer"],
            "citations": rag_result["citations"],
            "next_steps": next_steps
        }
