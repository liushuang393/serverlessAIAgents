"""FAQ System - Enterprise FAQ with RAG, Text2SQL, and Charts.

⚠️ 注意: このアプリは薄い App 層として設計されています。
業務ロジックは apps.faq_system.backend.agents.faq_agent に実装されています。

Features:
- RAG with multiple chunking strategies and rerankers
- Text2SQL for data queries with chart visualization
- Streaming responses with follow-up suggestions

Usage:
    # 推奨: App 層の Agent を直接使用
    from apps.faq_system.backend.agents.faq_agent import FAQAgent, FAQAgentConfig

    agent = FAQAgent(FAQAgentConfig())
    result = await agent.run({"question": "返品ポリシーは？"})
"""

# App 層の Agent を再エクスポート
from .backend.agents.faq_agent import FAQAgent, FAQAgentConfig


__all__ = ["FAQAgent", "FAQAgentConfig"]
