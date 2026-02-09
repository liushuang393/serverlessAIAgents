"""FAQ System - Enterprise FAQ with RAG, Text2SQL, and Charts.

⚠️ 注意: このアプリは薄い App 層として設計されています。
業務ロジックは agentflow/agents/faq_agent.py に実装されています。

Features:
- RAG with multiple chunking strategies and rerankers
- Text2SQL for data queries with chart visualization
- Streaming responses with follow-up suggestions

Usage:
    # 推奨: フレームワーク層の Agent を直接使用
    from agentflow.agents import FAQAgent, FAQAgentConfig

    agent = FAQAgent(FAQAgentConfig())
    result = await agent.run({"question": "返品ポリシーは？"})
"""

# フレームワーク層の Agent を再エクスポート
from agentflow.agents import FAQAgent, FAQAgentConfig


__all__ = ["FAQAgent", "FAQAgentConfig"]
