# -*- coding: utf-8 -*-
"""AgentFlow Agents - 専門化されたAgentコレクション.

通用Agentでは対応しにくいドメイン固有のタスクに対応する
専門化されたAgentを提供します。

アーキテクチャ:
    ┌─────────────────────────────────────────────────┐
    │              DeepAgentCoordinator               │
    │       (AgentPool: 6個の通用Agent)               │
    └─────────────────────────────────────────────────┘
                          │
                          ▼
    ┌─────────────────────────────────────────────────┐
    │           専門化Agent (このモジュール)          │
    │  - FAQAgent (RAG + SQL + Chart + Suggestion)   │
    │  - SalesAgent (売上分析)                        │
    │  - CustomerAgent (顧客対応)                    │
    └─────────────────────────────────────────────────┘
                          │
                          ▼
    ┌─────────────────────────────────────────────────┐
    │            Framework Services                   │
    │  RAG | Text2SQL | Chart | Suggestion | Auth    │
    └─────────────────────────────────────────────────┘

重要:
    全ての専門化Agentは ResilientAgent を継承し、
    Pydantic による型安全な入出力を提供します。

使用例:
    >>> from agentflow.agents import FAQAgent, FAQInput
    >>>
    >>> # 単独使用
    >>> agent = FAQAgent()
    >>> result = await agent.run({"question": "返品ポリシーは？"})
    >>>
    >>> # DeepAgentCoordinator と統合
    >>> from agentflow.patterns import DeepAgentCoordinator, AgentPool
    >>> pool = AgentPool()
    >>> pool.register_custom_agent("faq", FAQAgent())
    >>> coordinator = DeepAgentCoordinator(agent_pool=pool)
"""

from agentflow.agents.faq_agent import (
    ChartSchema,
    DocumentSchema,
    FAQAgent,
    FAQAgentConfig,
    FAQInput,
    FAQOutput,
    SuggestionSchema,
)
from agentflow.agents.sales_agent import (
    SalesAgent,
    SalesAgentConfig,
)

__all__ = [
    # FAQ Agent
    "FAQAgent",
    "FAQAgentConfig",
    "FAQInput",
    "FAQOutput",
    "DocumentSchema",
    "ChartSchema",
    "SuggestionSchema",
    # Sales Agent
    "SalesAgent",
    "SalesAgentConfig",
]
