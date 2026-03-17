"""shared/agents - アプリ横断で使える汎用 Agent."""

from shared.agents.sales_agent import (
    SalesAgent,
    SalesAgentConfig,
    SalesInput,
    SalesOutput,
)

__all__ = [
    "SalesAgent",
    "SalesAgentConfig",
    "SalesInput",
    "SalesOutput",
]
