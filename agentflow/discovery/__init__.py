"""Agent Discovery モジュール.

大規模デプロイメント向けの Agent 発見・登録機構。

Examples:
    >>> from agentflow.discovery import AgentDiscovery, AgentEntry
    >>> discovery = AgentDiscovery()
    >>> await discovery.register(AgentEntry(
    ...     agent_id="agent-001",
    ...     name="ResearchAgent",
    ...     endpoint="http://localhost:8001",
    ...     capabilities=["research", "summarize"],
    ... ))
"""

from agentflow.discovery.base import (
    AgentEntry,
    AgentStatus,
    LoadBalanceStrategy,
)
from agentflow.discovery.registry import InMemoryAgentRegistry as AgentDiscovery


__all__ = [
    "AgentDiscovery",
    "AgentEntry",
    "AgentStatus",
    "LoadBalanceStrategy",
]
