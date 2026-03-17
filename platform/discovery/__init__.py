"""Layer 5 discovery 公開 API."""

from platform.discovery.base import AgentEntry, AgentStatus, LoadBalanceStrategy
from platform.discovery.registry import InMemoryAgentRegistry
from platform.discovery.service import DiscoveryService

# テスト・レガシー互換エイリアス
AgentDiscovery = InMemoryAgentRegistry

__all__ = [
    "AgentDiscovery",
    "AgentEntry",
    "AgentStatus",
    "DiscoveryService",
    "InMemoryAgentRegistry",
    "LoadBalanceStrategy",
]
