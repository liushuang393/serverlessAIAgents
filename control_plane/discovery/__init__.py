"""Control-plane discovery 公開 API."""

from control_plane.discovery.base import AgentEntry, AgentStatus, LoadBalanceStrategy
from control_plane.discovery.registry import InMemoryAgentRegistry
from control_plane.discovery.service import DiscoveryService


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
