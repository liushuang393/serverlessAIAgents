"""LocalA2AHub compatibility wrapper.

内部 canonical 実装は LocalAgentBus に移し、このモジュールは旧 API を維持する。
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from kernel.agents.local_agent_bus import (
    AgentNotFoundError,
    LocalAgentBus,
    get_agent_bus,
    reset_agent_bus,
)


if TYPE_CHECKING:
    from kernel.agents.contracts import AgentDescriptor
    from kernel.protocols.a2a_card import AgentCard


class LocalA2AHub:
    """旧 LocalA2AHub API の互換ラッパー."""

    def __init__(self, bus: LocalAgentBus | None = None) -> None:
        """初期化."""
        self._bus = bus or LocalAgentBus()

    def register(
        self,
        agent_instance: Any,
        *,
        card: AgentCard | None = None,
        replace: bool = False,
    ) -> AgentCard:
        """Agent を登録し、A2A card を返す."""
        descriptor = self._bus.register(
            agent_instance,
            replace=replace,
            card=card,
        )
        resolved_card = self._bus.get_a2a_card(descriptor.agent_id)
        if resolved_card is None:
            msg = f"Failed to build AgentCard for {descriptor.agent_id}"
            raise ValueError(msg)
        return resolved_card

    async def unregister(self, agent_name: str) -> None:
        """Agent を解除."""
        await self._bus.unregister(agent_name)

    async def call(self, agent_name: str, input_data: dict[str, Any]) -> dict[str, Any]:
        """Agent を呼び出す."""
        return await self._bus.call(agent_name, input_data)

    def discover(self, agent_name: str) -> AgentCard | None:
        """A2A card を取得."""
        return self._bus.get_a2a_card(agent_name)

    def describe(self, agent_name: str) -> AgentDescriptor | None:
        """内部 descriptor を取得."""
        return self._bus.discover(agent_name)

    def list_agents(self) -> list[AgentCard]:
        """登録済み AgentCard 一覧."""
        return self._bus.list_cards()

    def get_agent(self, agent_name: str) -> Any | None:
        """Agent instance を取得."""
        return self._bus.get_agent(agent_name)

    @property
    def bus(self) -> LocalAgentBus:
        """内部 canonical bus を取得."""
        return self._bus

    @property
    def agent_count(self) -> int:
        """登録済み Agent 数."""
        return self._bus.agent_count

    def clear(self) -> None:
        """全 agent をクリア."""
        self._bus.clear()


_global_hub: LocalA2AHub | None = None


def get_hub() -> LocalA2AHub:
    """グローバル LocalA2AHub を取得."""
    global _global_hub
    if _global_hub is None:
        _global_hub = LocalA2AHub(bus=get_agent_bus())
    return _global_hub


def reset_hub() -> None:
    """グローバル LocalA2AHub をリセット."""
    global _global_hub
    if _global_hub is not None:
        _global_hub.clear()
    _global_hub = None
    reset_agent_bus()


__all__ = [
    "AgentNotFoundError",
    "LocalA2AHub",
    "get_hub",
    "reset_hub",
]
