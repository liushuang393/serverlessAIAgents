"""app_config.json から executable agent runtime を構築する."""

from __future__ import annotations

import time
from dataclasses import dataclass
from typing import TYPE_CHECKING, Any
from uuid import uuid4

from kernel.agents.agent_factory import AgentFactory
from kernel.agents.local_agent_bus import LocalAgentBus, get_agent_bus
from kernel.protocols.a2a_hub import LocalA2AHub, get_hub
from kernel.protocols.a2a_server import A2AServer
from kernel.protocols.agui_events import (
    AGUIEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
    NodeCompleteEvent,
    NodeErrorEvent,
    NodeStartEvent,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from pathlib import Path

    from kernel.protocols.a2a_card import AgentCard


@dataclass(slots=True)
class AppAgentRuntime:
    """実行可能 agent runtime."""

    config_path: Path
    bus: LocalAgentBus
    hub: LocalA2AHub
    server: A2AServer
    agents: dict[str, Any]
    cards: dict[str, AgentCard]

    def list_agent_ids(self) -> list[str]:
        """登録済み agent ID 一覧を返す."""
        return sorted(self.cards.keys())

    def get_agent(self, agent_id: str) -> Any | None:
        """agent instance を返す."""
        return self.agents.get(agent_id)

    def get_card(self, agent_id: str) -> AgentCard | None:
        """agent card を返す."""
        return self.cards.get(agent_id) or self.bus.get_a2a_card(agent_id)

    async def invoke(self, agent_id: str, input_data: dict[str, Any]) -> dict[str, Any]:
        """agent を同期的に実行する."""
        if self.get_agent(agent_id) is None:
            msg = f"Agent not found: {agent_id}"
            raise KeyError(msg)
        return await self.bus.call(agent_id, input_data)

    async def stream(self, agent_id: str, input_data: dict[str, Any]) -> AsyncIterator[AGUIEvent]:
        """agent 実行を AG-UI イベント列として返す."""
        flow_id = f"{agent_id}-{uuid4().hex[:8]}"
        now = time.time()
        yield FlowStartEvent(
            timestamp=now,
            flow_id=flow_id,
            data={"agent_id": agent_id},
        )
        yield NodeStartEvent(
            timestamp=time.time(),
            flow_id=flow_id,
            node_id=agent_id,
            node_name=agent_id,
            data={},
        )
        try:
            result = await self.invoke(agent_id, input_data)
        except Exception as exc:
            yield NodeErrorEvent(
                timestamp=time.time(),
                flow_id=flow_id,
                node_id=agent_id,
                node_name=agent_id,
                error_message=str(exc),
                error_type=type(exc).__name__,
                data={},
            )
            yield FlowErrorEvent(
                timestamp=time.time(),
                flow_id=flow_id,
                error_message=str(exc),
                error_type=type(exc).__name__,
                data={"agent_id": agent_id},
            )
            return

        yield NodeCompleteEvent(
            timestamp=time.time(),
            flow_id=flow_id,
            node_id=agent_id,
            node_name=agent_id,
            data={"result": result},
        )
        yield FlowCompleteEvent(
            timestamp=time.time(),
            flow_id=flow_id,
            result=result,
            include_result=True,
            data={"agent_id": agent_id},
        )


def bootstrap_app_agents(
    config_path: Path,
    *,
    bus: LocalAgentBus | None = None,
    hub: LocalA2AHub | None = None,
    agent_init_overrides: dict[str, dict[str, Any]] | None = None,
) -> AppAgentRuntime:
    """manifest から agent runtime を構築する."""
    target_bus = bus or (hub.bus if hub is not None else get_agent_bus())
    target_hub = hub or get_hub()
    server = A2AServer()
    agents = AgentFactory.from_app_config(
        config_path,
        hub=target_hub,
        bus=target_bus,
        agent_init_overrides=agent_init_overrides,
    )
    cards: dict[str, AgentCard] = {}

    for agent_id, agent in agents.items():
        if target_bus.discover(agent_id) is None:
            target_bus.register(agent)
        card = target_bus.get_a2a_card(agent_id)
        if card is None:
            msg = f"Failed to build agent card: {agent_id}"
            raise ValueError(msg)
        handlers = {skill.name: agent.run for skill in card.skills} or {"process": agent.run}
        server.register_agent(card, handlers)
        cards[agent_id] = card

    return AppAgentRuntime(
        config_path=config_path,
        bus=target_bus,
        hub=target_hub,
        server=server,
        agents=agents,
        cards=cards,
    )


__all__ = ["AppAgentRuntime", "bootstrap_app_agents"]
