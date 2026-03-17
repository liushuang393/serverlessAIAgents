"""app_config.json から executable agent runtime を構築する."""

from __future__ import annotations

import time
from collections.abc import AsyncIterator
from dataclasses import dataclass
from pathlib import Path
from typing import Any
from uuid import uuid4

from kernel.agents.agent_factory import AgentFactory
from kernel.protocols.a2a_card import AgentCard
from kernel.protocols.a2a_hub import LocalA2AHub
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


@dataclass(slots=True)
class AppAgentRuntime:
    """実行可能 agent runtime."""

    config_path: Path
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
        return self.cards.get(agent_id)

    async def invoke(self, agent_id: str, input_data: dict[str, Any]) -> dict[str, Any]:
        """agent を同期的に実行する."""
        agent = self.get_agent(agent_id)
        if agent is None:
            msg = f"Agent not found: {agent_id}"
            raise KeyError(msg)
        return await agent.run(input_data)

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
    hub: LocalA2AHub | None = None,
    agent_init_overrides: dict[str, dict[str, Any]] | None = None,
) -> AppAgentRuntime:
    """manifest から agent runtime を構築する."""
    target_hub = hub or LocalA2AHub()
    server = A2AServer()
    agents = AgentFactory.from_app_config(
        config_path,
        hub=target_hub,
        agent_init_overrides=agent_init_overrides,
    )
    cards: dict[str, AgentCard] = {}

    for agent_id, agent in agents.items():
        card = target_hub.discover(agent_id)
        if card is None:
            card = target_hub.register(agent, replace=True)
        server.register_agent(card, {"process": agent.run})
        cards[agent_id] = card

    return AppAgentRuntime(
        config_path=config_path,
        hub=target_hub,
        server=server,
        agents=agents,
        cards=cards,
    )


__all__ = ["AppAgentRuntime", "bootstrap_app_agents"]
