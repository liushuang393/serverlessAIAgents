"""Local agent bus.

内部編排の canonical entrypoint。
protocol 名に依存せず、registry 上の descriptor / instance を用いて呼び出す。
"""

from __future__ import annotations

import logging
import time
from typing import Any

from kernel.agents.agent_registry import get_global_agent_registry
from kernel.agents.contracts import (
    AgentDescriptor,
    AgentFeedback,
    AgentInvocation,
    AgentResult,
    descriptor_from_agent_instance,
)
from kernel.agents.routing import (
    AgentRouteCandidate,
    AgentRoutingPolicy,
    rank_agent_candidates,
)


class AgentNotFoundError(Exception):
    """指定された Agent が LocalAgentBus に存在しない."""

    def __init__(self, agent_name: str) -> None:
        self.agent_name = agent_name
        super().__init__(f"Agent not found in bus: {agent_name}")


class LocalAgentBus:
    """ローカルプロセス内 agent bus."""

    def __init__(self, registry: Any | None = None) -> None:
        """初期化."""
        self._registry = registry if registry is not None else get_global_agent_registry()
        self._logger = logging.getLogger("kernel.agent_bus")

    def register(
        self,
        agent_instance: Any,
        *,
        descriptor: AgentDescriptor | None = None,
        replace: bool = False,
        card: Any | None = None,
    ) -> AgentDescriptor:
        """Agent を registry/bus に登録."""
        resolved_name = getattr(agent_instance, "name", None) or type(agent_instance).__name__
        runtime_descriptor = descriptor or descriptor_from_agent_instance(
            agent_instance,
            agent_id=resolved_name,
            metadata={"local": True},
        )
        entry = self._registry.register_runtime(
            agent_id=runtime_descriptor.agent_id,
            descriptor=runtime_descriptor,
            instance=agent_instance,
            replace=replace,
            a2a_card=card,
        )
        self._logger.info("Agent registered on LocalAgentBus: %s", runtime_descriptor.agent_id)
        return entry.descriptor or runtime_descriptor

    def unregister(self, agent_name: str) -> bool:
        """Agent を解除."""
        removed = self._registry.unregister(agent_name)
        if removed:
            self._logger.info("Agent unregistered from LocalAgentBus: %s", agent_name)
        return removed

    async def call(self, agent_name: str, input_data: dict[str, Any]) -> dict[str, Any]:
        """agent を呼び出して dict を返す."""
        invocation = AgentInvocation(agent_id=agent_name, input_data=input_data)
        result = await self.invoke(invocation)
        if not result.success:
            msg = result.error or f"Agent execution failed: {agent_name}"
            raise RuntimeError(msg)
        return result.output_data

    async def invoke(self, invocation: AgentInvocation) -> AgentResult:
        """内部 canonical invocation API."""
        agent_name = invocation.agent_id
        agent = self._registry.get_instance(agent_name)
        if agent is None:
            raise AgentNotFoundError(agent_name)

        started_at = time.perf_counter()
        try:
            if hasattr(agent, "run"):
                raw_result = await agent.run(invocation.input_data)
            elif hasattr(agent, "invoke"):
                raw_result = await agent.invoke(invocation.input_data)
            elif hasattr(agent, "process"):
                raw_result = await agent.process(invocation.input_data)
            else:
                msg = f"Agent {agent_name} has no run/invoke/process method"
                raise AttributeError(msg)
        except Exception as exc:
            duration_ms = (time.perf_counter() - started_at) * 1000
            self._registry.record_feedback(
                agent_name,
                AgentFeedback(success=False, duration_ms=duration_ms),
            )
            return AgentResult(
                agent_id=agent_name,
                success=False,
                error=str(exc),
                duration_ms=duration_ms,
            )

        duration_ms = (time.perf_counter() - started_at) * 1000
        normalized = self._normalize_output(raw_result)
        self._registry.record_feedback(
            agent_name,
            AgentFeedback(success=True, duration_ms=duration_ms),
        )
        return AgentResult(
            agent_id=agent_name,
            success=True,
            output_data=normalized,
            duration_ms=duration_ms,
        )

    def discover(self, agent_name: str) -> AgentDescriptor | None:
        """descriptor を取得."""
        return self._registry.get_descriptor(agent_name)

    def get_agent(self, agent_name: str) -> Any | None:
        """agent instance を取得."""
        return self._registry.get_instance(agent_name)

    def get_a2a_card(self, agent_name: str) -> Any | None:
        """A2A card を取得."""
        return self._registry.get_a2a_card(agent_name)

    def get_feedback_summary(self, agent_name: str) -> Any | None:
        """feedback 集計を取得."""
        return self._registry.get_feedback_summary(agent_name)

    def list_descriptors(self) -> list[AgentDescriptor]:
        """descriptor 一覧."""
        return self._registry.list_descriptors()

    def list_cards(self) -> list[Any]:
        """A2A cards 一覧."""
        return self._registry.list_a2a_cards()

    def route(
        self,
        *,
        required_capability: str | None = None,
        policy: AgentRoutingPolicy | None = None,
        explicit_agent_id: str | None = None,
    ) -> list[AgentRouteCandidate]:
        """routing 候補を返す."""
        feedback_by_agent = {}
        for descriptor in self._registry.list_descriptors():
            summary = self._registry.get_feedback_summary(descriptor.agent_id)
            if summary is not None:
                feedback_by_agent[descriptor.agent_id] = summary
        return rank_agent_candidates(
            descriptors=self._registry.list_descriptors(),
            feedback_by_agent=feedback_by_agent,
            required_capability=required_capability,
            policy=policy,
            explicit_agent_id=explicit_agent_id,
        )

    @property
    def agent_count(self) -> int:
        """登録済み agent 数."""
        return len(self._registry.list_runtime_agent_ids())

    def clear(self) -> None:
        """全 runtime agent をクリア."""
        self._registry.clear()

    @staticmethod
    def _normalize_output(raw_result: Any) -> dict[str, Any]:
        """結果を dict に正規化."""
        if isinstance(raw_result, dict):
            return raw_result
        if hasattr(raw_result, "model_dump"):
            return raw_result.model_dump()
        return {"result": raw_result}


_global_agent_bus: LocalAgentBus | None = None


def get_agent_bus() -> LocalAgentBus:
    """グローバル LocalAgentBus を取得."""
    global _global_agent_bus
    if _global_agent_bus is None:
        _global_agent_bus = LocalAgentBus()
    return _global_agent_bus


def reset_agent_bus() -> None:
    """グローバル LocalAgentBus をリセット."""
    global _global_agent_bus
    if _global_agent_bus is not None:
        _global_agent_bus.clear()
    _global_agent_bus = None


__all__ = [
    "AgentNotFoundError",
    "LocalAgentBus",
    "get_agent_bus",
    "reset_agent_bus",
]
