"""LocalAgentBus / LocalA2AHub の互換テスト."""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel

from kernel.agents.agent_registry import AgentRegistry
from kernel.agents.contracts import AgentDescriptor, AgentFeedback, AgentInvocation
from kernel.agents.local_agent_bus import LocalAgentBus
from kernel.agents.resilient_agent import ResilientAgent
from kernel.protocols.a2a_hub import LocalA2AHub


class _EchoInput(BaseModel):
    value: str


class _EchoOutput(BaseModel):
    echo: str


class _EchoAgent(ResilientAgent[_EchoInput, _EchoOutput]):
    name = "EchoAgent"
    enable_code_execution = False

    async def process(self, input_data: _EchoInput) -> _EchoOutput:
        return _EchoOutput(echo=input_data.value)

    def _parse_input(self, input_data: dict[str, Any]) -> _EchoInput:
        return _EchoInput.model_validate(input_data)


class _FallbackAgent(ResilientAgent[_EchoInput, _EchoOutput]):
    name = "FallbackAgent"
    enable_code_execution = False

    async def process(self, input_data: _EchoInput) -> _EchoOutput:
        return _EchoOutput(echo=f"fallback:{input_data.value}")

    def _parse_input(self, input_data: dict[str, Any]) -> _EchoInput:
        return _EchoInput.model_validate(input_data)


async def test_local_agent_bus_invoke_records_feedback() -> None:
    registry = AgentRegistry()
    bus = LocalAgentBus(registry=registry)
    bus.register(_EchoAgent())

    result = await bus.invoke(AgentInvocation(agent_id="EchoAgent", input_data={"value": "hello"}))

    assert result.success is True
    assert result.output_data == {"echo": "hello"}
    summary = bus.get_feedback_summary("EchoAgent")
    assert summary is not None
    assert summary.total_runs == 1
    assert summary.success_count == 1


def test_local_agent_bus_route_prefers_feedback() -> None:
    registry = AgentRegistry()
    bus = LocalAgentBus(registry=registry)
    bus.register(
        _EchoAgent(),
        descriptor=AgentDescriptor(
            agent_id="EchoAgent",
            name="EchoAgent",
            description="primary",
            capabilities=["analysis"],
            metadata={"a2a_skills": ["echo"]},
        ),
    )
    bus.register(
        _FallbackAgent(),
        descriptor=AgentDescriptor(
            agent_id="FallbackAgent",
            name="FallbackAgent",
            description="fallback",
            capabilities=["analysis"],
            metadata={"a2a_skills": ["fallback"]},
        ),
    )
    registry.record_feedback("EchoAgent", AgentFeedback(success=True, duration_ms=5.0))
    registry.record_feedback("FallbackAgent", AgentFeedback(success=False, duration_ms=5.0))

    ranked = bus.route(required_capability="analysis")

    assert [candidate.agent_id for candidate in ranked[:2]] == ["EchoAgent", "FallbackAgent"]
    assert "feedback" in ranked[0].reasons


def test_local_a2a_hub_keeps_card_compatibility() -> None:
    hub = LocalA2AHub(bus=LocalAgentBus(registry=AgentRegistry()))

    card = hub.register(_EchoAgent())

    assert card.name == "EchoAgent"
    assert hub.discover("EchoAgent") is not None
    descriptor = hub.describe("EchoAgent")
    assert descriptor is not None
    assert descriptor.agent_id == "EchoAgent"
