"""PipelineEngine が _initialize_agents で Agent を A2AHub に自動登録することを検証."""
from __future__ import annotations

from typing import Any

import pytest

from kernel.agents.resilient_agent import ResilientAgent
from kernel.engines.pipeline_engine import PipelineEngine
from kernel.protocols.a2a_hub import get_hub, reset_hub
from pydantic import BaseModel


class _StubInput(BaseModel):
    value: str = "test"


class _StubOutput(BaseModel):
    ok: bool = True


class _StubAgent(ResilientAgent[_StubInput, _StubOutput]):
    name = "StubAgent"
    enable_code_execution = False

    async def process(self, input_data: _StubInput) -> _StubOutput:
        return _StubOutput(ok=True)

    def _parse_input(self, input_data: dict[str, Any]) -> _StubInput:
        return _StubInput.model_validate(input_data)


class _StubAgent2(ResilientAgent[_StubInput, _StubOutput]):
    name = "StubAgent2"
    enable_code_execution = False

    async def process(self, input_data: _StubInput) -> _StubOutput:
        return _StubOutput(ok=True)

    def _parse_input(self, input_data: dict[str, Any]) -> _StubInput:
        return _StubInput.model_validate(input_data)


@pytest.fixture(autouse=True)
def _clean_hub():
    reset_hub()
    yield
    reset_hub()


class TestPipelineHubRegistration:
    async def test_agents_registered_after_init(self):
        engine = PipelineEngine(
            stages=[{"name": "stub", "agent": _StubAgent()}],
        )
        await engine._initialize()
        hub = get_hub()
        assert hub.discover("StubAgent") is not None

    async def test_multiple_agents_registered(self):
        engine = PipelineEngine(
            stages=[
                {"name": "s1", "agent": _StubAgent()},
                {"name": "s2", "agent": _StubAgent2()},
            ],
        )
        await engine._initialize()
        hub = get_hub()
        assert hub.discover("StubAgent") is not None
        assert hub.discover("StubAgent2") is not None
        assert hub.agent_count == 2

    async def test_duplicate_registration_skipped(self):
        agent = _StubAgent()
        hub = get_hub()
        hub.register(agent)
        engine = PipelineEngine(stages=[{"name": "stub", "agent": agent}])
        await engine._initialize()
        assert hub.agent_count == 1

    async def test_agents_list_in_stage(self):
        engine = PipelineEngine(
            stages=[{"name": "multi", "agents": [_StubAgent(), _StubAgent2()]}],
        )
        await engine._initialize()
        hub = get_hub()
        assert hub.discover("StubAgent") is not None
        assert hub.discover("StubAgent2") is not None
