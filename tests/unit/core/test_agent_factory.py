"""agent_factory のユニットテスト."""

from __future__ import annotations

import json
import sys
from pathlib import Path

from agentflow.core.agent_factory import (
    AgentFactorySpec,
    AgentSharedContext,
    AgentFactory,
    TieredMemory,
    create,
)
from agentflow.protocols.a2a_hub import LocalA2AHub


class InjectedAgent:
    """共通コンテキスト注入を受け取るテスト用 Agent."""

    def __init__(
        self,
        *,
        tool_gateway: object,
        mcp: dict[str, object],
        skills: dict[str, object],
        memory: TieredMemory,
        agent_type: str,
    ) -> None:
        self.tool_gateway = tool_gateway
        self.mcp = mcp
        self.skills = skills
        self.memory = memory
        self.agent_type = agent_type


class FallbackAgent:
    """agent_type 注入時は TypeError を返すテスト用 Agent."""

    def __init__(self, *, name: str, agent_type: str | None = None) -> None:
        if agent_type is not None:
            msg = "agent_type injection is not supported"
            raise TypeError(msg)
        self.name = name


def test_create_supports_constructor_injection() -> None:
    """create() は shared_context と agent_type を構築時に注入する."""
    context = AgentSharedContext(
        tool_gateway=object(),
        mcp={"filesystem": True},
        skills={"rag": {"enabled": True}},
        memory=TieredMemory(short_term={"k": "v"}),
    )
    instance = create(
        AgentFactorySpec(
            agent_class=InjectedAgent,
            shared_context=context,
            agent_type="planner",
        )
    )
    assert instance.tool_gateway is context.tool_gateway
    assert instance.mcp == {"filesystem": True}
    assert instance.skills == {"rag": {"enabled": True}}
    assert instance.memory is context.memory
    assert instance.agent_type == "planner"
    assert instance._agent_type == "planner"
    assert instance._shared_context is context


def test_create_falls_back_when_injected_kwargs_not_supported() -> None:
    """注入引数で失敗した場合は init_kwargs のみで再試行する."""
    instance = create(
        AgentFactorySpec(
            agent_class=FallbackAgent,
            init_kwargs={"name": "fallback-agent"},
            agent_type=None,
        )
    )
    assert instance.name == "fallback-agent"
    assert instance._agent_type == "specialist"
    assert hasattr(instance, "memory")


def test_from_app_config_uses_class_name_and_init_overrides(tmp_path: Path) -> None:
    """class_name と init override で正確に agent を生成できる."""
    package_dir = tmp_path / "sample_agents"
    package_dir.mkdir()
    (package_dir / "__init__.py").write_text("", encoding="utf-8")
    (package_dir / "configured_agent.py").write_text(
        "\n".join(
            [
                "from __future__ import annotations",
                "from typing import Any",
                "from pydantic import BaseModel",
                "from agentflow.core.resilient_agent import ResilientAgent",
                "",
                "class DemoInput(BaseModel):",
                "    question: str = 'demo'",
                "",
                "class DemoOutput(BaseModel):",
                "    region: str",
                "",
                "class ConfiguredAgent(ResilientAgent[DemoInput, DemoOutput]):",
                "    name = 'ConfiguredAgent'",
                "    def __init__(self, *, region: str, **kwargs: Any) -> None:",
                "        super().__init__(**kwargs)",
                "        self.region = region",
                "    async def process(self, input_data: DemoInput) -> DemoOutput:",
                "        return DemoOutput(region=self.region)",
                "    def _parse_input(self, input_data: dict[str, Any]) -> DemoInput:",
                "        return DemoInput.model_validate(input_data)",
            ]
        ),
        encoding="utf-8",
    )
    config_path = tmp_path / "app_config.json"
    config_path.write_text(
        json.dumps(
            {
                "agents": [
                    {
                        "name": "ConfiguredAgent",
                        "module": "sample_agents.configured_agent",
                        "class_name": "ConfiguredAgent",
                        "init_kwargs": {},
                    }
                ]
            }
        ),
        encoding="utf-8",
    )
    sys.path.insert(0, str(tmp_path))
    try:
        hub = LocalA2AHub()
        agents = AgentFactory.from_app_config(
            config_path,
            hub=hub,
            agent_init_overrides={"ConfiguredAgent": {"region": "jp"}},
        )
    finally:
        sys.path.remove(str(tmp_path))

    instance = agents["ConfiguredAgent"]
    assert instance.region == "jp"
    assert hub.discover("ConfiguredAgent") is not None
