"""agent_factory のユニットテスト."""

from __future__ import annotations

from agentflow.core.agent_factory import (
    AgentFactorySpec,
    AgentSharedContext,
    TieredMemory,
    create,
)


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
