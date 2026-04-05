"""AgentFlow Lite エントリポイント.

フル8層アーキテクチャを理解せずに素早く始めたいユーザー向けの簡易API。
OpenAI Swarm の ``client.run(agent, messages)`` のようなシンプルさを目指す。

Example::

    from kernel.quick import QuickAgent, run, quick_flow, as_agent

    class Greet(QuickAgent):
        name = "greeter"
        async def process(self, inputs: dict[str, Any]) -> dict[str, Any]:
            return {"message": f"Hello, {inputs['user']}!"}

    @as_agent(name="add_one")
    async def add_one(inputs: dict[str, Any]) -> dict[str, Any]:
        return {"result": inputs["x"] + 1}

    flow = quick_flow(Greet, add_one)
    result = asyncio.run(flow.run({"user": "Bob", "x": 1}))
"""

from __future__ import annotations

import uuid
from abc import ABC, abstractmethod
from collections.abc import Awaitable, Callable
from typing import TYPE_CHECKING, Any

from kernel.flow import Flow, create_flow


if TYPE_CHECKING:
    from kernel.flow.types import AgentProtocol


# 関数 Agent のコールバック型
type _AgentFn = Callable[[dict[str, Any]], Awaitable[dict[str, Any]]]


class QuickAgent(ABC):
    """簡易 Agent 基底クラス。process() を実装するだけで AgentProtocol 互換になる."""

    name: str = "QuickAgent"

    @abstractmethod
    async def process(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Agent のメインロジック."""
        ...

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """AgentProtocol.run — process() に委譲."""
        return await self.process(inputs)


class _FuncAgent:
    """関数を AgentProtocol 互換にラップする内部クラス."""

    __wrapped__: _AgentFn

    def __init__(self, func: _AgentFn, name: str) -> None:
        self.name = name
        self._func = func

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """AgentProtocol.run の実装."""
        return await self._func(inputs)

    async def process(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """QuickAgent 互換の process()."""
        return await self._func(inputs)


def as_agent(*, name: str) -> Callable[[_AgentFn], _FuncAgent]:
    """非同期関数を Agent に変換するデコレータ.

    Example::

        @as_agent(name="my_step")
        async def my_step(inputs: dict[str, Any]) -> dict[str, Any]:
            return {"result": inputs["x"] + 1}
    """

    def _decorator(func: _AgentFn) -> _FuncAgent:
        agent = _FuncAgent(func=func, name=name)
        agent.__doc__ = func.__doc__
        agent.__module__ = func.__module__
        agent.__wrapped__ = func
        return agent

    return _decorator


async def run(
    agent: QuickAgent | AgentProtocol,
    inputs: dict[str, Any],
) -> dict[str, Any]:
    """Agent を最小構成で実行する便利関数.

    FlowBuilder を使わず直接 Agent.run() を呼び出す。
    """
    return await agent.run(inputs)


def quick_flow(*agents: QuickAgent | AgentProtocol | type) -> Flow:
    """複数 Agent を線形に接続するフローを構築する.

    ``create_flow().then(...).build()`` のショートカット。

    Raises:
        ValueError: Agent が1つも指定されていない場合
    """
    if not agents:
        msg = "少なくとも1つの Agent を指定してください"
        raise ValueError(msg)

    flow_id = f"quick-{uuid.uuid4().hex[:8]}"
    builder = create_flow(flow_id, name="QuickFlow")
    builder.then(*agents)
    return builder.build()


__all__ = [
    "QuickAgent",
    "as_agent",
    "quick_flow",
    "run",
]
