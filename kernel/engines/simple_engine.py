"""SimpleEngine - 単一Agent質問応答パターン.

最もシンプルなEngine Pattern、以下に適用：
- 単一ターン質問応答
- カスタマーサービスBot
- シンプルなAIアシスタント

使用例:
    >>> from kernel.engines import SimpleEngine
    >>> from kernel import agent
    >>>
    >>> @agent
    ... class QAAgent:
    ...     system_prompt = "あなたはアシスタントです"
    ...     async def process(self, inputs: dict) -> dict:
    ...         return {"answer": f"回答: {inputs['question']}"}
    >>>
    >>> engine = SimpleEngine(agent=QAAgent)
    >>> result = await engine.run({"question": "こんにちは"})
    >>> print(result)  # {"answer": "回答: こんにちは"}
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, cast

from kernel.engines.base import BaseEngine, EngineConfig


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


class SimpleEngine(BaseEngine):
    """単一Agent質問応答エンジン.

    特徴：
    - 最もシンプルなパターン
    - 単一Agentでリクエストを処理
    - Agent出力を直接返却

    Attributes:
        agent: Agentクラスまたはインスタンス
        skills: バインドするSkillsリスト
        tools: バインドするToolsリスト
    """

    def __init__(
        self,
        agent: type | Any,
        *,
        skills: list[str] | None = None,
        tools: list[str] | None = None,
        config: EngineConfig | None = None,
    ) -> None:
        """SimpleEngineを初期化.

        Args:
            agent: Agentクラスまたはインスタンス
            skills: Skills名リスト
            tools: Tools名リスト
            config: Engine設定
        """
        super().__init__(config)
        self._agent_cls = agent
        self._agent_instance: Any = None
        self._skills = skills or []
        self._tools = tools or []
        self._logger = logging.getLogger("kernel.engines.simple")

    async def _initialize(self) -> None:
        """Agentインスタンスを初期化し、ツールをバインド."""
        if isinstance(self._agent_cls, type):
            # クラスの場合、インスタンス化が必要
            self._agent_instance = self._agent_cls()
        else:
            # 既にインスタンスの場合
            self._agent_instance = self._agent_cls

        # Agentにinitializeメソッドがある場合、呼び出す
        if hasattr(self._agent_instance, "initialize"):
            await self._agent_instance.initialize()

        # ツールバインディング（skills/tools が指定されている場合）
        await self._bind_tools()

        self._logger.info(f"SimpleEngine initialized with {self._agent_instance}")

    async def _bind_tools(self) -> None:
        """ツールをAgentにバインド.

        skills と tools をツールURIに変換し、ToolBinder でバインド。
        """
        # ツールURIを収集
        tool_uris: list[str] = list(self._tools)

        # スキルをツールURIに変換
        for skill_name in self._skills:
            tool_uris.append(f"tool://skill/{skill_name}")

        if not tool_uris:
            return

        try:
            from kernel.agents.capability_spec import AgentCapabilitySpec
            from kernel.tools.tool_binding import ToolBinder
            from kernel.tools.tool_registry import get_global_tool_registry

            tool_registry = get_global_tool_registry()
            binder = ToolBinder(tool_registry)

            # AgentCapabilitySpec を作成
            agent_name = getattr(self._agent_cls, "__name__", self._agent_instance.__class__.__name__)
            capability = AgentCapabilitySpec(
                id=f"{agent_name}_runtime",
                name=agent_name,
                description="Runtime agent",
                required_tools=tool_uris,
            )

            # ツールをバインド
            await binder.bind_for_capability(self._agent_instance, capability)
            self._logger.debug(f"ツールバインド完了: {len(tool_uris)} ツール")

        except Exception as e:
            self._logger.warning(f"ツールバインドエラー: {e}")

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Agentを実行.

        Args:
            inputs: 入力データ

        Returns:
            Agent出力結果
        """
        # Agent を A2AHub 経由で統一呼び出し
        return await self.call_agent(self._agent_instance, inputs)

    async def _execute_stream(self, inputs: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """ストリーム実行（ノードイベントを発行）."""
        agent_name = getattr(self._agent_instance, "name", self._agent_instance.__class__.__name__)

        # ノード開始
        event = self._emit_node_start(agent_name)
        if event:
            yield event

        # 実行
        result = await self._execute(inputs)

        # ノード完了
        event = self._emit_node_complete(agent_name, result)
        if event:
            yield event

        # 結果
        yield {"type": "result", "data": result}
