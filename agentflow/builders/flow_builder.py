"""FlowBuilder - Flow構築の詳細実装.

目的: Flowの構築ロジックをカプセル化
職責:
- AgentCoordinatorの設定
- WorkflowConfigの生成
- パターン選択の検証
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, Literal

if TYPE_CHECKING:
    from agentflow.core.agent_block import AgentBlock


class FlowBuilder:
    """Flow構築のビルダークラス.

    Fluent APIでFlowを構築.

    Example:
        >>> builder = FlowBuilder()
        >>> flow = (builder
        ...     .add_agent(Agent1())
        ...     .add_agent(Agent2())
        ...     .set_pattern("sequential")
        ...     .enable_memory()
        ...     .build())
    """

    def __init__(self) -> None:
        """初期化."""
        self._agents: list[AgentBlock] = []
        self._pattern: Literal["sequential", "concurrent", "handoff"] = "sequential"
        self._enable_memory: bool = True
        self._enable_vector_search: bool = False
        self._name: str | None = None
        self._hooks: dict[str, Any] = {}
        self._logger = logging.getLogger(__name__)

    def add_agent(self, agent: AgentBlock) -> FlowBuilder:
        """Agentを追加.

        Args:
            agent: 追加するAgent

        Returns:
            self（メソッドチェーン用）
        """
        self._agents.append(agent)
        return self

    def add_agents(self, agents: list[AgentBlock]) -> FlowBuilder:
        """複数Agentを追加.

        Args:
            agents: 追加するAgentのリスト

        Returns:
            self（メソッドチェーン用）
        """
        self._agents.extend(agents)
        return self

    def set_pattern(
        self, pattern: Literal["sequential", "concurrent", "handoff"]
    ) -> FlowBuilder:
        """協調パターンを設定.

        Args:
            pattern: 協調パターン

        Returns:
            self（メソッドチェーン用）
        """
        self._pattern = pattern
        return self

    def enable_memory(self, vector_search: bool = False) -> FlowBuilder:
        """記憶システムを有効化.

        Args:
            vector_search: ベクトル検索も有効化

        Returns:
            self（メソッドチェーン用）
        """
        self._enable_memory = True
        self._enable_vector_search = vector_search
        return self

    def disable_memory(self) -> FlowBuilder:
        """記憶システムを無効化.

        Returns:
            self（メソッドチェーン用）
        """
        self._enable_memory = False
        return self

    def set_name(self, name: str) -> FlowBuilder:
        """Flow名を設定.

        Args:
            name: Flow名

        Returns:
            self（メソッドチェーン用）
        """
        self._name = name
        return self

    def on_node_start(self, callback: Any) -> FlowBuilder:
        """ノード開始フックを設定.

        Args:
            callback: コールバック関数

        Returns:
            self（メソッドチェーン用）
        """
        self._hooks["on_node_start"] = callback
        return self

    def on_node_complete(self, callback: Any) -> FlowBuilder:
        """ノード完了フックを設定.

        Args:
            callback: コールバック関数

        Returns:
            self（メソッドチェーン用）
        """
        self._hooks["on_node_complete"] = callback
        return self

    def build(self) -> Any:
        """Flowを構築.

        Returns:
            FlowWrapperインスタンス

        Raises:
            ValueError: Agentが設定されていない場合
        """
        if not self._agents:
            msg = "少なくとも1つのAgentが必要です"
            raise ValueError(msg)

        from agentflow.quick import FlowWrapper
        from agentflow.patterns.multi_agent import AgentCoordinator, SharedContext

        context = SharedContext(
            enable_memory=self._enable_memory,
            enable_vector_search=self._enable_vector_search,
        )

        coordinator = AgentCoordinator(
            agents=self._agents,
            pattern=self._pattern,
            shared_context=context,
        )

        name = self._name or f"flow-{len(self._agents)}agents-{self._pattern}"

        self._logger.info(f"Flow構築完了: {name}, agents={len(self._agents)}")

        return FlowWrapper(
            coordinator=coordinator,
            context=context,
            name=name,
        )

