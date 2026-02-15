"""Flowチェーンビルダー.

流暢なチェーンAPIを提供し、複雑なフローを構築。

Example:
    >>> flow = (
    ...     FlowBuilder("my-flow")
    ...     .gate(CognitiveGate, check=lambda r: r["proceed"])
    ...     .gate(Gatekeeper, check=lambda r: r["is_acceptable"])
    ...     .then(Clarification)
    ...     .then(Dao, Fa, Shu, Qi)
    ...     .review(Review, retry_from="dao", max_revisions=2)
    ...     .build()
    ... )
    >>> result = await flow.run({"question": "..."})

設計原則:
- チェーン呼び出し：各メソッドはselfを返す
- 型安全：Agentインスタンスまたはクラスをサポート
- 意味が明確：gate/then/parallel/review
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from agentflow.flow.graph import FlowGraph
from agentflow.flow.nodes import AgentNode, GateNode, ParallelNode, ReviewNode
from agentflow.flow.types import AgentProtocol, FlowConfig


if TYPE_CHECKING:
    from collections.abc import Callable

    from agentflow.flow.context import FlowContext
    from agentflow.flow.flow import Flow


class FlowBuilder:
    """フロービルダー.

    チェーンAPIを使用してフローグラフを構築。

    Attributes:
        flow_id: フローID
        name: フロー名
    """

    def __init__(self, flow_id: str, *, name: str | None = None) -> None:
        """初期化.

        Args:
            flow_id: フロー一意ID
            name: フロー表示名（デフォルトはIDと同じ）
        """
        self._logger = logging.getLogger("agentflow.flow.builder")
        self.flow_id = flow_id
        self.name = name or flow_id

        self._graph = FlowGraph()
        self._config = FlowConfig()
        self._node_counter = 0

    def _next_id(self, prefix: str) -> str:
        """ノードIDを生成."""
        self._node_counter += 1
        return f"{prefix}_{self._node_counter}"

    def _resolve_agent(self, agent: AgentProtocol | type) -> AgentProtocol:
        """Agentを解決：クラスの場合はインスタンス化."""
        if isinstance(agent, type):
            return agent()
        return agent

    def gate(
        self,
        agent: AgentProtocol | type,
        *,
        id: str | None = None,
        name: str | None = None,
        input_mapper: Callable[[FlowContext], dict[str, Any]] | None = None,
        check: Callable[[dict[str, Any]], bool] | None = None,
        on_fail: Callable[[FlowContext], dict[str, Any] | Any] | None = None,
    ) -> FlowBuilder:
        """ゲートノードを追加.

        ゲートは条件チェックに使用され、通過しない場合は早期リターン。

        Args:
            agent: ゲートAgent
            id: ノードID（オプション）
            name: ノード名（オプション）
            input_mapper: 入力マッピング関数（前ステージの結果を渡す）
            check: 条件チェック関数、Trueを返すと通過
            on_fail: 失敗時の処理関数

        Returns:
            self（チェーンメソッド呼び出しをサポート）

        Example:
            >>> .gate(
            ...     GatekeeperAgent,
            ...     check=lambda r: r["is_acceptable"],
            ...     on_fail=lambda ctx: {"status": "rejected"}
            ... )
        """
        resolved = self._resolve_agent(agent)
        node_id = id or self._next_id("gate")
        node_name = name or getattr(resolved, "name", node_id)

        node = GateNode(
            id=node_id,
            name=node_name,
            agent=resolved,
            input_mapper=input_mapper,
            check=check,
            on_fail=on_fail,
        )
        self._graph.add_node(node)
        self._logger.debug(f"ゲートを追加: {node_id}")
        return self

    def then(
        self,
        *agents: AgentProtocol | type,
        ids: list[str] | None = None,
        names: list[str] | None = None,
        input_mappers: dict[str, Callable[[FlowContext], dict[str, Any]]] | None = None,
    ) -> FlowBuilder:
        """順次実行するAgentノードを追加.

        Args:
            *agents: 1つ以上のAgent
            ids: ノードIDリスト（オプション）
            names: ノード名リスト（オプション）
            input_mappers: 入力マッピング関数辞書（オプション）

        Returns:
            self

        Example:
            >>> .then(ClarificationAgent)
            >>> .then(DaoAgent, FaAgent, ShuAgent, QiAgent)
        """
        for i, agent in enumerate(agents):
            resolved = self._resolve_agent(agent)
            node_id = ids[i] if ids and i < len(ids) else self._next_id("agent")
            node_name = names[i] if names and i < len(names) else getattr(resolved, "name", node_id)
            mapper = input_mappers.get(node_id) if input_mappers else None

            node = AgentNode(
                id=node_id,
                name=node_name,
                agent=resolved,
                input_mapper=mapper,
            )
            self._graph.add_node(node)
            self._logger.debug(f"Agentを追加: {node_id}")

        return self

    def parallel(
        self,
        *agents: tuple[str, AgentProtocol | type],
        id: str | None = None,
        name: str | None = None,
        input_mappers: dict[str, Callable[[FlowContext], dict[str, Any]]] | None = None,
    ) -> FlowBuilder:
        """並列実行ノードを追加.

        Args:
            *agents: (id, agent)タプルリスト
            id: 並列ノードグループID
            name: 並列ノードグループ名
            input_mappers: 各Agentの入力マッピング関数

        Returns:
            self

        Example:
            >>> .parallel(
            ...     ("dao", DaoAgent),
            ...     ("fa", FaAgent),
            ... )
        """
        node_id = id or self._next_id("parallel")
        resolved_agents = [(aid, self._resolve_agent(a)) for aid, a in agents]

        node = ParallelNode(
            id=node_id,
            name=name or f"並列実行({len(agents)}個)",
            agents=resolved_agents,
            input_mappers=input_mappers or {},
        )
        self._graph.add_node(node)
        self._logger.debug(f"並列ノードを追加: {node_id}")
        return self

    def review(
        self,
        agent: AgentProtocol | type,
        *,
        id: str | None = None,
        name: str | None = None,
        retry_from: str | None = None,
        max_revisions: int = 2,
        input_mapper: Callable[[FlowContext], dict[str, Any]] | None = None,
        on_pass: Callable[[FlowContext], dict[str, Any]] | None = None,
        on_coach: Callable[[FlowContext], dict[str, Any]] | None = None,
        verdict_key: str = "overall_verdict",
    ) -> FlowBuilder:
        """レビューノードを追加.

        レビューノードは判定結果に基づいて決定:
        - PASS: フローを終了し、成功を返す
        - REVISE: 指定ノードにロールバックして再実行
        - COACH: コーチング型改善指導（即終了せずレポートに指摘を表示）

        Args:
            agent: レビューAgent
            id: ノードID
            name: ノード名
            retry_from: REVISE時にロールバックするノードID
            max_revisions: 最大リビジョン回数
            input_mapper: 入力マッピング関数
            on_pass: PASS時の処理関数
            on_coach: COACH時の処理関数
            verdict_key: 判定結果フィールド名

        Returns:
            self

        Example:
            >>> .review(
            ...     ReviewAgent,
            ...     retry_from="dao",
            ...     max_revisions=2,
            ...     on_pass=lambda ctx: generate_report(ctx)
            ... )
        """
        resolved = self._resolve_agent(agent)
        node_id = id or self._next_id("review")
        node_name = name or getattr(resolved, "name", "レビュー")

        node = ReviewNode(
            id=node_id,
            name=node_name,
            agent=resolved,
            input_mapper=input_mapper,
            on_pass=on_pass,
            on_coach=on_coach,
            retry_from=retry_from,
            max_revisions=max_revisions,
            verdict_key=verdict_key,
        )
        self._graph.add_node(node)
        self._logger.debug(f"レビューノードを追加: {node_id}, ロールバック先: {retry_from}")
        return self

    def with_config(
        self,
        *,
        enable_progress: bool | None = None,
        enable_memory: bool | None = None,
        max_revisions: int | None = None,
        auto_initialize: bool | None = None,
    ) -> FlowBuilder:
        """Flowを設定.

        Args:
            enable_progress: 進捗追跡を有効化
            enable_memory: メモリシステムを有効化
            max_revisions: 最大リビジョン回数
            auto_initialize: Agentを自動初期化

        Returns:
            self
        """
        self._config = FlowConfig(
            enable_progress=enable_progress if enable_progress is not None else self._config.enable_progress,
            enable_memory=enable_memory if enable_memory is not None else self._config.enable_memory,
            max_revisions=max_revisions if max_revisions is not None else self._config.max_revisions,
            auto_initialize=auto_initialize if auto_initialize is not None else self._config.auto_initialize,
        )
        return self

    def build(self) -> Flow:
        """Flowインスタンスを構築.

        Returns:
            実行可能なFlowインスタンス

        Raises:
            ValueError: ノードが追加されていない場合
        """
        from agentflow.flow.flow import Flow

        if self._graph.node_count == 0:
            msg = "フローにノードがありません。少なくとも1つのAgentを追加してください"
            raise ValueError(msg)

        self._logger.info(f"Flowを構築: {self.flow_id}, ノード数: {self._graph.node_count}")

        return Flow(
            flow_id=self.flow_id,
            name=self.name,
            graph=self._graph,
            config=self._config,
        )


def create_flow(flow_id: str, *, name: str | None = None) -> FlowBuilder:
    """フロービルダーを作成.

    これはFlowを構築する推奨エントリーポイント。

    Args:
        flow_id: フロー一意ID
        name: フロー表示名

    Returns:
        FlowBuilderインスタンス

    Example:
        >>> flow = (
        ...     create_flow("decision-engine")
        ...     .gate(GatekeeperAgent)
        ...     .then(DaoAgent, FaAgent)
        ...     .review(ReviewAgent, retry_from="dao")
        ...     .build()
        ... )
    """
    return FlowBuilder(flow_id, name=name)


__all__ = ["FlowBuilder", "create_flow"]

