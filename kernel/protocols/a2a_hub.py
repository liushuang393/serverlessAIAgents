"""LocalA2AHub - ローカルプロセス内 A2A 通信ハブ.

全 Agent はここに登録し、Agent 間通信は全てこのハブを経由する。
ローカル呼び出しはインメモリ直接ルーティング（HTTP 不要）。
未登録 Agent へのフォールバックは A2AClient 経由でリモート呼び出し。

使用例:
    >>> from kernel.protocols.a2a_hub import LocalA2AHub, get_hub
    >>>
    >>> hub = get_hub()
    >>> hub.register(my_agent)
    >>> result = await hub.call("MyAgent", {"question": "..."})
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from kernel.protocols.a2a_card import AgentCapabilities, AgentCard, AgentSkill


if TYPE_CHECKING:
    from kernel.agents.resilient_agent import ResilientAgent


class AgentNotFoundError(Exception):
    """指定された Agent が LocalA2AHub に登録されていない."""

    def __init__(self, agent_name: str) -> None:
        self.agent_name = agent_name
        super().__init__(f"Agent not found in hub: {agent_name}")


class LocalA2AHub:
    """ローカルプロセス内の A2A 通信ハブ.

    責務:
    - Agent インスタンスの登録・管理
    - AgentCard の自動生成・公開
    - インメモリルーティングによる Agent 呼び出し
    - リモートフォールバック（A2AClient 経由）

    Attributes:
        _agents: Agent名 → Agentインスタンスのマッピング
        _cards: Agent名 → AgentCard のマッピング
    """

    def __init__(self) -> None:
        """ハブを初期化."""
        self._agents: dict[str, ResilientAgent[Any, Any]] = {}
        self._cards: dict[str, AgentCard] = {}
        self._logger = logging.getLogger("kernel.a2a_hub")

    def register(
        self,
        agent_instance: Any,
        *,
        card: AgentCard | None = None,
        replace: bool = False,
    ) -> AgentCard:
        """Agent をハブに登録.

        ResilientAgent 以外のインスタンス（@agent デコレータ生成等）も
        name 属性と run() メソッドがあれば登録可能。

        Args:
            agent_instance: Agent インスタンス（name/run 属性必須）
            card: 明示的な AgentCard（None の場合は自動生成）
            replace: True の場合、既存の同名 Agent を上書き

        Returns:
            登録された AgentCard

        Raises:
            ValueError: 同名 Agent が既に登録されていて replace=False の場合
        """
        name = getattr(agent_instance, "name", None) or type(agent_instance).__name__
        if name in self._agents and not replace:
            msg = f"Agent already registered: {name}"
            raise ValueError(msg)

        if card is None:
            card = self._build_card(agent_instance)

        self._agents[name] = agent_instance
        self._cards[name] = card
        self._logger.info(f"Agent registered: {name}")
        return card

    def unregister(self, agent_name: str) -> None:
        """Agent をハブから登録解除.

        Args:
            agent_name: 解除する Agent 名
        """
        self._agents.pop(agent_name, None)
        self._cards.pop(agent_name, None)
        self._logger.info(f"Agent unregistered: {agent_name}")

    async def call(
        self,
        agent_name: str,
        input_data: dict[str, Any],
    ) -> dict[str, Any]:
        """Agent を A2A プロトコル経由で呼び出す.

        ローカル登録済み → インメモリ直接呼び出し
        未登録 → AgentNotFoundError（リモートは A2AClient を直接使用）

        Args:
            agent_name: 呼び出し先 Agent 名
            input_data: 入力データ（dict 形式）

        Returns:
            Agent の出力データ

        Raises:
            AgentNotFoundError: Agent が登録されていない場合
        """
        agent = self._agents.get(agent_name)
        if agent is None:
            raise AgentNotFoundError(agent_name)

        self._logger.debug(f"A2A call: -> {agent_name}")
        result: dict[str, Any] = await agent.run(input_data)
        self._logger.debug(f"A2A call: <- {agent_name} (ok)")
        return result

    def discover(self, agent_name: str) -> AgentCard | None:
        """Agent の AgentCard を取得.

        Args:
            agent_name: Agent 名

        Returns:
            AgentCard、未登録の場合は None
        """
        return self._cards.get(agent_name)

    def list_agents(self) -> list[AgentCard]:
        """登録済み全 Agent の AgentCard を返す.

        Returns:
            AgentCard のリスト
        """
        return list(self._cards.values())

    def get_agent(self, agent_name: str) -> ResilientAgent[Any, Any] | None:
        """Agent インスタンスを直接取得（テスト・デバッグ用）.

        Args:
            agent_name: Agent 名

        Returns:
            ResilientAgent インスタンス、未登録の場合は None
        """
        return self._agents.get(agent_name)

    @property
    def agent_count(self) -> int:
        """登録済み Agent 数を返す."""
        return len(self._agents)

    def clear(self) -> None:
        """全 Agent を登録解除（テスト用）."""
        self._agents.clear()
        self._cards.clear()
        self._logger.info("Hub cleared: all agents unregistered")

    def _build_card(self, agent_instance: Any) -> AgentCard:
        """Agent インスタンスから AgentCard を自動生成.

        ResilientAgent / AgentBlock / 任意の run() 対応オブジェクトに対応。
        型アノテーションがあれば Input/Output の JSON Schema を抽出する。

        Args:
            agent_instance: Agent インスタンス

        Returns:
            自動生成された AgentCard
        """
        agent_name = getattr(agent_instance, "name", None) or type(agent_instance).__name__

        # Input/Output の JSON Schema を抽出
        input_schema: dict[str, Any] = {}
        output_schema: dict[str, Any] = {}

        orig_bases = getattr(type(agent_instance), "__orig_bases__", ())
        for base in orig_bases:
            args = getattr(base, "__args__", None)
            if args and len(args) >= 2:
                input_type, output_type = args[0], args[1]
                if hasattr(input_type, "model_json_schema"):
                    input_schema = input_type.model_json_schema()
                if hasattr(output_type, "model_json_schema"):
                    output_schema = output_type.model_json_schema()
                break

        # デフォルトスキル（= process メソッドに対応）
        skill = AgentSkill(
            name="process",
            description=f"{agent_name} のメイン処理",
            input_schema=input_schema,
            output_schema=output_schema,
        )

        # capabilities を自動設定（ストリーミング対応は run_stream の有無で判定）
        has_streaming = hasattr(agent_instance, "run_stream") and callable(getattr(agent_instance, "run_stream", None))
        capabilities = AgentCapabilities(
            streaming=has_streaming,
            push_notifications=False,
            state_transition_history=True,
        )

        return AgentCard(
            name=agent_name,
            description=getattr(agent_instance, "__doc__", "") or f"Agent: {agent_name}",
            version="1.0.0",
            skills=[skill],
            capabilities=capabilities,
            url=f"local://{agent_name}",
            metadata={
                "timeout_seconds": getattr(agent_instance, "timeout_seconds", None),
                "max_retries": getattr(agent_instance, "max_retries", None),
                "local": True,
            },
        )


# ===========================================================================
# シングルトン — グローバル LocalA2AHub
# ===========================================================================
_global_hub: LocalA2AHub | None = None


def get_hub() -> LocalA2AHub:
    """グローバル LocalA2AHub インスタンスを取得.

    Returns:
        LocalA2AHub インスタンス（初回呼び出し時に自動生成）
    """
    global _global_hub
    if _global_hub is None:
        _global_hub = LocalA2AHub()
    return _global_hub


def reset_hub() -> None:
    """グローバル LocalA2AHub をリセット（テスト用）."""
    global _global_hub
    if _global_hub is not None:
        _global_hub.clear()
    _global_hub = None
