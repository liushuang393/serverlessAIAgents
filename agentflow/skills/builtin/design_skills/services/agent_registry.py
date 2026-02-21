"""DesignAgentRegistry - Design Skills Engine用Agent管理サービス.

DecisionGovernanceEngineのAgentRegistryと同じパターンに従い、
YAML定義からAgentインスタンスを生成・管理する。

使用例:
    >>> from agentflow.skills.builtin.design_skills.services.agent_registry import (
    ...     DesignAgentRegistry,
    ... )
    >>>
    >>> registry = DesignAgentRegistry()
    >>> await registry.initialize()
    >>>
    >>> intent_agent = registry.get_agent("intent_analyzer")
    >>> all_agents = registry.get_ordered_agents()
"""

import importlib
import logging
from pathlib import Path
from typing import Any, ClassVar

from agentflow.core.flow_definition import (
    AgentDefinition,
    FlowDefinition,
    FlowDefinitionRegistry,
)
from agentflow.patterns.progress_emitter import AgentMeta


class DesignAgentRegistry:
    """Design Skills Engine用Agentレジストリ.

    YAML定義に基づいてAgentを生成・管理。
    前後端で共有可能な定義をFlowDefinitionRegistryに登録。

    Attributes:
        _flow_definition: FlowDefinition インスタンス
        _agents: Agent インスタンス辞書 (agent_id -> instance)
        _llm_client: 共有LLMクライアント
    """

    # YAMLファイルのデフォルトパス
    DEFAULT_YAML_PATH = Path(__file__).parent.parent / "agents" / "agent_definitions.yaml"

    # Agentクラスマッピング -- 遅延インポート用
    _AGENTS_PKG = "agentflow.skills.builtin.design_skills.agents"
    AGENT_CLASS_MAP: ClassVar[dict[str, str]] = {
        "IntentAnalyzerAgent": f"{_AGENTS_PKG}.intent_analyzer_agent",
        "PromptPlannerAgent": f"{_AGENTS_PKG}.prompt_planner_agent",
        "WorkflowExecutorAgent": f"{_AGENTS_PKG}.workflow_executor_agent",
    }

    def __init__(
        self,
        yaml_path: str | Path | None = None,
        llm_client: Any = None,
    ) -> None:
        """初期化.

        Args:
            yaml_path: YAML定義ファイルパス(省略時はデフォルト)
            llm_client: 共有LLMクライアント(省略時は各Agentで自動取得)
        """
        self._logger = logging.getLogger("design_skills.agent_registry")
        self._yaml_path = Path(yaml_path) if yaml_path else self.DEFAULT_YAML_PATH
        self._llm_client = llm_client
        self._agents: dict[str, Any] = {}
        self._flow_definition: FlowDefinition | None = None
        self._initialized = False

    async def initialize(self) -> None:
        """レジストリを初期化.

        YAML定義を読み込み、FlowDefinitionRegistryに登録。
        Agentインスタンスは遅延生成。
        """
        if self._initialized:
            return

        self._logger.info(f"Agent定義を読み込み中: {self._yaml_path}")

        self._flow_definition = FlowDefinition.from_yaml(self._yaml_path)
        registry = FlowDefinitionRegistry.get_instance()
        registry.register(self._flow_definition)

        self._initialized = True
        self._logger.info(
            f"フロー '{self._flow_definition.flow_id}' を登録完了 (Agent数: {len(self._flow_definition.agents)})"
        )

    def _ensure_initialized(self) -> None:
        """初期化済みであることを確認."""
        if not self._initialized or not self._flow_definition:
            msg = "DesignAgentRegistry未初期化。initialize()を先に呼び出してください。"
            raise RuntimeError(msg)

    def _get_flow_definition(self) -> FlowDefinition:
        """初期化済みの FlowDefinition を取得."""
        self._ensure_initialized()
        flow_definition = self._flow_definition
        if flow_definition is None:
            msg = "FlowDefinition が初期化されていません。"
            raise RuntimeError(msg)
        return flow_definition

    def _create_agent(self, agent_def: AgentDefinition) -> Any:
        """Agentインスタンスを生成.

        Args:
            agent_def: Agent定義

        Returns:
            Agentインスタンス
        """
        class_name = agent_def.class_name
        if not class_name:
            msg = f"Agent {agent_def.id} にclass_nameが定義されていません"
            raise ValueError(msg)

        module_path = self.AGENT_CLASS_MAP.get(class_name)
        if not module_path:
            msg = f"不明なAgentクラス: {class_name}"
            raise ValueError(msg)

        # 遅延インポート
        module = importlib.import_module(module_path)
        agent_class = getattr(module, class_name)
        agent = agent_class(llm_client=self._llm_client)
        self._logger.debug(f"Agent生成: {agent_def.id} ({class_name})")
        return agent

    def get_agent(self, agent_id: str) -> Any:
        """Agent IDからインスタンスを取得(遅延初期化).

        Args:
            agent_id: Agent ID

        Returns:
            Agentインスタンス

        Raises:
            ValueError: Agent IDが見つからない場合
        """
        self._ensure_initialized()
        flow_definition = self._get_flow_definition()

        if agent_id in self._agents:
            return self._agents[agent_id]

        agent_def = flow_definition.get_agent(agent_id)
        if not agent_def:
            msg = f"Agentが見つかりません: {agent_id}"
            raise ValueError(msg)

        agent = self._create_agent(agent_def)
        self._agents[agent_id] = agent
        return agent

    def get_all_agents(self) -> dict[str, Any]:
        """全Agentインスタンスを取得.

        Returns:
            Agent ID -> インスタンスの辞書
        """
        flow_definition = self._get_flow_definition()
        for agent_def in flow_definition.agents:
            if agent_def.id not in self._agents:
                self._agents[agent_def.id] = self._create_agent(agent_def)
        return self._agents.copy()

    def get_ordered_agents(self) -> list[Any]:
        """YAML定義順でAgentリストを取得.

        Returns:
            Agentインスタンスリスト(順序付き)
        """
        flow_definition = self._get_flow_definition()
        return [self.get_agent(a.id) for a in flow_definition.agents]

    def get_agent_ids(self) -> list[str]:
        """全Agent IDリストを取得(定義順)."""
        flow_definition = self._get_flow_definition()
        return flow_definition.get_agent_ids()

    def get_agent_metas(self) -> list[AgentMeta]:
        """ProgressEmitter用のAgentMetaリストを取得."""
        flow_definition = self._get_flow_definition()
        return [
            AgentMeta(
                id=agent_def.id,
                name=agent_def.name,
                label=agent_def.label,
                icon=agent_def.icon,
            )
            for agent_def in flow_definition.agents
        ]

    @property
    def total_agents(self) -> int:
        """Agent総数."""
        flow_definition = self._get_flow_definition()
        return len(flow_definition.agents)

    @property
    def flow_id(self) -> str:
        """Flow ID."""
        flow_definition = self._get_flow_definition()
        return flow_definition.flow_id


__all__ = ["DesignAgentRegistry"]
