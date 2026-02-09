"""AgentRegistry - Agent管理サービス.

YAML定義からAgentインスタンスを生成・管理する。
workflow.pyのAgent初期化ロジックを集約。

設計原則:
- 単一データ源: agent_definitions.yaml から全てを読み込み
- 遅延初期化: 必要時にのみAgentをインスタンス化
- 前後端共有: FlowDefinitionRegistry と連携

使用例:
    >>> from apps.decision_governance_engine.services.agent_registry import AgentRegistry
    >>>
    >>> registry = AgentRegistry()
    >>> registry.initialize()  # YAML読み込み + Agent生成
    >>>
    >>> # Agentインスタンス取得
    >>> dao_agent = registry.get_agent("dao")
    >>> all_agents = registry.get_all_agents()
    >>>
    >>> # フロントエンド用定義取得
    >>> definitions = registry.get_frontend_definitions()
"""

import logging
from pathlib import Path
from typing import Any

from agentflow.core.flow_definition import (
    AgentDefinition,
    FlowDefinition,
    FlowDefinitionRegistry,
)
from agentflow.patterns.progress_emitter import AgentMeta


class AgentRegistry:
    """Agent管理レジストリ.

    YAML定義に基づいてAgentを生成・管理。
    前後端で共有可能な定義をFlowDefinitionRegistryに登録。

    Attributes:
        flow_definition: FlowDefinition インスタンス
        _agents: Agent インスタンス辞書 (agent_id -> instance)
        _llm_client: 共有LLMクライアント
        _enable_rag: RAG有効化フラグ

    使用例:
        >>> registry = AgentRegistry()
        >>> await registry.initialize()
        >>>
        >>> # Agent取得
        >>> dao = registry.get_agent("dao")
        >>>
        >>> # 全Agentリスト（順序付き）
        >>> agents = registry.get_ordered_agents()
        >>>
        >>> # フロントエンド用定義
        >>> defs = registry.get_frontend_definitions()
    """

    # YAMLファイルのデフォルトパス
    DEFAULT_YAML_PATH = Path(__file__).parent.parent / "agents" / "agent_definitions.yaml"

    # Agent クラスマッピング（遅延インポート用）
    AGENT_CLASS_MAP: dict[str, str] = {
        "CognitiveGateAgent": "apps.decision_governance_engine.agents.cognitive_gate_agent",
        "GatekeeperAgent": "apps.decision_governance_engine.agents.gatekeeper_agent",
        "ClarificationAgent": "apps.decision_governance_engine.agents.clarification_agent",
        "DaoAgent": "apps.decision_governance_engine.agents.dao_agent",
        "FaAgent": "apps.decision_governance_engine.agents.fa_agent",
        "ShuAgent": "apps.decision_governance_engine.agents.shu_agent",
        "QiAgent": "apps.decision_governance_engine.agents.qi_agent",
        "ReviewAgent": "apps.decision_governance_engine.agents.review_agent",
    }

    def __init__(
        self,
        yaml_path: str | Path | None = None,
        llm_client: Any = None,
        enable_rag: bool = True,
    ) -> None:
        """初期化.

        Args:
            yaml_path: YAML定義ファイルパス（省略時はデフォルト）
            llm_client: 共有LLMクライアント（省略時は各Agentで自動取得）
            enable_rag: RAG機能を有効化するか
        """
        self._logger = logging.getLogger("decision_engine.agent_registry")
        self._yaml_path = Path(yaml_path) if yaml_path else self.DEFAULT_YAML_PATH
        self._llm_client = llm_client
        self._enable_rag = enable_rag
        self._agents: dict[str, Any] = {}
        self._flow_definition: FlowDefinition | None = None
        self._initialized = False

    async def initialize(self) -> None:
        """レジストリを初期化.

        YAML定義を読み込み、FlowDefinitionRegistryに登録。
        Agentインスタンスは遅延生成される。
        """
        if self._initialized:
            return

        self._logger.info(f"Loading agent definitions from {self._yaml_path}")

        # YAML読み込み
        self._flow_definition = FlowDefinition.from_yaml(self._yaml_path)

        # FlowDefinitionRegistry に登録
        registry = FlowDefinitionRegistry.get_instance()
        registry.register(self._flow_definition)

        self._initialized = True
        self._logger.info(
            f"Registered flow '{self._flow_definition.flow_id}' "
            f"with {len(self._flow_definition.agents)} agents"
        )

    def _ensure_initialized(self) -> None:
        """初期化されていることを確認."""
        if not self._initialized or not self._flow_definition:
            msg = "AgentRegistry not initialized. Call initialize() first."
            raise RuntimeError(
                msg
            )

    def _create_agent(self, agent_def: AgentDefinition) -> Any:
        """Agentインスタンスを生成.

        Args:
            agent_def: Agent定義

        Returns:
            Agent インスタンス
        """
        class_name = agent_def.class_name
        if not class_name:
            msg = f"Agent {agent_def.id} has no class_name defined"
            raise ValueError(msg)

        # モジュールパスを取得
        module_path = self.AGENT_CLASS_MAP.get(class_name)
        if not module_path:
            msg = f"Unknown agent class: {class_name}"
            raise ValueError(msg)

        # 遅延インポート
        import importlib
        module = importlib.import_module(module_path)
        agent_class = getattr(module, class_name)

        # インスタンス生成
        agent = agent_class(llm_client=self._llm_client)
        self._logger.debug(f"Created agent: {agent_def.id} ({class_name})")
        return agent

    def get_agent(self, agent_id: str) -> Any:
        """Agent IDからAgentインスタンスを取得.

        遅延初期化: 初回アクセス時にインスタンス生成。

        Args:
            agent_id: Agent ID

        Returns:
            Agent インスタンス

        Raises:
            ValueError: Agent IDが見つからない場合
        """
        self._ensure_initialized()

        if agent_id in self._agents:
            return self._agents[agent_id]

        # 定義から検索
        agent_def = self._flow_definition.get_agent(agent_id)
        if not agent_def:
            msg = f"Agent not found: {agent_id}"
            raise ValueError(msg)

        # インスタンス生成・キャッシュ
        agent = self._create_agent(agent_def)
        self._agents[agent_id] = agent
        return agent

    def get_all_agents(self) -> dict[str, Any]:
        """全Agentインスタンスを取得.

        Returns:
            Agent ID -> インスタンス の辞書
        """
        self._ensure_initialized()

        for agent_def in self._flow_definition.agents:
            if agent_def.id not in self._agents:
                self._agents[agent_def.id] = self._create_agent(agent_def)

        return self._agents.copy()

    def get_ordered_agents(self) -> list[Any]:
        """YAML定義順でAgentリストを取得.

        Returns:
            Agent インスタンスリスト（順序付き）
        """
        self._ensure_initialized()

        agents = []
        for agent_def in self._flow_definition.agents:
            agents.append(self.get_agent(agent_def.id))
        return agents

    def get_agent_ids(self) -> list[str]:
        """全Agent IDリストを取得（定義順）.

        Returns:
            Agent ID リスト
        """
        self._ensure_initialized()
        return self._flow_definition.get_agent_ids()

    def get_agent_definition(self, agent_id: str) -> AgentDefinition | None:
        """Agent定義を取得.

        Args:
            agent_id: Agent ID

        Returns:
            AgentDefinition または None
        """
        self._ensure_initialized()
        return self._flow_definition.get_agent(agent_id)

    def get_agent_definitions(self) -> list[AgentDefinition]:
        """全Agent定義リストを取得.

        Returns:
            AgentDefinition リスト
        """
        self._ensure_initialized()
        return list(self._flow_definition.agents)

    def get_agent_metas(self) -> list[AgentMeta]:
        """ProgressEmitter用のAgentMetaリストを取得.

        Returns:
            AgentMeta リスト（順序付き）
        """
        self._ensure_initialized()
        return [
            AgentMeta(
                id=agent_def.id,
                name=agent_def.name,
                label=agent_def.label,
                icon=agent_def.icon,
            )
            for agent_def in self._flow_definition.agents
        ]

    def get_frontend_definitions(self) -> dict[str, Any]:
        """フロントエンド用の定義を取得.

        実装詳細を除外した軽量版。

        Returns:
            フロントエンド用辞書
        """
        self._ensure_initialized()
        return self._flow_definition.to_frontend_dict()

    def get_flow_definition(self) -> FlowDefinition:
        """FlowDefinition を取得.

        Returns:
            FlowDefinition インスタンス
        """
        self._ensure_initialized()
        return self._flow_definition

    @property
    def flow_id(self) -> str:
        """Flow IDを取得."""
        self._ensure_initialized()
        return self._flow_definition.flow_id

    @property
    def total_agents(self) -> int:
        """Agent総数を取得."""
        self._ensure_initialized()
        return len(self._flow_definition.agents)

    def get_gate_agents(self) -> list[AgentDefinition]:
        """ゲートAgent（is_gate=True）のリストを取得.

        Returns:
            ゲートAgent定義リスト
        """
        self._ensure_initialized()
        return [a for a in self._flow_definition.agents if a.is_gate]

    def get_rag_agents(self) -> list[AgentDefinition]:
        """RAG使用Agent（uses_rag=True）のリストを取得.

        Returns:
            RAG使用Agent定義リスト
        """
        self._ensure_initialized()
        return [a for a in self._flow_definition.agents if a.uses_rag]

    async def initialize_rag_agents(self) -> None:
        """RAG使用Agentを初期化.

        uses_rag=True の Agent の initialize_rag() を呼び出す。
        """
        if not self._enable_rag:
            self._logger.info("RAG is disabled, skipping RAG initialization")
            return

        self._ensure_initialized()

        for agent_def in self.get_rag_agents():
            agent = self.get_agent(agent_def.id)
            if hasattr(agent, "initialize_rag"):
                try:
                    await agent.initialize_rag()
                    self._logger.info(f"Initialized RAG for {agent_def.id}")
                except Exception as e:
                    self._logger.warning(
                        f"Failed to initialize RAG for {agent_def.id}: {e}"
                    )

    # ========================================
    # 後方互換用メソッド
    # ========================================

    def get_agent_name_map(self) -> dict[str, str]:
        """Agent名マッピングを取得（後方互換）.

        class_name -> agent_id のマッピング。
        例: {"DaoAgent": "dao", ...}

        Returns:
            Agent名マッピング
        """
        self._ensure_initialized()
        return {
            agent_def.class_name: agent_def.id
            for agent_def in self._flow_definition.agents
            if agent_def.class_name
        }

    @staticmethod
    def load_definitions_static() -> dict[str, Any]:
        """静的にYAML定義を読み込む（初期化不要）.

        Returns:
            YAML定義の辞書形式
        """
        yaml_path = AgentRegistry.DEFAULT_YAML_PATH
        if yaml_path.exists():
            flow = FlowDefinition.from_yaml(yaml_path)
            return flow.to_frontend_dict()
        return {"agents": [], "flow_id": "", "name": ""}


__all__ = [
    "AgentRegistry",
]
