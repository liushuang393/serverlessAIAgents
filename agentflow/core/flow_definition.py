"""Flow Definition - 統一されたフロー・Agent定義.

目的:
- 前後端で共有可能なAgent/Flow定義を提供
- TypeScript型生成をサポート
- API経由で定義を取得可能に
- YAML設定ファイルからの読み込み

使用例:
    >>> from agentflow.core.flow_definition import FlowDefinition, AgentDefinition
    >>>
    >>> flow = FlowDefinition(
    ...     flow_id="decision-engine",
    ...     name="Decision Governance Engine",
    ...     agents=[
    ...         AgentDefinition(id="gatekeeper", name="門番", label="入口検証"),
    ...         AgentDefinition(id="dao", name="道", label="本質分析"),
    ...     ]
    ... )
    >>>
    >>> # JSON/TypeScript出力
    >>> print(flow.to_json())
    >>> print(flow.to_typescript())
    >>>
    >>> # YAMLから読み込み
    >>> flow = FlowDefinition.from_yaml("path/to/agent_definitions.yaml")
"""

import logging
from enum import Enum
from pathlib import Path
from typing import Any

from pydantic import BaseModel, Field


class AgentStatus(str, Enum):
    """Agent状態（前後端共通）."""

    WAITING = "waiting"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"


class AgentDefinition(BaseModel):
    """Agent定義（前後端共通）.

    単一AgentのID・名前・ラベル・アイコンを定義。
    フロントエンドで表示に使用される。
    Pipeline設定（gate/review）も含む。
    """

    id: str = Field(..., description="Agent ID（英数字）")
    name: str = Field(..., description="Agent名（表示用）")
    label: str = Field(..., description="Agent機能ラベル")
    icon: str = Field(default="○", description="表示アイコン（絵文字）")
    description: str = Field(default="", description="Agent説明")
    class_name: str = Field(default="", description="Agent実装クラス名")
    module_path: str = Field(
        default="", description="Agentモジュールパス（省略時はclass_nameから推測）"
    )

    # Pipeline 設定
    is_gate: bool = Field(default=False, description="ゲートAgent（拒否時に早期終了）")
    is_review: bool = Field(default=False, description="レビューAgent（PASS/REVISE/REJECT判定）")
    gate_check_field: str = Field(default="", description="Gate通過判定フィールド名")
    retry_from: str = Field(default="", description="REVISE時のロールバック先Agent ID")

    # 機能設定
    uses_rag: bool = Field(default=False, description="RAGを使用するか")
    uses_llm: bool = Field(default=True, description="LLMを使用するか")
    timeout_seconds: int = Field(default=60, description="タイムアウト秒数")

    # 進捗メッセージ
    progress_messages: list[tuple[int, str]] = Field(
        default_factory=list, description="進捗メッセージ [(progress%, message), ...]"
    )

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return self.model_dump()

    def to_frontend_dict(self) -> dict[str, Any]:
        """フロントエンド用の辞書に変換（実装詳細を除外）."""
        return {
            "id": self.id,
            "name": self.name,
            "label": self.label,
            "icon": self.icon,
        }


class FlowDefinition(BaseModel):
    """Flow定義（前後端共通）.

    フロー全体の定義。含まれるAgentリストを管理。
    """

    flow_id: str = Field(..., description="Flow ID")
    name: str = Field(..., description="Flow名")
    version: str = Field(default="1.0.0", description="バージョン")
    description: str = Field(default="", description="Flow説明")
    agents: list[AgentDefinition] = Field(default_factory=list, description="Agent定義リスト")

    @classmethod
    def from_yaml(cls, yaml_path: str | Path) -> "FlowDefinition":
        """YAMLファイルからFlowDefinitionを生成.

        Args:
            yaml_path: YAMLファイルパス

        Returns:
            FlowDefinition インスタンス

        Raises:
            FileNotFoundError: ファイルが見つからない場合
            ValueError: YAML形式が不正な場合
        """
        try:
            import yaml
        except ImportError as e:
            msg = "PyYAML is required for YAML loading. Install it with: pip install pyyaml"
            raise ImportError(msg) from e

        path = Path(yaml_path)
        if not path.exists():
            msg = f"YAML file not found: {path}"
            raise FileNotFoundError(msg)

        with open(path, encoding="utf-8") as f:
            data = yaml.safe_load(f)

        return cls.from_dict(data)

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> "FlowDefinition":
        """辞書からFlowDefinitionを生成.

        Args:
            data: 辞書データ

        Returns:
            FlowDefinition インスタンス
        """
        # agents リストを AgentDefinition に変換
        agents = [AgentDefinition(**agent_data) for agent_data in data.get("agents", [])]

        return cls(
            flow_id=data.get("flow_id", ""),
            name=data.get("name", ""),
            version=data.get("version", "1.0.0"),
            description=data.get("description", ""),
            agents=agents,
        )

    def get_agent(self, agent_id: str) -> AgentDefinition | None:
        """Agent IDでAgentを取得."""
        for agent in self.agents:
            if agent.id == agent_id:
                return agent
        return None

    def get_agent_ids(self) -> list[str]:
        """全Agent IDリストを取得."""
        return [a.id for a in self.agents]

    def instantiate_agent(
        self,
        agent_def: AgentDefinition,
        llm_client: Any | None = None,
        **kwargs: Any,
    ) -> Any:
        """AgentDefinitionからAgentインスタンスを生成.

        Args:
            agent_def: Agent定義
            llm_client: 共有LLMクライアント（オプション）
            **kwargs: 追加の初期化引数

        Returns:
            Agentインスタンス

        Raises:
            ImportError: モジュールが見つからない場合
            AttributeError: クラスが見つからない場合
        """
        import importlib
        import logging

        logger = logging.getLogger(__name__)

        if not agent_def.class_name:
            msg = f"Agent {agent_def.id} has no class_name defined"
            raise ValueError(msg)

        # モジュールパスを決定
        module_path = agent_def.module_path
        if not module_path:
            # class_name から推測（CamelCase → snake_case）
            import re

            snake_name = re.sub(r"(?<!^)(?=[A-Z])", "_", agent_def.class_name).lower()
            # デフォルトは agentflow.agents.{snake_name}
            module_path = f"agentflow.agents.{snake_name}"

        try:
            module = importlib.import_module(module_path)
            agent_class = getattr(module, agent_def.class_name)
        except (ImportError, AttributeError) as e:
            logger.exception(f"Failed to import {agent_def.class_name} from {module_path}: {e}")
            raise

        # インスタンス化
        init_kwargs = {**kwargs}
        if llm_client is not None:
            init_kwargs["llm_client"] = llm_client

        logger.debug(f"Created agent: {agent_def.id} ({agent_def.class_name})")
        return agent_class(**init_kwargs)

    def instantiate_all_agents(
        self,
        llm_client: Any | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """全AgentをインスタンスId辞書として生成.

        Args:
            llm_client: 共有LLMクライアント
            **kwargs: 追加の初期化引数

        Returns:
            {agent_id: agent_instance} の辞書
        """
        agents = {}
        for agent_def in self.agents:
            if agent_def.class_name:
                agents[agent_def.id] = self.instantiate_agent(
                    agent_def, llm_client=llm_client, **kwargs
                )
        return agents

    def to_stage_configs(
        self,
        agents: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """FlowDefinitionからPipelineEngine用StageConfig辞書リストを生成.

        Args:
            agents: {agent_id: agent_instance} の辞書（省略時は定義のみ）

        Returns:
            StageConfig用の辞書リスト

        使用例:
            >>> flow = FlowDefinition.from_yaml("agents.yaml")
            >>> agents = flow.instantiate_all_agents(llm_client=llm)
            >>> stages = flow.to_stage_configs(agents)
            >>> engine = PipelineEngine(stages=stages)
        """
        stage_configs = []

        for agent_def in self.agents:
            config: dict[str, Any] = {
                "name": agent_def.id,
            }

            # Agent インスタンスを設定
            if agents and agent_def.id in agents:
                config["agent"] = agents[agent_def.id]

            # Gate 設定
            if agent_def.is_gate:
                config["gate"] = True
                if agent_def.gate_check_field:
                    field = agent_def.gate_check_field
                    config["gate_check"] = lambda r, f=field: r.get(f, True)

            # Review 設定
            if agent_def.is_review:
                config["review"] = True
                if agent_def.retry_from:
                    config["retry_from"] = agent_def.retry_from

            stage_configs.append(config)

        return stage_configs

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return self.model_dump()

    def to_frontend_dict(self) -> dict[str, Any]:
        """フロントエンド用の辞書に変換.

        実装詳細（class_name, uses_rag等）を除外した軽量版。
        """
        return {
            "flow_id": self.flow_id,
            "name": self.name,
            "version": self.version,
            "description": self.description,
            "agents": [a.to_frontend_dict() for a in self.agents],
        }

    def to_json(self) -> str:
        """JSON文字列に変換."""
        return self.model_dump_json(indent=2)

    def to_yaml(self) -> str:
        """YAML文字列に変換.

        Returns:
            YAML形式の文字列
        """
        try:
            import yaml
        except ImportError:
            msg = "PyYAML is required for YAML export. Install it with: pip install pyyaml"
            raise ImportError(msg)

        return yaml.dump(
            self.model_dump(),
            allow_unicode=True,
            default_flow_style=False,
            sort_keys=False,
        )

    def save_yaml(self, yaml_path: str | Path) -> None:
        """YAMLファイルに保存.

        Args:
            yaml_path: 保存先ファイルパス
        """
        path = Path(yaml_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(self.to_yaml(), encoding="utf-8")

    def to_typescript(self) -> str:
        """TypeScript型定義を生成.

        フロントエンドで使用するTypeScript型を自動生成。
        """
        lines = [
            f"/** {self.name} - 自動生成された型定義 */",
            "/** @generated - DO NOT EDIT MANUALLY */",
            "",
            "/** Agent ID 型 */",
            f"export type AgentId = {' | '.join(repr(a.id) for a in self.agents)};",
            "",
            "/** Agent 定義 */",
            "export interface AgentDefinition {",
            "  id: AgentId;",
            "  name: string;",
            "  label: string;",
            "  icon: string;",
            "}",
            "",
            "/** Agent 定義リスト（後端から自動同期） */",
            "export const AGENT_DEFINITIONS: AgentDefinition[] = [",
        ]

        for agent in self.agents:
            lines.append(
                f"  {{ id: '{agent.id}', name: '{agent.name}', label: '{agent.label}', icon: '{agent.icon}' }},"
            )

        lines.extend(
            [
                "];",
                "",
                "/** Flow 情報 */",
                "export const FLOW_INFO = {",
                f"  flowId: '{self.flow_id}',",
                f"  name: '{self.name}',",
                f"  version: '{self.version}',",
                "} as const;",
            ]
        )

        return "\n".join(lines)

    def save_typescript(self, ts_path: str | Path) -> None:
        """TypeScript定義ファイルに保存.

        Args:
            ts_path: 保存先ファイルパス
        """
        path = Path(ts_path)
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(self.to_typescript(), encoding="utf-8")


class FlowDefinitionRegistry:
    """Flow定義レジストリ.

    複数のFlowDefinitionを管理し、API経由で提供。
    YAML設定ファイルからの一括読み込みをサポート。

    使用例:
        >>> registry = FlowDefinitionRegistry.get_instance()
        >>> # YAMLから読み込み
        >>> registry.load_from_yaml("configs/flows/")
        >>> # 個別登録
        >>> registry.register(flow_definition)
        >>> # 取得
        >>> flow = registry.get("decision-engine")
        >>> # フロントエンド用JSON取得
        >>> frontend_data = registry.export_for_frontend("decision-engine")
    """

    _instance: "FlowDefinitionRegistry | None" = None
    _logger = logging.getLogger("agentflow.flow_definition")

    def __init__(self) -> None:
        """初期化."""
        self._definitions: dict[str, FlowDefinition] = {}

    @classmethod
    def get_instance(cls) -> "FlowDefinitionRegistry":
        """シングルトンインスタンスを取得."""
        if cls._instance is None:
            cls._instance = cls()
        return cls._instance

    @classmethod
    def reset_instance(cls) -> None:
        """シングルトンインスタンスをリセット（テスト用）."""
        cls._instance = None

    def register(self, definition: FlowDefinition) -> None:
        """Flow定義を登録.

        Args:
            definition: FlowDefinition インスタンス
        """
        self._definitions[definition.flow_id] = definition
        self._logger.info(f"Registered flow: {definition.flow_id}")

    def get(self, flow_id: str) -> FlowDefinition | None:
        """Flow定義を取得.

        Args:
            flow_id: Flow ID

        Returns:
            FlowDefinition または None
        """
        return self._definitions.get(flow_id)

    def list_all(self) -> list[FlowDefinition]:
        """全Flow定義を取得.

        Returns:
            FlowDefinition リスト
        """
        return list(self._definitions.values())

    def load_from_yaml(self, path: str | Path) -> int:
        """YAML ファイル/ディレクトリから読み込み.

        ファイルパスの場合は単一ファイルを読み込み、
        ディレクトリパスの場合は配下の全 .yaml/.yml ファイルを読み込み。

        Args:
            path: YAMLファイルまたはディレクトリのパス

        Returns:
            読み込んだ Flow 数
        """
        path = Path(path)
        loaded_count = 0

        if path.is_file():
            # 単一ファイル
            try:
                flow = FlowDefinition.from_yaml(path)
                self.register(flow)
                loaded_count = 1
            except Exception as e:
                self._logger.exception(f"Failed to load {path}: {e}")
        elif path.is_dir():
            # ディレクトリ配下の全 YAML
            for yaml_file in path.glob("*.yaml"):
                try:
                    flow = FlowDefinition.from_yaml(yaml_file)
                    self.register(flow)
                    loaded_count += 1
                except Exception as e:
                    self._logger.exception(f"Failed to load {yaml_file}: {e}")
            for yml_file in path.glob("*.yml"):
                try:
                    flow = FlowDefinition.from_yaml(yml_file)
                    self.register(flow)
                    loaded_count += 1
                except Exception as e:
                    self._logger.exception(f"Failed to load {yml_file}: {e}")
        else:
            self._logger.warning(f"Path does not exist: {path}")

        return loaded_count

    def export_for_frontend(self, flow_id: str) -> dict[str, Any] | None:
        """フロントエンド用にFlow定義をエクスポート.

        実装詳細を除外した軽量版を返す。

        Args:
            flow_id: Flow ID

        Returns:
            フロントエンド用辞書 または None
        """
        flow = self.get(flow_id)
        if flow:
            return flow.to_frontend_dict()
        return None

    def export_all_for_frontend(self) -> list[dict[str, Any]]:
        """全Flowをフロントエンド用にエクスポート.

        Returns:
            フロントエンド用辞書リスト
        """
        return [flow.to_frontend_dict() for flow in self._definitions.values()]

    def generate_typescript(self, flow_id: str) -> str | None:
        """指定FlowのTypeScript定義を生成.

        Args:
            flow_id: Flow ID

        Returns:
            TypeScript定義文字列 または None
        """
        flow = self.get(flow_id)
        if flow:
            return flow.to_typescript()
        return None

    def save_typescript(self, flow_id: str, output_path: str | Path) -> bool:
        """指定FlowのTypeScript定義をファイルに保存.

        Args:
            flow_id: Flow ID
            output_path: 出力先ファイルパス

        Returns:
            成功の場合 True
        """
        flow = self.get(flow_id)
        if flow:
            flow.save_typescript(output_path)
            self._logger.info(f"Saved TypeScript definition to {output_path}")
            return True
        return False
