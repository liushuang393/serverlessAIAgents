"""Agent Block Manager.

このモジュールは Agent Block のライフサイクル管理を提供します。
Agent の読み込み、検証、登録、取得、依存関係解決などの機能を含みます。
"""

from pathlib import Path
from typing import Any

from agentflow.core.agent_block import AgentBlock
from agentflow.core.exceptions import AgentBlockNotFoundError, AgentBlockValidationError
from agentflow.core.loader import AgentLoader
from agentflow.core.metadata import AgentMetadata
from agentflow.core.validator import AgentValidator, ValidationResult


class AgentInfo:
    """Agent 情報.

    Agent の基本情報を保持するデータクラスです。
    """

    def __init__(
        self,
        agent_id: str,
        name: str,
        version: str,
        description: str,
        category: str,
        author: str,
    ) -> None:
        """AgentInfo を初期化.

        Args:
            agent_id: Agent ID
            name: Agent 名
            version: バージョン
            description: 説明
            category: カテゴリ
            author: 作者
        """
        self.agent_id = agent_id
        self.name = name
        self.version = version
        self.description = description
        self.category = category
        self.author = author

    def __repr__(self) -> str:
        """文字列表現を返す."""
        return f"AgentInfo(id={self.agent_id}, name={self.name}, version={self.version})"


class AgentBlockManager:
    """Agent Block Manager.

    Agent Block のライフサイクル管理を行います。
    - Agent の読み込み
    - メタデータの検証
    - Agent の登録・取得
    - 依存関係の解決

    Example:
        >>> manager = AgentBlockManager()
        >>> agent = manager.load_agent(Path("./my-agent"))
        >>> manager.register_agent("my-agent", agent)
        >>> retrieved = manager.get_agent("my-agent")
    """

    def __init__(self, registry_path: Path | None = None) -> None:
        """AgentBlockManager を初期化.

        Args:
            registry_path: Agent レジストリのパス (デフォルト: ~/.agentflow/registry)
        """
        self.registry: dict[str, AgentBlock] = {}
        self.registry_path = registry_path or Path.home() / ".agentflow" / "registry"
        self.registry_path.mkdir(parents=True, exist_ok=True)

        self._loader = AgentLoader()
        self._validator = AgentValidator()

    def _require_metadata(self, agent_block: AgentBlock, agent_id: str) -> AgentMetadata:
        """AgentBlock から必須メタデータを取得."""
        metadata = agent_block.metadata
        if metadata is None:
            msg = f"Agent metadata is missing: {agent_id}"
            raise AgentBlockValidationError(msg)
        return metadata

    def load_agent(self, agent_path: Path) -> AgentBlock:
        """Agent を読み込む.

        指定されたパスから Agent を読み込み、検証します。

        Args:
            agent_path: Agent のディレクトリパス

        Returns:
            読み込まれた AgentBlock インスタンス

        Raises:
            AgentBlockValidationError: Agent の検証に失敗した場合
            FileNotFoundError: agent.yaml が見つからない場合

        Example:
            >>> manager = AgentBlockManager()
            >>> agent = manager.load_agent(Path("./my-agent"))
        """
        # Agent を読み込む
        agent_block = self._loader.load_from_directory(agent_path)

        # メタデータを検証
        metadata = self._require_metadata(agent_block, str(agent_path))
        validation_result = self.validate_agent(metadata)
        if not validation_result.is_valid:
            error_messages = "\n".join(validation_result.errors)
            msg = f"Agent validation failed:\n{error_messages}"
            raise AgentBlockValidationError(msg)

        return agent_block

    def validate_agent(self, agent_meta: AgentMetadata) -> ValidationResult:
        """Agent メタデータを検証.

        Args:
            agent_meta: 検証する AgentMetadata

        Returns:
            ValidationResult: 検証結果

        Example:
            >>> manager = AgentBlockManager()
            >>> result = manager.validate_agent(metadata)
            >>> if result.is_valid:
            ...     print("Valid!")
        """
        return self._validator.validate(agent_meta)

    def register_agent(self, agent_id: str, agent_block: AgentBlock) -> None:
        """Agent を登録.

        Args:
            agent_id: Agent ID
            agent_block: 登録する AgentBlock

        Example:
            >>> manager = AgentBlockManager()
            >>> manager.register_agent("my-agent", agent)
        """
        self.registry[agent_id] = agent_block

    def unregister_agent(self, agent_id: str) -> None:
        """Agent の登録を解除.

        Args:
            agent_id: Agent ID

        Raises:
            AgentBlockNotFoundError: Agent が見つからない場合

        Example:
            >>> manager = AgentBlockManager()
            >>> manager.unregister_agent("my-agent")
        """
        if agent_id not in self.registry:
            msg = f"Agent not found: {agent_id}"
            raise AgentBlockNotFoundError(msg)
        del self.registry[agent_id]

    def get_agent(self, agent_id: str) -> AgentBlock:
        """Agent を取得.

        Args:
            agent_id: Agent ID

        Returns:
            AgentBlock: 取得した Agent

        Raises:
            AgentBlockNotFoundError: Agent が見つからない場合

        Example:
            >>> manager = AgentBlockManager()
            >>> agent = manager.get_agent("my-agent")
        """
        if agent_id not in self.registry:
            msg = f"Agent not found: {agent_id}"
            raise AgentBlockNotFoundError(msg)
        return self.registry[agent_id]

    def list_agents(self, filters: dict[str, Any] | None = None) -> list[AgentInfo]:
        """Agent 一覧を取得.

        Args:
            filters: フィルター条件 (category, author など)

        Returns:
            AgentInfo のリスト

        Example:
            >>> manager = AgentBlockManager()
            >>> agents = manager.list_agents(filters={"category": "nlp"})
            >>> for agent in agents:
            ...     print(agent.name)
        """
        agent_infos: list[AgentInfo] = []

        for agent_id, agent_block in self.registry.items():
            metadata = self._require_metadata(agent_block, agent_id)
            meta = metadata.meta

            # フィルター適用
            if filters:
                if "category" in filters and meta.category != filters["category"]:
                    continue
                if "author" in filters and meta.author != filters["author"]:
                    continue

            agent_info = AgentInfo(
                agent_id=agent_id,
                name=meta.name,
                version=meta.version,
                description=meta.description,
                category=meta.category,
                author=meta.author,
            )
            agent_infos.append(agent_info)

        return agent_infos

    def resolve_dependencies(self, agent_id: str) -> list[str]:
        """Agent の依存関係を解決.

        Args:
            agent_id: Agent ID

        Returns:
            依存する Agent ID のリスト (依存順)

        Raises:
            AgentBlockNotFoundError: Agent が見つからない場合

        Example:
            >>> manager = AgentBlockManager()
            >>> deps = manager.resolve_dependencies("my-agent")
            >>> print(deps)  # ['dep1', 'dep2', 'my-agent']
        """
        if agent_id not in self.registry:
            msg = f"Agent not found: {agent_id}"
            raise AgentBlockNotFoundError(msg)

        agent_block = self.registry[agent_id]
        metadata = self._require_metadata(agent_block, agent_id)
        dependencies = metadata.dependencies

        # 依存する Agent ID を収集
        dep_agent_ids: list[str] = []
        if dependencies and dependencies.agents:
            dep_agent_ids = dependencies.agents.copy()

        # 再帰的に依存関係を解決 (簡易実装: 循環依存チェックなし)
        resolved: list[str] = []
        for dep_id in dep_agent_ids:
            if dep_id in self.registry:
                # 依存先の依存関係を先に解決
                sub_deps = self.resolve_dependencies(dep_id)
                for sub_dep in sub_deps:
                    if sub_dep not in resolved:
                        resolved.append(sub_dep)

        # 最後に自分自身を追加
        if agent_id not in resolved:
            resolved.append(agent_id)

        return resolved
