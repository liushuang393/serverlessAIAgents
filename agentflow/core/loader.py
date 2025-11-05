"""Agent Loader.

このモジュールは Agent をファイルシステムから読み込む機能を提供します。
"""

import importlib.util
import sys
from pathlib import Path
from typing import Any

from agentflow.core.agent_block import AgentBlock
from agentflow.core.exceptions import AgentBlockValidationError
from agentflow.core.metadata import AgentMetadata
from agentflow.core.schemas import SchemaLoader


class AgentLoader:
    """Agent Loader.

    ファイルシステムから Agent を読み込みます。

    Example:
        >>> loader = AgentLoader()
        >>> agent = loader.load_from_directory(Path("./my-agent"))
    """

    def __init__(self) -> None:
        """AgentLoader を初期化."""
        self._schema_loader = SchemaLoader()

    def load_from_directory(self, agent_path: Path) -> AgentBlock:
        """ディレクトリから Agent を読み込む.

        Args:
            agent_path: Agent のディレクトリパス

        Returns:
            AgentBlock: 読み込まれた Agent

        Raises:
            FileNotFoundError: agent.yaml が見つからない場合
            AgentBlockValidationError: Agent の読み込みに失敗した場合

        Example:
            >>> loader = AgentLoader()
            >>> agent = loader.load_from_directory(Path("./my-agent"))
        """
        # agent.yaml を読み込む
        yaml_path = agent_path / "agent.yaml"
        if not yaml_path.exists():
            msg = f"agent.yaml not found in {agent_path}"
            raise FileNotFoundError(msg)

        metadata = self._schema_loader.load_from_file(yaml_path)

        # PocketFlow エントリーポイントを読み込む
        entry_point = metadata.pocketflow.entry
        flow_module, flow_func = self._parse_entry_point(entry_point)

        # Python モジュールを動的にロード
        module_path = agent_path / flow_module
        if not module_path.exists():
            msg = f"Flow module not found: {module_path}"
            raise FileNotFoundError(msg)

        flow_creator = self._load_flow_creator(module_path, flow_func)

        # AgentBlock を作成
        return self._create_agent_block(
            metadata=metadata,
            flow_creator=flow_creator,
            agent_path=agent_path,
        )

    def _parse_entry_point(self, entry_point: str) -> tuple[str, str]:
        """エントリーポイント文字列をパース.

        Args:
            entry_point: "module.py:function" 形式の文字列

        Returns:
            (module_path, function_name) のタプル

        Raises:
            AgentBlockValidationError: エントリーポイントの形式が不正な場合

        Example:
            >>> loader = AgentLoader()
            >>> module, func = loader._parse_entry_point("flow.py:create_flow")
            >>> print(module, func)  # flow.py create_flow
        """
        if ":" not in entry_point:
            msg = (
                f"Invalid entry point format: {entry_point}. "
                "Expected format: 'module.py:function'"
            )
            raise AgentBlockValidationError(msg)

        parts = entry_point.split(":", 1)
        return parts[0], parts[1]

    def _load_flow_creator(self, module_path: Path, function_name: str) -> Any:
        """Flow 作成関数を動的にロード.

        Args:
            module_path: Python モジュールのパス
            function_name: 関数名

        Returns:
            Flow 作成関数

        Raises:
            AgentBlockValidationError: モジュールまたは関数が見つからない場合

        Example:
            >>> loader = AgentLoader()
            >>> func = loader._load_flow_creator(Path("flow.py"), "create_flow")
        """
        try:
            # モジュールを動的にロード
            spec = importlib.util.spec_from_file_location("agent_flow", module_path)
            if spec is None or spec.loader is None:
                msg = f"Failed to load module spec: {module_path}"
                raise AgentBlockValidationError(msg)

            module = importlib.util.module_from_spec(spec)
            sys.modules["agent_flow"] = module
            spec.loader.exec_module(module)

            # 関数を取得
            if not hasattr(module, function_name):
                msg = f"Function '{function_name}' not found in {module_path}"
                raise AgentBlockValidationError(msg)

            return getattr(module, function_name)

        except Exception as e:
            msg = f"Failed to load flow creator: {e}"
            raise AgentBlockValidationError(msg) from e

    def _create_agent_block(
        self,
        metadata: AgentMetadata,
        flow_creator: Any,
        agent_path: Path,
    ) -> AgentBlock:
        """AgentBlock インスタンスを作成.

        Args:
            metadata: Agent メタデータ
            flow_creator: Flow 作成関数
            agent_path: Agent のディレクトリパス

        Returns:
            AgentBlock: 作成された Agent

        Example:
            >>> loader = AgentLoader()
            >>> agent = loader._create_agent_block(metadata, flow_creator, path)
        """

        # 動的に AgentBlock サブクラスを作成
        class DynamicAgentBlock(AgentBlock):
            """動的に作成された AgentBlock."""

            def __init__(self) -> None:
                """DynamicAgentBlock を初期化."""
                metadata_file = agent_path / "agent.yaml"
                super().__init__(metadata_path=metadata_file)
                self._flow_creator = flow_creator
                self._metadata = metadata

            async def run(self, **inputs: Any) -> dict[str, Any]:
                """Agent を実行.

                Args:
                    **inputs: 入力パラメータ

                Returns:
                    実行結果
                """
                if self._engine is None:
                    msg = "Engine not initialized. Call initialize() first."
                    raise RuntimeError(msg)

                # Flow を作成して実行
                flow = self._flow_creator()
                workflow_id = f"{self._metadata.meta.id}-workflow"

                # Workflow を登録
                self._engine.register_workflow(workflow_id, flow)

                # 実行
                return await self._engine.execute(workflow_id, inputs)

            @property
            def metadata(self) -> AgentMetadata:
                """メタデータを取得."""
                return self._metadata

        return DynamicAgentBlock()

    def load_metadata_only(self, agent_path: Path) -> AgentMetadata:
        """メタデータのみを読み込む.

        Args:
            agent_path: Agent のディレクトリパス

        Returns:
            AgentMetadata: 読み込まれたメタデータ

        Raises:
            FileNotFoundError: agent.yaml が見つからない場合

        Example:
            >>> loader = AgentLoader()
            >>> metadata = loader.load_metadata_only(Path("./my-agent"))
        """
        yaml_path = agent_path / "agent.yaml"
        if not yaml_path.exists():
            msg = f"agent.yaml not found in {agent_path}"
            raise FileNotFoundError(msg)

        return self._schema_loader.load_from_file(yaml_path)
