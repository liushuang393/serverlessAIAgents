"""AgentBlock 基底クラス.

このモジュールは全てのエージェントが継承する基底クラスを提供します。
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from pathlib import Path
from typing import TYPE_CHECKING, Any

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.schemas import SchemaLoader
from agentflow.decorators import auto_adapt


if TYPE_CHECKING:
    from agentflow.core.metadata import AgentMetadata
    from agentflow.protocols.a2a_card import AgentCard
    from agentflow.protocols.agui_emitter import AGUIEventEmitter


@auto_adapt()
class AgentBlock(ABC):
    """全てのエージェントが継承する基底クラス.

    このクラスは以下の機能を提供します:
    - メタデータの自動読み込み
    - プロトコルアダプターの自動適用 (@auto_adapt デコレーター)
    - ライフサイクルメソッド (init, run, cleanup)
    - エンジンとの統合

    Example:
        >>> class MyAgent(AgentBlock):
        ...     async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        ...         # エージェントのロジックを実装
        ...         return {"result": "ok"}
        ...
        >>> agent = MyAgent()
        >>> result = await agent.run({"text": "hello"})
    """

    def __init__(
        self,
        metadata_path: str | Path = "agent.yaml",
        engine: AgentFlowEngine | None = None,
    ) -> None:
        """AgentBlock を初期化.

        Args:
            metadata_path: agent.yaml ファイルのパス (デフォルト: "agent.yaml")
            engine: AgentFlowEngine インスタンス (オプション)
        """
        self._metadata_path = Path(metadata_path)
        self._engine = engine or AgentFlowEngine()
        self._initialized = False

    def load_metadata(self, metadata_path: str | Path | None = None) -> AgentMetadata:
        """メタデータを読み込み.

        Args:
            metadata_path: agent.yaml ファイルのパス (オプション)

        Returns:
            AgentMetadata インスタンス

        Raises:
            FileNotFoundError: メタデータファイルが見つからない場合
            ValueError: メタデータの検証に失敗した場合
        """
        if metadata_path:
            self._metadata_path = Path(metadata_path)

        if not self._metadata_path.exists():
            msg = f"Metadata file not found: {self._metadata_path}"
            raise FileNotFoundError(msg)

        loader = SchemaLoader()
        self._metadata = loader.load_from_file(self._metadata_path)

        # プロトコルアダプターを再生成
        from agentflow.adapters.protocol_adapter import ProtocolAdapter

        if self._metadata.protocols.mcp:
            self._mcp_tools = ProtocolAdapter.generate_mcp_tools(self._metadata)

        if self._metadata.protocols.a2a and self._metadata.protocols.a2a.enabled:
            self._a2a_card = ProtocolAdapter.generate_a2a_card(self._metadata)

        return self._metadata

    @abstractmethod
    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """エージェントを実行 (サブクラスで実装必須).

        Args:
            input_data: 入力データ

        Returns:
            出力データ
        """

    async def initialize(self) -> None:
        """エージェントを初期化 (オプション).

        サブクラスでオーバーライドして、初期化処理を実装できます。
        例: モデルのロード、接続の確立など。
        """
        self._initialized = True

    async def cleanup(self) -> None:
        """エージェントをクリーンアップ (オプション).

        サブクラスでオーバーライドして、クリーンアップ処理を実装できます。
        例: 接続のクローズ、リソースの解放など。
        """
        self._initialized = False

    @property
    def metadata(self) -> AgentMetadata | None:
        """エージェントメタデータを取得.

        Returns:
            AgentMetadata インスタンス、または None
        """
        return getattr(self, "_metadata", None)

    @property
    def engine(self) -> AgentFlowEngine:
        """AgentFlowEngine インスタンスを取得.

        Returns:
            AgentFlowEngine インスタンス
        """
        return self._engine

    @property
    def is_initialized(self) -> bool:
        """エージェントが初期化されているかを確認.

        Returns:
            初期化済みの場合 True
        """
        return self._initialized

    def get_mcp_tools(self) -> list[dict[str, Any]]:
        """MCP ツール定義を取得 (@auto_adapt デコレーターが提供).

        Returns:
            MCP ツール定義のリスト
        """
        return []

    def get_a2a_card(self) -> AgentCard | None:
        """A2A AgentCard を取得 (@auto_adapt デコレーターが提供).

        Returns:
            AgentCard インスタンス、または None
        """
        return None

    def create_agui_emitter(self, engine: Any) -> AGUIEventEmitter:
        """AG-UI イベントエミッターを作成 (@auto_adapt デコレーターが提供).

        Args:
            engine: AgentFlowEngine インスタンス

        Returns:
            AGUIEventEmitter インスタンス
        """
        # Lazy import to avoid circular dependency
        from agentflow.protocols.agui_emitter import AGUIEventEmitter

        return AGUIEventEmitter(engine)

    async def __aenter__(self) -> AgentBlock:
        """非同期コンテキストマネージャーのエントリー.

        Returns:
            self
        """
        await self.initialize()
        return self

    async def __aexit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: Any,
    ) -> None:
        """非同期コンテキストマネージャーの終了.

        Args:
            exc_type: 例外の型
            exc_val: 例外の値
            exc_tb: トレースバック
        """
        await self.cleanup()
