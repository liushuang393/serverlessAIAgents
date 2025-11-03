"""エージェントデコレーター.

このモジュールは AgentFlow のエージェントクラスに適用できるデコレーターを提供します。
"""

import functools
from collections.abc import Callable
from pathlib import Path
from typing import Any, TypeVar

from agentflow.adapters.protocol_adapter import ProtocolAdapter
from agentflow.core.metadata import AgentMetadata
from agentflow.core.schemas import SchemaLoader
from agentflow.protocols.a2a_card import AgentCard
from agentflow.protocols.agui_emitter import AGUIEventEmitter


T = TypeVar("T")


def auto_adapt(
    protocols: list[str] | None = None,
    metadata_path: str | Path = "agent.yaml",
) -> Callable[[type[T]], type[T]]:
    """プロトコルアダプターを自動適用するデコレーター.

    このデコレーターは AgentMetadata から各プロトコルのアダプターを自動生成し、
    エージェントクラスにプロトコルメソッドを注入します。

    Args:
        protocols: 有効にするプロトコルのリスト (["mcp", "a2a", "agui"])
                   None の場合、metadata から自動判定
        metadata_path: agent.yaml ファイルのパス (デフォルト: "agent.yaml")

    Returns:
        デコレートされたクラス

    Example:
        >>> @auto_adapt(protocols=["mcp", "a2a"])
        ... class MyAgent:
        ...     def __init__(self):
        ...         pass
        ...
        >>> agent = MyAgent()
        >>> tools = agent.get_mcp_tools()
        >>> card = agent.get_a2a_card()
    """
    if protocols is None:
        protocols = []

    def decorator(cls: type[T]) -> type[T]:
        """クラスデコレーター."""
        original_init = cls.__init__

        @functools.wraps(original_init)
        def new_init(self: Any, *args: Any, **kwargs: Any) -> None:
            """新しい __init__ メソッド."""
            # 元の __init__ を呼び出し
            original_init(self, *args, **kwargs)

            # メタデータを読み込み
            loader = SchemaLoader()
            metadata_file = Path(metadata_path)

            # クラスの __file__ 属性からディレクトリを取得
            if hasattr(cls, "__file__"):
                class_dir = Path(cls.__file__).parent
                metadata_file = class_dir / metadata_path
            elif not metadata_file.is_absolute():
                # 相対パスの場合、カレントディレクトリから探す
                metadata_file = Path.cwd() / metadata_path

            if metadata_file.exists():
                self._metadata: AgentMetadata = loader.load_from_file(metadata_file)
            else:
                # メタデータファイルが見つからない場合、空のメタデータを作成 (テスト用)
                self._metadata = None  # type: ignore

            # プロトコルアダプターを生成
            if self._metadata:
                # MCP プロトコル
                if "mcp" in protocols or (not protocols and self._metadata.protocols.mcp):
                    self._mcp_tools = ProtocolAdapter.generate_mcp_tools(self._metadata)

                # A2A プロトコル
                if "a2a" in protocols or (
                    not protocols
                    and self._metadata.protocols.a2a
                    and self._metadata.protocols.a2a.enabled
                ):
                    self._a2a_card = ProtocolAdapter.generate_a2a_card(self._metadata)

                # AG-UI プロトコル
                if "agui" in protocols or (
                    not protocols
                    and self._metadata.protocols.agui
                    and self._metadata.protocols.agui.enabled
                ):
                    # AGUIEventEmitter は engine が必要なので、ここでは準備だけ
                    self._agui_enabled = True

        # __init__ を置き換え
        cls.__init__ = new_init  # type: ignore

        # プロトコルインターフェースメソッドを追加
        if "mcp" in protocols or not protocols:

            def get_mcp_tools(self: Any) -> list[dict[str, Any]]:
                """MCP ツール定義を取得.

                Returns:
                    MCP ツール定義のリスト
                """
                return getattr(self, "_mcp_tools", [])

            cls.get_mcp_tools = get_mcp_tools  # type: ignore

        if "a2a" in protocols or not protocols:

            def get_a2a_card(self: Any) -> AgentCard | None:
                """A2A AgentCard を取得.

                Returns:
                    AgentCard インスタンス、または None
                """
                return getattr(self, "_a2a_card", None)

            cls.get_a2a_card = get_a2a_card  # type: ignore

        if "agui" in protocols or not protocols:

            def create_agui_emitter(self: Any, engine: Any) -> AGUIEventEmitter:
                """AG-UI イベントエミッターを作成.

                Args:
                    engine: AgentFlowEngine インスタンス

                Returns:
                    AGUIEventEmitter インスタンス
                """
                metadata = getattr(self, "_metadata", None)
                if metadata:
                    return ProtocolAdapter.wrap_flow_with_agui(engine, "default-flow", metadata)
                msg = "Metadata not loaded"
                raise ValueError(msg)

            cls.create_agui_emitter = create_agui_emitter  # type: ignore

        # メタデータアクセスメソッドを追加
        def get_metadata(self: Any) -> AgentMetadata | None:
            """エージェントメタデータを取得.

            Returns:
                AgentMetadata インスタンス、または None
            """
            return getattr(self, "_metadata", None)

        cls.get_metadata = get_metadata  # type: ignore

        return cls

    return decorator
