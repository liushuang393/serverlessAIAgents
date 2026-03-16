"""Protocol adapters for MCP, A2A, AG-UI, and A2UI.

このモジュールは遅延インポートを使用して、MCP ライブラリの互換性問題を回避します。
"""

from typing import TYPE_CHECKING

from agentflow.protocols.a2a_card import AgentCard, AgentSkill
from agentflow.protocols.a2a_client import A2AClient
from agentflow.protocols.a2a_server import A2AServer

# A2UI Protocol
from agentflow.protocols.a2ui import (
    A2UIComponent,
    A2UIEmitter,
    A2UIRenderer,
    ButtonComponent,
    CardComponent,
    ComponentRegistry,
    FormComponent,
    ImageComponent,
    InputComponent,
    ListComponent,
    TextComponent,
)
from agentflow.protocols.agui_emitter import AGUIEventEmitter
from agentflow.protocols.agui_events import (
    AGUIEvent,
    AGUIEventType,
    ClarificationQuestion,
    ClarificationReceivedEvent,
    ClarificationRequiredEvent,
    FlowCancelEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
    LogEvent,
    NodeCompleteEvent,
    NodeErrorEvent,
    NodeStartEvent,
    ProgressEvent,
)

# MCP 関連は遅延インポート（mcp パッケージはオプション依存）
# __getattr__ で初回アクセス時にのみインポートする
from agentflow.protocols.mcp_config import LazyLoadingConfig, MCPConfig, MCPServerConfig

# MCP Tool - 工具基底クラスとクライアント (v0.3.0 追加)
# mcp_tool は外部 mcp パッケージに依存しないため即時インポート可
from agentflow.protocols.mcp_tool import (
    MCPTool,
    MCPToolClient,
    MCPToolRequest,
    MCPToolResponse,
)

# MCP Client / LazyMCPClient の遅延マッピング
_MCP_LAZY_IMPORTS: dict[str, tuple[str, str]] = {
    "MCPClient": ("agentflow.protocols.mcp_client", "MCPClient"),
    "LazyMCPClient": ("agentflow.protocols.mcp_lazy_client", "LazyMCPClient"),
    "ToolIndexEntry": ("agentflow.protocols.mcp_lazy_client", "ToolIndexEntry"),
    "ToolSearchResult": ("agentflow.protocols.mcp_lazy_client", "ToolSearchResult"),
}


def __getattr__(name: str) -> object:
    """遅延インポートを実装.

    mcp パッケージに依存するクラスは初回アクセス時のみインポートする。
    これにより mcp 未インストール環境でもモジュールロードが可能になる。

    Args:
        name: インポートする属性名

    Returns:
        インポートされた属性

    Raises:
        AttributeError: 属性が見つからない場合
    """
    if name in _MCP_LAZY_IMPORTS:
        module_path, attr_name = _MCP_LAZY_IMPORTS[name]
        import importlib

        module = importlib.import_module(module_path)
        return getattr(module, attr_name)
    msg = f"module {__name__!r} has no attribute {name!r}"
    raise AttributeError(msg)


__all__ = [
    # A2A
    "A2AClient",
    "A2AServer",
    # A2UI
    "A2UIComponent",
    "A2UIEmitter",
    "A2UIRenderer",
    # AG-UI
    "AGUIEvent",
    "AGUIEventEmitter",
    "AGUIEventType",
    "AgentCard",
    "AgentSkill",
    "ButtonComponent",
    "CardComponent",
    "ClarificationQuestion",
    "ClarificationReceivedEvent",
    "ClarificationRequiredEvent",
    "ComponentRegistry",
    "FlowCancelEvent",
    "FlowCompleteEvent",
    "FlowErrorEvent",
    "FlowStartEvent",
    "FormComponent",
    "ImageComponent",
    "InputComponent",
    "LazyLoadingConfig",
    # MCP Lazy Client (v0.4.0)
    "LazyMCPClient",
    "ListComponent",
    "LogEvent",
    # MCP
    "MCPClient",
    "MCPConfig",
    "MCPServerConfig",
    # MCP Tool (v0.3.0)
    "MCPTool",
    "MCPToolClient",
    "MCPToolRequest",
    "MCPToolResponse",
    "NodeCompleteEvent",
    "NodeErrorEvent",
    "NodeStartEvent",
    "ProgressEvent",
    "TextComponent",
    "ToolIndexEntry",
    "ToolSearchResult",
]
