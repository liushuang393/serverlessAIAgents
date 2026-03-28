"""Protocol adapters for MCP, A2A, AG-UI, and A2UI.

このモジュールは遅延インポートを使用して、MCP ライブラリの互換性問題を回避します。
実体は kernel/protocols/ に配置。
"""

from kernel.protocols.a2a_card import AgentCard, AgentSkill
from kernel.protocols.a2a_client import A2AClient
from kernel.protocols.a2a_server import A2AServer


# A2UI Protocol — 遅延インポート（kernel.core.registry 循環回避）
_A2UI_LAZY_IMPORTS: dict[str, tuple[str, str]] = {
    "A2UIComponent": ("kernel.protocols.a2ui.components", "A2UIComponent"),
    "A2UIEmitter": ("kernel.protocols.a2ui.emitter", "A2UIEmitter"),
    "A2UIRenderer": ("kernel.protocols.a2ui.renderer", "A2UIRenderer"),
    "ButtonComponent": ("kernel.protocols.a2ui.components", "ButtonComponent"),
    "CardComponent": ("kernel.protocols.a2ui.components", "CardComponent"),
    "ComponentRegistry": ("kernel.protocols.a2ui.renderer", "ComponentRegistry"),
    "FormComponent": ("kernel.protocols.a2ui.components", "FormComponent"),
    "ImageComponent": ("kernel.protocols.a2ui.components", "ImageComponent"),
    "InputComponent": ("kernel.protocols.a2ui.components", "InputComponent"),
    "ListComponent": ("kernel.protocols.a2ui.components", "ListComponent"),
    "TextComponent": ("kernel.protocols.a2ui.components", "TextComponent"),
}
# AG-UI emitter（kernel.core.engine に依存するため遅延必須）
_AGUI_LAZY_IMPORTS: dict[str, tuple[str, str]] = {
    "AGUIEventEmitter": ("kernel.protocols.agui_emitter", "AGUIEventEmitter"),
}

from kernel.protocols.agui_events import (
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
from kernel.protocols.mcp.config import LazyLoadingConfig, MCPConfig, MCPServerConfig

# MCP Tool - 工具基底クラスとクライアント
from kernel.protocols.mcp.tool import (
    MCPTool,
    MCPToolClient,
    MCPToolRequest,
    MCPToolResponse,
)


# MCP Client / LazyMCPClient の遅延マッピング
_MCP_LAZY_IMPORTS: dict[str, tuple[str, str]] = {
    "MCPClient": ("kernel.protocols.mcp_client", "MCPClient"),
    "LazyMCPClient": ("kernel.protocols.mcp_lazy_client", "LazyMCPClient"),
    "ToolIndexEntry": ("kernel.protocols.mcp_lazy_client", "ToolIndexEntry"),
    "ToolSearchResult": ("kernel.protocols.mcp_lazy_client", "ToolSearchResult"),
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
    # A2UI + AG-UI + MCP の遅延インポートを統合検索
    _all_lazy = {**_A2UI_LAZY_IMPORTS, **_AGUI_LAZY_IMPORTS, **_MCP_LAZY_IMPORTS}
    if name in _all_lazy:
        module_path, attr_name = _all_lazy[name]
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
    # MCP Lazy Client
    "LazyMCPClient",
    "ListComponent",
    "LogEvent",
    # MCP
    "MCPClient",
    "MCPConfig",
    "MCPServerConfig",
    # MCP Tool
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
