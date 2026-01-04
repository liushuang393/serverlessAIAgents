"""Protocol adapters for MCP, A2A, AG-UI, and A2UI.

このモジュールは遅延インポートを使用して、MCP ライブラリの互換性問題を回避します。
"""

from typing import TYPE_CHECKING

from agentflow.protocols.a2a_card import AgentCard, AgentSkill
from agentflow.protocols.a2a_client import A2AClient
from agentflow.protocols.a2a_server import A2AServer
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

# MCP Client は遅延インポート (Pydantic 互換性問題を回避)
from agentflow.protocols.mcp_client import MCPClient
from agentflow.protocols.mcp_config import MCPConfig, MCPServerConfig

# MCP Tool - 工具基底クラスとクライアント (v0.3.0 追加)
from agentflow.protocols.mcp_tool import (
    MCPTool,
    MCPToolClient,
    MCPToolRequest,
    MCPToolResponse,
)


def __getattr__(name: str) -> object:
    """遅延インポートを実装.

    Args:
        name: インポートする属性名

    Returns:
        インポートされた属性

    Raises:
        AttributeError: 属性が見つからない場合
    """
    if name == "MCPClient":
        return MCPClient
    msg = f"module {__name__!r} has no attribute {name!r}"
    raise AttributeError(msg)


__all__ = [
    # A2A
    "A2AClient",
    "A2AServer",
    "AgentCard",
    "AgentSkill",
    # AG-UI
    "AGUIEvent",
    "AGUIEventEmitter",
    "AGUIEventType",
    "ClarificationQuestion",
    "ClarificationReceivedEvent",
    "ClarificationRequiredEvent",
    "FlowCancelEvent",
    "FlowCompleteEvent",
    "FlowErrorEvent",
    "FlowStartEvent",
    "LogEvent",
    "NodeCompleteEvent",
    "NodeErrorEvent",
    "NodeStartEvent",
    "ProgressEvent",
    # A2UI
    "A2UIComponent",
    "A2UIEmitter",
    "A2UIRenderer",
    "ButtonComponent",
    "CardComponent",
    "ComponentRegistry",
    "FormComponent",
    "ImageComponent",
    "InputComponent",
    "ListComponent",
    "TextComponent",
    # MCP
    "MCPClient",
    "MCPConfig",
    "MCPServerConfig",
    # MCP Tool (v0.3.0)
    "MCPTool",
    "MCPToolClient",
    "MCPToolRequest",
    "MCPToolResponse",
]
