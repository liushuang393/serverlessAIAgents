"""Protocol adapters for MCP, A2A, and AG-UI.

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

# MCP Client は遅延インポート (Pydantic 互換性問題を回避)
from agentflow.protocols.mcp_client import MCPClient
from agentflow.protocols.mcp_config import MCPConfig, MCPServerConfig


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
    "A2AClient",
    "A2AServer",
    "AGUIEvent",
    "AGUIEventEmitter",
    "AGUIEventType",
    "AgentCard",
    "AgentSkill",
    "FlowCancelEvent",
    "FlowCompleteEvent",
    "FlowErrorEvent",
    "FlowStartEvent",
    "LogEvent",
    "MCPClient",
    "MCPConfig",
    "MCPServerConfig",
    "NodeCompleteEvent",
    "NodeErrorEvent",
    "NodeStartEvent",
    "ProgressEvent",
]
