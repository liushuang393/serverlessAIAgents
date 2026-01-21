# -*- coding: utf-8 -*-
"""フレームワーク統合モジュール.

このモジュールは、FastAPI等のフレームワークとの統合を提供します。

使用例:
    >>> from agentflow.integrations.fastapi import AgentRouter
    >>> app.include_router(AgentRouter(agents=["MyAgent"]), prefix="/api")
    >>>
    >>> # SSEFlowRunner を使用
    >>> from agentflow.integrations import SSEFlowRunner
    >>> runner = SSEFlowRunner(pipeline)
    >>> async for event in runner.run_with_events(data):
    ...     yield event
    >>>
    >>> # WebSocket を使用（NEW）
    >>> from agentflow.integrations import WebSocketManager
    >>> manager = WebSocketManager()
    >>> await manager.handle_connection(websocket, session_id)
"""

from agentflow.integrations.fastapi_integration import (
    AgentRouter,
    FlowRouter,
    create_app,
    create_sse_response,
)
from agentflow.integrations.sse_flow_runner import (
    FlowProtocol,
    SSEConfig,
    SSEFlowRunner,
    SimplePipelineProtocol,
)

# NEW: WebSocket 統合
from agentflow.integrations.websocket_integration import (
    WSEventType,
    WSEvent,
    WSCommand,
    ConnectionState,
    ConnectionManager,
    WebSocketManager,
    create_websocket_router,
)

# NEW: リアルタイム状態同期
from agentflow.integrations.realtime_sync import (
    RealtimeStateSync,
    SyncEvent,
    SyncEventType,
    ClientConnection,
)

# 新規追加: 工単生成器
from agentflow.integrations.ticket_generator import (
    TicketGenerator,
    TicketGeneratorConfig,
    Ticket,
    TicketPriority,
    TicketStatus,
    TicketType,
    TicketProviderBase,
    InMemoryTicketProvider,
    JiraTicketProvider,
    ServiceNowTicketProvider,
)

# ==========================================================================
# NEW: Context Bridge（L0統合I/F）
# ==========================================================================
from agentflow.integrations.context_bridge import (
    FlowContext,
    ContextBridge,
    InvocationResult,
    SourceSystemType,
    get_current_context,
    set_current_context,
    reset_context,
)

__all__ = [
    # FastAPI 統合
    "AgentRouter",
    "FlowRouter",
    "create_app",
    "create_sse_response",
    # SSE
    "FlowProtocol",
    "SSEConfig",
    "SSEFlowRunner",
    "SimplePipelineProtocol",
    # ==========================================================================
    # NEW: WebSocket 統合
    # ==========================================================================
    "WSEventType",
    "WSEvent",
    "WSCommand",
    "ConnectionState",
    "ConnectionManager",
    "WebSocketManager",
    "create_websocket_router",
    # ==========================================================================
    # NEW: リアルタイム状態同期
    # ==========================================================================
    "RealtimeStateSync",
    "SyncEvent",
    "SyncEventType",
    "ClientConnection",
    # ==========================================================================
    # 工単生成器
    # ==========================================================================
    "TicketGenerator",
    "TicketGeneratorConfig",
    "Ticket",
    "TicketPriority",
    "TicketStatus",
    "TicketType",
    "TicketProviderBase",
    "InMemoryTicketProvider",
    "JiraTicketProvider",
    "ServiceNowTicketProvider",
    # ==========================================================================
    # Context Bridge（L0統合I/F）
    # ==========================================================================
    "FlowContext",
    "ContextBridge",
    "InvocationResult",
    "SourceSystemType",
    "get_current_context",
    "set_current_context",
    "reset_context",
]

