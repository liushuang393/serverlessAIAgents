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

# ==========================================================================
# NEW: Context Bridge（L0統合I/F）
# ==========================================================================
from agentflow.integrations.context_bridge import (
    ContextBridge,
    FlowContext,
    InvocationResult,
    SourceSystemType,
    get_current_context,
    reset_context,
    set_current_context,
)
from agentflow.integrations.fastapi_integration import (
    AgentRouter,
    FlowRouter,
    create_app,
    create_sse_response,
)

# NEW: リアルタイム状態同期
from agentflow.integrations.realtime_sync import (
    ClientConnection,
    RealtimeStateSync,
    SyncEvent,
    SyncEventType,
)
from agentflow.integrations.sse_flow_runner import (
    FlowProtocol,
    SimplePipelineProtocol,
    SSEConfig,
    SSEFlowRunner,
)

# 新規追加: 工単生成器
from agentflow.integrations.ticket_generator import (
    InMemoryTicketProvider,
    JiraTicketProvider,
    ServiceNowTicketProvider,
    Ticket,
    TicketGenerator,
    TicketGeneratorConfig,
    TicketPriority,
    TicketProviderBase,
    TicketStatus,
    TicketType,
)

# NEW: WebSocket 統合
from agentflow.integrations.websocket_integration import (
    ConnectionManager,
    ConnectionState,
    WebSocketManager,
    WSCommand,
    WSEvent,
    WSEventType,
    create_websocket_router,
)


__all__ = [
    # FastAPI 統合
    "AgentRouter",
    "ClientConnection",
    "ConnectionManager",
    "ConnectionState",
    "ContextBridge",
    # ==========================================================================
    # Context Bridge（L0統合I/F）
    # ==========================================================================
    "FlowContext",
    # SSE
    "FlowProtocol",
    "FlowRouter",
    "InMemoryTicketProvider",
    "InvocationResult",
    "JiraTicketProvider",
    # ==========================================================================
    # NEW: リアルタイム状態同期
    # ==========================================================================
    "RealtimeStateSync",
    "SSEConfig",
    "SSEFlowRunner",
    "ServiceNowTicketProvider",
    "SimplePipelineProtocol",
    "SourceSystemType",
    "SyncEvent",
    "SyncEventType",
    "Ticket",
    # ==========================================================================
    # 工単生成器
    # ==========================================================================
    "TicketGenerator",
    "TicketGeneratorConfig",
    "TicketPriority",
    "TicketProviderBase",
    "TicketStatus",
    "TicketType",
    "WSCommand",
    "WSEvent",
    # ==========================================================================
    # NEW: WebSocket 統合
    # ==========================================================================
    "WSEventType",
    "WebSocketManager",
    "create_app",
    "create_sse_response",
    "create_websocket_router",
    "get_current_context",
    "reset_context",
    "set_current_context",
]
