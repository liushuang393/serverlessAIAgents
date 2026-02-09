"""AgentFlow API Layer - 統一前後台交互層.

このモジュールは、フロントエンド・バックエンド間の全交互を
高内聚・低耦合で封装します。

設計原則:
- 統一性: 全アプリが同じインターフェースを使用
- 再利用性: WebSocket/SSE/REST の共通ロジック
- 型安全: Pydantic による厳密な型定義
- 拡張性: カスタムハンドラの追加が容易

アーキテクチャ:
    ┌─────────────────────────────────────────────────────┐
    │                   API Layer                          │
    │  agentflow.api                                       │
    ├─────────────────────────────────────────────────────┤
    │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐ │
    │  │ APIResponse │  │ WSManager   │  │ SSEEmitter  │ │
    │  │ (統一応答)   │  │ (WebSocket) │  │ (SSE)       │ │
    │  └─────────────┘  └─────────────┘  └─────────────┘ │
    │          │                │               │         │
    │          └────────────────┼───────────────┘         │
    │                           │                         │
    │                    ┌──────▼──────┐                  │
    │                    │ RichResponse │                  │
    │                    │ (富文本構築)  │                  │
    │                    └─────────────┘                  │
    └─────────────────────────────────────────────────────┘

使用例:
    >>> from agentflow.api import (
    ...     APIResponse,
    ...     WebSocketHub,
    ...     SSEEmitter,
    ...     RichResponseBuilder,
    ... )
    >>>
    >>> # 統一応答
    >>> response = APIResponse.success(data={"result": "ok"})
    >>>
    >>> # WebSocket
    >>> hub = WebSocketHub()
    >>> await hub.broadcast({"type": "update", "data": {...}})
    >>>
    >>> # 富文本構築
    >>> builder = RichResponseBuilder()
    >>> builder.add_markdown("# Title").add_table(data)
"""

from agentflow.api.response import (
    APIError,
    APIResponse,
    ErrorCode,
    PagedResponse,
    StreamEvent,
    StreamEventType,
)
from agentflow.api.rich_builder import (
    ComponentFactory,
    RichResponseBuilder,
)
from agentflow.api.router_factory import (
    RouterConfig,
    create_agent_router,
    create_sse_endpoint,
    create_websocket_router,
)
from agentflow.api.sse_emitter import (
    SSEConfig,
    SSEEmitter,
    SSEEvent,
)
from agentflow.api.websocket_hub import (
    WebSocketHub,
    WSClient,
    WSHandler,
    WSMessage,
    WSMessageType,
    WSMiddleware,
)


__all__ = [
    "APIError",
    # 統一応答
    "APIResponse",
    "ComponentFactory",
    "ErrorCode",
    "PagedResponse",
    # 富文本構築
    "RichResponseBuilder",
    "RouterConfig",
    "SSEConfig",
    # SSE
    "SSEEmitter",
    "SSEEvent",
    "StreamEvent",
    "StreamEventType",
    "WSClient",
    "WSHandler",
    "WSMessage",
    "WSMessageType",
    "WSMiddleware",
    # WebSocket
    "WebSocketHub",
    # ルーター
    "create_agent_router",
    "create_sse_endpoint",
    "create_websocket_router",
]
