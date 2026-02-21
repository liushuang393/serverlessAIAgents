"""AgentFlow API SDK - FastAPI 路由工厂与通用工具.

このモジュールは AgentFlow アプリケーション用の API 構築ツールを提供します：
- 標準 API エンドポイント自動生成
- SSE レスポンス生成
- 統一エラーハンドリング

Example:
    >>> from agentflow.sdk.api import create_agent_router
    >>>
    >>> router = create_agent_router(
    ...     engine=DecisionEngine,
    ...     prefix="/api",
    ... )
    >>> app.include_router(router)
"""

from agentflow.sdk.api.errors import (
    AgentApiException,
    ErrorCode,
    ErrorResponse,
    error_handler,
)
from agentflow.sdk.api.router import create_agent_router
from agentflow.sdk.api.sse import create_sse_response, sse_event_generator


__all__ = [
    "AgentApiException",
    "ErrorCode",
    "ErrorResponse",
    "create_agent_router",
    "create_sse_response",
    "error_handler",
    "sse_event_generator",
]
