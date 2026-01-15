# -*- coding: utf-8 -*-
"""Router Factory - 統一ルーター生成.

設計原則:
- ワンライナー: 最小コードでルーター作成
- 統一性: 全アプリで同じパターン
- 拡張性: カスタムハンドラの追加が容易

使用例:
    >>> from agentflow.api import create_agent_router, create_websocket_router
    >>>
    >>> # Agent ルーター（REST + SSE）
    >>> router = create_agent_router(MyAgent, prefix="/api/agent")
    >>> app.include_router(router)
    >>>
    >>> # WebSocket ルーター
    >>> ws_router = create_websocket_router(hub, prefix="/ws")
    >>> app.include_router(ws_router)
"""

from __future__ import annotations

import json
import logging
from collections.abc import AsyncIterator, Callable
from dataclasses import dataclass, field
from typing import Any, TYPE_CHECKING

from pydantic import BaseModel, Field

from agentflow.api.response import APIResponse, ErrorCode
from agentflow.api.sse_emitter import SSEEmitter

if TYPE_CHECKING:
    from fastapi import APIRouter, WebSocket
    from agentflow.api.websocket_hub import WebSocketHub
    from agentflow.core.agent_block import AgentBlock


logger = logging.getLogger(__name__)


@dataclass
class RouterConfig:
    """ルーター設定."""

    # パス設定
    prefix: str = ""
    tags: list[str] = field(default_factory=list)

    # 機能フラグ
    enable_stream: bool = True
    enable_health: bool = True
    enable_schema: bool = True

    # セキュリティ
    require_auth: bool = False
    auth_dependency: Any | None = None


class AgentRequest(BaseModel):
    """Agent リクエスト共通モデル."""

    input: dict[str, Any] = Field(default_factory=dict, description="入力データ")
    options: dict[str, Any] = Field(default_factory=dict, description="オプション")
    session_id: str | None = Field(None, description="セッションID")


def create_agent_router(
    agent_class: type,
    config: RouterConfig | None = None,
) -> "APIRouter":
    """Agent 用ルーターを作成.

    自動生成されるエンドポイント:
    - POST /run: 同期実行
    - POST /run/stream: SSE ストリーム実行
    - GET /health: ヘルスチェック
    - GET /schema: Agent スキーマ

    Args:
        agent_class: Agent クラス
        config: ルーター設定

    Returns:
        FastAPI Router
    """
    from fastapi import APIRouter
    from fastapi.responses import StreamingResponse

    config = config or RouterConfig()
    router = APIRouter(prefix=config.prefix, tags=config.tags or ["Agent"])

    # Agent インスタンス（遅延初期化）
    _agent_instance = None

    def get_agent():
        nonlocal _agent_instance
        if _agent_instance is None:
            _agent_instance = agent_class()
        return _agent_instance

    @router.post("/run")
    async def run_agent(request: AgentRequest) -> dict[str, Any]:
        """Agent を同期実行."""
        try:
            agent = get_agent()
            result = await agent.run(request.input)
            return APIResponse.ok(data=result).model_dump()
        except Exception as e:
            logger.exception("Agent execution error: %s", e)
            return APIResponse.fail(
                code=ErrorCode.AGENT_ERROR,
                message=str(e),
            ).model_dump()

    if config.enable_stream:
        @router.post("/run/stream")
        async def run_agent_stream(request: AgentRequest) -> StreamingResponse:
            """Agent を SSE ストリームで実行."""
            emitter = SSEEmitter()

            async def generate() -> AsyncIterator[str]:
                try:
                    agent = get_agent()
                    yield emitter.start()

                    if hasattr(agent, "run_stream"):
                        async for event in agent.run_stream(request.input):
                            if event.get("type") == "progress":
                                yield emitter.progress(
                                    event.get("progress", 0),
                                    event.get("message", ""),
                                )
                            elif event.get("type") == "result":
                                yield emitter.data(event.get("data", {}))
                    else:
                        # ストリーム非対応の場合は同期実行
                        result = await agent.run(request.input)
                        yield emitter.data(result)

                    yield emitter.complete()

                except Exception as e:
                    logger.exception("Stream error: %s", e)
                    yield emitter.error(str(e))

            return StreamingResponse(
                generate(),
                media_type="text/event-stream",
            )

    if config.enable_health:
        @router.get("/health")
        async def health_check() -> dict[str, str]:
            """ヘルスチェック."""
            return {"status": "ok", "agent": agent_class.__name__}

    if config.enable_schema:
        @router.get("/schema")
        async def get_schema() -> dict[str, Any]:
            """Agent スキーマ取得."""
            agent = get_agent()
            if hasattr(agent, "get_definition"):
                return agent.get_definition()
            return {
                "name": agent_class.__name__,
                "type": "agent",
            }

    return router


def create_websocket_router(
    hub: "WebSocketHub",
    config: RouterConfig | None = None,
) -> "APIRouter":
    """WebSocket 用ルーターを作成.

    Args:
        hub: WebSocket Hub インスタンス
        config: ルーター設定

    Returns:
        FastAPI Router
    """
    from fastapi import APIRouter, WebSocket, WebSocketDisconnect

    config = config or RouterConfig()
    router = APIRouter(prefix=config.prefix, tags=config.tags or ["WebSocket"])

    @router.websocket("/{client_id}")
    async def websocket_endpoint(websocket: WebSocket, client_id: str):
        """WebSocket エンドポイント."""
        await hub.handle_connection(websocket, client_id)

    @router.get("/clients")
    async def get_clients() -> dict[str, Any]:
        """接続中クライアント一覧."""
        return {
            "count": hub.client_count,
            "clients": list(hub._clients.keys()),
        }

    return router


def create_sse_endpoint(
    handler: Callable[..., AsyncIterator[dict[str, Any]]],
    path: str = "/stream",
) -> Callable:
    """SSE エンドポイントを作成.

    Args:
        handler: 非同期ジェネレータ関数
        path: エンドポイントパス

    Returns:
        FastAPI エンドポイント関数
    """
    from fastapi.responses import StreamingResponse

    async def endpoint(**kwargs: Any) -> StreamingResponse:
        emitter = SSEEmitter()

        async def generate() -> AsyncIterator[str]:
            try:
                yield emitter.start()
                async for event in handler(**kwargs):
                    if isinstance(event, dict):
                        event_type = event.get("type", "data")
                        if event_type == "progress":
                            yield emitter.progress(
                                event.get("value", 0),
                                event.get("message", ""),
                            )
                        elif event_type == "error":
                            yield emitter.error(event.get("message", ""))
                        else:
                            yield emitter.data(event)
                    else:
                        yield emitter.data({"value": event})
                yield emitter.complete()
            except Exception as e:
                logger.exception("SSE error: %s", e)
                yield emitter.error(str(e))

        return StreamingResponse(
            generate(),
            media_type="text/event-stream",
        )

    return endpoint


__all__ = [
    "create_agent_router",
    "create_websocket_router",
    "create_sse_endpoint",
    "RouterConfig",
]
