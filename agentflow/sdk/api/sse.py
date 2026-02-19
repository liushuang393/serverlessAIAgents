"""SSE (Server-Sent Events) レスポンス生成.

AgentFlow アプリケーション用の SSE ストリーミング機能を提供。

Example:
    >>> from agentflow.sdk.api.sse import create_sse_response
    >>>
    >>> @router.get("/stream")
    >>> async def stream_endpoint():
    ...     async def events():
    ...         yield {"event_type": "flow.start", ...}
    ...         yield {"event_type": "flow.complete", ...}
    ...     return create_sse_response(events())
"""

import json
from collections.abc import AsyncGenerator
from typing import Any

from fastapi.responses import StreamingResponse
from pydantic import BaseModel


async def sse_event_generator(
    event_source: AsyncGenerator[dict[str, Any] | BaseModel],
    ping_interval: float = 30.0,
) -> AsyncGenerator[str]:
    """SSE イベントジェネレーター.

    AG-UI イベントを SSE 形式に変換して yield。

    Args:
        event_source: イベントソース（dict または Pydantic モデル）
        ping_interval: ping 間隔（秒）

    Yields:
        SSE 形式の文字列
    """
    import asyncio

    last_ping = asyncio.get_event_loop().time()

    async for event in event_source:
        # Pydantic モデルの場合は dict に変換
        event_data = event.model_dump() if isinstance(event, BaseModel) else event

        # イベントタイプを取得
        event_type = event_data.get("event_type", "message")

        # SSE 形式で yield
        yield f"event: {event_type}\n"
        yield f"data: {json.dumps(event_data, ensure_ascii=False)}\n\n"

        # ping チェック
        current_time = asyncio.get_event_loop().time()
        if current_time - last_ping >= ping_interval:
            yield "event: ping\ndata: {}\n\n"
            last_ping = current_time


def create_sse_response(
    event_source: AsyncGenerator[dict[str, Any] | BaseModel],
    ping_interval: float = 30.0,
) -> StreamingResponse:
    """SSE レスポンスを作成.

    AG-UI イベントソースから SSE StreamingResponse を生成。

    Args:
        event_source: イベントソース
        ping_interval: ping 間隔（秒）

    Returns:
        FastAPI StreamingResponse

    Example:
        >>> @router.get("/stream")
        >>> async def stream():
        ...     async def events():
        ...         yield AGUIEvent(event_type="flow.start", ...)
        ...     return create_sse_response(events())
    """
    return StreamingResponse(
        sse_event_generator(event_source, ping_interval),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",  # nginx バッファリング無効化
        },
    )
