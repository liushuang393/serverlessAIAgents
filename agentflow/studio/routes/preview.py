"""プレビューAPI ルート.

ワークフローのリアルタイム実行とデバッグ機能。
"""

from __future__ import annotations

import json
import time
from typing import TYPE_CHECKING, Any

from fastapi import APIRouter
from fastapi.responses import StreamingResponse

from agentflow.studio.models import PreviewRunRequest, PreviewRunResponse


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


def create_preview_router() -> APIRouter:
    """プレビューAPIルーターを作成.

    Returns:
        FastAPI APIRouter
    """
    router = APIRouter(prefix="/api/preview", tags=["preview"])

    @router.post("/run")
    async def preview_run(request: PreviewRunRequest) -> PreviewRunResponse:
        """ワークフローをプレビュー実行.

        v0.4.0: PreviewService を使用した統一実装。
        """
        from agentflow.services import PreviewService

        start_time = time.time()
        logs: list[dict[str, Any]] = []
        result: dict[str, Any] | None = None

        try:
            service = PreviewService()

            async for event in service.run_stream(request.workflow, request.input_data):
                logs.append(
                    {
                        "type": event.type,
                        "message": event.message,
                        "timestamp": time.time(),
                        "data": event.data,
                    }
                )

                if event.type == "complete" and event.data:
                    result = event.data
                elif event.type == "error":
                    duration_ms = (time.time() - start_time) * 1000
                    return PreviewRunResponse(
                        status="error",
                        result=None,
                        logs=logs,
                        duration_ms=duration_ms,
                        error=event.message,
                    )

            duration_ms = (time.time() - start_time) * 1000

            return PreviewRunResponse(
                status="success",
                result=result,
                logs=logs,
                duration_ms=duration_ms,
                error=None,
            )

        except Exception as e:
            duration_ms = (time.time() - start_time) * 1000
            logs.append(
                {
                    "type": "error",
                    "message": str(e),
                    "timestamp": time.time(),
                }
            )
            return PreviewRunResponse(
                status="error",
                result=None,
                logs=logs,
                duration_ms=duration_ms,
                error=str(e),
            )

    @router.post("/stream")
    async def preview_stream(request: PreviewRunRequest) -> StreamingResponse:
        """ワークフローをストリーム実行.

        SSE (Server-Sent Events) でリアルタイムに実行状況を返します。
        """
        from agentflow.services import PreviewService

        async def event_generator() -> AsyncIterator[str]:
            try:
                service = PreviewService()

                async for event in service.run_stream(
                    request.workflow,
                    request.input_data,
                ):
                    yield f"data: {json.dumps(event.to_dict())}\n\n"

            except Exception as e:
                yield f"data: {json.dumps({'type': 'error', 'message': str(e)})}\n\n"

        return StreamingResponse(event_generator(), media_type="text/event-stream")

    @router.post("/validate")
    async def preview_validate(request: PreviewRunRequest) -> dict[str, Any]:
        """ワークフローを検証.

        実行せずに構文やノード接続をチェック。
        """
        from agentflow.services import PreviewService

        service = PreviewService()
        errors = await service.validate(request.workflow)

        return {
            "valid": len(errors) == 0,
            "errors": errors,
            "warnings": [],
        }

    return router
