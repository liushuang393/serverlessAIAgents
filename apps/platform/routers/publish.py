# -*- coding: utf-8 -*-
"""Publish Router - 発布 API エンドポイント.

POST /api/publish/deploy - 一键发布
GET /api/publish/{publish_id} - 発布ステータス取得
POST /api/publish/{publish_id}/cancel - 発布キャンセル
GET /api/publish/stream/{publish_id} - SSE ストリーム
"""

from __future__ import annotations

import json
from typing import Any

from fastapi import APIRouter, Depends, HTTPException
from fastapi.responses import StreamingResponse

from apps.platform.engine import PlatformEngine
from apps.platform.schemas.publish_schemas import (
    PublishRequest,
    PublishResponse,
    PublishStatus,
)

router = APIRouter(prefix="/api/publish", tags=["publish"])

# 依存性注入用のエンジンインスタンス
_engine: PlatformEngine | None = None


def get_engine() -> PlatformEngine:
    """Platform エンジンを取得."""
    global _engine
    if _engine is None:
        _engine = PlatformEngine()
    return _engine


@router.post("/deploy", response_model=dict[str, Any])
async def deploy(
    request: PublishRequest,
    engine: PlatformEngine = Depends(get_engine),
) -> dict[str, Any]:
    """一键发布を実行.

    非同期で発布を開始し、publish_id を返します。
    ステータスは /api/publish/{publish_id} または SSE で確認できます。

    Args:
        request: 発布リクエスト
        engine: Platform エンジン

    Returns:
        発布開始情報（publish_id を含む）
    """
    # 非同期で発布開始
    publish_id: str | None = None

    async for event in engine.publish(request):
        publish_id = event.publish_id
        # 最初のイベントで返す
        if event.event_type == "phase_start" and event.phase == "validate":
            return {
                "success": True,
                "publish_id": publish_id,
                "message": "Publish started",
                "status": event.status.value,
            }

    # ストリームが即座に終了した場合
    return {
        "success": False,
        "publish_id": publish_id,
        "message": "Publish failed to start",
    }


@router.post("/deploy/sync", response_model=PublishResponse)
async def deploy_sync(
    request: PublishRequest,
    engine: PlatformEngine = Depends(get_engine),
) -> PublishResponse:
    """一键发布を同期実行.

    発布完了まで待機し、結果を返します。

    Args:
        request: 発布リクエスト
        engine: Platform エンジン

    Returns:
        発布レスポンス
    """
    return await engine.publish_sync(request)


@router.get("/{publish_id}", response_model=PublishResponse)
async def get_publish_status(
    publish_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> PublishResponse:
    """発布ステータスを取得.

    Args:
        publish_id: 発布ID
        engine: Platform エンジン

    Returns:
        発布レスポンス

    Raises:
        HTTPException: 発布が見つからない場合
    """
    response = engine.get_publish_status(publish_id)
    if response is None:
        raise HTTPException(status_code=404, detail=f"Publish not found: {publish_id}")
    return response


@router.post("/{publish_id}/cancel")
async def cancel_publish(
    publish_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> dict[str, Any]:
    """発布をキャンセル.

    Args:
        publish_id: 発布ID
        engine: Platform エンジン

    Returns:
        キャンセル結果
    """
    success = await engine.cancel_publish(publish_id)
    if not success:
        raise HTTPException(
            status_code=400,
            detail=f"Cannot cancel publish: {publish_id}",
        )
    return {"success": True, "publish_id": publish_id, "message": "Publish cancelled"}


@router.get("/stream/{publish_id}")
async def stream_publish(
    publish_id: str,
    engine: PlatformEngine = Depends(get_engine),
) -> StreamingResponse:
    """発布イベントを SSE ストリーム.

    Args:
        publish_id: 発布ID
        engine: Platform エンジン

    Returns:
        SSE ストリーム
    """
    response = engine.get_publish_status(publish_id)
    if response is None:
        raise HTTPException(status_code=404, detail=f"Publish not found: {publish_id}")

    async def event_generator():
        """SSE イベントジェネレーター."""
        # 現在のステータスを送信
        data = response.model_dump_json()
        yield f"event: status\ndata: {data}\n\n"

        # 完了済みの場合は終了
        if response.status in [
            PublishStatus.COMPLETED,
            PublishStatus.FAILED,
            PublishStatus.CANCELLED,
        ]:
            yield f"event: done\ndata: {{}}\n\n"

    return StreamingResponse(
        event_generator(),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",
        },
    )
