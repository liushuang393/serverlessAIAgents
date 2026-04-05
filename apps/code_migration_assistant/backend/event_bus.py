"""WebSocket / Redis イベント配信."""

from __future__ import annotations

import logging
import time
from typing import Any

from apps.code_migration_assistant.backend.task_runtime import (
    CONTRACT_VERSION,
    TaskRuntime,
    local_instance_id,
    redis_store,
    task_websockets,
)

logger = logging.getLogger("migration_server")


def normalize_stream_event(task_id: str, raw_event: dict[str, Any]) -> dict[str, Any]:
    """内部イベントを AG-UI 互換イベントへ正規化する."""
    event = dict(raw_event)

    raw_event_type = str(event.get("event_type") or "").strip()
    raw_event_name = str(event.get("event") or "").strip()

    event_map = {
        "node_start": "node.start",
        "node_complete": "node.complete",
        "flow_start": "flow.start",
        "flow_complete": "flow.complete",
        "flow_error": "flow.error",
    }

    if raw_event_type:
        event_type = event_map.get(raw_event_type, raw_event_type)
    else:
        event_type = event_map.get(raw_event_name, "log")

    timestamp_raw = event.get("timestamp")
    timestamp = float(timestamp_raw) if isinstance(timestamp_raw, (int, float)) else time.time()

    event.pop("event", None)
    event["event_type"] = event_type
    event["flow_id"] = task_id
    event["timestamp"] = timestamp
    event["contract_version"] = CONTRACT_VERSION

    return event


async def send_ws_event(task_id: str, event: dict[str, Any]) -> None:
    """タスクに紐づく全 WebSocket へイベント送信する."""
    subscribers = task_websockets.get(task_id)
    if subscribers is None:
        return

    sockets = list(subscribers)
    stale: list[Any] = []
    for websocket in sockets:
        try:
            await websocket.send_json(event)
        except Exception:
            stale.append(websocket)

    for websocket in stale:
        subscribers.discard(websocket)
    if not subscribers:
        task_websockets.pop(task_id, None)


async def append_distributed_event(task_id: str, event: dict[str, Any]) -> None:
    """分散ストアへイベント履歴を記録し、他プロセスへ配信する."""
    store = redis_store
    if store is None:
        return

    try:
        if hasattr(store, "append_event"):
            await store.append_event(task_id, event)
        if hasattr(store, "publish_event"):
            await store.publish_event(task_id, event)
    except Exception:
        logger.warning("distributed event publish failed", exc_info=True)


async def emit_event(task_id: str, raw_event: dict[str, Any]) -> None:
    """イベントを正規化し、WebSocket と分散ストアへ配信する."""
    normalized = normalize_stream_event(task_id, raw_event)
    await send_ws_event(task_id, normalized)
    await append_distributed_event(task_id, normalized)


async def sync_runtime_state(runtime: TaskRuntime) -> None:
    """ランタイム状態を分散ストアへ同期する."""
    store = redis_store
    if store is None or not hasattr(store, "set_state"):
        return

    try:
        await store.set_state(runtime.task_id, runtime.to_state(owner_instance=local_instance_id()))
    except Exception:
        logger.warning("distributed state sync failed", exc_info=True)


async def redis_event_listener() -> None:
    """分散イベントチャネルを購読し、ローカル WebSocket へ中継する."""
    store = redis_store
    if store is None or not hasattr(store, "subscribe_events"):
        return

    async for payload in store.subscribe_events():
        task_id = payload.get("task_id")
        producer_id = payload.get("producer_id")
        event = payload.get("event")
        if not isinstance(task_id, str) or not isinstance(event, dict):
            continue
        if isinstance(producer_id, str) and producer_id == local_instance_id():
            continue
        await send_ws_event(task_id, event)
