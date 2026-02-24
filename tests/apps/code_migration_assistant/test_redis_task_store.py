"""Redis task store and cross-process event tests."""

from __future__ import annotations

import asyncio
from collections import defaultdict
from typing import Any

import pytest

from apps.code_migration_assistant.backend.task_store import RedisTaskStore


class _FakePubSub:
    def __init__(self, redis: "_FakeRedis") -> None:
        self._redis = redis
        self._queue: asyncio.Queue[dict[str, Any]] = asyncio.Queue()
        self._channels: set[str] = set()
        self._closed = False

    async def subscribe(self, channel: str) -> None:
        self._channels.add(channel)
        self._redis.register_subscriber(channel, self)

    async def get_message(self, timeout: float = 1.0) -> dict[str, Any] | None:
        if self._closed:
            return None
        try:
            return await asyncio.wait_for(self._queue.get(), timeout=timeout)
        except TimeoutError:
            return None

    async def unsubscribe(self, channel: str) -> None:
        self._channels.discard(channel)
        self._redis.unregister_subscriber(channel, self)

    async def close(self) -> None:
        self._closed = True
        for channel in list(self._channels):
            self._redis.unregister_subscriber(channel, self)
        self._channels.clear()

    async def push_message(self, channel: str, data: str) -> None:
        await self._queue.put({"type": "message", "channel": channel, "data": data})


class _FakeRedis:
    def __init__(self) -> None:
        self._kv: dict[str, str] = {}
        self._lists: dict[str, list[str]] = defaultdict(list)
        self._subscribers: dict[str, set[_FakePubSub]] = defaultdict(set)

    async def ping(self) -> bool:
        return True

    async def close(self) -> None:
        return None

    async def setex(self, key: str, _ttl: int, payload: str) -> None:
        self._kv[key] = payload

    async def get(self, key: str) -> str | None:
        return self._kv.get(key)

    async def delete(self, key: str) -> int:
        existed = 1 if key in self._kv else 0
        self._kv.pop(key, None)
        self._lists.pop(key, None)
        return existed

    async def lpush(self, key: str, payload: str) -> None:
        self._lists[key].insert(0, payload)

    async def ltrim(self, key: str, start: int, end: int) -> None:
        if end < 0:
            self._lists[key] = []
            return
        self._lists[key] = self._lists[key][start : end + 1]

    async def expire(self, _key: str, _ttl: int) -> None:
        return None

    async def lrange(self, key: str, start: int, end: int) -> list[str]:
        if end < 0:
            return []
        return self._lists[key][start : end + 1]

    async def publish(self, channel: str, payload: str) -> int:
        subscribers = list(self._subscribers.get(channel, set()))
        for subscriber in subscribers:
            await subscriber.push_message(channel, payload)
        return len(subscribers)

    def pubsub(self, ignore_subscribe_messages: bool = True) -> _FakePubSub:  # noqa: ARG002
        return _FakePubSub(self)

    def register_subscriber(self, channel: str, subscriber: _FakePubSub) -> None:
        self._subscribers[channel].add(subscriber)

    def unregister_subscriber(self, channel: str, subscriber: _FakePubSub) -> None:
        subscribers = self._subscribers.get(channel)
        if subscribers is None:
            return
        subscribers.discard(subscriber)
        if not subscribers:
            self._subscribers.pop(channel, None)


@pytest.mark.asyncio
async def test_redis_task_store_state_and_event_history_are_safe() -> None:
    fake = _FakeRedis()
    store = RedisTaskStore[dict[str, Any]](
        "redis://fake",
        event_history_limit=5,
        client_factory=lambda: fake,
    )
    assert await store.connect() is True

    async def _write(idx: int) -> None:
        await store.set_state(f"task-{idx}", {"task_id": f"task-{idx}", "status": "running"})

    await asyncio.gather(*[_write(idx) for idx in range(30)])

    for idx in range(30):
        state = await store.get_state(f"task-{idx}")
        assert state is not None
        assert state["status"] == "running"

    for idx in range(20):
        await store.append_event("task-events", {"seq": idx})

    events = await store.get_events("task-events", limit=10)
    assert len(events) == 5
    assert events[0]["seq"] == 15
    assert events[-1]["seq"] == 19


@pytest.mark.asyncio
async def test_redis_task_store_publish_subscribe_roundtrip() -> None:
    fake = _FakeRedis()
    store_a = RedisTaskStore[dict[str, Any]]("redis://fake", client_factory=lambda: fake)
    store_b = RedisTaskStore[dict[str, Any]]("redis://fake", client_factory=lambda: fake)
    await store_a.connect()
    await store_b.connect()

    async def _consume_one() -> dict[str, Any]:
        async for payload in store_b.subscribe_events():
            return payload
        return {}

    consumer = asyncio.create_task(_consume_one())
    await asyncio.sleep(0.05)
    event = {"event_type": "log", "message": "hello"}
    await store_a.publish_event("task-1", event)
    payload = await asyncio.wait_for(consumer, timeout=1.0)

    assert payload["task_id"] == "task-1"
    assert payload["event"]["message"] == "hello"
    assert payload["producer_id"] == store_a.instance_id


@pytest.mark.asyncio
async def test_redis_task_store_pop_state() -> None:
    fake = _FakeRedis()
    store = RedisTaskStore[dict[str, Any]]("redis://fake", client_factory=lambda: fake)
    await store.connect()

    await store.set_state("task-pop", {"task_id": "task-pop", "status": "queued"})
    popped = await store.pop_state("task-pop")
    assert popped is not None
    assert popped["status"] == "queued"
    assert await store.get_state("task-pop") is None


@pytest.mark.asyncio
async def test_redis_task_store_command_bus_roundtrip() -> None:
    fake = _FakeRedis()
    store_a = RedisTaskStore[dict[str, Any]]("redis://fake", client_factory=lambda: fake)
    store_b = RedisTaskStore[dict[str, Any]]("redis://fake", client_factory=lambda: fake)
    await store_a.connect()
    await store_b.connect()

    async def _consume_command() -> dict[str, Any]:
        async for payload in store_b.subscribe_commands():
            return payload
        return {}

    consumer = asyncio.create_task(_consume_command())
    await asyncio.sleep(0.05)
    command_id = "cmd-1"
    await store_a.publish_command(
        {
            "task_id": "task-1",
            "command_id": command_id,
            "requester_instance_id": store_a.instance_id,
            "command": {"command": "provide_business_fact", "actor": "operator"},
        }
    )
    command_payload = await asyncio.wait_for(consumer, timeout=1.0)
    assert command_payload["task_id"] == "task-1"
    assert command_payload["command_id"] == command_id

    result = {
        "task_id": "task-1",
        "command_id": command_id,
        "requester_instance_id": store_a.instance_id,
        "ok": True,
        "payload": {"status": "accepted"},
    }
    await store_b.set_command_result(command_id, result)
    popped = await store_a.pop_command_result(command_id)
    assert popped == result
    assert await store_a.pop_command_result(command_id) is None
