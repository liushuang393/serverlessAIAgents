# -*- coding: utf-8 -*-
"""Task store abstractions for backend runtime state."""

from __future__ import annotations

import asyncio
import importlib
import json
import logging
import threading
import uuid
from collections.abc import AsyncIterator, Callable
from typing import Any, Generic, Protocol, TypeVar

T = TypeVar("T")

logger = logging.getLogger(__name__)


class TaskStore(Protocol[T]):
    """Task runtime store contract."""

    def set(self, task_id: str, value: T) -> None:
        """Persist task runtime."""

    def get(self, task_id: str) -> T | None:
        """Get task runtime by id."""

    def pop(self, task_id: str) -> T | None:
        """Remove task runtime."""

    def clear(self) -> None:
        """Clear all task runtimes."""

    def as_dict(self) -> dict[str, T]:
        """Expose dictionary view for compatibility."""


class InMemoryTaskStore(Generic[T]):
    """In-memory default store implementation."""

    def __init__(self) -> None:
        self._tasks: dict[str, T] = {}
        self._lock = threading.RLock()

    def set(self, task_id: str, value: T) -> None:
        with self._lock:
            self._tasks[task_id] = value

    def get(self, task_id: str) -> T | None:
        with self._lock:
            return self._tasks.get(task_id)

    def pop(self, task_id: str) -> T | None:
        with self._lock:
            return self._tasks.pop(task_id, None)

    def clear(self) -> None:
        with self._lock:
            self._tasks.clear()

    def as_dict(self) -> dict[str, T]:
        return self._tasks


RedisClientFactory = Callable[[], Any]


class RedisTaskStore(Generic[T]):
    """Redis-backed distributed task state + event store."""

    def __init__(
        self,
        redis_url: str,
        *,
        key_prefix: str = "code_migration_assistant",
        state_ttl_seconds: int = 86400,
        event_history_limit: int = 200,
        client_factory: RedisClientFactory | None = None,
    ) -> None:
        self._redis_url = redis_url
        self._key_prefix = key_prefix
        self._state_ttl_seconds = state_ttl_seconds
        self._event_history_limit = event_history_limit
        self._client_factory = client_factory
        self._client: Any = None
        self._connected = False
        self._connect_lock = asyncio.Lock()
        self._state_lock = asyncio.Lock()
        self._instance_id = uuid.uuid4().hex
        self._local_cache: dict[str, T] = {}

    @property
    def instance_id(self) -> str:
        """Current process instance id."""
        return self._instance_id

    @property
    def event_channel(self) -> str:
        """Redis pubsub event channel."""
        return f"{self._key_prefix}:events"

    @property
    def command_channel(self) -> str:
        """Redis pubsub command channel."""
        return f"{self._key_prefix}:commands"

    async def connect(self) -> bool:
        """Connect to Redis.

        Returns:
            True when connected and ping succeeded.
        """
        async with self._connect_lock:
            if self._connected and self._client is not None:
                return True
            try:
                client = self._create_client()
                await client.ping()
                self._client = client
                self._connected = True
                logger.info("RedisTaskStore connected: %s", self._redis_url)
                return True
            except Exception:  # noqa: BLE001
                logger.warning("RedisTaskStore connection failed", exc_info=True)
                self._client = None
                self._connected = False
                return False

    async def close(self) -> None:
        """Close Redis connection."""
        async with self._connect_lock:
            client = self._client
            self._client = None
            self._connected = False
            if client is not None:
                await client.close()

    async def is_connected(self) -> bool:
        """Check Redis connection status."""
        return self._connected and self._client is not None

    async def set_state(self, task_id: str, value: T) -> None:
        """Persist task state to Redis and local cache."""
        client = await self._require_client()
        payload = self._to_json(value)
        key = self._state_key(task_id)
        try:
            async with self._state_lock:
                self._local_cache[task_id] = value
                await client.setex(key, self._state_ttl_seconds, payload)
        except asyncio.CancelledError:
            raise
        except Exception:  # noqa: BLE001
            await self._mark_disconnected(client)
            raise

    async def get_state(self, task_id: str) -> T | None:
        """Get task state from local cache or Redis."""
        async with self._state_lock:
            if task_id in self._local_cache:
                return self._local_cache[task_id]

        client = await self._require_client()
        key = self._state_key(task_id)
        try:
            raw = await client.get(key)
        except asyncio.CancelledError:
            raise
        except Exception:  # noqa: BLE001
            await self._mark_disconnected(client)
            raise
        if raw is None:
            return None
        value = self._from_json(raw)
        async with self._state_lock:
            self._local_cache[task_id] = value
        return value

    async def pop_state(self, task_id: str) -> T | None:
        """Remove task state from both local cache and Redis."""
        client = await self._require_client()
        key = self._state_key(task_id)
        previous = await self.get_state(task_id)
        try:
            async with self._state_lock:
                self._local_cache.pop(task_id, None)
                await client.delete(key)
        except asyncio.CancelledError:
            raise
        except Exception:  # noqa: BLE001
            await self._mark_disconnected(client)
            raise
        return previous

    async def append_event(self, task_id: str, event: dict[str, Any]) -> None:
        """Append task event to Redis history list."""
        client = await self._require_client()
        key = self._event_list_key(task_id)
        payload = json.dumps(event, ensure_ascii=False)
        try:
            await client.lpush(key, payload)
            await client.ltrim(key, 0, self._event_history_limit - 1)
            await client.expire(key, self._state_ttl_seconds)
        except asyncio.CancelledError:
            raise
        except Exception:  # noqa: BLE001
            await self._mark_disconnected(client)
            raise

    async def get_events(self, task_id: str, limit: int = 30) -> list[dict[str, Any]]:
        """Get latest task events ordered oldest -> newest."""
        client = await self._require_client()
        key = self._event_list_key(task_id)
        safe_limit = max(1, limit)
        try:
            raw_events = await client.lrange(key, 0, safe_limit - 1)
        except asyncio.CancelledError:
            raise
        except Exception:  # noqa: BLE001
            await self._mark_disconnected(client)
            raise
        normalized: list[dict[str, Any]] = []
        for raw in reversed(raw_events):
            if not isinstance(raw, str):
                continue
            try:
                event = json.loads(raw)
            except json.JSONDecodeError:
                continue
            if isinstance(event, dict):
                normalized.append(event)
        return normalized

    async def publish_event(self, task_id: str, event: dict[str, Any]) -> None:
        """Publish event for cross-process websocket fanout."""
        client = await self._require_client()
        payload = {
            "task_id": task_id,
            "event": event,
            "producer_id": self._instance_id,
        }
        try:
            await client.publish(self.event_channel, json.dumps(payload, ensure_ascii=False))
        except asyncio.CancelledError:
            raise
        except Exception:  # noqa: BLE001
            await self._mark_disconnected(client)
            raise

    async def publish_command(self, command: dict[str, Any]) -> None:
        """Publish remote command request."""
        client = await self._require_client()
        try:
            await client.publish(self.command_channel, json.dumps(command, ensure_ascii=False))
        except asyncio.CancelledError:
            raise
        except Exception:  # noqa: BLE001
            await self._mark_disconnected(client)
            raise

    async def set_command_result(self, command_id: str, result: dict[str, Any]) -> None:
        """Persist remote command result for requester to consume."""
        client = await self._require_client()
        key = self._command_result_key(command_id)
        payload = json.dumps(result, ensure_ascii=False)
        try:
            await client.setex(key, self._state_ttl_seconds, payload)
        except asyncio.CancelledError:
            raise
        except Exception:  # noqa: BLE001
            await self._mark_disconnected(client)
            raise

    async def pop_command_result(self, command_id: str) -> dict[str, Any] | None:
        """Get and delete remote command result."""
        client = await self._require_client()
        key = self._command_result_key(command_id)
        try:
            raw = await client.get(key)
            if raw is None:
                return None
            await client.delete(key)
        except asyncio.CancelledError:
            raise
        except Exception:  # noqa: BLE001
            await self._mark_disconnected(client)
            raise

        try:
            parsed = json.loads(raw)
        except json.JSONDecodeError:
            return None
        return parsed if isinstance(parsed, dict) else None

    async def subscribe_events(self) -> AsyncIterator[dict[str, Any]]:
        """Subscribe to cross-process events."""
        async for payload in self._subscribe_json_channel(self.event_channel):
            yield payload

    async def subscribe_commands(self) -> AsyncIterator[dict[str, Any]]:
        """Subscribe to cross-process command requests."""
        async for payload in self._subscribe_json_channel(self.command_channel):
            yield payload

    async def _subscribe_json_channel(self, channel: str) -> AsyncIterator[dict[str, Any]]:
        """Subscribe a JSON payload channel."""
        client = await self._require_client()
        pubsub = client.pubsub(ignore_subscribe_messages=True)
        await pubsub.subscribe(channel)
        try:
            while True:
                try:
                    message = await pubsub.get_message(timeout=1.0)
                except asyncio.CancelledError:
                    raise
                except Exception:  # noqa: BLE001
                    await self._mark_disconnected(client)
                    raise
                if message is None:
                    await asyncio.sleep(0.05)
                    continue
                if str(message.get("type")) != "message":
                    continue
                data = message.get("data")
                if not isinstance(data, str):
                    continue
                try:
                    payload = json.loads(data)
                except json.JSONDecodeError:
                    continue
                if isinstance(payload, dict):
                    yield payload
        finally:
            try:
                await pubsub.unsubscribe(channel)
            except Exception:  # noqa: BLE001
                logger.debug("Redis pubsub unsubscribe failed", exc_info=True)
            try:
                await pubsub.close()
            except Exception:  # noqa: BLE001
                logger.debug("Redis pubsub close failed", exc_info=True)

    def _create_client(self) -> Any:
        if self._client_factory is not None:
            return self._client_factory()
        redis_module = importlib.import_module("redis.asyncio")
        return redis_module.from_url(
            self._redis_url,
            encoding="utf-8",
            decode_responses=True,
        )

    async def _require_client(self) -> Any:
        if not await self.is_connected():
            connected = await self.connect()
            if not connected:
                msg = f"RedisTaskStore is not connected: {self._redis_url}"
                raise ConnectionError(msg)
        return self._client

    async def _mark_disconnected(self, client: Any) -> None:
        async with self._connect_lock:
            if client is not self._client:
                return
            self._client = None
            self._connected = False
        try:
            await client.close()
        except Exception:  # noqa: BLE001
            logger.debug("Redis client close failed during disconnect mark", exc_info=True)

    def _state_key(self, task_id: str) -> str:
        return f"{self._key_prefix}:state:{task_id}"

    def _event_list_key(self, task_id: str) -> str:
        return f"{self._key_prefix}:events:{task_id}"

    def _command_result_key(self, command_id: str) -> str:
        return f"{self._key_prefix}:command_result:{command_id}"

    @staticmethod
    def _to_json(value: Any) -> str:
        return json.dumps(value, ensure_ascii=False, default=str)

    @staticmethod
    def _from_json(raw: str) -> Any:
        return json.loads(raw)
