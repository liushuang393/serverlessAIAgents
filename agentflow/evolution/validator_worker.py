"""Redis streams based validator queue for Evolution V2."""

from __future__ import annotations

import asyncio
import json
import logging
import uuid
from dataclasses import dataclass
from typing import Any

from agentflow.evolution.types import StrategyCapsule, ValidationEvent


_logger = logging.getLogger(__name__)

VALIDATE_STREAM_KEY = "evolution:validate:stream"
VALIDATE_RETRY_KEY = "evolution:validate:retry"
VALIDATE_DLQ_KEY = "evolution:validate:dlq"
VALIDATE_GROUP = "evolution-validator-v1"


@dataclass(slots=True)
class QueueSubmitResult:
    """Queue submit result."""

    queued: bool
    job_id: str | None = None
    error: str | None = None


class RedisValidatorQueue:
    """Redis stream producer/consumer wrapper."""

    def __init__(
        self,
        *,
        redis_url: str,
        max_retries: int = 5,
    ) -> None:
        self._redis_url = redis_url
        self._max_retries = max_retries
        self._redis: Any | None = None

    async def connect(self) -> bool:
        try:
            import redis.asyncio as redis

            self._redis = redis.from_url(self._redis_url, decode_responses=True)
            await self._redis.ping()
            await self._ensure_group()
            return True
        except Exception as exc:
            _logger.warning("validator queue unavailable: %s", exc)
            self._redis = None
            return False

    async def close(self) -> None:
        if self._redis is not None:
            await self._redis.close()
            self._redis = None

    async def submit_candidate(
        self,
        *,
        strategy_id: str,
        capsule: StrategyCapsule,
        metadata: dict[str, Any],
    ) -> QueueSubmitResult:
        if self._redis is None:
            connected = await self.connect()
            if not connected:
                return QueueSubmitResult(queued=False, error="redis_unavailable")
        redis_client = self._redis
        if redis_client is None:
            return QueueSubmitResult(queued=False, error="redis_unavailable")

        job_id = f"val-{uuid.uuid4().hex[:12]}"
        payload = {
            "job_id": job_id,
            "strategy_id": strategy_id,
            "capsule": capsule.model_dump(mode="json"),
            "metadata": metadata,
            "retry_count": 0,
        }
        try:
            await redis_client.xadd(VALIDATE_STREAM_KEY, {"payload": json.dumps(payload)})
            return QueueSubmitResult(queued=True, job_id=job_id)
        except Exception as exc:
            _logger.warning("submit validation candidate failed: %s", exc)
            return QueueSubmitResult(queued=False, error=str(exc))

    async def read_batch(
        self,
        *,
        consumer_name: str,
        count: int = 10,
        block_ms: int = 1000,
    ) -> list[tuple[str, dict[str, Any]]]:
        if self._redis is None:
            connected = await self.connect()
            if not connected:
                return []
        redis_client = self._redis
        if redis_client is None:
            return []

        try:
            response = await redis_client.xreadgroup(
                groupname=VALIDATE_GROUP,
                consumername=consumer_name,
                streams={VALIDATE_STREAM_KEY: ">"},
                count=count,
                block=block_ms,
            )
        except Exception as exc:
            _logger.warning("xreadgroup failed: %s", exc)
            return []

        items: list[tuple[str, dict[str, Any]]] = []
        for _stream, messages in response:
            for message_id, data in messages:
                payload_raw = data.get("payload")
                if not payload_raw:
                    continue
                try:
                    payload = json.loads(payload_raw)
                except json.JSONDecodeError:
                    payload = {"raw": payload_raw}
                items.append((message_id, payload))
        return items

    async def ack(self, message_id: str) -> None:
        if self._redis is None:
            return
        await self._redis.xack(VALIDATE_STREAM_KEY, VALIDATE_GROUP, message_id)

    async def requeue(self, payload: dict[str, Any], *, reason: str) -> None:
        if self._redis is None:
            return

        retry_count = int(payload.get("retry_count", 0)) + 1
        payload["retry_count"] = retry_count
        payload["retry_reason"] = reason

        if retry_count > self._max_retries:
            await self._redis.xadd(VALIDATE_DLQ_KEY, {"payload": json.dumps(payload)})
            return

        delay_seconds = 2 ** min(retry_count, 6)
        await asyncio.sleep(delay_seconds)
        await self._redis.xadd(VALIDATE_RETRY_KEY, {"payload": json.dumps(payload)})
        await self._redis.xadd(VALIDATE_STREAM_KEY, {"payload": json.dumps(payload)})

    async def _ensure_group(self) -> None:
        assert self._redis is not None
        try:
            await self._redis.xgroup_create(
                name=VALIDATE_STREAM_KEY,
                groupname=VALIDATE_GROUP,
                id="0",
                mkstream=True,
            )
        except Exception as exc:
            # BUSYGROUP is expected when group already exists.
            if "BUSYGROUP" not in str(exc):
                raise


class StrategyValidatorWorker:
    """Worker runtime for asynchronous strategy validation."""

    def __init__(
        self,
        *,
        queue: RedisValidatorQueue,
        registry: Any,
        validator: Any,
    ) -> None:
        self._queue = queue
        self._registry = registry
        self._validator = validator

    async def run_once(self, consumer_name: str = "validator-1") -> int:
        items = await self._queue.read_batch(consumer_name=consumer_name)
        processed = 0
        for message_id, payload in items:
            try:
                event = await self._validate_payload(payload)
                await self._registry.update_status(
                    event.strategy_id,
                    self._status_from_validation(event.status),
                )
                await self._queue.ack(message_id)
                processed += 1
            except Exception as exc:
                _logger.warning("validation task failed: %s", exc)
                await self._queue.requeue(payload, reason=str(exc))
                await self._queue.ack(message_id)
        return processed

    async def _validate_payload(self, payload: dict[str, Any]) -> ValidationEvent:
        strategy_id = str(payload.get("strategy_id", ""))
        job_id = str(payload.get("job_id", ""))
        capsule_raw = payload.get("capsule") or {}
        capsule = StrategyCapsule.model_validate(capsule_raw)
        metadata = payload.get("metadata") or {}

        result = await self._validator.validate(capsule=capsule, metadata=metadata)

        return ValidationEvent(
            strategy_id=strategy_id,
            job_id=job_id,
            checks=result.get("checks", {}),
            status=result.get("status", "failed"),
            score_delta=float(result.get("score_delta", 0.0)),
        )

    def _status_from_validation(self, status: str) -> str:
        return "verified" if status == "passed" else "suspect"


class NoopStrategyValidator:
    """Default validator used when no external worker logic is configured."""

    async def validate(
        self,
        *,
        capsule: StrategyCapsule,
        metadata: dict[str, Any],
    ) -> dict[str, Any]:
        checks = {
            "replay": bool(capsule.tool_sequence),
            "tests": True,
            "security": True,
        }
        status = "passed" if all(checks.values()) else "failed"
        return {
            "status": status,
            "checks": checks,
            "score_delta": 0.05 if status == "passed" else -0.15,
        }
