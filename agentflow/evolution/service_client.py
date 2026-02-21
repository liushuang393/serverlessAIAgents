"""HTTP client for external Strategy Service."""

from __future__ import annotations

from typing import Any, cast

import httpx

from agentflow.evolution.types import (
    ExecutionEvent,
    ScoreUpdateRequest,
    StrategyCapsule,
    StrategyRecord,
    StrategySearchRequest,
    StrategySearchResponse,
    ValidationEvent,
)


class StrategyServiceClient:
    """Client for `/v1/*` strategy service endpoints."""

    def __init__(
        self,
        *,
        base_url: str,
        api_key: str | None = None,
        timeout_seconds: float = 10.0,
    ) -> None:
        headers = {"Content-Type": "application/json"}
        if api_key:
            headers["Authorization"] = f"Bearer {api_key}"

        self._base_url = base_url.rstrip("/")
        self._client = httpx.AsyncClient(
            base_url=self._base_url,
            headers=headers,
            timeout=timeout_seconds,
        )

    async def close(self) -> None:
        await self._client.aclose()

    async def health(self) -> dict[str, Any]:
        response = await self._client.get("/v1/health")
        response.raise_for_status()
        return cast("dict[str, Any]", response.json())

    async def search(self, request: StrategySearchRequest) -> StrategySearchResponse:
        response = await self._client.post(
            "/v1/strategies/search",
            json=request.model_dump(mode="json"),
        )
        response.raise_for_status()
        return StrategySearchResponse.model_validate(response.json())

    async def register(self, capsule: StrategyCapsule) -> StrategyRecord:
        response = await self._client.post(
            "/v1/strategies/register",
            json=capsule.model_dump(mode="json"),
        )
        response.raise_for_status()
        return StrategyRecord.model_validate(response.json())

    async def score_update(self, request: ScoreUpdateRequest) -> StrategyRecord | None:
        response = await self._client.post(
            f"/v1/strategies/{request.strategy_id}/score-update",
            json=request.model_dump(mode="json"),
        )
        response.raise_for_status()
        payload = cast("dict[str, Any] | None", response.json())
        if not payload:
            return None
        return StrategyRecord.model_validate(payload)

    async def report_validation(self, event: ValidationEvent) -> dict[str, Any]:
        response = await self._client.post(
            "/v1/validations/result",
            json=event.model_dump(mode="json"),
        )
        response.raise_for_status()
        return cast("dict[str, Any]", response.json())

    async def batch_events(self, events: list[ExecutionEvent]) -> dict[str, Any]:
        response = await self._client.post(
            "/v1/executions/events:batch",
            json={"events": [event.model_dump(mode="json") for event in events]},
        )
        response.raise_for_status()
        return cast("dict[str, Any]", response.json())
