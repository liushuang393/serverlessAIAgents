"""In-process Strategy Service adapter for local development."""

from __future__ import annotations

from agentflow.evolution.registry import StrategyRegistry
from agentflow.evolution.types import (
    ScoreUpdateRequest,
    StrategyCapsule,
    StrategyRecord,
    StrategySearchRequest,
    StrategySearchResponse,
    StrategyStatus,
)


class InProcessStrategyServiceAdapter:
    """Wraps StrategyRegistry with service-like methods."""

    def __init__(self, registry: StrategyRegistry | None = None) -> None:
        self._registry = registry or StrategyRegistry()

    async def search(self, request: StrategySearchRequest) -> StrategySearchResponse:
        return await self._registry.search(request)

    async def register(
        self,
        capsule: StrategyCapsule,
        *,
        status: StrategyStatus = StrategyStatus.CANDIDATE,
    ) -> StrategyRecord:
        return await self._registry.register(capsule, status=status)

    async def score_update(self, request: ScoreUpdateRequest) -> StrategyRecord | None:
        return await self._registry.score_update(request)

    async def health(self) -> dict[str, str]:
        return {"status": "ok", "backend": "in_process_registry"}
