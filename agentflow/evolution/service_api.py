"""FastAPI Strategy Service endpoints for Evolution V2."""

from __future__ import annotations

from typing import Any

from fastapi import APIRouter, FastAPI
from pydantic import BaseModel, Field

from agentflow.evolution.recorder import InMemoryExecutionRecorder
from agentflow.evolution.registry import StrategyRegistry
from agentflow.evolution.types import (
    ExecutionEvent,
    ScoreUpdateRequest,
    StrategyCapsule,
    StrategySearchRequest,
    StrategyStatus,
    ValidationEvent,
)


class BatchEventsRequest(BaseModel):
    """Batch execution events payload."""

    events: list[ExecutionEvent] = Field(default_factory=list)


class StrategyService:
    """In-process Strategy Service state holder."""

    def __init__(
        self,
        *,
        registry: StrategyRegistry | None = None,
        recorder: InMemoryExecutionRecorder | None = None,
    ) -> None:
        self.registry = registry or StrategyRegistry()
        self.recorder = recorder or InMemoryExecutionRecorder()

    def create_router(self) -> APIRouter:
        router = APIRouter(prefix="/v1", tags=["evolution-strategy-service"])

        @router.get("/health")
        async def health() -> dict[str, str]:
            return {"status": "ok", "service": "strategy-service"}

        @router.post("/strategies/search")
        async def search_strategies(request: StrategySearchRequest) -> dict[str, Any]:
            response = await self.registry.search(request)
            return response.model_dump(mode="json")

        @router.post("/strategies/register")
        async def register_strategy(capsule: StrategyCapsule) -> dict[str, Any]:
            record = await self.registry.register(capsule)
            return record.model_dump(mode="json")

        @router.post("/strategies/{strategy_id}/score-update")
        async def score_update(strategy_id: str, request: ScoreUpdateRequest) -> dict[str, Any] | None:
            payload = request.model_copy(update={"strategy_id": strategy_id})
            record = await self.registry.score_update(payload)
            if record is None:
                return None
            return record.model_dump(mode="json")

        @router.post("/validations/result")
        async def validation_result(event: ValidationEvent) -> dict[str, Any]:
            status = StrategyStatus.VERIFIED if event.status == "passed" else StrategyStatus.SUSPECT
            await self.registry.update_status(event.strategy_id, status)
            return {
                "accepted": True,
                "strategy_id": event.strategy_id,
                "status": status.value,
            }

        @router.post("/executions/events:batch")
        async def batch_events(request: BatchEventsRequest) -> dict[str, Any]:
            await self.recorder.append_events(request.events)
            run_ids = sorted({event.run_id for event in request.events})
            return {
                "accepted": len(request.events),
                "run_ids": run_ids,
            }

        return router


def create_strategy_service_app(service: StrategyService | None = None) -> FastAPI:
    """Create standalone Strategy Service FastAPI app."""
    state = service or StrategyService()
    app = FastAPI(title="AgentFlow Strategy Service", version="1.0.0")
    app.include_router(state.create_router())
    return app
