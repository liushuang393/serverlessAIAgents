"""Protocol interfaces for Evolution V2 components."""

from __future__ import annotations

from typing import Any, Protocol

from agentflow.evolution.types import (
    ExecutionEvent,
    OutcomeRecord,
    RetrievalDecision,
    RetrievalDecisionInput,
    ScoreUpdateRequest,
    StrategyCapsule,
    StrategyDecision,
    StrategyRecord,
    StrategySearchRequest,
    StrategySearchResponse,
    StrategyStatus,
    ValidationEvent,
)


class StrategyRouterProtocol(Protocol):
    """Route strategy before plan execution."""

    async def route(
        self,
        *,
        task: str,
        plan: Any,
        context: dict[str, Any],
    ) -> StrategyDecision:
        """Return strategy decision for orchestrator."""


class ExecutionRecorderProtocol(Protocol):
    """Execution recorder hook protocol."""

    async def on_step_start(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        """Record step start."""

    async def on_step_success(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        """Record step success."""

    async def on_step_error(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        """Record step failure."""

    async def on_llm_result(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        """Record LLM event."""

    async def on_tool_result(
        self,
        run_id: str,
        step_id: str,
        payload: dict[str, Any] | None = None,
    ) -> None:
        """Record tool call event."""

    async def list_events(self, run_id: str) -> list[ExecutionEvent]:
        """Get events for a run."""


class StrategyExtractorProtocol(Protocol):
    """Extract strategy capsule from execution traces."""

    async def extract(
        self,
        *,
        task: str,
        context: dict[str, Any],
        events: list[ExecutionEvent],
        outcome: OutcomeRecord,
    ) -> StrategyCapsule | None:
        """Return extracted capsule if candidate should be created."""


class StrategyValidatorProtocol(Protocol):
    """Validator contract."""

    async def submit_candidate(
        self,
        *,
        strategy_id: str,
        capsule: StrategyCapsule,
        metadata: dict[str, Any],
    ) -> str | None:
        """Submit validation task and return job id if queued."""

    async def report_result(self, event: ValidationEvent) -> None:
        """Report validation result."""


class StrategyRegistryProtocol(Protocol):
    """Registry contract."""

    async def register(
        self,
        capsule: StrategyCapsule,
        *,
        status: StrategyStatus = StrategyStatus.CANDIDATE,
        parent_strategy_id: str | None = None,
    ) -> StrategyRecord:
        """Persist strategy capsule."""

    async def search(self, request: StrategySearchRequest) -> StrategySearchResponse:
        """Search candidate strategies."""

    async def get(self, strategy_id: str) -> StrategyRecord | None:
        """Get strategy by id."""

    async def update_status(self, strategy_id: str, status: StrategyStatus) -> None:
        """Update lifecycle status."""

    async def score_update(self, request: ScoreUpdateRequest) -> StrategyRecord | None:
        """Update score metrics with new outcome."""


class ScoringEngineProtocol(Protocol):
    """Scoring engine contract."""

    def compute_final_score(
        self,
        *,
        success_rate_30d: float,
        success_rate_7d: float,
        reuse: float,
        freshness: float,
        condition_match: float,
        cost_efficiency: float,
        suspicion_penalty: float,
    ) -> float:
        """Compute final ranking score."""

    def should_mark_suspect(
        self,
        *,
        failure_streak: int,
        success_7d: float,
        success_30d: float,
        environment_mismatch: bool,
        last_verified_age_days: int,
        max_age_days: int = 30,
    ) -> bool:
        """Return suspect marker decision."""


class RetrievalDecisionEngineProtocol(Protocol):
    """Retrieval gate contract."""

    async def should_retrieve(self, input_data: RetrievalDecisionInput) -> RetrievalDecision:
        """Return retrieval decision."""
