"""Flight watch specialist agent."""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field

from apps.messaging_hub.flight_watch import FlightSearchRequest, FlightWatchService
from kernel.agents.resilient_agent import ResilientAgent


class FlightWatchAgentInput(BaseModel):
    """Flight watch agent input."""

    action: str = Field(default="search")
    request: FlightSearchRequest | None = Field(default=None)
    user_id: str = Field(default="system")
    conversation_id: str | None = Field(default=None)
    subscription_id: str | None = Field(default=None)
    execution_context: dict[str, Any] = Field(default_factory=dict)


class FlightWatchAgentOutput(BaseModel):
    """Flight watch agent output."""

    status: str = Field(default="completed")
    search_result: dict[str, Any] | None = Field(default=None)
    subscription: dict[str, Any] | None = Field(default=None)
    error: str | None = Field(default=None)


class FlightWatchAgent(ResilientAgent[FlightWatchAgentInput, FlightWatchAgentOutput]):
    """機票検索・監視を行う specialist agent."""

    name = "FlightWatchAgent"

    def __init__(self, service: FlightWatchService) -> None:
        """初期化."""
        super().__init__()
        self._service = service

    def _parse_input(self, input_data: dict[str, Any]) -> FlightWatchAgentInput:
        """入力をモデル化する."""
        return FlightWatchAgentInput.model_validate(input_data)

    async def process(self, input_data: FlightWatchAgentInput) -> FlightWatchAgentOutput:
        """検索または購読作成を実行する."""
        if input_data.request is None:
            return FlightWatchAgentOutput(status="failed", error="request_missing")

        if input_data.action == "subscribe":
            subscription = await self._service.create_subscription(
                request=input_data.request,
                user_id=input_data.user_id,
                conversation_id=input_data.conversation_id,
            )
            return FlightWatchAgentOutput(status="monitoring", subscription=subscription.model_dump())

        result = await self._service.search(input_data.request)
        return FlightWatchAgentOutput(status="completed", search_result=result.model_dump())


FlightWatchAgentInput.model_rebuild(_types_namespace={"FlightSearchRequest": FlightSearchRequest})
