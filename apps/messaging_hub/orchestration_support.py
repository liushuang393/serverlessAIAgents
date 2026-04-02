"""Support utilities for orchestration service."""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from apps.messaging_hub.flight_watch import FlightSearchRequest, FlightWatchService


if TYPE_CHECKING:
    from control_plane.services.agent_aggregator import AgentAggregatorService


class CapabilityRouter:
    """A2A runtime と aggregator を統合した capability router."""

    def __init__(
        self,
        *,
        aggregator: AgentAggregatorService,
        a2a_hub: Any,
        security_mode: str,
    ) -> None:
        """初期化."""
        self._aggregator = aggregator
        self._a2a_hub = a2a_hub
        self._security_mode = security_mode

    def select_best_candidate(self, required_capability: str) -> dict[str, Any] | None:
        """candidate を選択する."""
        scored: list[tuple[float, dict[str, Any]]] = []
        seen_names: set[str] = set()
        for route_candidate in self._a2a_hub.bus.route(required_capability=required_capability):
            descriptor = self._a2a_hub.describe(route_candidate.agent_id)
            if descriptor is None:
                continue
            if required_capability not in descriptor.capabilities:
                continue
            score = route_candidate.score
            if self._security_mode != "autonomous" and "os_execute" in descriptor.capabilities:
                score -= 0.05
            scored.append(
                (
                    score,
                    {
                        "name": descriptor.agent_id,
                        "app_name": str(descriptor.metadata.get("app_name", "runtime")),
                        "business_base": str(descriptor.metadata.get("business_base", "runtime")),
                    },
                )
            )
            seen_names.add(descriptor.agent_id)
        for candidate in self._aggregator.search_by_capability(required_capability):
            if candidate.name in seen_names:
                continue
            if self._a2a_hub.discover(candidate.name) is None:
                continue
            score = 1.0
            feedback = self._a2a_hub.bus.get_feedback_summary(candidate.name)
            if feedback is not None:
                score += feedback.success_rate
                score -= feedback.average_duration_ms / 100000.0
            if candidate.app_name == "messaging_hub":
                score += 0.1
            if self._security_mode != "autonomous" and candidate.business_base == "operations":
                score -= 0.05
            scored.append((score, candidate.to_dict()))
        if not scored:
            return None
        scored.sort(key=lambda item: item[0], reverse=True)
        return scored[0][1]


def is_flight_request(
    *,
    message: str,
    required_capability: str | None,
    input_data: dict[str, Any] | None,
) -> bool:
    """flight request 判定."""
    if required_capability == "flight_watch":
        return True
    if input_data is not None and "request" in input_data:
        return True
    lowered = message.lower()
    return any(keyword in lowered for keyword in ("flight", "ticket", "airfare", "机票", "航空券"))


def infer_capability_from_message(message: str, capability_hints: tuple[tuple[str, str], ...]) -> str | None:
    """メッセージから capability hint を抽出する."""
    lowered = message.lower()
    for keyword, capability in capability_hints:
        if keyword in lowered:
            return capability
    return None


def extract_partial_flight_request(
    *,
    message: str,
    input_data: dict[str, Any],
) -> dict[str, Any]:
    """flight watch 用の部分入力を取得する."""
    request = input_data.get("request")
    if isinstance(request, dict):
        return dict(request)
    partial_request, _ = FlightWatchService.extract_request_from_message(message)
    return partial_request


def build_agent_payload(
    *,
    agent_name: str,
    task_id: str,
    message: str,
    user_id: str,
    conversation_id: str | None,
    input_data: dict[str, Any],
    execution_context: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """agent 入力 payload を構築する."""
    if agent_name == "BusinessAdvisorAgent":
        return {
            "question": message,
            "context": input_data.get("context", ""),
            "execution_context": execution_context or {},
        }
    if agent_name == "FileOrganizerAgent":
        return {
            "action": str(input_data.get("action", "analyze")),
            "path": str(input_data.get("path", ".")),
            "execution_context": execution_context or {},
        }
    if agent_name == "MeetingAgent":
        return {
            "action": str(input_data.get("action", "brief")),
            "event_id": str(input_data.get("event_id", "")),
            "transcript": str(input_data.get("transcript", "")),
            "execution_context": execution_context or {},
        }
    if agent_name == "PersonalAssistantCoordinator":
        return {
            "message": message,
            "user_id": user_id,
            "context": {"conversation_id": conversation_id, "execution_context": execution_context or {}},
        }
    return {
        "message": message,
        "task_description": message,
        "task_id": task_id,
        "user_id": user_id,
        "conversation_id": conversation_id,
        "input_data": input_data,
        "execution_context": execution_context or {},
        **input_data,
    }


def extract_user_preferences(request: FlightSearchRequest) -> dict[str, Any]:
    """再利用しやすいユーザー嗜好を抽出する."""
    return {
        "origin": request.origin,
        "destination": request.destination,
        "budget": request.budget,
        "max_stops": request.max_stops,
        "ranking_weights": request.ranking_weights.model_dump(mode="json"),
        "preferred_departure_time": request.preferred_departure_time,
        "preferred_arrival_time": request.preferred_arrival_time,
    }


def summarize_flight_result(search_payload: dict[str, Any]) -> list[str]:
    """検索結果を簡潔に要約する."""
    offers = search_payload.get("offers", [])
    if not isinstance(offers, list) or not offers:
        return ["候補は見つかりませんでした"]
    top_offer = offers[0] if isinstance(offers[0], dict) else {}
    return [
        f"候補数: {len(offers)}",
        f"最安値: ${top_offer.get('price', '-')}",
        f"所要時間: {top_offer.get('total_duration_minutes', '-')} 分",
    ]
