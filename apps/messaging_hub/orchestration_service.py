"""High-level orchestration service for Messaging Hub."""

from __future__ import annotations

import time
import uuid
from datetime import UTC, datetime
from enum import StrEnum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from apps.messaging_hub.flight_watch import FlightSearchRequest, FlightWatchService
from apps.messaging_hub.generated_artifact_manager import (
    ArtifactType,
    GeneratedArtifactManager,
)
from apps.messaging_hub.harness_memory import HarnessMemoryService
from apps.messaging_hub.orchestration_support import (
    CapabilityRouter,
    build_agent_payload,
    extract_partial_flight_request,
    extract_user_preferences,
    infer_capability_from_message,
    is_flight_request,
    summarize_flight_result,
)
from apps.messaging_hub.storage.sqlite_store import SQLiteMessagingHubStore
from apps.messaging_hub.task_harness import (
    HarnessPlan,
    ProviderCandidate,
    TaskHarnessPlanner,
    TaskHarnessVerifier,
)
from apps.messaging_hub.agents.runtime_artifact_agent import RuntimeArtifactAgentRegistry
from control_plane.services.agent_aggregator import AgentAggregatorService
from control_plane.services.app_discovery import AppDiscoveryService
from kernel.protocols.a2a_hub import get_hub
from kernel.protocols.a2ui.components import CardComponent, FormComponent, InputComponent, ListComponent, TextComponent
from kernel.protocols.agui_events import (
    A2UIComponentEvent,
    ClarificationQuestion,
    ClarificationRequiredEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
    NodeCompleteEvent,
    NodeErrorEvent,
    NodeStartEvent,
    ProgressEvent,
    to_legacy_dict,
)


if TYPE_CHECKING:
    from kernel.runtime.websocket import WebSocketHub


class OrchestrationTaskStatus(StrEnum):
    """orchestration task status."""

    PENDING = "pending"
    RUNNING = "running"
    CLARIFICATION_REQUIRED = "clarification_required"
    APPROVAL_REQUIRED = "approval_required"
    COMPLETED = "completed"
    MONITORING = "monitoring"
    FAILED = "failed"


class OrchestrationTaskResult(BaseModel):
    """orchestration response payload."""

    ok: bool = Field(default=True)
    task_id: str
    status: OrchestrationTaskStatus
    summary: str
    headline: str = Field(default="")
    key_points: list[str] = Field(default_factory=list)
    actions: list[str] = Field(default_factory=list)
    risks: list[str] = Field(default_factory=list)
    result: dict[str, Any] | None = Field(default=None)
    clarification_ticket_id: str | None = Field(default=None)
    clarification_questions: list[dict[str, Any]] = Field(default_factory=list)
    generated_artifact: dict[str, Any] | None = Field(default=None)


class OrchestrationService:
    """Messaging Hub の高レベル自律編成サービス."""

    _CAPABILITY_HINTS: tuple[tuple[str, str], ...] = (
        ("business", "business_advice"),
        ("起业", "business_advice"),
        ("meeting", "meeting"),
        ("会議", "meeting"),
        ("calendar", "meeting"),
        ("file", "file_organization"),
        ("整理", "file_organization"),
        ("flight", "flight_watch"),
        ("机票", "flight_watch"),
        ("ticket", "flight_watch"),
        ("monitor", "monitoring"),
    )

    def __init__(
        self,
        *,
        store: SQLiteMessagingHubStore,
        websocket_hub: WebSocketHub,
        assistant: Any,
        flight_watch_service: FlightWatchService,
        generated_artifact_manager: GeneratedArtifactManager,
        security_mode: str,
        skill_gateway: Any | None = None,
        mcp_manager: Any | None = None,
        memory_service: HarnessMemoryService | None = None,
        runtime_artifact_registry: RuntimeArtifactAgentRegistry | None = None,
    ) -> None:
        """初期化."""
        self._store = store
        self._hub = websocket_hub
        self._assistant = assistant
        self._flight_watch_service = flight_watch_service
        self._generated_artifact_manager = generated_artifact_manager
        self._security_mode = security_mode
        self._app_discovery = AppDiscoveryService()
        self._aggregator = AgentAggregatorService(self._app_discovery)
        self._catalog_loaded = False
        self._a2a_hub = get_hub()
        self._capability_router = CapabilityRouter(
            aggregator=self._aggregator,
            a2a_hub=self._a2a_hub,
            security_mode=security_mode,
        )
        self._harness_planner = TaskHarnessPlanner(
            skill_gateway=skill_gateway,
            mcp_manager=mcp_manager,
        )
        self._harness_verifier = TaskHarnessVerifier()
        self._memory_service = memory_service
        self._runtime_artifact_registry = runtime_artifact_registry

    async def ensure_catalog_loaded(self) -> None:
        """agent catalog を読み込む."""
        if self._catalog_loaded:
            return
        await self._app_discovery.scan()
        self._catalog_loaded = True

    async def create_task(
        self,
        *,
        message: str,
        user_id: str,
        conversation_id: str | None = None,
        required_capability: str | None = None,
        input_data: dict[str, Any] | None = None,
    ) -> OrchestrationTaskResult:
        """新規 task を作成して実行する."""
        task_id = f"task_{uuid.uuid4().hex}"
        now_iso = datetime.now(UTC).isoformat()
        partial_request: dict[str, Any] | None = None
        if is_flight_request(message=message, required_capability=required_capability, input_data=input_data):
            partial_request = extract_partial_flight_request(message=message, input_data=input_data or {})
        harness_plan = await self._harness_planner.build_plan(
            message=message,
            required_capability=required_capability,
            input_data=input_data,
            partial_request=partial_request,
        )
        if self._memory_service is not None:
            memory_snapshot = await self._memory_service.snapshot_for_plan(
                harness_plan=harness_plan,
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
            )
            harness_plan.memory_context = memory_snapshot.model_dump(mode="json")
        await self._store.upsert_assistant_job(
            {
                "job_id": task_id,
                "job_type": "orchestration",
                "user_id": user_id,
                "conversation_id": conversation_id,
                "status": OrchestrationTaskStatus.RUNNING.value,
                "input_text": message,
                "run_id": task_id,
                "context": {
                    "required_capability": required_capability,
                    "input_data": input_data or {},
                    "harness_plan": harness_plan.model_dump(mode="json"),
                },
                "result": None,
                "created_at": now_iso,
                "updated_at": now_iso,
            }
        )
        if self._memory_service is not None:
            await self._memory_service.remember_from_plan(
                harness_plan=harness_plan,
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
                values={
                    "raw_user_request": message,
                    "task_plan": harness_plan.execution_plan,
                },
            )
        await self._emit_flow_start(task_id, conversation_id, message)
        try:
            if is_flight_request(message=message, required_capability=required_capability, input_data=input_data):
                return await self._handle_flight_task(
                    task_id=task_id,
                    message=message,
                    user_id=user_id,
                    conversation_id=conversation_id,
                    input_data=input_data or {},
                    harness_plan=harness_plan,
                )
            if required_capability:
                return await self._handle_capability_task(
                    task_id=task_id,
                    message=message,
                    user_id=user_id,
                    conversation_id=conversation_id,
                    required_capability=required_capability,
                    input_data=input_data or {},
                    harness_plan=harness_plan,
                )
            return await self._handle_general_task(
                task_id=task_id,
                message=message,
                user_id=user_id,
                conversation_id=conversation_id,
                harness_plan=harness_plan,
            )
        except Exception as exc:
            await self._update_task_record(
                task_id,
                status=OrchestrationTaskStatus.FAILED,
                result=None,
                error=str(exc),
            )
            await self._emit_error(task_id, conversation_id, str(exc))
            return OrchestrationTaskResult(
                ok=False,
                task_id=task_id,
                status=OrchestrationTaskStatus.FAILED,
                summary=f"❌ オーケストレーションに失敗しました: {exc}",
                headline="実行失敗",
                risks=[str(exc)],
            )

    async def resume_task_with_clarifications(
        self,
        *,
        task_id: str,
        answers: dict[str, Any],
    ) -> OrchestrationTaskResult | None:
        """clarification 回答を受けて task を再開する."""
        task = await self._store.get_assistant_job(task_id)
        if task is None:
            return None
        ticket = await self._store.get_clarification_ticket_by_job(task_id)
        if ticket is None:
            return None
        context = task.get("context", {})
        partial_request = context.get("partial_request", {})
        if not isinstance(partial_request, dict):
            partial_request = {}
        merged = self._flight_watch_service.merge_clarification_answers(partial_request, answers)
        ticket["answers"] = answers
        ticket["status"] = "answered"
        ticket["updated_at"] = datetime.now(UTC).isoformat()
        await self._store.upsert_clarification_ticket(ticket)
        harness_plan = await self._harness_planner.build_plan(
            message=str(task.get("input_text", "")),
            required_capability="flight_watch",
            input_data={"request": merged, "resume": True},
            partial_request=merged,
        )
        return await self._handle_flight_task(
            task_id=task_id,
            message=str(task.get("input_text", "")),
            user_id=str(task.get("user_id", "system")),
            conversation_id=str(task.get("conversation_id")) if task.get("conversation_id") else None,
            input_data={"request": merged, "resume": True},
            harness_plan=harness_plan,
        )

    async def get_task(self, task_id: str) -> dict[str, Any] | None:
        """task を取得する."""
        task = await self._store.get_assistant_job(task_id)
        if task is None:
            return None
        ticket = await self._store.get_clarification_ticket_by_job(task_id)
        if ticket is not None:
            task["clarification"] = ticket
        return task

    async def list_task_events(self, task_id: str) -> list[dict[str, Any]]:
        """task events を返す."""
        return await self._store.list_assistant_job_events(task_id)

    async def _handle_flight_task(
        self,
        *,
        task_id: str,
        message: str,
        user_id: str,
        conversation_id: str | None,
        input_data: dict[str, Any],
        harness_plan: HarnessPlan,
    ) -> OrchestrationTaskResult:
        """flight request を処理する."""
        structured_request = input_data.get("request")
        partial_request: dict[str, Any]
        if isinstance(structured_request, dict):
            partial_request = dict(structured_request)
        else:
            partial_request, _ = self._flight_watch_service.extract_request_from_message(message)
        harness_plan = await self._refresh_harness_plan(
            task_id=task_id,
            message=message,
            required_capability="flight_watch",
            input_data=input_data,
            partial_request=partial_request,
        )
        missing_fields = [
            field_name
            for field_name in ("origin", "destination", "depart_window", "return_window")
            if field_name not in partial_request
        ]
        if missing_fields:
            return await self._create_clarification_result(
                task_id=task_id,
                message=message,
                user_id=user_id,
                conversation_id=conversation_id,
                partial_request=partial_request,
                missing_fields=missing_fields,
            )

        request = FlightSearchRequest.model_validate(partial_request)
        total_steps = 2 if request.create_watch else 1
        await self._emit_node_start(task_id, conversation_id, "flight_search", "Flight Search")
        search_result = await self._invoke_best_agent(
            required_capability="flight_watch",
            payload={
                "action": "search",
                "request": request.model_dump(),
                "user_id": user_id,
                "conversation_id": conversation_id,
            },
        )
        search_payload = search_result.get("search_result", search_result)
        provider_candidates_raw = search_payload.get("metadata", {}).get("provider_candidates", [])
        if isinstance(provider_candidates_raw, list):
            provider_candidates = [
                ProviderCandidate.model_validate(candidate)
                for candidate in provider_candidates_raw
                if isinstance(candidate, dict)
            ]
            harness_plan = self._harness_planner.attach_provider_candidates(
                harness_plan,
                provider_candidates,
            )
        await self._emit_node_complete(task_id, conversation_id, "flight_search", "Flight Search", search_payload)
        await self._emit_progress(task_id, conversation_id, current=1, total=total_steps, message="フライト検索完了")
        await self._emit_flight_results_component(task_id, conversation_id, search_payload)

        result_payload: dict[str, Any] = {
            "search_result": search_payload,
            "harness_plan": harness_plan.model_dump(mode="json"),
        }
        status = OrchestrationTaskStatus.COMPLETED
        actions = ["検索結果を確認してください"]
        risks: list[str] = []
        if request.create_watch:
            await self._emit_node_start(task_id, conversation_id, "flight_watch", "Flight Watch")
            subscription_result = await self._invoke_best_agent(
                required_capability="flight_watch",
                payload={
                    "action": "subscribe",
                    "request": request.model_dump(),
                    "user_id": user_id,
                    "conversation_id": conversation_id,
                },
            )
            subscription_payload = subscription_result.get("subscription", subscription_result)
            result_payload["subscription"] = subscription_payload
            status = OrchestrationTaskStatus.MONITORING
            actions = ["値下がり通知を待機してください", "必要なら購読を一時停止または解除してください"]
            await self._emit_node_complete(task_id, conversation_id, "flight_watch", "Flight Watch", subscription_payload)
            await self._emit_progress(task_id, conversation_id, current=2, total=2, message="監視購読を作成")
            await self._emit_watch_status_component(task_id, conversation_id, subscription_payload)

        verification = await self._harness_verifier.verify(
            goal=harness_plan.goal,
            result={
                "search_result": search_payload,
                "offers_found": len(search_payload.get("offers", []))
                if isinstance(search_payload.get("offers"), list)
                else 0,
                "subscription": result_payload.get("subscription"),
            },
            spec=harness_plan.verification,
        )
        result_payload["verification"] = verification.model_dump(mode="json")
        if not verification.is_acceptable:
            risks.append(verification.feedback)
            actions.append("provider discovery を見直して再計画してください")
        if self._memory_service is not None:
            await self._memory_service.remember_from_plan(
                harness_plan=harness_plan,
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
                values={
                    "provider_candidates": provider_candidates_raw,
                    "task_episode": result_payload,
                    "user_preferences": extract_user_preferences(request),
                    "monitor_baseline": result_payload.get("subscription"),
                },
            )
            await self._memory_service.remember_private(
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
                agent_name="FlightWatchAgent",
                key="specialist_scratchpad",
                content=search_payload,
                description="flight watch specialist search result",
            )

        await self._update_task_record(
            task_id,
            status=status,
            result=result_payload,
            error=None,
            extra_context={"harness_plan": harness_plan.model_dump(mode="json")},
        )
        await self._emit_flow_complete(task_id, conversation_id, result_payload)
        return OrchestrationTaskResult(
            task_id=task_id,
            status=status,
            summary="機票検索を完了しました。" if status == OrchestrationTaskStatus.COMPLETED else "機票監視を開始しました。",
            headline="Flight Watch",
            key_points=summarize_flight_result(search_payload),
            actions=actions,
            risks=risks,
            result=result_payload,
        )

    async def _create_clarification_result(
        self,
        *,
        task_id: str,
        message: str,
        user_id: str,
        conversation_id: str | None,
        partial_request: dict[str, Any],
        missing_fields: list[str],
    ) -> OrchestrationTaskResult:
        """clarification を生成する."""
        questions = self._flight_watch_service.build_clarification_questions(partial_request, missing_fields)
        ticket_id = f"clar_{uuid.uuid4().hex}"
        now_iso = datetime.now(UTC).isoformat()
        await self._store.upsert_clarification_ticket(
            {
                "ticket_id": ticket_id,
                "job_id": task_id,
                "status": "pending",
                "original_question": message,
                "questions": questions,
                "answers": {},
                "created_at": now_iso,
                "updated_at": now_iso,
            }
        )
        await self._update_task_record(
            task_id,
            status=OrchestrationTaskStatus.CLARIFICATION_REQUIRED,
            result={"missing_fields": missing_fields},
            error=None,
            extra_context={"partial_request": partial_request, "task_kind": "flight_watch"},
        )
        clarification_questions = [
            ClarificationQuestion(
                id=str(item.get("id", "")),
                text=str(item.get("text", "")),
                type=str(item.get("type", "text")),
                required=bool(item.get("required", True)),
                options=item.get("options", []) if isinstance(item.get("options"), list) else [],
            )
            for item in questions
        ]
        event = ClarificationRequiredEvent(
            timestamp=time.time(),
            flow_id=task_id,
            data={"task_id": task_id, "conversation_id": conversation_id},
            original_question=message,
            questions=clarification_questions,
            message="検索条件が不足しています。補足すると同じ task を再開します。",
        )
        await self._emit_agui_event(task_id, conversation_id, event)
        await self._emit_clarification_component(task_id, conversation_id, ticket_id, questions)
        return OrchestrationTaskResult(
            task_id=task_id,
            status=OrchestrationTaskStatus.CLARIFICATION_REQUIRED,
            summary="検索条件が不足しています。出発地・目的地・往復日程を補足してください。",
            headline="補足が必要です",
            key_points=[f"不足: {field_name}" for field_name in missing_fields],
            actions=["clarification API で回答を送信してください"],
            risks=[],
            result={"missing_fields": missing_fields},
            clarification_ticket_id=ticket_id,
            clarification_questions=questions,
        )

    async def _handle_capability_task(
        self,
        *,
        task_id: str,
        message: str,
        user_id: str,
        conversation_id: str | None,
        required_capability: str,
        input_data: dict[str, Any],
        harness_plan: HarnessPlan,
    ) -> OrchestrationTaskResult:
        """明示 capability task を処理する."""
        await self.ensure_catalog_loaded()
        candidate = self._capability_router.select_best_candidate(required_capability)
        if candidate is None:
            artifact = await self._generated_artifact_manager.create_runtime_artifact(
                description=message,
                requested_by=user_id,
                artifact_type=ArtifactType.AGENT,
                metadata={
                    "required_capability": required_capability,
                    "harness_plan": harness_plan.model_dump(mode="json"),
                },
            )
            runtime_agent_name: str | None = None
            if self._runtime_artifact_registry is not None:
                runtime_agent_name = await self._runtime_artifact_registry.materialize(artifact)
                if runtime_agent_name is not None:
                    bound = await self._generated_artifact_manager.bind_runtime_agent(
                        artifact.artifact_id,
                        agent_name=runtime_agent_name,
                    )
                    if bound is not None:
                        artifact = bound
            result_payload = {
                "generated_artifact": artifact.model_dump(mode="json"),
                "harness_plan": harness_plan.model_dump(mode="json"),
                "runtime_agent": runtime_agent_name,
            }
            await self._update_task_record(
                task_id,
                status=OrchestrationTaskStatus.COMPLETED,
                result=result_payload,
                error=None,
                extra_context={"harness_plan": harness_plan.model_dump(mode="json")},
            )
            await self._emit_flow_complete(task_id, conversation_id, result_payload)
            return OrchestrationTaskResult(
                task_id=task_id,
                status=OrchestrationTaskStatus.COMPLETED,
                summary="適切な agent が見つからなかったため、runtime artifact を生成しました。",
                headline="Gap Fill",
                key_points=[
                    f"required_capability={required_capability}",
                    f"artifact={artifact.name}",
                    f"runtime_agent={runtime_agent_name or 'pending'}",
                ],
                actions=["artifact を validate / approve してください", "同じ capability 要求は runtime agent が引き継げます"],
                risks=["runtime artifact は未 publish です"],
                result=result_payload,
                generated_artifact=artifact.model_dump(mode="json"),
            )

        node_id = f"agent_{candidate['name']}"
        await self._emit_node_start(task_id, conversation_id, node_id, candidate["name"])
        payload = build_agent_payload(
            agent_name=candidate["name"],
            task_id=task_id,
            message=message,
            user_id=user_id,
            conversation_id=conversation_id,
            input_data=input_data,
        )
        result_payload = await self._invoke_agent(candidate["name"], payload)
        await self._emit_node_complete(task_id, conversation_id, node_id, candidate["name"], result_payload)
        await self._emit_progress(task_id, conversation_id, current=1, total=1, message=f"{candidate['name']} 実行完了")
        if self._memory_service is not None:
            await self._memory_service.remember_from_plan(
                harness_plan=harness_plan,
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
                values={"task_episode": result_payload},
            )
            await self._memory_service.remember_private(
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
                agent_name=str(candidate["name"]),
                key="specialist_scratchpad",
                content=result_payload,
                description="specialist agent output",
            )
        await self._update_task_record(
            task_id,
            status=OrchestrationTaskStatus.COMPLETED,
            result={
                "agent": candidate["name"],
                "output": result_payload,
                "harness_plan": harness_plan.model_dump(mode="json"),
            },
            error=None,
            extra_context={"harness_plan": harness_plan.model_dump(mode="json")},
        )
        await self._emit_flow_complete(task_id, conversation_id, result_payload)
        return OrchestrationTaskResult(
            task_id=task_id,
            status=OrchestrationTaskStatus.COMPLETED,
            summary=f"{candidate['name']} で要求を処理しました。",
            headline="A2A Routing",
            key_points=[f"capability={required_capability}", f"agent={candidate['name']}"],
            actions=[],
            risks=[],
            result={
                "agent": candidate["name"],
                "output": result_payload,
                "harness_plan": harness_plan.model_dump(mode="json"),
            },
        )

    async def _handle_general_task(
        self,
        *,
        task_id: str,
        message: str,
        user_id: str,
        conversation_id: str | None,
        harness_plan: HarnessPlan,
    ) -> OrchestrationTaskResult:
        """既存 assistant fast-path を実行する."""
        hinted_capability = infer_capability_from_message(message, self._CAPABILITY_HINTS)
        if hinted_capability is not None and hinted_capability != "flight_watch":
            return await self._handle_capability_task(
                task_id=task_id,
                message=message,
                user_id=user_id,
                conversation_id=conversation_id,
                required_capability=hinted_capability,
                input_data={},
                harness_plan=harness_plan,
            )

        result = await self._assistant.process(
            message,
            user_id=user_id,
            context={"run_id": task_id, "conversation_id": conversation_id},
        )
        result["harness_plan"] = harness_plan.model_dump(mode="json")
        if self._memory_service is not None:
            await self._memory_service.remember_from_plan(
                harness_plan=harness_plan,
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
                values={"task_episode": result},
            )
        await self._update_task_record(
            task_id,
            status=OrchestrationTaskStatus.COMPLETED,
            result=result,
            error=None,
            extra_context={"harness_plan": harness_plan.model_dump(mode="json")},
        )
        await self._emit_flow_complete(task_id, conversation_id, result)
        return OrchestrationTaskResult(
            task_id=task_id,
            status=OrchestrationTaskStatus.COMPLETED,
            summary=str(result.get("summary", "")),
            headline=str(result.get("headline", "")),
            key_points=result.get("key_points", []) if isinstance(result.get("key_points"), list) else [],
            actions=result.get("actions", []) if isinstance(result.get("actions"), list) else [],
            risks=result.get("risks", []) if isinstance(result.get("risks"), list) else [],
            result=result,
        )

    async def _invoke_best_agent(self, required_capability: str, payload: dict[str, Any]) -> dict[str, Any]:
        """best candidate agent を実行する."""
        await self.ensure_catalog_loaded()
        candidate = self._capability_router.select_best_candidate(required_capability)
        if candidate is None:
            raise RuntimeError(f"a2a_candidate_not_found:{required_capability}")
        return await self._invoke_agent(candidate["name"], payload)

    async def _invoke_agent(self, agent_name: str, payload: dict[str, Any]) -> dict[str, Any]:
        """A2A hub 経由で agent を実行する."""
        return await self._a2a_hub.call(agent_name, payload)

    async def _refresh_harness_plan(
        self,
        *,
        task_id: str,
        message: str,
        required_capability: str | None,
        input_data: dict[str, Any],
        partial_request: dict[str, Any],
    ) -> HarnessPlan:
        """最新の partial request を反映して harness plan を更新する."""
        refreshed = await self._harness_planner.build_plan(
            message=message,
            required_capability=required_capability,
            input_data=input_data,
            partial_request=partial_request,
        )
        task = await self._store.get_assistant_job(task_id)
        if self._memory_service is not None and task is not None:
            snapshot = await self._memory_service.snapshot_for_plan(
                harness_plan=refreshed,
                task_id=task_id,
                user_id=str(task.get("user_id", "system")),
                conversation_id=str(task.get("conversation_id")) if task.get("conversation_id") else None,
            )
            refreshed.memory_context = snapshot.model_dump(mode="json")
        await self._update_task_record(
            task_id,
            status=OrchestrationTaskStatus.RUNNING,
            result=None,
            error=None,
            extra_context={"harness_plan": refreshed.model_dump(mode="json")},
        )
        return refreshed

    async def _update_task_record(
        self,
        task_id: str,
        *,
        status: OrchestrationTaskStatus,
        result: dict[str, Any] | None,
        error: str | None,
        extra_context: dict[str, Any] | None = None,
    ) -> None:
        """task レコードを更新する."""
        task = await self._store.get_assistant_job(task_id)
        if task is None:
            return
        context = task.get("context", {})
        if not isinstance(context, dict):
            context = {}
        if extra_context:
            context.update(extra_context)
        task["status"] = status.value
        task["result"] = result
        task["error"] = error
        task["context"] = context
        task["updated_at"] = datetime.now(UTC).isoformat()
        await self._store.upsert_assistant_job(task)

    async def _emit_flow_start(self, task_id: str, conversation_id: str | None, message: str) -> None:
        """flow.start を送信する."""
        event = FlowStartEvent(
            timestamp=time.time(),
            flow_id=task_id,
            data={"task_id": task_id, "conversation_id": conversation_id, "message": message},
        )
        await self._emit_agui_event(task_id, conversation_id, event)

    async def _emit_flow_complete(
        self,
        task_id: str,
        conversation_id: str | None,
        result: dict[str, Any],
    ) -> None:
        """flow.complete を送信する."""
        event = FlowCompleteEvent(
            timestamp=time.time(),
            flow_id=task_id,
            data={"task_id": task_id, "conversation_id": conversation_id, "result": result},
            result=result,
            include_result=True,
        )
        await self._emit_agui_event(task_id, conversation_id, event)

    async def _emit_error(self, task_id: str, conversation_id: str | None, message: str) -> None:
        """flow.error を送信する."""
        event = FlowErrorEvent(
            timestamp=time.time(),
            flow_id=task_id,
            data={"task_id": task_id, "conversation_id": conversation_id},
            error_message=message,
            error_type="RuntimeError",
        )
        await self._emit_agui_event(task_id, conversation_id, event)

    async def _emit_node_start(
        self,
        task_id: str,
        conversation_id: str | None,
        node_id: str,
        node_name: str,
    ) -> None:
        """node.start を送信する."""
        event = NodeStartEvent(
            timestamp=time.time(),
            flow_id=task_id,
            data={"task_id": task_id, "conversation_id": conversation_id},
            node_id=node_id,
            node_name=node_name,
        )
        await self._emit_agui_event(task_id, conversation_id, event)

    async def _emit_node_complete(
        self,
        task_id: str,
        conversation_id: str | None,
        node_id: str,
        node_name: str,
        result: dict[str, Any],
    ) -> None:
        """node.complete を送信する."""
        event = NodeCompleteEvent(
            timestamp=time.time(),
            flow_id=task_id,
            data={"task_id": task_id, "conversation_id": conversation_id, "result": result},
            node_id=node_id,
            node_name=node_name,
        )
        await self._emit_agui_event(task_id, conversation_id, event)

    async def _emit_progress(
        self,
        task_id: str,
        conversation_id: str | None,
        *,
        current: int,
        total: int,
        message: str,
    ) -> None:
        """progress を送信する."""
        percentage = round((current / max(total, 1)) * 100, 2)
        event = ProgressEvent(
            timestamp=time.time(),
            flow_id=task_id,
            data={"task_id": task_id, "conversation_id": conversation_id, "message": message},
            current=current,
            total=total,
            percentage=percentage,
        )
        await self._emit_agui_event(task_id, conversation_id, event)

    async def _emit_flight_results_component(
        self,
        task_id: str,
        conversation_id: str | None,
        search_payload: dict[str, Any],
    ) -> None:
        """flight result list の A2UI を送信する."""
        offers_raw = search_payload.get("offers", [])
        items: list[TextComponent] = []
        for offer in offers_raw[:5]:
            if not isinstance(offer, dict):
                continue
            items.append(
                TextComponent(
                    f"{offer.get('carrier', '')}: ${offer.get('price')} / "
                    f"{offer.get('total_duration_minutes')}min / {offer.get('stops')} stops"
                )
            )
        component = CardComponent(
            title="Flight Results",
            children=[
                TextComponent("検索結果を上位順に表示します。"),
                ListComponent(items=items),
            ],
        )
        event = A2UIComponentEvent(
            timestamp=time.time(),
            flow_id=task_id,
            surface_id=f"conversation:{conversation_id or task_id}",
            component=component.to_dict(),
            data={"task_id": task_id},
        )
        await self._emit_agui_event(task_id, conversation_id, event)

    async def _emit_watch_status_component(
        self,
        task_id: str,
        conversation_id: str | None,
        subscription_payload: dict[str, Any],
    ) -> None:
        """watch status card を送信する."""
        component = CardComponent(
            title="Flight Watch Status",
            children=[
                TextComponent(f"subscription_id: {subscription_payload.get('subscription_id', '')}"),
                TextComponent(f"baseline_price: {subscription_payload.get('baseline_price', '')}"),
                TextComponent(f"next_check_at: {subscription_payload.get('next_check_at', '')}"),
            ],
        )
        event = A2UIComponentEvent(
            timestamp=time.time(),
            flow_id=task_id,
            surface_id=f"conversation:{conversation_id or task_id}",
            component=component.to_dict(),
            data={"task_id": task_id},
        )
        await self._emit_agui_event(task_id, conversation_id, event)

    async def _emit_clarification_component(
        self,
        task_id: str,
        conversation_id: str | None,
        ticket_id: str,
        questions: list[dict[str, Any]],
    ) -> None:
        """clarification form を送信する."""
        fields = [
            InputComponent(
                name=str(question.get("id", "")),
                input_type="text" if str(question.get("type", "text")) != "select" else "select",
                placeholder=str(question.get("text", "")),
                options=question.get("options", []),
            )
            for question in questions
        ]
        component = FormComponent(
            action=f"/api/orchestration/tasks/{task_id}/clarifications",
            fields=fields,
            ticket_id=ticket_id,
            title="補足入力",
        )
        event = A2UIComponentEvent(
            timestamp=time.time(),
            flow_id=task_id,
            surface_id=f"conversation:{conversation_id or task_id}",
            component=component.to_dict(),
            data={"task_id": task_id, "ticket_id": ticket_id},
        )
        await self._emit_agui_event(task_id, conversation_id, event)

    async def _emit_agui_event(
        self,
        task_id: str,
        conversation_id: str | None,
        event: Any,
    ) -> None:
        """AG-UI イベントを永続化し、WS に配信する."""
        payload = event.to_dict()
        await self._store.add_assistant_job_event(job_id=task_id, event_type=str(payload.get("event_type", "")), payload=payload)
        rooms = [f"task:{task_id}", task_id]
        if conversation_id:
            rooms.append(conversation_id)
            rooms.append(f"conversation:{conversation_id}")
        for room in rooms:
            await self._hub.broadcast_room(room, {"type": str(event.event_type.value), "data": payload})
            await self._hub.broadcast_room(room, {"type": to_legacy_dict(event).get("type", ""), "data": payload})
        await self._hub.broadcast({"type": str(event.event_type.value), "data": payload})
