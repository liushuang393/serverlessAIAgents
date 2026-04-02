"""High-level orchestration service for Messaging Hub."""

from __future__ import annotations

import time
import uuid
from datetime import UTC, datetime
from enum import StrEnum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from apps.messaging_hub.execution_substrate import (
    ActionLogStatus,
    ExecutionFeedbackSource,
    ExecutionProfile,
    ExecutionSubstrateService,
)
from apps.messaging_hub.flight_watch import FlightSearchRequest, FlightWatchService
from apps.messaging_hub.generated_artifact_manager import (
    ArtifactType,
    GeneratedArtifactManager,
)
from apps.messaging_hub.orchestration_support import (
    CapabilityRouter,
    build_agent_payload,
    extract_partial_flight_request,
    extract_user_preferences,
    infer_capability_from_message,
    is_flight_request,
    summarize_flight_result,
)
from apps.messaging_hub.task_harness import (
    HarnessPlan,
    ProviderCandidate,
    TaskHarnessPlanner,
    TaskHarnessVerifier,
)
from contracts.policy import EvalResult, PolicyDecision
from control_plane.services.agent_aggregator import AgentAggregatorService
from control_plane.services.app_discovery import AppDiscoveryService
from kernel.protocols.a2a_hub import get_hub
from kernel.protocols.a2ui.components import (
    A2UIComponent,
    CardComponent,
    FormComponent,
    InputComponent,
    ListComponent,
    TextComponent,
)
from kernel.protocols.agui_events import (
    A2UIComponentEvent,
    ClarificationQuestion,
    ClarificationRequiredEvent,
    FlowCompleteEvent,
    FlowErrorEvent,
    FlowStartEvent,
    NodeCompleteEvent,
    NodeStartEvent,
    ProgressEvent,
    to_legacy_dict,
)
from kernel.state.models import DecisionType


if TYPE_CHECKING:
    from apps.messaging_hub.agents.runtime_artifact_agent import RuntimeArtifactAgentRegistry
    from apps.messaging_hub.harness_memory import HarnessMemoryService
    from apps.messaging_hub.storage.sqlite_store import SQLiteMessagingHubStore
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
    execution_session_id: str = Field(default="")
    execution_summary: dict[str, Any] = Field(default_factory=dict)


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
        execution_substrate: ExecutionSubstrateService | None = None,
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
            security_mode=security_mode,
        )
        self._harness_verifier = TaskHarnessVerifier()
        self._memory_service = memory_service
        self._runtime_artifact_registry = runtime_artifact_registry
        self._execution_substrate = execution_substrate

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
        execution_session_id = ""
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
        if self._execution_substrate is not None:
            session = await self._execution_substrate.start_session(
                task_id=task_id,
                user_id=user_id,
                conversation_id=conversation_id,
                execution_profile=harness_plan.execution_profile,
                context_snapshot=self._build_execution_context_snapshot(
                    message=message,
                    required_capability=required_capability,
                    input_data=input_data or {},
                    harness_plan=harness_plan,
                ),
                status=OrchestrationTaskStatus.RUNNING.value,
                metadata={"task_kind": harness_plan.task_kind},
            )
            execution_session_id = session.session_id
            await self._execution_substrate.record_decision(
                task_id=task_id,
                step="blueprint_resolution",
                decision_type=DecisionType.BRANCH,
                choice=harness_plan.blueprint_id,
                reason=harness_plan.intent_summary,
                metadata={
                    "task_kind": harness_plan.task_kind,
                    "required_capability": required_capability,
                    "execution_profile": harness_plan.execution_profile.value,
                },
            )
            await self._execution_substrate.record_checkpoint(
                task_id=task_id,
                stage="plan_created",
                snapshot={
                    "required_capability": required_capability,
                    "missing_inputs": harness_plan.missing_inputs,
                    "harness_plan": harness_plan.model_dump(mode="json"),
                },
            )
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
                    "execution_session_id": execution_session_id,
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
            await self._record_failure_feedback(task_id=task_id, message=str(exc))
            await self._emit_error(task_id, conversation_id, str(exc))
            execution_fields = await self._execution_response_fields(task_id)
            return OrchestrationTaskResult(
                ok=False,
                task_id=task_id,
                status=OrchestrationTaskStatus.FAILED,
                summary=f"❌ オーケストレーションに失敗しました: {exc}",
                headline="実行失敗",
                risks=[str(exc)],
                execution_session_id=execution_fields["execution_session_id"],
                execution_summary=execution_fields["execution_summary"],
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
        if self._execution_substrate is not None:
            await self._execution_substrate.record_feedback(
                task_id=task_id,
                source=ExecutionFeedbackSource.HUMAN,
                title="clarification_answers_received",
                feedback="ユーザー補足を受領しました",
                metadata={"answers": answers},
            )
            await self._execution_substrate.record_checkpoint(
                task_id=task_id,
                stage="after_clarification",
                snapshot={"answers": answers, "merged_request": merged},
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
        execution_fields = await self._execution_response_fields(task_id)
        task["execution_session_id"] = execution_fields["execution_session_id"]
        task["execution_summary"] = execution_fields["execution_summary"]
        return task

    async def list_task_events(self, task_id: str) -> list[dict[str, Any]]:
        """task events を返す."""
        return await self._store.list_assistant_job_events(task_id)

    async def get_task_execution(self, task_id: str) -> dict[str, Any] | None:
        """task execution inspection を返す."""
        if self._execution_substrate is None:
            return None
        inspection = await self._execution_substrate.inspect_task(task_id)
        if inspection is None:
            return None
        return inspection.model_dump(mode="json")

    async def get_task_replay(self, task_id: str) -> dict[str, Any] | None:
        """task replay を返す."""
        if self._execution_substrate is None:
            return None
        replay = await self._execution_substrate.replay_task(
            task_id=task_id,
            agui_events=await self._store.list_assistant_job_events(task_id),
            execution_events=await self._store.list_execution_events(run_id=task_id, limit=500),
        )
        if replay is None:
            return None
        return replay.model_dump(mode="json")

    async def get_monitoring_summary(self, approvals: list[dict[str, Any]]) -> dict[str, Any] | None:
        """execution monitoring summary を返す."""
        if self._execution_substrate is None:
            return None
        summary = await self._execution_substrate.monitoring_summary(approvals=approvals)
        return summary.model_dump(mode="json")

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
                "execution_context": self._build_specialist_execution_context(
                    harness_plan=harness_plan,
                    required_capability="flight_watch",
                    selected_provider_evidence=[],
                ),
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
        if self._execution_substrate is not None:
            await self._execution_substrate.record_action(
                task_id=task_id,
                stage="provider_discovery",
                action_type="discover_provider_candidates",
                summary="provider discovery を実行しました",
                details={
                    "candidate_domains": harness_plan.provider_strategy.selected_domains,
                    "provider_used": search_payload.get("provider_used"),
                },
            )
            await self._execution_substrate.record_decision(
                task_id=task_id,
                step="provider_routing",
                decision_type=DecisionType.ACTION,
                choice=str(search_payload.get("provider_used", "unknown")),
                reason="provider discovery と検索結果に基づき実行先を確定しました",
                alternatives=harness_plan.provider_strategy.selected_domains,
                metadata={
                    "candidate_count": len(provider_candidates_raw) if isinstance(provider_candidates_raw, list) else 0
                },
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
            if self._execution_substrate is not None:
                await self._execution_substrate.record_checkpoint(
                    task_id=task_id,
                    stage="pre_mutation_subscription",
                    snapshot={
                        "request": request.model_dump(mode="json"),
                        "search_result": search_payload,
                    },
                    metadata={"reason": "subscription mutation 前の復元点"},
                )
            await self._emit_node_start(task_id, conversation_id, "flight_watch", "Flight Watch")
            subscription_result = await self._invoke_best_agent(
                required_capability="flight_watch",
                payload={
                    "action": "subscribe",
                    "request": request.model_dump(),
                    "user_id": user_id,
                    "conversation_id": conversation_id,
                    "execution_context": self._build_specialist_execution_context(
                        harness_plan=harness_plan,
                        required_capability="flight_watch",
                        selected_provider_evidence=harness_plan.context_hierarchy.retrieval_evidence_refs,
                    ),
                },
            )
            subscription_payload = subscription_result.get("subscription", subscription_result)
            result_payload["subscription"] = subscription_payload
            status = OrchestrationTaskStatus.MONITORING
            actions = ["値下がり通知を待機してください", "必要なら購読を一時停止または解除してください"]
            if self._execution_substrate is not None:
                await self._execution_substrate.record_action(
                    task_id=task_id,
                    stage="subscription",
                    action_type="create_subscription",
                    summary="flight watch subscription を作成しました",
                    details={"subscription_id": subscription_payload.get("subscription_id")},
                )
            await self._emit_node_complete(
                task_id, conversation_id, "flight_watch", "Flight Watch", subscription_payload
            )
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
        await self._record_verification_feedback(
            task_id=task_id,
            title="flight_task_verification",
            verification=verification,
            metadata={
                "task_kind": "flight_watch",
                "verifier_context": self._build_verifier_context(
                    harness_plan=harness_plan,
                    result_payload=result_payload,
                ),
            },
        )
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
        execution_fields = await self._execution_response_fields(task_id)
        return OrchestrationTaskResult(
            task_id=task_id,
            status=status,
            summary="機票検索を完了しました。"
            if status == OrchestrationTaskStatus.COMPLETED
            else "機票監視を開始しました。",
            headline="Flight Watch",
            key_points=summarize_flight_result(search_payload),
            actions=actions,
            risks=risks,
            result=result_payload,
            execution_session_id=execution_fields["execution_session_id"],
            execution_summary=execution_fields["execution_summary"],
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
        if self._execution_substrate is not None:
            await self._execution_substrate.record_decision(
                task_id=task_id,
                step="clarification_required",
                decision_type=DecisionType.BRANCH,
                choice="clarification_required",
                reason="必須入力が不足しているため同一 task を中断して補足を待機します",
                alternatives=missing_fields,
                metadata={"missing_fields": missing_fields},
            )
            await self._execution_substrate.record_checkpoint(
                task_id=task_id,
                stage="clarification_requested",
                snapshot={"partial_request": partial_request, "missing_fields": missing_fields},
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
        execution_fields = await self._execution_response_fields(task_id)
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
            execution_session_id=execution_fields["execution_session_id"],
            execution_summary=execution_fields["execution_summary"],
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
            harness_plan = self._harness_planner.apply_execution_profile(
                harness_plan,
                ExecutionProfile.GAP_FILL_GOVERNED,
            )
            policy_decision = PolicyDecision(
                policy_name="runtime_artifact_governance",
                decision="approval_required",
                reason="新規 runtime artifact は validate / approve を経て運用へ昇格させる",
                action="create_runtime_artifact",
                metadata={"required_capability": required_capability},
            )
            if self._execution_substrate is not None:
                await self._execution_substrate.record_decision(
                    task_id=task_id,
                    step="agent_route",
                    decision_type=DecisionType.FALLBACK,
                    choice="runtime_artifact_gap_fill",
                    reason="既存 agent が見つからないため runtime artifact で capability gap を埋めます",
                    alternatives=[required_capability],
                    metadata={"required_capability": required_capability},
                    policy_decision=policy_decision,
                )
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
            if self._execution_substrate is not None:
                await self._execution_substrate.record_action(
                    task_id=task_id,
                    stage="gap_fill",
                    action_type="create_runtime_artifact",
                    summary="runtime artifact を生成しました",
                    details={"artifact_name": artifact.name, "runtime_agent": runtime_agent_name},
                    artifact_refs=[artifact.artifact_id],
                )
                await self._execution_substrate.record_feedback(
                    task_id=task_id,
                    source=ExecutionFeedbackSource.VERIFIER,
                    title="gap_fill_artifact_created",
                    feedback="runtime artifact が生成され、後続 review 対象になりました",
                    passed=True,
                    score=1.0,
                    eval_result=EvalResult(
                        evaluator="gap_fill_artifact_verifier",
                        passed=True,
                        score=1.0,
                        reason="generated_artifact が存在します",
                        artifact_id=artifact.artifact_id,
                        metadata={"runtime_agent": runtime_agent_name},
                    ),
                )
            await self._update_task_record(
                task_id,
                status=OrchestrationTaskStatus.COMPLETED,
                result=result_payload,
                error=None,
                extra_context={"harness_plan": harness_plan.model_dump(mode="json")},
            )
            await self._emit_flow_complete(task_id, conversation_id, result_payload)
            execution_fields = await self._execution_response_fields(task_id)
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
                actions=[
                    "artifact を validate / approve してください",
                    "同じ capability 要求は runtime agent が引き継げます",
                ],
                risks=["runtime artifact は未 publish です"],
                result=result_payload,
                generated_artifact=artifact.model_dump(mode="json"),
                execution_session_id=execution_fields["execution_session_id"],
                execution_summary=execution_fields["execution_summary"],
            )

        node_id = f"agent_{candidate['name']}"
        if self._execution_substrate is not None:
            await self._execution_substrate.record_decision(
                task_id=task_id,
                step="agent_route",
                decision_type=DecisionType.ACTION,
                choice=str(candidate["name"]),
                reason="capability router が最適候補を選択しました",
                alternatives=[required_capability],
                metadata={"required_capability": required_capability},
            )
        await self._emit_node_start(task_id, conversation_id, node_id, candidate["name"])
        payload = build_agent_payload(
            agent_name=candidate["name"],
            task_id=task_id,
            message=message,
            user_id=user_id,
            conversation_id=conversation_id,
            input_data=input_data,
            execution_context=self._build_specialist_execution_context(
                harness_plan=harness_plan,
                required_capability=required_capability,
                selected_provider_evidence=harness_plan.context_hierarchy.retrieval_evidence_refs,
            ),
        )
        result_payload = await self._invoke_agent(candidate["name"], payload)
        if self._execution_substrate is not None:
            await self._execution_substrate.record_action(
                task_id=task_id,
                stage="capability_execution",
                action_type="invoke_agent",
                summary=f"{candidate['name']} を実行しました",
                details={"required_capability": required_capability},
            )
        await self._emit_node_complete(task_id, conversation_id, node_id, candidate["name"], result_payload)
        await self._emit_progress(task_id, conversation_id, current=1, total=1, message=f"{candidate['name']} 実行完了")
        verification = await self._harness_verifier.verify(
            goal=harness_plan.goal,
            result={"result_payload": result_payload},
            spec=harness_plan.verification,
        )
        await self._record_verification_feedback(
            task_id=task_id,
            title="capability_route_verification",
            verification=verification,
            metadata={
                "agent": candidate["name"],
                "required_capability": required_capability,
                "verifier_context": self._build_verifier_context(
                    harness_plan=harness_plan,
                    result_payload=result_payload,
                ),
            },
        )
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
        execution_fields = await self._execution_response_fields(task_id)
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
            execution_session_id=execution_fields["execution_session_id"],
            execution_summary=execution_fields["execution_summary"],
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
            context={
                "run_id": task_id,
                "conversation_id": conversation_id,
                "execution_context": self._build_main_agent_context(harness_plan=harness_plan),
            },
        )
        result["harness_plan"] = harness_plan.model_dump(mode="json")
        if self._execution_substrate is not None:
            await self._execution_substrate.record_action(
                task_id=task_id,
                stage="general_execution",
                action_type="assistant_process",
                summary="default assistant fast-path を実行しました",
                details={"hinted_capability": hinted_capability},
            )
        verification = await self._harness_verifier.verify(
            goal=harness_plan.goal,
            result={"result_payload": result},
            spec=harness_plan.verification,
        )
        await self._record_verification_feedback(
            task_id=task_id,
            title="general_task_verification",
            verification=verification,
            metadata={
                "task_kind": harness_plan.task_kind,
                "verifier_context": self._build_verifier_context(
                    harness_plan=harness_plan,
                    result_payload=result,
                ),
            },
        )
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
        execution_fields = await self._execution_response_fields(task_id)
        return OrchestrationTaskResult(
            task_id=task_id,
            status=OrchestrationTaskStatus.COMPLETED,
            summary=str(result.get("summary", "")),
            headline=str(result.get("headline", "")),
            key_points=result.get("key_points", []) if isinstance(result.get("key_points"), list) else [],
            actions=result.get("actions", []) if isinstance(result.get("actions"), list) else [],
            risks=result.get("risks", []) if isinstance(result.get("risks"), list) else [],
            result=result,
            execution_session_id=execution_fields["execution_session_id"],
            execution_summary=execution_fields["execution_summary"],
        )

    async def _invoke_best_agent(self, required_capability: str, payload: dict[str, Any]) -> dict[str, Any]:
        """best candidate agent を実行する."""
        await self.ensure_catalog_loaded()
        candidate = self._capability_router.select_best_candidate(required_capability)
        if candidate is None:
            msg = f"a2a_candidate_not_found:{required_capability}"
            raise RuntimeError(msg)
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
        if self._execution_substrate is not None:
            await self._execution_substrate.record_checkpoint(
                task_id=task_id,
                stage="plan_refreshed",
                snapshot={"harness_plan": refreshed.model_dump(mode="json"), "partial_request": partial_request},
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
        if self._execution_substrate is not None:
            await self._execution_substrate.sync_task_status(
                task_id=task_id,
                status=status.value,
                context_snapshot=context,
            )

    async def record_quality_feedback(
        self,
        *,
        task_id: str,
        source: str,
        score: float,
        elapsed_seconds: float,
        input_tokens: int,
    ) -> None:
        """外部 quality scoring を execution feedback として記録する."""
        if self._execution_substrate is None:
            return
        await self._execution_substrate.record_feedback(
            task_id=task_id,
            source=ExecutionFeedbackSource.SCORER,
            title=f"{source}_quality_score",
            feedback="応答品質スコアを記録しました",
            passed=score >= 0.5,
            score=score,
            eval_result=EvalResult(
                evaluator=source,
                passed=score >= 0.5,
                score=score,
                reason="multi-dimensional response scoring",
                metrics={"elapsed_seconds": elapsed_seconds, "input_tokens": float(input_tokens)},
            ),
            metadata={"elapsed_seconds": elapsed_seconds, "input_tokens": input_tokens},
        )

    def _build_execution_context_snapshot(
        self,
        *,
        message: str,
        required_capability: str | None,
        input_data: dict[str, Any],
        harness_plan: HarnessPlan,
    ) -> dict[str, Any]:
        """execution session に保存する context snapshot."""
        return {
            "message": message,
            "required_capability": required_capability,
            "input_data": input_data,
            "task_kind": harness_plan.task_kind,
            "execution_profile": harness_plan.execution_profile.value,
            "context_hierarchy": harness_plan.context_hierarchy.model_dump(mode="json"),
            "gate_policy": harness_plan.gate_policy.model_dump(mode="json"),
            "checkpoint_policy": harness_plan.checkpoint_policy.model_dump(mode="json"),
        }

    def _build_main_agent_context(self, *, harness_plan: HarnessPlan) -> dict[str, Any]:
        """main agent に渡す高信号コンテキスト."""
        return {
            "main_agent": {
                "session_summary": harness_plan.context_hierarchy.session_summary,
                "current_message": harness_plan.context_hierarchy.request_context.get("current_message", ""),
                "open_actions": list(harness_plan.context_hierarchy.open_actions),
                "task_goal": harness_plan.goal,
                "policy_summary": {
                    "global_policy": harness_plan.context_hierarchy.global_policy,
                    "gate_policy": harness_plan.gate_policy.model_dump(mode="json"),
                },
            }
        }

    def _build_specialist_execution_context(
        self,
        *,
        harness_plan: HarnessPlan,
        required_capability: str,
        selected_provider_evidence: list[dict[str, Any]],
    ) -> dict[str, Any]:
        """specialist に渡す実行コンテキスト."""
        allowed_tools = self._allowed_tools_for_role(harness_plan, role_name="specialist")
        return {
            "specialist": {
                "required_capability": required_capability,
                "request_context": harness_plan.context_hierarchy.request_context,
                "selected_provider_evidence": selected_provider_evidence,
                "allowed_tools": allowed_tools,
            }
        }

    def _build_verifier_context(
        self,
        *,
        harness_plan: HarnessPlan,
        result_payload: dict[str, Any],
    ) -> dict[str, Any]:
        """verifier に渡す実行コンテキスト."""
        return {
            "verifier": {
                "goal": harness_plan.goal,
                "result": result_payload,
                "verification_profile": harness_plan.verification.verification_profile,
                "success_signals": list(harness_plan.verification.success_signals),
            }
        }

    @staticmethod
    def _allowed_tools_for_role(harness_plan: HarnessPlan, *, role_name: str) -> list[str]:
        """指定 role の allowed tools を返す."""
        for worker in harness_plan.workers:
            if worker.role.value == role_name:
                return list(worker.allowed_tools)
        return []

    async def _record_verification_feedback(
        self,
        *,
        task_id: str,
        title: str,
        verification: Any,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """VerificationResult を feedback / decision へ変換する."""
        if self._execution_substrate is None:
            return
        await self._execution_substrate.record_action(
            task_id=task_id,
            stage="verification",
            action_type="verify_result",
            summary=title,
            status=ActionLogStatus.COMPLETED
            if bool(getattr(verification, "is_acceptable", False))
            else ActionLogStatus.FAILED,
            details={
                "verification_profile": str(
                    (metadata or {}).get("verifier_context", {}).get("verifier", {}).get("verification_profile", "")
                ),
                "score": float(getattr(verification, "score", 0.0)),
            },
        )
        await self._execution_substrate.record_feedback(
            task_id=task_id,
            source=ExecutionFeedbackSource.VERIFIER,
            title=title,
            feedback=str(getattr(verification, "feedback", "")),
            passed=bool(getattr(verification, "is_acceptable", False)),
            score=float(getattr(verification, "score", 0.0)),
            eval_result=EvalResult(
                evaluator=title,
                passed=bool(getattr(verification, "is_acceptable", False)),
                score=float(getattr(verification, "score", 0.0)),
                reason=str(getattr(verification, "feedback", "")),
                metadata=metadata or {},
            ),
            metadata={
                "should_replan": bool(getattr(verification, "should_replan", False)),
                "details": dict(getattr(verification, "details", {}) or {}),
                **(metadata or {}),
            },
        )
        await self._execution_substrate.record_decision(
            task_id=task_id,
            step=title,
            decision_type=DecisionType.BRANCH,
            choice="accept" if bool(getattr(verification, "is_acceptable", False)) else "replan",
            reason=str(getattr(verification, "feedback", "")),
            alternatives=["accept", "replan"],
            metadata={
                "score": float(getattr(verification, "score", 0.0)),
                "should_replan": bool(getattr(verification, "should_replan", False)),
                **(metadata or {}),
            },
        )
        await self._execution_substrate.record_checkpoint(
            task_id=task_id,
            stage="after_verification",
            snapshot={
                "title": title,
                "is_acceptable": bool(getattr(verification, "is_acceptable", False)),
                "score": float(getattr(verification, "score", 0.0)),
            },
        )

    async def _record_failure_feedback(
        self,
        *,
        task_id: str,
        message: str,
    ) -> None:
        """失敗を feedback として残す."""
        if self._execution_substrate is None:
            return
        await self._execution_substrate.record_feedback(
            task_id=task_id,
            source=ExecutionFeedbackSource.VERIFIER,
            title="execution_failure",
            feedback=message,
            passed=False,
            score=0.0,
            eval_result=EvalResult(
                evaluator="execution_failure",
                passed=False,
                score=0.0,
                reason=message,
            ),
        )

    async def _execution_response_fields(self, task_id: str) -> dict[str, Any]:
        """レスポンスへ追加する execution fields を返す."""
        if self._execution_substrate is None:
            return {"execution_session_id": "", "execution_summary": {}}
        inspection = await self._execution_substrate.inspect_task(task_id)
        if inspection is None:
            return {"execution_session_id": "", "execution_summary": {}}
        execution_session = inspection.execution_session
        if not isinstance(execution_session, dict):
            execution_session = {}  # type: ignore[unreachable]
        return {
            "execution_session_id": str(execution_session.get("session_id", "")),
            "execution_summary": inspection.execution_summary,
        }

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
        items: list[A2UIComponent] = []
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
        fields: list[A2UIComponent] = [
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
        await self._store.add_assistant_job_event(
            job_id=task_id, event_type=str(payload.get("event_type", "")), payload=payload
        )
        rooms = [f"task:{task_id}", task_id]
        if conversation_id:
            rooms.append(conversation_id)
            rooms.append(f"conversation:{conversation_id}")
        for room in rooms:
            await self._hub.broadcast_room(room, {"type": str(event.event_type.value), "data": payload})
            await self._hub.broadcast_room(room, {"type": to_legacy_dict(event).get("type", ""), "data": payload})
        await self._hub.broadcast({"type": str(event.event_type.value), "data": payload})
