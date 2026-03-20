"""Artifact-driven orchestration for the GEO Platform MVP."""

from __future__ import annotations

import asyncio
import logging
import threading
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, TypeVar
from uuid import uuid4

from apps.legacy_modernization_geo_platform.agents._models import (
    ReportAssemblerOutput,
)
from apps.legacy_modernization_geo_platform.backend.intelligence import (
    GeoIntelligenceAdapter,
)
from apps.legacy_modernization_geo_platform.backend.publisher import GeoPublisher
from apps.legacy_modernization_geo_platform.backend.qa import GeoQualityGate
from apps.legacy_modernization_geo_platform.backend.reporting import build_campaign_report
from apps.legacy_modernization_geo_platform.backend.repository import GeoRepository
from apps.legacy_modernization_geo_platform.backend.schemas import (
    AccountScoreArtifact,
    AccountSignalArtifact,
    ApprovalRecord,
    ApprovalRequest,
    ApprovalStatus,
    ArtifactMeta,
    BrandMemoryArtifact,
    CampaignReport,
    ContentBlueprintArtifact,
    ContentDraftArtifact,
    ContentDraftPage,
    EvidenceMatrixArtifact,
    GeoExecuteRequest,
    GeoQAReport,
    LegacySemanticsArtifact,
    PublishManifest,
    QuestionGraphArtifact,
    TaskCommandRequest,
    TaskEvent,
    TaskStateResponse,
    TaskStatus,
    normalize_content_language,
)
from apps.legacy_modernization_geo_platform.backend.settings import GeoPlatformSettings
from fastapi import WebSocket
from pydantic import BaseModel

from kernel.agents.app_agent_runtime import AppAgentRuntime


logger = logging.getLogger(__name__)
ModelT = TypeVar("ModelT", bound=BaseModel)

_APPROVAL_REASON_COPY: dict[str, str] = {
    "ja": "比較表現または数値・引用リスクのため",
    "en": "Comparative or numeric claims require human review before publishing.",
    "zh": "存在对比或数值表达风险，发布前需要人工复核。",
}
_REWRITE_SECTION_TITLE_COPY: dict[str, str] = {
    "ja": "レビュー反映メモ",
    "en": "Review Notes",
    "zh": "评审修订说明",
}
_REWRITE_ADJUSTMENT_NOTE_COPY: dict[str, str] = {
    "ja": "比較表現は適用条件を明示し、断定を避けるよう調整しました。",
    "en": "Comparative claims were rewritten with explicit conditions and non-absolute wording.",
    "zh": "已补充对比结论的适用条件，并调整为非绝对化表述。",
}
_REVIEW_REQUESTED_NOTE_COPY: dict[str, str] = {
    "ja": "レビュー要請",
    "en": "Review requested",
    "zh": "请求复审",
}


@dataclass(slots=True)
class PendingApproval:
    """In-memory approval wait state."""

    record: ApprovalRecord
    signal: threading.Event = field(default_factory=threading.Event)
    decision: ApprovalStatus | None = None
    comment: str | None = None
    reviewer_name: str | None = None


@dataclass(slots=True)
class GeoTaskRuntime:
    """Runtime state for a running task."""

    task_id: str
    request: GeoExecuteRequest
    status: TaskStatus = TaskStatus.QUEUED
    current_stage: str | None = None
    artifact_paths: dict[str, Path] = field(default_factory=dict)
    worker_thread: threading.Thread | None = None
    pending_approval: PendingApproval | None = None
    rewrite_iterations: int = 0
    cancel_requested: threading.Event = field(default_factory=threading.Event)


class GeoOrchestrator:
    """Manage task lifecycles, event streaming, and artifact generation."""

    def __init__(
        self,
        *,
        settings: GeoPlatformSettings,
        repository: GeoRepository,
        intelligence_adapter: GeoIntelligenceAdapter | None = None,
        quality_gate: GeoQualityGate | None = None,
        publisher: GeoPublisher | None = None,
        agent_runtime: AppAgentRuntime | None = None,
    ) -> None:
        """Initialize the orchestrator."""
        self._settings = settings
        self._repository = repository
        self._intelligence = intelligence_adapter or GeoIntelligenceAdapter()
        self._quality_gate = quality_gate or GeoQualityGate()
        self._publisher = publisher or GeoPublisher(settings)
        self._agent_runtime = agent_runtime
        self._runtimes: dict[str, GeoTaskRuntime] = {}
        self._subscribers: dict[str, set[WebSocket]] = {}
        self._broadcast_loop: asyncio.AbstractEventLoop | None = None
        self._register_agents()

    async def start(self, request: GeoExecuteRequest) -> str:
        """Start a new task asynchronously."""
        if self._broadcast_loop is None:
            self._broadcast_loop = asyncio.get_running_loop()
        task_id = f"geo-{datetime.now(timezone.utc):%Y%m%d%H%M%S}-{uuid4().hex[:6]}"
        runtime = GeoTaskRuntime(task_id=task_id, request=request)
        self._runtimes[task_id] = runtime
        self._repository.create_task(task_id, request, TaskStatus.QUEUED)
        worker = threading.Thread(
            target=self._run_pipeline,
            args=(runtime,),
            name=f"geo-worker-{task_id}",
            daemon=True,
        )
        runtime.worker_thread = worker
        worker.start()
        return task_id

    async def connect_websocket(self, task_id: str, websocket: WebSocket) -> None:
        """Register a websocket and replay history."""
        await websocket.accept()
        subscribers = self._subscribers.setdefault(task_id, set())
        subscribers.add(websocket)
        for event in self._repository.list_events(task_id):
            await websocket.send_json(event.model_dump(mode="json"))

    async def disconnect_websocket(self, task_id: str, websocket: WebSocket) -> None:
        """Remove a websocket subscriber."""
        subscribers = self._subscribers.get(task_id)
        if subscribers is None:
            return
        subscribers.discard(websocket)
        if not subscribers:
            self._subscribers.pop(task_id, None)

    def get_state(self, task_id: str) -> TaskStateResponse | None:
        """Resolve the persisted state for a task."""
        return self._repository.get_task_state(task_id)

    async def submit_approval(
        self,
        task_id: str,
        request_id: str,
        payload: ApprovalRequest,
    ) -> ApprovalRecord:
        """Apply an approval decision."""
        runtime = self._runtimes.get(task_id)
        status = payload.action or (ApprovalStatus.APPROVED if payload.approved else ApprovalStatus.REJECTED)
        updated = self._repository.update_approval(
            request_id,
            status=status,
            comment=payload.comment,
            reviewer_name=payload.reviewer_name,
        )
        if runtime is not None and runtime.pending_approval is not None:
            pending = runtime.pending_approval
            if pending.record.request_id == request_id:
                pending.decision = status
                pending.comment = payload.comment
                pending.reviewer_name = payload.reviewer_name
                pending.signal.set()
        self._emit_event(
            TaskEvent(
                event_type="approval_submitted",
                task_id=task_id,
                stage="publish_review",
                message=f"Approval {status.value} by {payload.reviewer_name}",
                payload={"request_id": request_id, "status": status.value},
            ),
        )
        return updated

    async def stream_events(self, task_id: str, *, poll_interval: float = 0.25) -> Any:
        """Persisted task event stream を順次返す."""
        seen = 0
        while True:
            events = self._repository.list_events(task_id)
            while seen < len(events):
                yield events[seen]
                seen += 1

            state = self._repository.get_task_state(task_id)
            if state is None:
                msg = f"Task not found: {task_id}"
                raise KeyError(msg)
            if state.status in {TaskStatus.COMPLETED, TaskStatus.FAILED, TaskStatus.CANCELLED} and seen >= len(events):
                break
            await asyncio.sleep(poll_interval)

    async def apply_command(self, task_id: str, command: TaskCommandRequest) -> dict[str, Any]:
        """Apply an operator command."""
        runtime = self._runtimes.get(task_id)
        if runtime is None:
            raise KeyError(f"Task not found: {task_id}")

        applied = False
        message = "Command accepted"
        if command.command == "content.rewrite":
            note = command.comment or str(command.payload.get("note", "")).strip()
            if not note:
                message = "Rewrite note is required"
            elif runtime.pending_approval is not None:
                self._repository.update_approval(
                    runtime.pending_approval.record.request_id,
                    status=ApprovalStatus.REWRITE,
                    comment=note,
                    reviewer_name=command.actor,
                )
                runtime.pending_approval.decision = ApprovalStatus.REWRITE
                runtime.pending_approval.comment = note
                runtime.pending_approval.reviewer_name = command.actor
                runtime.pending_approval.signal.set()
                applied = True
                message = "Rewrite queued for review stage"
            else:
                self._apply_rewrite(task_id=task_id, note=note)
                applied = True
                message = "Draft rewritten and QA re-evaluated"
        elif command.command == "campaign.kill":
            runtime.cancel_requested.set()
            if runtime.pending_approval is not None:
                runtime.pending_approval.signal.set()
            applied = True
            message = "Task cancellation requested"
        elif command.command == "content.publish":
            state = self._repository.get_task_state(task_id)
            if state is not None and state.published_pages:
                applied = True
                message = "Content already published"
            else:
                message = "Publish is controlled by the QA/approval pipeline"
        else:
            message = f"Unsupported command: {command.command}"

        payload = {
            "command": command.command,
            "status": "accepted" if applied else "ignored",
            "applied": applied,
            "message": message,
        }
        self._emit_event(
            TaskEvent(
                event_type="command_result",
                task_id=task_id,
                stage=runtime.current_stage,
                message=message,
                payload=payload,
            ),
        )
        return payload

    def _register_agents(self) -> None:
        """8 つの GEO Agent を A2AHub に登録."""
        from apps.legacy_modernization_geo_platform.agents.account_score_agent import AccountScoreAgent
        from apps.legacy_modernization_geo_platform.agents.brand_memory_agent import BrandMemoryAgent
        from apps.legacy_modernization_geo_platform.agents.content_blueprint_agent import ContentBlueprintAgent
        from apps.legacy_modernization_geo_platform.agents.content_draft_agent import ContentDraftAgent
        from apps.legacy_modernization_geo_platform.agents.demand_signal_agent import DemandSignalAgent
        from apps.legacy_modernization_geo_platform.agents.evidence_matrix_agent import EvidenceMatrixAgent
        from apps.legacy_modernization_geo_platform.agents.legacy_semantics_agent import LegacySemanticsAgent
        from apps.legacy_modernization_geo_platform.agents.question_graph_agent import QuestionGraphAgent

        from kernel.protocols.a2a_hub import get_hub

        hub = get_hub()
        agents = [
            BrandMemoryAgent(),
            DemandSignalAgent(),
            AccountScoreAgent(),
            QuestionGraphAgent(),
            EvidenceMatrixAgent(),
            LegacySemanticsAgent(),
            ContentBlueprintAgent(),
            ContentDraftAgent(),
        ]
        for agent in agents:
            agent_name = getattr(agent, "name", None) or type(agent).__name__
            if hub.discover(agent_name) is None:
                hub.register(agent)

    def _run_pipeline(self, runtime: GeoTaskRuntime) -> None:
        """Execute the MVP pipeline inside a dedicated worker thread."""
        task_id = runtime.task_id
        request = runtime.request
        try:
            runtime.status = TaskStatus.RUNNING
            self._repository.update_task(task_id, status=TaskStatus.RUNNING, current_stage="brand_memory")
            self._emit_event(
                TaskEvent(
                    event_type="flow.start",
                    task_id=task_id,
                    stage=request.package,
                    message=f"Campaign {request.campaign_name} started",
                ),
            )

            intelligence_snapshot = asyncio.run(self._intelligence.gather_market_intelligence(request))
            self._invoke_registered_agent(
                "Supervisor",
                {"task_id": task_id, "request": request.model_dump(mode="json")},
            )
            brand_memory = self._run_stage(
                runtime,
                stage="brand_memory",
                agent="BrandMemory",
                artifact_name="brand_memory_artifact",
                factory=lambda: self._execute_artifact_agent(
                    "BrandMemory",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                    },
                    BrandMemoryArtifact,
                    fallback=lambda: self._build_brand_memory(task_id, request),
                ),
            )
            signal_artifact = self._run_stage(
                runtime,
                stage="demand_signal",
                agent="DemandSignal",
                artifact_name="account_signal_artifact",
                factory=lambda: self._execute_artifact_agent(
                    "DemandSignal",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                        "intelligence_snapshot": intelligence_snapshot,
                    },
                    AccountSignalArtifact,
                    fallback=lambda: self._build_demand_signal(task_id, request, intelligence_snapshot),
                ),
            )
            score_artifact = self._run_stage(
                runtime,
                stage="icp_scoring",
                agent="AccountScore",
                artifact_name="account_score_artifact",
                factory=lambda: self._execute_artifact_agent(
                    "AccountScore",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                        "signal_artifact": signal_artifact.model_dump(mode="json"),
                    },
                    AccountScoreArtifact,
                    fallback=lambda: self._build_account_score(task_id, request, signal_artifact),
                ),
            )
            question_graph = self._run_stage(
                runtime,
                stage="question_map",
                agent="QuestionGraph",
                artifact_name="question_graph_artifact",
                factory=lambda: self._execute_artifact_agent(
                    "QuestionGraph",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                        "score_artifact": score_artifact.model_dump(mode="json"),
                    },
                    QuestionGraphArtifact,
                    fallback=lambda: self._build_question_graph(task_id, request, score_artifact),
                ),
            )
            evidence_matrix = self._run_stage(
                runtime,
                stage="evidence_collection",
                agent="EvidenceMatrix",
                artifact_name="evidence_matrix",
                factory=lambda: self._execute_artifact_agent(
                    "EvidenceMatrix",
                    {
                        "task_id": task_id,
                        "question_graph": question_graph.model_dump(mode="json"),
                        "intelligence_snapshot": intelligence_snapshot,
                    },
                    EvidenceMatrixArtifact,
                    fallback=lambda: self._build_evidence_matrix(task_id, question_graph, intelligence_snapshot),
                ),
            )
            legacy_semantics = self._run_stage(
                runtime,
                stage="legacy_semantics",
                agent="LegacySemantics",
                artifact_name="legacy_semantics_artifact",
                factory=lambda: self._execute_artifact_agent(
                    "LegacySemantics",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                        "brand_memory": brand_memory.model_dump(mode="json"),
                    },
                    LegacySemanticsArtifact,
                    fallback=lambda: self._build_legacy_semantics(task_id, request, brand_memory),
                ),
            )
            blueprint = self._run_stage(
                runtime,
                stage="strategy",
                agent="ContentBlueprint",
                artifact_name="content_blueprint_artifact",
                factory=lambda: self._execute_artifact_agent(
                    "ContentBlueprint",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                        "question_graph": question_graph.model_dump(mode="json"),
                    },
                    ContentBlueprintArtifact,
                    fallback=lambda: self._build_content_blueprint(task_id, request, question_graph),
                ),
            )
            content_draft = self._run_stage(
                runtime,
                stage="content_composition",
                agent="ContentDraft",
                artifact_name="content_draft_artifact",
                factory=lambda: self._execute_artifact_agent(
                    "ContentDraft",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                        "blueprint": blueprint.model_dump(mode="json"),
                        "evidence_matrix": evidence_matrix.model_dump(mode="json"),
                        "legacy_semantics": legacy_semantics.model_dump(mode="json"),
                    },
                    ContentDraftArtifact,
                    fallback=lambda: self._build_content_draft(
                        task_id,
                        request,
                        blueprint,
                        evidence_matrix,
                        legacy_semantics,
                    ),
                ),
            )
            qa_report = self._run_stage(
                runtime,
                stage="geo_qa",
                agent="GeoQA",
                artifact_name="geo_qa_report",
                factory=lambda: self._execute_artifact_agent(
                    "GeoQA",
                    {
                        "task_id": task_id,
                        "draft": content_draft.model_dump(mode="json"),
                        "evidence_matrix": evidence_matrix.model_dump(mode="json"),
                    },
                    GeoQAReport,
                    fallback=lambda: self._quality_gate.evaluate(
                        task_id=task_id,
                        draft=content_draft,
                        evidence_matrix=evidence_matrix,
                    ),
                ),
            )
            qa_report = self._handle_approval_loop(
                runtime=runtime,
                qa_report=qa_report,
                evidence_matrix=evidence_matrix,
                request=request,
            )
            if runtime.status in {TaskStatus.CANCELLED, TaskStatus.FAILED}:
                return
            publish_manifest = self._run_stage(
                runtime,
                stage="publishing",
                agent="Publishing",
                artifact_name="publish_manifest",
                factory=lambda: self._execute_artifact_agent(
                    "Publishing",
                    {
                        "task_id": task_id,
                        "draft": self._load_artifact(
                            task_id,
                            "content_draft_artifact",
                            ContentDraftArtifact,
                        ).model_dump(mode="json"),
                    },
                    PublishManifest,
                    fallback=lambda: self._publisher.publish(
                        task_id,
                        self._load_artifact(
                            task_id,
                            "content_draft_artifact",
                            ContentDraftArtifact,
                        ),
                    ),
                ),
            )
            for page in publish_manifest.pages:
                self._repository.save_published_page(page, task_id)
            self._emit_event(
                TaskEvent(
                    event_type="publish_complete",
                    task_id=task_id,
                    stage="publishing",
                    agent="Publishing",
                    message=f"Published {len(publish_manifest.pages)} page(s)",
                    payload={"page_urls": [item.page_url for item in publish_manifest.pages]},
                ),
            )
            report_output = self._execute_report_agent(
                {
                    "task_id": task_id,
                    "request": request.model_dump(mode="json"),
                    "signal_artifact": signal_artifact.model_dump(mode="json"),
                    "qa_report": qa_report.model_dump(mode="json"),
                    "publish_manifest": publish_manifest.model_dump(mode="json"),
                    "provider_status": {"primary_provider": intelligence_snapshot.primary_provider},
                },
                fallback=lambda: build_campaign_report(
                    task_id=task_id,
                    request=request,
                    signal_artifact=signal_artifact,
                    qa_report=qa_report,
                    publish_manifest=publish_manifest,
                    provider_status={"primary_provider": intelligence_snapshot.primary_provider},
                ),
            )
            self._run_stage(
                runtime,
                stage="report",
                agent="ReportAssembler",
                artifact_name="campaign_report",
                factory=lambda: report_output.artifact,
            )
            report_path = self._settings.reports_dir / f"{task_id}_campaign_report.md"
            report_path.write_text(report_output.markdown, encoding="utf-8")
            self._repository.save_report(task_id, report_output.markdown, report_output.summary)
            runtime.status = TaskStatus.COMPLETED
            self._repository.update_task(task_id, status=TaskStatus.COMPLETED, current_stage="report")
            self._emit_event(
                TaskEvent(
                    event_type="flow.complete",
                    task_id=task_id,
                    stage="report",
                    message="Campaign completed",
                    payload={"report_path": str(report_path)},
                ),
            )
        except Exception as exc:  # pragma: no cover - integration path
            logger.exception("geo pipeline failed for %s", task_id)
            runtime.status = TaskStatus.FAILED
            self._repository.update_task(
                task_id,
                status=TaskStatus.FAILED,
                current_stage=runtime.current_stage,
                error_message=str(exc),
            )
            self._emit_event(
                TaskEvent(
                    event_type="flow.error",
                    task_id=task_id,
                    stage=runtime.current_stage,
                    message=str(exc),
                ),
            )

    def _run_stage(
        self,
        runtime: GeoTaskRuntime,
        *,
        stage: str,
        agent: str,
        artifact_name: str,
        factory: Callable[[], ModelT],
    ) -> ModelT:
        """Run a stage, persist the artifact, and emit events."""
        self._check_cancelled(runtime)
        runtime.current_stage = stage
        self._repository.update_task(runtime.task_id, status=TaskStatus.RUNNING, current_stage=stage)
        self._emit_event(
            TaskEvent(
                event_type="node.start",
                task_id=runtime.task_id,
                stage=stage,
                agent=agent,
                message=f"{agent} started",
            ),
        )
        artifact = factory()
        summary = self._summarize_artifact(artifact_name, artifact)
        artifact_path = self._write_artifact(runtime.task_id, artifact_name, stage, artifact, summary)
        runtime.artifact_paths[artifact_name] = artifact_path
        self._emit_event(
            TaskEvent(
                event_type="artifact.created",
                task_id=runtime.task_id,
                stage=stage,
                agent=agent,
                message=summary,
                payload={
                    "artifact_name": artifact_name,
                    "artifact_path": str(artifact_path),
                    "summary": summary,
                },
            ),
        )
        self._emit_event(
            TaskEvent(
                event_type="node.complete",
                task_id=runtime.task_id,
                stage=stage,
                agent=agent,
                message=f"{agent} completed",
            ),
        )
        return artifact

    def _invoke_registered_agent(self, agent_name: str, input_data: dict[str, Any]) -> dict[str, Any] | None:
        """A2AHub 経由で Agent を同期呼び出し（スレッドセーフ）."""
        from kernel.protocols.a2a_hub import get_hub

        hub = get_hub()
        if hub.discover(agent_name) is not None:
            return asyncio.run(hub.call(agent_name, input_data))
        # フォールバック: AppAgentRuntime
        runtime = self._agent_runtime
        if runtime is not None and runtime.get_agent(agent_name) is not None:
            return asyncio.run(runtime.invoke(agent_name, input_data))
        return None

    def _execute_artifact_agent(
        self,
        agent_name: str,
        input_data: dict[str, Any],
        artifact_model: type[ModelT],
        *,
        fallback: Callable[[], ModelT],
    ) -> ModelT:
        result = self._invoke_registered_agent(agent_name, input_data)
        if result is None:
            return fallback()
        artifact_payload = result.get("artifact")
        if not isinstance(artifact_payload, dict):
            return fallback()
        return artifact_model.model_validate(artifact_payload)

    def _execute_report_agent(
        self,
        input_data: dict[str, Any],
        *,
        fallback: Callable[[], tuple[CampaignReport, str, dict[str, Any]]],
    ) -> ReportAssemblerOutput:
        result = self._invoke_registered_agent("ReportAssembler", input_data)
        if isinstance(result, dict):
            return ReportAssemblerOutput.model_validate(result)
        artifact, markdown, summary = fallback()
        return ReportAssemblerOutput(
            artifact=artifact,
            markdown=markdown,
            summary=summary,
        )

    @staticmethod
    def _resolve_runtime_language(runtime: GeoTaskRuntime) -> str:
        """Resolve normalized task language from execution request."""
        requested = runtime.request.inputs.content_languages
        first_language = requested[0] if requested else "ja"
        return normalize_content_language(first_language)

    def _handle_approval_loop(
        self,
        *,
        runtime: GeoTaskRuntime,
        qa_report: GeoQAReport,
        evidence_matrix: EvidenceMatrixArtifact,
        request: GeoExecuteRequest,
    ) -> GeoQAReport:
        """Pause for review while the report remains medium/high risk."""
        while request.constraints.human_approval_before_publish and qa_report.risk_level in {"MEDIUM", "HIGH"}:
            decision = self._request_approval(runtime, qa_report)
            if decision == ApprovalStatus.APPROVED:
                runtime.pending_approval = None
                return qa_report
            if decision == ApprovalStatus.REJECTED:
                runtime.pending_approval = None
                runtime.status = TaskStatus.FAILED
                self._repository.update_task(
                    runtime.task_id,
                    status=TaskStatus.FAILED,
                    current_stage="geo_qa",
                    error_message="Publishing rejected during approval",
                )
                self._emit_event(
                    TaskEvent(
                        event_type="flow.error",
                        task_id=runtime.task_id,
                        stage="geo_qa",
                        message="Publishing rejected during approval",
                    ),
                )
                return qa_report
            if decision == ApprovalStatus.REWRITE:
                language = self._resolve_runtime_language(runtime)
                default_note = _REVIEW_REQUESTED_NOTE_COPY[language]
                note = runtime.pending_approval.comment if runtime.pending_approval is not None else default_note
                if runtime.rewrite_iterations >= request.options.max_auto_iterations:
                    runtime.status = TaskStatus.FAILED
                    self._repository.update_task(
                        runtime.task_id,
                        status=TaskStatus.FAILED,
                        current_stage="geo_qa",
                        error_message="Rewrite limit reached",
                    )
                    self._emit_event(
                        TaskEvent(
                            event_type="flow.error",
                            task_id=runtime.task_id,
                            stage="geo_qa",
                            message="Rewrite limit reached",
                        ),
                    )
                    runtime.pending_approval = None
                    return qa_report
                self._apply_rewrite(task_id=runtime.task_id, note=note or default_note)
                runtime.rewrite_iterations += 1
                runtime.pending_approval = None
                qa_report = self._run_stage(
                    runtime,
                    stage="geo_qa",
                    agent="GeoQA",
                    artifact_name="geo_qa_report",
                    factory=lambda: self._quality_gate.evaluate(
                        task_id=runtime.task_id,
                        draft=self._load_artifact(runtime.task_id, "content_draft_artifact", ContentDraftArtifact),
                        evidence_matrix=evidence_matrix,
                    ),
                )
                continue
            runtime.pending_approval = None
            return qa_report
        return qa_report

    def _request_approval(self, runtime: GeoTaskRuntime, qa_report: GeoQAReport) -> ApprovalStatus:
        """Emit an approval event and wait for a decision."""
        language = self._resolve_runtime_language(runtime)
        approval_reason = _APPROVAL_REASON_COPY[language]
        request_id = f"apr-{uuid4().hex[:10]}"
        approval_record = self._repository.create_approval(
            request_id=request_id,
            task_id=runtime.task_id,
            stage="publish_review",
            object_id=runtime.task_id,
            risk_level=qa_report.risk_level,
            reason=approval_reason,
            actions=["approve", "reject", "rewrite"],
        )
        runtime.pending_approval = PendingApproval(record=approval_record)
        runtime.status = TaskStatus.WAITING_APPROVAL
        self._repository.update_task(runtime.task_id, status=TaskStatus.WAITING_APPROVAL, current_stage="geo_qa")
        self._emit_event(
            TaskEvent(
                event_type="approval_required",
                task_id=runtime.task_id,
                stage="publish_review",
                message="Human approval required before publish",
                payload={
                    "request_id": request_id,
                    "risk_level": qa_report.risk_level,
                    "reason": approval_reason,
                    "actions": ["approve", "reject", "rewrite"],
                },
            ),
        )
        while True:
            runtime.pending_approval.signal.wait(timeout=0.1)
            if runtime.cancel_requested.is_set():
                runtime.status = TaskStatus.CANCELLED
                self._repository.update_task(
                    runtime.task_id,
                    status=TaskStatus.CANCELLED,
                    current_stage="geo_qa",
                )
                self._emit_event(
                    TaskEvent(
                        event_type="flow.error",
                        task_id=runtime.task_id,
                        stage="geo_qa",
                        message="Task was cancelled",
                    ),
                )
                return ApprovalStatus.REJECTED
            if runtime.pending_approval.signal.is_set():
                break
        runtime.status = TaskStatus.RUNNING
        self._repository.update_task(runtime.task_id, status=TaskStatus.RUNNING, current_stage="geo_qa")
        return runtime.pending_approval.decision or ApprovalStatus.REJECTED

    def _apply_rewrite(self, *, task_id: str, note: str) -> None:
        """Apply a lightweight rewrite to the current draft artifact."""
        draft = self._load_artifact(task_id, "content_draft_artifact", ContentDraftArtifact)
        language = normalize_content_language(draft.target_language)
        section_title = _REWRITE_SECTION_TITLE_COPY[language]
        adjustment_note = _REWRITE_ADJUSTMENT_NOTE_COPY[language]
        rewritten_pages: list[ContentDraftPage] = []
        for page in draft.pages:
            rewritten_body = (
                f"{page.body_markdown}\n\n## {section_title}\n"
                f"- {note}\n"
                f"- {adjustment_note}"
            )
            rewritten_pages.append(
                ContentDraftPage(
                    slug=page.slug,
                    title=page.title,
                    summary=page.summary,
                    body_markdown=rewritten_body.replace("全面刷新", "段階刷新"),
                    cta=page.cta,
                    faq_entries=list(page.faq_entries),
                    json_ld=dict(page.json_ld),
                ),
            )
        rewritten = ContentDraftArtifact(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:rewrite", stage="content_rewrite"),
            pages=rewritten_pages,
            target_language=draft.target_language,
            evidence=list(draft.evidence),
            unknowns=[],
        )
        artifact_path = self._write_artifact(
            task_id,
            "content_draft_artifact",
            "content_rewrite",
            rewritten,
            "Content draft rewritten from operator feedback",
        )
        runtime = self._runtimes.get(task_id)
        if runtime is not None:
            runtime.artifact_paths["content_draft_artifact"] = artifact_path
        self._emit_event(
            TaskEvent(
                event_type="artifact.updated",
                task_id=task_id,
                stage="content_rewrite",
                agent="ContentComposition",
                message="Draft updated from review feedback",
                payload={"artifact_name": "content_draft_artifact", "artifact_path": str(artifact_path)},
            ),
        )

    def _emit_event(self, event: TaskEvent) -> None:
        """Persist and broadcast an event."""
        self._repository.add_event(event)
        loop = self._broadcast_loop
        if loop is None or not self._subscribers.get(event.task_id):
            return
        asyncio.run_coroutine_threadsafe(self._broadcast_event(event), loop)

    async def _broadcast_event(self, event: TaskEvent) -> None:
        """Send an event to connected websocket subscribers."""
        subscribers = list(self._subscribers.get(event.task_id, set()))
        payload = event.model_dump(mode="json")
        stale: list[WebSocket] = []
        for websocket in subscribers:
            try:
                await websocket.send_json(payload)
            except Exception:  # pragma: no cover - defensive cleanup
                stale.append(websocket)
        if stale:
            current = self._subscribers.get(event.task_id)
            if current is not None:
                for websocket in stale:
                    current.discard(websocket)

    def _write_artifact(
        self,
        task_id: str,
        artifact_name: str,
        stage: str,
        artifact: BaseModel,
        summary: str,
    ) -> Path:
        """Write an artifact to disk and index it in the repository."""
        artifact_dir = self._settings.artifacts_dir / task_id
        artifact_dir.mkdir(parents=True, exist_ok=True)
        artifact_path = artifact_dir / f"{artifact_name}.json"
        artifact_path.write_text(artifact.model_dump_json(indent=2), encoding="utf-8")
        self._repository.add_artifact(
            task_id,
            artifact_name=artifact_name,
            stage=stage,
            path=artifact_path,
            summary=summary,
        )
        return artifact_path

    def _load_artifact(self, task_id: str, artifact_name: str, model_type: type[ModelT]) -> ModelT:
        """Read an artifact back from disk."""
        artifact_path = self._settings.artifacts_dir / task_id / f"{artifact_name}.json"
        return model_type.model_validate_json(artifact_path.read_text(encoding="utf-8"))

    def _summarize_artifact(self, artifact_name: str, artifact: BaseModel) -> str:
        """Build a concise artifact summary."""
        if artifact_name == "account_signal_artifact":
            return f"Demand signals normalized: {len(getattr(artifact, 'signals', []))}"
        if artifact_name == "question_graph_artifact":
            return f"Question graph created for {len(getattr(artifact, 'personas', []))} persona(s)"
        if artifact_name == "content_draft_artifact":
            return f"Drafted {len(getattr(artifact, 'pages', []))} publishable page(s)"
        if artifact_name == "publish_manifest":
            return f"Published {len(getattr(artifact, 'pages', []))} page(s) and discovery files"
        return artifact_name.replace("_", " ")

    def _check_cancelled(self, runtime: GeoTaskRuntime) -> None:
        """Raise when cancellation has been requested."""
        if runtime.cancel_requested.is_set():
            runtime.status = TaskStatus.CANCELLED
            self._repository.update_task(
                runtime.task_id,
                status=TaskStatus.CANCELLED,
                current_stage=runtime.current_stage,
            )
            raise RuntimeError("Task cancelled")

