"""Artifact-driven orchestration for the GEO Platform MVP."""

from __future__ import annotations

import asyncio
import logging
import re
import threading
from dataclasses import dataclass, field
from datetime import datetime, timezone
from pathlib import Path
from typing import Any, Callable, TypeVar
from uuid import uuid4

from fastapi import WebSocket
from pydantic import BaseModel

from apps.Legacy_modernization_geo_platform.backend.intelligence import (
    GeoIntelligenceAdapter,
    IntelligenceSnapshot,
)
from apps.Legacy_modernization_geo_platform.backend.publisher import GeoPublisher
from apps.Legacy_modernization_geo_platform.backend.qa import GeoQualityGate
from apps.Legacy_modernization_geo_platform.backend.reporting import build_campaign_report
from apps.Legacy_modernization_geo_platform.backend.repository import GeoRepository
from apps.Legacy_modernization_geo_platform.backend.schemas import (
    AccountScoreArtifact,
    AccountScoreEntry,
    AccountSignalArtifact,
    ApprovalRecord,
    ApprovalRequest,
    ApprovalStatus,
    ArtifactMeta,
    BrandMemoryArtifact,
    CampaignReport,
    ContentBlueprintArtifact,
    ContentBlueprintPage,
    ContentDraftArtifact,
    ContentDraftPage,
    EvidenceMatrixArtifact,
    EvidenceMatrixEntry,
    FAQEntry,
    FunnelCluster,
    GeoExecuteRequest,
    GeoQAReport,
    LegacySemanticsArtifact,
    PersonaQuestionSet,
    QuestionGraphArtifact,
    SignalEntry,
    TaskCommandRequest,
    TaskEvent,
    TaskStateResponse,
    TaskStatus,
)
from apps.Legacy_modernization_geo_platform.backend.settings import GeoPlatformSettings


logger = logging.getLogger(__name__)
ModelT = TypeVar("ModelT", bound=BaseModel)


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
    ) -> None:
        """Initialize the orchestrator."""
        self._settings = settings
        self._repository = repository
        self._intelligence = intelligence_adapter or GeoIntelligenceAdapter()
        self._quality_gate = quality_gate or GeoQualityGate()
        self._publisher = publisher or GeoPublisher(settings)
        self._runtimes: dict[str, GeoTaskRuntime] = {}
        self._subscribers: dict[str, set[WebSocket]] = {}
        self._broadcast_loop: asyncio.AbstractEventLoop | None = None

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
            brand_memory = self._run_stage(
                runtime,
                stage="brand_memory",
                agent="BrandMemory",
                artifact_name="brand_memory_artifact",
                factory=lambda: self._build_brand_memory(task_id, request),
            )
            signal_artifact = self._run_stage(
                runtime,
                stage="demand_signal",
                agent="DemandSignal",
                artifact_name="account_signal_artifact",
                factory=lambda: self._build_demand_signal(task_id, request, intelligence_snapshot),
            )
            score_artifact = self._run_stage(
                runtime,
                stage="icp_scoring",
                agent="ICPScoring",
                artifact_name="account_score_artifact",
                factory=lambda: self._build_account_score(task_id, request, signal_artifact),
            )
            question_graph = self._run_stage(
                runtime,
                stage="question_map",
                agent="BuyerQuestionMap",
                artifact_name="question_graph_artifact",
                factory=lambda: self._build_question_graph(task_id, request, score_artifact),
            )
            evidence_matrix = self._run_stage(
                runtime,
                stage="evidence_collection",
                agent="EvidenceCollector",
                artifact_name="evidence_matrix",
                factory=lambda: self._build_evidence_matrix(task_id, question_graph, intelligence_snapshot),
            )
            legacy_semantics = self._run_stage(
                runtime,
                stage="legacy_semantics",
                agent="LegacySemantics",
                artifact_name="legacy_semantics_artifact",
                factory=lambda: self._build_legacy_semantics(task_id, request, brand_memory),
            )
            blueprint = self._run_stage(
                runtime,
                stage="strategy",
                agent="Strategy",
                artifact_name="content_blueprint_artifact",
                factory=lambda: self._build_content_blueprint(task_id, request, question_graph),
            )
            content_draft = self._run_stage(
                runtime,
                stage="content_composition",
                agent="ContentComposition",
                artifact_name="content_draft_artifact",
                factory=lambda: self._build_content_draft(
                    task_id,
                    request,
                    blueprint,
                    evidence_matrix,
                    legacy_semantics,
                ),
            )
            qa_report = self._run_stage(
                runtime,
                stage="geo_qa",
                agent="GeoQA",
                artifact_name="geo_qa_report",
                factory=lambda: self._quality_gate.evaluate(
                    task_id=task_id,
                    draft=content_draft,
                    evidence_matrix=evidence_matrix,
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
                factory=lambda: self._publisher.publish(
                    task_id,
                    self._load_artifact(task_id, "content_draft_artifact", ContentDraftArtifact),
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
            report_artifact, markdown, summary = build_campaign_report(
                task_id=task_id,
                request=request,
                signal_artifact=signal_artifact,
                qa_report=qa_report,
                publish_manifest=publish_manifest,
                provider_status={"primary_provider": intelligence_snapshot.primary_provider},
            )
            self._run_stage(
                runtime,
                stage="report",
                agent="ReportAssembler",
                artifact_name="campaign_report",
                factory=lambda: report_artifact,
            )
            report_path = self._settings.reports_dir / f"{task_id}_campaign_report.md"
            report_path.write_text(markdown, encoding="utf-8")
            self._repository.save_report(task_id, markdown, summary)
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
                note = runtime.pending_approval.comment if runtime.pending_approval is not None else "Review requested"
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
                self._apply_rewrite(task_id=runtime.task_id, note=note or "Review requested")
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
        request_id = f"apr-{uuid4().hex[:10]}"
        approval_record = self._repository.create_approval(
            request_id=request_id,
            task_id=runtime.task_id,
            stage="publish_review",
            object_id=runtime.task_id,
            risk_level=qa_report.risk_level,
            reason="比較表現または数値・引用リスクのため",
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
                    "reason": "比較表現または数値・引用リスクのため",
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
        rewritten_pages: list[ContentDraftPage] = []
        for page in draft.pages:
            rewritten_body = (
                f"{page.body_markdown}\n\n## レビュー反映メモ\n"
                f"- {note}\n"
                "- 比較表現は適用条件を明示し、断定を避けるよう調整しました。"
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

    def _build_brand_memory(self, task_id: str, request: GeoExecuteRequest) -> BrandMemoryArtifact:
        """Build brand memory artifact."""
        stacks = request.targets.legacy_stacks or request.inputs.target_services
        regions = request.targets.regions or request.inputs.regions or ["Japan"]
        return BrandMemoryArtifact(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:brand_memory", stage="brand_memory"),
            positioning="AI検索時代に旧システム刷新の検討需要を捕捉し、診断から段階移行まで導く。",
            differentiators=[
                "COBOL/RPG/旧Java を横断した刷新提案",
                "Business semantics を前提にした段階移行",
                "AI検索向けの根拠付き情報資産を継続生成",
            ],
            supported_stacks=stacks or ["COBOL", "RPG", "Struts"],
            target_regions=regions,
        )

    def _build_demand_signal(
        self,
        task_id: str,
        request: GeoExecuteRequest,
        intelligence_snapshot: IntelligenceSnapshot,
    ) -> AccountSignalArtifact:
        """Build account signal artifact from live intelligence."""
        company = (
            request.inputs.target_accounts[0]
            if request.inputs.target_accounts
            else f"{(request.targets.industries or ['enterprise'])[0]} prospects"
        )
        signals = [
            SignalEntry(
                type="tech_stack",
                description=source.summary or source.snippet or source.title,
                source=f"{source.title} ({source.publisher})",
                confidence=0.78 if source.reliability == "HIGH" else 0.62,
            )
            for source in intelligence_snapshot.sources[:5]
        ]
        fit_score = min(96, 58 + len(signals) * 7 + len(request.targets.legacy_stacks) * 4)
        return AccountSignalArtifact(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:demand_signal", stage="demand_signal"),
            company=company,
            signals=signals,
            urgency_hypothesis="保守要員不足と技術負債圧縮の同時要請が強い",
            modernization_fit_score=fit_score,
            evidence=[item.to_evidence_dict() for item in intelligence_snapshot.sources[:5]],
            unknowns=list(dict.fromkeys(intelligence_snapshot.warnings)),
            extensions={"primary_provider": intelligence_snapshot.primary_provider},
        )

    def _build_account_score(
        self,
        task_id: str,
        request: GeoExecuteRequest,
        signal_artifact: AccountSignalArtifact,
    ) -> AccountScoreArtifact:
        """Build account scoring artifact."""
        accounts = request.inputs.target_accounts or [signal_artifact.company]
        base_fit = signal_artifact.modernization_fit_score
        urgency_seed = 70 if "不足" in signal_artifact.urgency_hypothesis else 58
        scores = [
            AccountScoreEntry(
                company=company,
                fit_score=max(55, min(97, base_fit - index * 3)),
                urgency_score=max(52, min(95, urgency_seed + len(request.targets.legacy_stacks) * 3 - index * 2)),
                priority="high" if base_fit - index * 3 >= 80 else "medium",
                rationale=[
                    signal_artifact.urgency_hypothesis,
                    f"Primary stacks: {', '.join(request.targets.legacy_stacks or ['legacy'])}",
                ],
            )
            for index, company in enumerate(accounts[:3])
        ]
        return AccountScoreArtifact(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:icp_scoring", stage="icp_scoring"),
            account_scores=scores,
            recommended_focus=request.targets.legacy_stacks or request.inputs.target_services or ["COBOL modernization"],
            evidence=[item.model_dump(mode="json") for item in scores],
        )

    def _build_question_graph(
        self,
        task_id: str,
        request: GeoExecuteRequest,
        score_artifact: AccountScoreArtifact,
    ) -> QuestionGraphArtifact:
        """Build buyer question clusters."""
        stack = (request.targets.legacy_stacks or ["COBOL"])[0]
        industry = (request.targets.industries or ["manufacturing"])[0]
        personas = [
            PersonaQuestionSet(
                role="cio",
                questions=[
                    f"{stack} を全面刷新せずに段階移行できるか",
                    "投資対効果をどう説明するか",
                ],
                high_intent_questions=[
                    f"{industry} での {stack} 段階移行事例はあるか",
                ],
            ),
            PersonaQuestionSet(
                role="it_manager",
                questions=[
                    "既存業務ルールを失わずに移行設計できるか",
                    "保守要員不足にどう備えるか",
                ],
                high_intent_questions=[
                    f"{stack} から Java/Spring Boot への移行順序はどう決めるか",
                ],
            ),
            PersonaQuestionSet(
                role="engineering_lead",
                questions=[
                    "影響分析とテスト生成をどこまで自動化できるか",
                    "既存バッチとAPIを共存させられるか",
                ],
                high_intent_questions=[
                    "段階移行中の品質ゲートをどう設計するか",
                ],
            ),
        ]
        funnel_clusters = [
            FunnelCluster(
                stage="comparison",
                questions=[item for persona in personas for item in persona.high_intent_questions[:1]],
                recommended_page_type="comparison_page",
            ),
            FunnelCluster(
                stage="approval",
                questions=[
                    "経営層に説明できる移行ロードマップはどう作るか",
                    "一括刷新と段階移行のリスク差分は何か",
                ],
                recommended_page_type="executive_lp",
            ),
        ]
        return QuestionGraphArtifact(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:question_map", stage="question_map"),
            personas=personas,
            funnel_clusters=funnel_clusters,
            content_clusters=[
                {
                    "theme": f"{industry} {stack} modernization",
                    "recommended_asset": "industry_lp",
                    "priority": score_artifact.account_scores[0].priority if score_artifact.account_scores else "medium",
                }
            ],
        )

    def _build_evidence_matrix(
        self,
        task_id: str,
        question_graph: QuestionGraphArtifact,
        intelligence_snapshot: IntelligenceSnapshot,
    ) -> EvidenceMatrixArtifact:
        """Build evidence rows from live sources."""
        questions = [
            question
            for persona in question_graph.personas
            for question in [*persona.high_intent_questions, *persona.questions]
        ]
        entries: list[EvidenceMatrixEntry] = []
        for index, question in enumerate(questions[:6]):
            if not intelligence_snapshot.sources:
                break
            source = intelligence_snapshot.sources[index % len(intelligence_snapshot.sources)]
            entries.append(
                EvidenceMatrixEntry(
                    claim=f"{question} に対して、段階移行と業務ルール保持の両立が重要である。",
                    question_ref=question,
                    source_url=source.url,
                    title=source.title,
                    publisher=source.publisher,
                    summary=source.summary,
                    snippet=source.snippet,
                    reliability=source.reliability,
                    citation_ready=source.citation_ready,
                    fresh=source.is_fresh,
                ),
            )
        return EvidenceMatrixArtifact(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:evidence", stage="evidence_collection"),
            entries=entries,
            provider_status={
                "primary_provider": intelligence_snapshot.primary_provider,
                "warnings": intelligence_snapshot.warnings,
                "source_count": len(entries),
            },
            evidence=[item.model_dump(mode="json") for item in entries],
            unknowns=list(dict.fromkeys(intelligence_snapshot.warnings)),
        )

    def _build_legacy_semantics(
        self,
        task_id: str,
        request: GeoExecuteRequest,
        brand_memory: BrandMemoryArtifact,
    ) -> LegacySemanticsArtifact:
        """Build legacy semantics artifact."""
        stack = (request.targets.legacy_stacks or brand_memory.supported_stacks or ["legacy"])[0]
        return LegacySemanticsArtifact(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:legacy_semantics", stage="legacy_semantics"),
            business_processes=["受注処理", "請求管理", "在庫更新"],
            business_events=["バッチ締め処理", "帳票出力", "マスタ同期"],
            state_model={
                "source_stack": stack,
                "migration_style": "incremental",
                "target_runtime": "Java / Spring Boot",
            },
            business_rules=[
                "業務ルールをコード変換前に明文化する",
                "影響分析の単位はジョブ・画面・帳票で分割する",
                "品質ゲートを段階移行ごとに設ける",
            ],
        )

    def _build_content_blueprint(
        self,
        task_id: str,
        request: GeoExecuteRequest,
        question_graph: QuestionGraphArtifact,
    ) -> ContentBlueprintArtifact:
        """Build page blueprint artifact."""
        industry = _slugify((request.targets.industries or ["enterprise"])[0])
        stack = _slugify((request.targets.legacy_stacks or ["legacy"])[0])
        primary_question = question_graph.personas[0].high_intent_questions[0]
        page = ContentBlueprintPage(
            slug=f"{industry}-{stack}-modernization-guide",
            title=f"{(request.targets.industries or ['製造業'])[0]} 向け {(request.targets.legacy_stacks or ['COBOL'])[0]} モダナイゼーションガイド",
            persona="it_manager",
            primary_question=primary_question,
            page_type="industry_lp",
            cta="無料診断を依頼する",
        )
        return ContentBlueprintArtifact(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:strategy", stage="strategy"),
            pages=[page],
            target_language=(request.inputs.content_languages or ["ja"])[0],
        )

    def _build_content_draft(
        self,
        task_id: str,
        request: GeoExecuteRequest,
        blueprint: ContentBlueprintArtifact,
        evidence_matrix: EvidenceMatrixArtifact,
        legacy_semantics: LegacySemanticsArtifact,
    ) -> ContentDraftArtifact:
        """Build deterministic page drafts."""
        pages: list[ContentDraftPage] = []
        question = blueprint.pages[0].primary_question if blueprint.pages else "段階移行は可能か"
        evidence_lines = [
            f"- 根拠: {entry.title} / {entry.publisher} / {entry.source_url}"
            for entry in evidence_matrix.entries[:4]
        ]
        semantics_lines = [f"- {item}" for item in legacy_semantics.business_rules]
        for page in blueprint.pages:
            body_markdown = "\n".join(
                [
                    f"# {page.title}",
                    "",
                    "## 課題の背景",
                    "旧システム刷新では、保守要員不足・調査難易度・一括刷新リスクが同時に発生します。",
                    "",
                    "## 段階移行の考え方",
                    "既存資産を可視化し、業務ルールを抽出してから API・バッチ・画面を順次刷新します。",
                    "",
                    "## 比較観点",
                    "全面刷新と比較すると、段階刷新は業務停止リスクを抑えつつ投資判断を分割できます。",
                    "",
                    "## 根拠サマリー",
                    *evidence_lines,
                    "",
                    "## 業務意味の維持ポイント",
                    *semantics_lines,
                    "",
                    "## 次の一手",
                    f"{question} を起点に、対象資産の棚卸しと優先順位付けから着手します。",
                ],
            )
            faq_entries = [
                FAQEntry(question=question, answer="段階移行は現行業務の安定運用と並行して実施できます。"),
                FAQEntry(question="一括刷新との違いは何ですか", answer="投資とリスクを工程ごとに分割できる点です。"),
                FAQEntry(question="どこから調査を始めるべきですか", answer="高頻度バッチ、帳票、外部連携を優先して可視化します。"),
            ]
            json_ld = {
                "@context": "https://schema.org",
                "@type": "FAQPage",
                "inLanguage": "ja-JP",
                "mainEntity": [
                    {
                        "@type": "Question",
                        "name": entry.question,
                        "acceptedAnswer": {"@type": "Answer", "text": entry.answer},
                    }
                    for entry in faq_entries
                ],
            }
            pages.append(
                ContentDraftPage(
                    slug=page.slug,
                    title=page.title,
                    summary=(
                        f"{(request.targets.legacy_stacks or ['旧システム'])[0]} の刷新を、"
                        "診断・業務ルール抽出・段階移行で進めるための実務ガイド。"
                    ),
                    body_markdown=body_markdown,
                    cta=page.cta,
                    faq_entries=faq_entries,
                    json_ld=json_ld,
                ),
            )
        return ContentDraftArtifact(
            meta=ArtifactMeta(task_id=task_id, trace_id=f"{task_id}:content", stage="content_composition"),
            pages=pages,
            target_language=blueprint.target_language,
            evidence=[item.model_dump(mode="json") for item in evidence_matrix.entries[:6]],
            unknowns=list(evidence_matrix.unknowns),
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


def _slugify(value: str) -> str:
    """Create a URL-safe slug from the provided value."""
    lowered = value.strip().lower()
    normalized = re.sub(r"[^a-z0-9]+", "-", lowered)
    return normalized.strip("-") or "geo-page"
