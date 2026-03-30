"""Artifact-driven orchestration for the GEO Platform MVP.

パイプライン制御・イベント配信・タスクライフサイクル管理を担当。
承認ロジックは approval_handler、Agent 呼び出しは pipeline_stages に分離。
パイプラインは asyncio.create_task で駆動し、並列ステージは asyncio.gather で実行する。
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any, TypeVar
from uuid import uuid4

from pydantic import BaseModel

from apps.legacy_modernization_geo_platform.backend.approval_handler import (
    ApprovalContext,
    ApprovalStatus,
    PendingApproval,
    apply_rewrite,
    create_approval_flow,
    handle_approval_loop,
)
from apps.legacy_modernization_geo_platform.backend.intelligence import (
    GeoIntelligenceAdapter,
)
from apps.legacy_modernization_geo_platform.backend.pipeline_stages import (
    execute_artifact_agent,
    execute_report_agent,
    summarize_artifact,
)
from apps.legacy_modernization_geo_platform.backend.publisher import GeoPublisher
from apps.legacy_modernization_geo_platform.backend.qa import GeoQualityGate
from apps.legacy_modernization_geo_platform.backend.schemas import (
    AccountScoreArtifact,
    AccountSignalArtifact,
    ApprovalRecord,
    ApprovalRequest,
    BrandMemoryArtifact,
    ContentBlueprintArtifact,
    ContentDraftArtifact,
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
)

if TYPE_CHECKING:
    from collections.abc import Callable
    from pathlib import Path

    from fastapi import WebSocket

    from apps.legacy_modernization_geo_platform.backend.repository import GeoRepository
    from apps.legacy_modernization_geo_platform.backend.settings import GeoPlatformSettings
    from kernel.agents.app_agent_runtime import AppAgentRuntime

logger = logging.getLogger(__name__)
ModelT = TypeVar("ModelT", bound=BaseModel)


@dataclass(slots=True)
class GeoTaskRuntime:
    """実行中タスクのランタイム状態."""

    task_id: str
    request: GeoExecuteRequest
    status: TaskStatus = TaskStatus.QUEUED
    current_stage: str | None = None
    artifact_paths: dict[str, Path] = field(default_factory=dict)
    worker_task: asyncio.Task[None] | None = None
    pending_approval: PendingApproval | None = None
    rewrite_iterations: int = 0
    cancel_requested: asyncio.Event = field(default_factory=asyncio.Event)


class GeoOrchestrator:
    """タスクライフサイクル、イベントストリーミング、成果物生成を管理する."""

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
        self._settings = settings
        self._repository = repository
        self._intelligence = intelligence_adapter or GeoIntelligenceAdapter()
        self._quality_gate = quality_gate or GeoQualityGate()
        self._publisher = publisher or GeoPublisher(settings)
        self._agent_runtime = agent_runtime
        self._runtimes: dict[str, GeoTaskRuntime] = {}
        self._subscribers: dict[str, set[WebSocket]] = {}
        self._broadcast_loop: asyncio.AbstractEventLoop | None = None
        self._approval_flow = create_approval_flow()

    # ------------------------------------------------------------------
    # 承認コンテキスト構築
    # ------------------------------------------------------------------

    def _approval_context(self) -> ApprovalContext:
        """承認ハンドラへ渡す依存コンテキストを構築する."""
        return ApprovalContext(
            repository=self._repository,
            quality_gate=self._quality_gate,
            settings=self._settings,
            runtimes=self._runtimes,
            emit_event=self._emit_event,
            run_stage=self._run_stage,
            write_artifact=self._write_artifact,
            load_artifact=self._load_artifact,
            agent_runtime=self._agent_runtime,
            approval_flow=self._approval_flow,
        )

    # ------------------------------------------------------------------
    # パブリック API
    # ------------------------------------------------------------------

    async def start(self, request: GeoExecuteRequest) -> str:
        """新しいタスクを非同期で開始する."""
        if self._broadcast_loop is None:
            self._broadcast_loop = asyncio.get_running_loop()
        task_id = f"geo-{datetime.now(UTC):%Y%m%d%H%M%S}-{uuid4().hex[:6]}"
        runtime = GeoTaskRuntime(task_id=task_id, request=request)
        self._runtimes[task_id] = runtime
        self._repository.create_task(task_id, request, TaskStatus.QUEUED)
        task = asyncio.create_task(
            self._run_pipeline(runtime),
            name=f"geo-worker-{task_id}",
        )
        runtime.worker_task = task
        return task_id

    async def connect_websocket(self, task_id: str, websocket: WebSocket) -> None:
        """WebSocket を登録し、履歴を再生する."""
        await websocket.accept()
        subscribers = self._subscribers.setdefault(task_id, set())
        subscribers.add(websocket)
        for event in self._repository.list_events(task_id):
            await websocket.send_json(event.model_dump(mode="json"))

    async def disconnect_websocket(self, task_id: str, websocket: WebSocket) -> None:
        """WebSocket サブスクライバーを削除する."""
        subscribers = self._subscribers.get(task_id)
        if subscribers is None:
            return
        subscribers.discard(websocket)
        if not subscribers:
            self._subscribers.pop(task_id, None)

    def get_state(self, task_id: str) -> TaskStateResponse | None:
        """永続化されたタスク状態を返す."""
        return self._repository.get_task_state(task_id)

    async def submit_approval(
        self,
        task_id: str,
        request_id: str,
        payload: ApprovalRequest,
    ) -> ApprovalRecord:
        """承認決定を適用する."""
        runtime = self._runtimes.get(task_id)
        try:
            status = (
                ApprovalStatus(payload.action)
                if payload.action
                else (ApprovalStatus.APPROVED if payload.approved else ApprovalStatus.REJECTED)
            )
        except ValueError:
            status = ApprovalStatus.APPROVED if payload.approved else ApprovalStatus.REJECTED
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
        """永続化されたタスクイベントストリームを順次返す."""
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
        """オペレーターコマンドを適用する."""
        runtime = self._runtimes.get(task_id)
        if runtime is None:
            msg = f"Task not found: {task_id}"
            raise KeyError(msg)

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
                apply_rewrite(self._approval_context(), task_id=task_id, note=note)
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

        result_payload = {
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
                payload=result_payload,
            ),
        )
        return result_payload

    # ------------------------------------------------------------------
    # パイプライン実行（async + 並列ステージ）
    # ------------------------------------------------------------------

    async def _run_pipeline(self, runtime: GeoTaskRuntime) -> None:
        """非同期パイプラインを実行する（asyncio.create_task から駆動）."""
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

            intelligence_snapshot = await self._intelligence.gather_market_intelligence(request)
            rt = self._agent_runtime

            # --- 並列グループ 1: BrandMemory + DemandSignal ---
            brand_memory, signal_artifact = await asyncio.gather(
                self._run_stage(
                    runtime,
                    stage="brand_memory",
                    agent="BrandMemory",
                    artifact_name="brand_memory_artifact",
                    factory=lambda: execute_artifact_agent(
                        rt,
                        "BrandMemory",
                        {"task_id": task_id, "request": request.model_dump(mode="json")},
                        BrandMemoryArtifact,
                    ),
                ),
                self._run_stage(
                    runtime,
                    stage="demand_signal",
                    agent="DemandSignal",
                    artifact_name="account_signal_artifact",
                    factory=lambda: execute_artifact_agent(
                        rt,
                        "DemandSignal",
                        {
                            "task_id": task_id,
                            "request": request.model_dump(mode="json"),
                            "intelligence_snapshot": intelligence_snapshot,
                        },
                        AccountSignalArtifact,
                    ),
                ),
            )

            score_artifact = await self._run_stage(
                runtime,
                stage="icp_scoring",
                agent="AccountScore",
                artifact_name="account_score_artifact",
                factory=lambda: execute_artifact_agent(
                    rt,
                    "AccountScore",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                        "signal_artifact": signal_artifact.model_dump(mode="json"),
                    },
                    AccountScoreArtifact,
                ),
            )

            # --- 並列グループ 2: QuestionGraph + LegacySemantics ---
            question_graph, legacy_semantics = await asyncio.gather(
                self._run_stage(
                    runtime,
                    stage="question_map",
                    agent="QuestionGraph",
                    artifact_name="question_graph_artifact",
                    factory=lambda: execute_artifact_agent(
                        rt,
                        "QuestionGraph",
                        {
                            "task_id": task_id,
                            "request": request.model_dump(mode="json"),
                            "score_artifact": score_artifact.model_dump(mode="json"),
                        },
                        QuestionGraphArtifact,
                    ),
                ),
                self._run_stage(
                    runtime,
                    stage="legacy_semantics",
                    agent="LegacySemantics",
                    artifact_name="legacy_semantics_artifact",
                    factory=lambda: execute_artifact_agent(
                        rt,
                        "LegacySemantics",
                        {
                            "task_id": task_id,
                            "request": request.model_dump(mode="json"),
                            "brand_memory": brand_memory.model_dump(mode="json"),
                        },
                        LegacySemanticsArtifact,
                    ),
                ),
            )

            evidence_matrix = await self._run_stage(
                runtime,
                stage="evidence_collection",
                agent="EvidenceMatrix",
                artifact_name="evidence_matrix",
                factory=lambda: execute_artifact_agent(
                    rt,
                    "EvidenceMatrix",
                    {
                        "task_id": task_id,
                        "question_graph": question_graph.model_dump(mode="json"),
                        "intelligence_snapshot": intelligence_snapshot,
                    },
                    EvidenceMatrixArtifact,
                ),
            )
            blueprint = await self._run_stage(
                runtime,
                stage="strategy",
                agent="ContentBlueprint",
                artifact_name="content_blueprint_artifact",
                factory=lambda: execute_artifact_agent(
                    rt,
                    "ContentBlueprint",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                        "question_graph": question_graph.model_dump(mode="json"),
                    },
                    ContentBlueprintArtifact,
                ),
            )
            content_draft = await self._run_stage(
                runtime,
                stage="content_composition",
                agent="ContentDraft",
                artifact_name="content_draft_artifact",
                factory=lambda: execute_artifact_agent(
                    rt,
                    "ContentDraft",
                    {
                        "task_id": task_id,
                        "request": request.model_dump(mode="json"),
                        "blueprint": blueprint.model_dump(mode="json"),
                        "evidence_matrix": evidence_matrix.model_dump(mode="json"),
                        "legacy_semantics": legacy_semantics.model_dump(mode="json"),
                    },
                    ContentDraftArtifact,
                ),
            )
            qa_report = await self._run_stage(
                runtime,
                stage="geo_qa",
                agent="GeoQA",
                artifact_name="geo_qa_report",
                factory=lambda: execute_artifact_agent(
                    rt,
                    "GeoQA",
                    {
                        "task_id": task_id,
                        "draft": content_draft.model_dump(mode="json"),
                        "evidence_matrix": evidence_matrix.model_dump(mode="json"),
                    },
                    GeoQAReport,
                ),
            )

            qa_report = await handle_approval_loop(
                self._approval_context(),
                runtime=runtime,
                qa_report=qa_report,
                evidence_matrix=evidence_matrix,
                request=request,
            )
            if runtime.status in {TaskStatus.CANCELLED, TaskStatus.FAILED}:
                return

            publish_manifest = await self._run_stage(
                runtime,
                stage="publishing",
                agent="Publishing",
                artifact_name="publish_manifest",
                factory=lambda: execute_artifact_agent(
                    rt,
                    "Publishing",
                    {
                        "task_id": task_id,
                        "draft": self._load_artifact(
                            task_id, "content_draft_artifact", ContentDraftArtifact
                        ).model_dump(mode="json"),
                    },
                    PublishManifest,
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

            report_output = await execute_report_agent(
                rt,
                {
                    "task_id": task_id,
                    "request": request.model_dump(mode="json"),
                    "signal_artifact": signal_artifact.model_dump(mode="json"),
                    "qa_report": qa_report.model_dump(mode="json"),
                    "publish_manifest": publish_manifest.model_dump(mode="json"),
                    "provider_status": {"primary_provider": intelligence_snapshot.primary_provider},
                },
            )
            await self._run_stage(
                runtime,
                stage="report",
                agent="ReportAssembler",
                artifact_name="campaign_report",
                artifact=report_output.artifact,
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
                TaskEvent(event_type="flow.error", task_id=task_id, stage=runtime.current_stage, message=str(exc)),
            )

    # ------------------------------------------------------------------
    # ステージ実行・成果物永続化
    # ------------------------------------------------------------------

    async def _run_stage(
        self,
        runtime: GeoTaskRuntime,
        *,
        stage: str,
        agent: str,
        artifact_name: str,
        factory: Callable[[], Any] | None = None,
        artifact: BaseModel | None = None,
    ) -> Any:
        """非同期でステージを実行し、成果物を永続化してイベントを発行する.

        factory が coroutine を返す場合は await する。
        artifact を直接渡した場合は factory を無視して永続化のみ行う。
        """
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
        if artifact is None:
            if factory is None:
                msg = "factory または artifact のいずれかを指定する必要があります"
                raise ValueError(msg)
            result = factory()
            if asyncio.iscoroutine(result):
                artifact = await result
            else:
                artifact = result
        summary = summarize_artifact(artifact_name, artifact)
        artifact_path = self._write_artifact(runtime.task_id, artifact_name, stage, artifact, summary)
        runtime.artifact_paths[artifact_name] = artifact_path
        self._emit_event(
            TaskEvent(
                event_type="artifact.created",
                task_id=runtime.task_id,
                stage=stage,
                agent=agent,
                message=summary,
                payload={"artifact_name": artifact_name, "artifact_path": str(artifact_path), "summary": summary},
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

    # ------------------------------------------------------------------
    # イベント配信
    # ------------------------------------------------------------------

    def _emit_event(self, event: TaskEvent) -> None:
        """イベントを永続化し、WebSocket サブスクライバーへブロードキャストする."""
        self._repository.add_event(event)
        loop = self._broadcast_loop
        if loop is None or not self._subscribers.get(event.task_id):
            return
        asyncio.run_coroutine_threadsafe(self._broadcast_event(event), loop)

    async def _broadcast_event(self, event: TaskEvent) -> None:
        """接続中の WebSocket へイベントを送信する."""
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

    # ------------------------------------------------------------------
    # 成果物 I/O
    # ------------------------------------------------------------------

    def _write_artifact(
        self,
        task_id: str,
        artifact_name: str,
        stage: str,
        artifact: BaseModel,
        summary: str,
    ) -> Path:
        """成果物をディスクに書き込み、リポジトリにインデックスする."""
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
        """ディスクから成果物を読み込む."""
        artifact_path = self._settings.artifacts_dir / task_id / f"{artifact_name}.json"
        return model_type.model_validate_json(artifact_path.read_text(encoding="utf-8"))

    def _check_cancelled(self, runtime: GeoTaskRuntime) -> None:
        """キャンセル要求時に例外を送出する."""
        if runtime.cancel_requested.is_set():
            runtime.status = TaskStatus.CANCELLED
            self._repository.update_task(
                runtime.task_id,
                status=TaskStatus.CANCELLED,
                current_stage=runtime.current_stage,
            )
            msg = "Task cancelled"
            raise RuntimeError(msg)
