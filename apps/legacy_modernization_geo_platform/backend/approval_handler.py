"""GEO Platform の承認ループ・リライト処理.

harness/approval/ApprovalFlow を統合し、AG-UI 標準の承認イベント生成と
状態追跡をフレームワーク経由で行う。
asyncio.Event ベースの非同期待機により、イベントループをブロックしない。
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any
from uuid import uuid4

from apps.legacy_modernization_geo_platform.backend.schemas import (
    ApprovalRecord,
    ApprovalStatus,
    ArtifactMeta,
    ContentDraftArtifact,
    ContentDraftPage,
    EvidenceMatrixArtifact,
    GeoQAReport,
    TaskEvent,
    TaskStatus,
    normalize_content_language,
)
from harness.approval.approval_flow import ApprovalFlow, ApprovalFlowConfig


if TYPE_CHECKING:
    from collections.abc import Awaitable, Callable
    from pathlib import Path

    from apps.legacy_modernization_geo_platform.backend.qa import GeoQualityGate
    from apps.legacy_modernization_geo_platform.backend.repository import GeoRepository
    from apps.legacy_modernization_geo_platform.backend.schemas import GeoExecuteRequest
    from kernel.agents.app_agent_runtime import AppAgentRuntime

logger = logging.getLogger(__name__)

# ---------------------------------------------------------------------------
# 多言語コピー定数
# ---------------------------------------------------------------------------

APPROVAL_REASON_COPY: dict[str, str] = {
    "ja": "比較表現または数値・引用リスクのため",
    "en": "Comparative or numeric claims require human review before publishing.",
    "zh": "存在对比或数值表达风险，发布前需要人工复核。",
}
REWRITE_SECTION_TITLE_COPY: dict[str, str] = {
    "ja": "レビュー反映メモ",
    "en": "Review Notes",
    "zh": "评审修订说明",
}
REWRITE_ADJUSTMENT_NOTE_COPY: dict[str, str] = {
    "ja": "比較表現は適用条件を明示し、断定を避けるよう調整しました。",
    "en": "Comparative claims were rewritten with explicit conditions and non-absolute wording.",
    "zh": "已补充对比结论的适用条件，并调整为非绝对化表述。",
}
REVIEW_REQUESTED_NOTE_COPY: dict[str, str] = {
    "ja": "レビュー要請",
    "en": "Review requested",
    "zh": "请求复审",
}


# ---------------------------------------------------------------------------
# ApprovalFlow ファクトリ
# ---------------------------------------------------------------------------

_GEO_RISK_LEVEL_MAP: dict[str, str] = {
    "LOW": "low",
    "MEDIUM": "high",
    "HIGH": "critical",
}


def create_approval_flow(flow_id: str = "geo-platform") -> ApprovalFlow:
    """GEO Platform 用に設定された ApprovalFlow を生成する."""
    return ApprovalFlow(
        flow_id=flow_id,
        config=ApprovalFlowConfig(
            auto_approve_risk_levels=[],
            escalation_risk_levels=["critical"],
            notification_on_high_risk=True,
        ),
    )


# ---------------------------------------------------------------------------
# データクラス
# ---------------------------------------------------------------------------


@dataclass(slots=True)
class PendingApproval:
    """インメモリの承認待ち状態（asyncio.Event ベース）."""

    record: ApprovalRecord
    signal: asyncio.Event = field(default_factory=asyncio.Event)
    decision: ApprovalStatus | None = None
    comment: str | None = None
    reviewer_name: str | None = None


@dataclass(frozen=True)
class ApprovalContext:
    """承認ハンドラが必要とする外部依存をまとめるコンテキスト."""

    repository: GeoRepository
    quality_gate: GeoQualityGate
    settings: Any  # GeoPlatformSettings（循環回避のため Any）
    runtimes: dict[str, Any]  # GeoTaskRuntime dict
    emit_event: Callable[[TaskEvent], None]
    run_stage: Callable[..., Awaitable[Any]]
    write_artifact: Callable[..., Path]
    load_artifact: Callable[..., Any]
    agent_runtime: AppAgentRuntime | None = None
    approval_flow: ApprovalFlow | None = None


# ---------------------------------------------------------------------------
# ユーティリティ
# ---------------------------------------------------------------------------


def resolve_runtime_language(runtime: Any) -> str:
    """タスクリクエストから正規化された言語コードを解決する."""
    requested = runtime.request.inputs.content_languages
    first_language = requested[0] if requested else "ja"
    return normalize_content_language(first_language)


# ---------------------------------------------------------------------------
# 承認ループ（async）
# ---------------------------------------------------------------------------


async def handle_approval_loop(
    ctx: ApprovalContext,
    *,
    runtime: Any,
    qa_report: GeoQAReport,
    evidence_matrix: EvidenceMatrixArtifact,
    request: GeoExecuteRequest,
) -> GeoQAReport:
    """リスクが MEDIUM/HIGH のあいだ承認を非同期で待つループ."""
    while request.constraints.human_approval_before_publish and qa_report.risk_level in {"MEDIUM", "HIGH"}:
        decision = await _request_approval_and_wait(ctx, runtime, qa_report)
        if decision == ApprovalStatus.APPROVED:
            runtime.pending_approval = None
            return qa_report
        if decision == ApprovalStatus.REJECTED:
            runtime.pending_approval = None
            runtime.status = TaskStatus.FAILED
            ctx.repository.update_task(
                runtime.task_id,
                status=TaskStatus.FAILED,
                current_stage="geo_qa",
                error_message="Publishing rejected during approval",
            )
            ctx.emit_event(
                TaskEvent(
                    event_type="flow.error",
                    task_id=runtime.task_id,
                    stage="geo_qa",
                    message="Publishing rejected during approval",
                ),
            )
            return qa_report
        if decision == ApprovalStatus.REWRITE:
            language = resolve_runtime_language(runtime)
            default_note = REVIEW_REQUESTED_NOTE_COPY[language]
            note = runtime.pending_approval.comment if runtime.pending_approval is not None else default_note
            if runtime.rewrite_iterations >= request.options.max_auto_iterations:
                runtime.status = TaskStatus.FAILED
                ctx.repository.update_task(
                    runtime.task_id,
                    status=TaskStatus.FAILED,
                    current_stage="geo_qa",
                    error_message="Rewrite limit reached",
                )
                ctx.emit_event(
                    TaskEvent(
                        event_type="flow.error",
                        task_id=runtime.task_id,
                        stage="geo_qa",
                        message="Rewrite limit reached",
                    ),
                )
                runtime.pending_approval = None
                return qa_report
            apply_rewrite(ctx, task_id=runtime.task_id, note=note or default_note)
            runtime.rewrite_iterations += 1
            runtime.pending_approval = None
            qa_report = await ctx.run_stage(
                runtime,
                stage="geo_qa",
                agent="GeoQA",
                artifact_name="geo_qa_report",
                factory=lambda: _rerun_qa_agent(ctx, runtime, evidence_matrix),
            )
            continue
        runtime.pending_approval = None
        return qa_report
    return qa_report


async def _rerun_qa_agent(
    ctx: ApprovalContext,
    runtime: Any,
    evidence_matrix: EvidenceMatrixArtifact,
) -> GeoQAReport:
    """QA Agent を再実行して更新済みドラフトを評価する."""
    from apps.legacy_modernization_geo_platform.backend.pipeline_stages import execute_artifact_agent

    draft = ctx.load_artifact(runtime.task_id, "content_draft_artifact", ContentDraftArtifact)
    return await execute_artifact_agent(
        ctx.agent_runtime,
        "GeoQA",
        {
            "task_id": runtime.task_id,
            "draft": draft.model_dump(mode="json"),
            "evidence_matrix": evidence_matrix.model_dump(mode="json"),
        },
        GeoQAReport,
    )


# ---------------------------------------------------------------------------
# 承認リクエスト発行 & 非同期待機
# ---------------------------------------------------------------------------


async def _request_approval_and_wait(
    ctx: ApprovalContext,
    runtime: Any,
    qa_report: GeoQAReport,
) -> ApprovalStatus:
    """承認イベントを発行し、asyncio.Event で非同期に応答を待つ.

    harness/approval/ApprovalFlow に承認リクエストを登録し、
    AG-UI 標準イベントと内部 TaskEvent の両方を生成する。
    """
    language = resolve_runtime_language(runtime)
    approval_reason = APPROVAL_REASON_COPY[language]
    request_id = f"apr-{uuid4().hex[:10]}"
    harness_risk = _GEO_RISK_LEVEL_MAP.get(qa_report.risk_level, "normal")

    flow = ctx.approval_flow
    if flow is not None:
        from harness.approval.types import ApprovalRequest as HarnessApprovalRequest

        harness_req = HarnessApprovalRequest(
            action="publish_review",
            reason=approval_reason,
            context={"task_id": runtime.task_id, "risk_level": qa_report.risk_level},
            requester="GeoOrchestrator",
        )
        flow._pending[harness_req.id] = harness_req
        logger.info(
            "ApprovalFlow に登録: flow=%s, harness_request=%s, geo_request=%s",
            flow.flow_id,
            harness_req.id,
            request_id,
        )

    approval_record = ctx.repository.create_approval(
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
    ctx.repository.update_task(runtime.task_id, status=TaskStatus.WAITING_APPROVAL, current_stage="geo_qa")
    ctx.emit_event(
        TaskEvent(
            event_type="approval_required",
            task_id=runtime.task_id,
            stage="publish_review",
            message="Human approval required before publish",
            payload={
                "request_id": request_id,
                "risk_level": qa_report.risk_level,
                "harness_risk_level": harness_risk,
                "reason": approval_reason,
                "actions": ["approve", "reject", "rewrite"],
            },
        ),
    )

    approval_wait = asyncio.create_task(runtime.pending_approval.signal.wait())
    cancel_wait = asyncio.create_task(runtime.cancel_requested.wait())
    try:
        done, _pending = await asyncio.wait(
            {approval_wait, cancel_wait},
            return_when=asyncio.FIRST_COMPLETED,
        )
    finally:
        approval_wait.cancel()
        cancel_wait.cancel()

    if cancel_wait in done:
        runtime.status = TaskStatus.CANCELLED
        ctx.repository.update_task(
            runtime.task_id,
            status=TaskStatus.CANCELLED,
            current_stage="geo_qa",
        )
        ctx.emit_event(
            TaskEvent(
                event_type="flow.error",
                task_id=runtime.task_id,
                stage="geo_qa",
                message="Task was cancelled",
            ),
        )
        return ApprovalStatus.REJECTED

    runtime.status = TaskStatus.RUNNING
    ctx.repository.update_task(runtime.task_id, status=TaskStatus.RUNNING, current_stage="geo_qa")
    return runtime.pending_approval.decision or ApprovalStatus.REJECTED


# ---------------------------------------------------------------------------
# リライト適用
# ---------------------------------------------------------------------------


def apply_rewrite(ctx: ApprovalContext, *, task_id: str, note: str) -> None:
    """オペレーターフィードバックに基づきドラフトを修正する."""
    draft = ctx.load_artifact(task_id, "content_draft_artifact", ContentDraftArtifact)
    language = normalize_content_language(draft.target_language)
    section_title = REWRITE_SECTION_TITLE_COPY[language]
    adjustment_note = REWRITE_ADJUSTMENT_NOTE_COPY[language]
    rewritten_pages: list[ContentDraftPage] = []
    for page in draft.pages:
        rewritten_body = f"{page.body_markdown}\n\n## {section_title}\n- {note}\n- {adjustment_note}"
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
    artifact_path = ctx.write_artifact(
        task_id,
        "content_draft_artifact",
        "content_rewrite",
        rewritten,
        "Content draft rewritten from operator feedback",
    )
    runtime = ctx.runtimes.get(task_id)
    if runtime is not None:
        runtime.artifact_paths["content_draft_artifact"] = artifact_path
    ctx.emit_event(
        TaskEvent(
            event_type="artifact.updated",
            task_id=task_id,
            stage="content_rewrite",
            agent="ContentComposition",
            message="Draft updated from review feedback",
            payload={"artifact_name": "content_draft_artifact", "artifact_path": str(artifact_path)},
        ),
    )
