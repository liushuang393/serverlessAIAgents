"""AutonomousPipeline - 自律編排パイプラインのトップレベルエントリーポイント.

ユーザー要求を受けて以下を1本のパイプラインで実行する:
1. ガードレール事前チェック
2. PlannerAgent によるタスク分解 → ExecutionPlan 生成
3. DynamicFlowGenerator によるフロー構築
4. ミドルウェア付きフロー実行（RiskGate + StepVerifier + Audit）
5. ガードレール事後チェック
6. 失敗時の再計画（最大 max_replans 回）
"""

from __future__ import annotations

import logging
import uuid
from typing import TYPE_CHECKING, Any

from harness.governance.audit import AuditEvent, AuditLogger, LoggingAuditLogger
from harness.orchestration.audit_middleware import AuditMiddleware
from harness.orchestration.models import (
    PlannerInput,
    PlannerOutput,
    ReplanRequest,
)
from harness.orchestration.risk_gate import RiskGateMiddleware
from harness.orchestration.step_verifier import StepVerifierMiddleware
from kernel.agents.dual_verifier import DualVerifier


if TYPE_CHECKING:
    from harness.guardrails.pipeline import GuardrailPipeline
    from harness.orchestration.dynamic_flow import DynamicFlowGenerator
    from harness.orchestration.models import ExecutionPlan
    from harness.orchestration.planner import PlannerAgent


_logger = logging.getLogger(__name__)


class AutonomousPipeline:
    """自律編排パイプライン.

    ユーザー要求を受け取り、タスク分解→フロー実行→検証を自律的に行う。
    段階的信頼（LOW/MEDIUM/HIGH/CRITICAL）に基づくリスクゲート、
    ステップ毎の品質検証、ガードレール、監査ログを統合する。

    Example:
        >>> pipeline = AutonomousPipeline(
        ...     planner=PlannerAgent(),
        ...     flow_generator=DynamicFlowGenerator(agent_map={...}),
        ... )
        >>> result = await pipeline.execute("Q1 売上データを分析してレポートを生成")
    """

    def __init__(
        self,
        planner: PlannerAgent,
        flow_generator: DynamicFlowGenerator,
        *,
        guardrail_pipeline: GuardrailPipeline | None = None,
        audit_logger: AuditLogger | None = None,
        dual_verifier: DualVerifier | None = None,
        max_replans: int = 2,
        max_step_retries: int = 2,
    ) -> None:
        """初期化.

        Args:
            planner: タスク分解エージェント
            flow_generator: 動的フロー生成器
            guardrail_pipeline: ガードレールパイプライン（省略時はチェックなし）
            audit_logger: 監査ロガー（省略時は LoggingAuditLogger）
            dual_verifier: 品質検証器（省略時はデフォルト構成）
            max_replans: 最大再計画回数
            max_step_retries: ステップ毎の最大リトライ回数
        """
        self._planner = planner
        self._flow_generator = flow_generator
        self._guardrails = guardrail_pipeline
        self._audit_logger = audit_logger or LoggingAuditLogger()
        self._verifier = dual_verifier or DualVerifier()
        self._max_replans = max_replans
        self._max_step_retries = max_step_retries

    async def execute(
        self,
        user_request: str,
        *,
        context: dict[str, Any] | None = None,
        constraints: list[str] | None = None,
        available_agents: list[str] | None = None,
    ) -> dict[str, Any]:
        """パイプラインを実行.

        Args:
            user_request: ユーザーの要求テキスト
            context: 追加コンテキスト
            constraints: 制約条件リスト
            available_agents: 利用可能エージェントIDリスト

        Returns:
            実行結果（result, plan, audit_trail, warnings 等）
        """
        run_id = f"run-{uuid.uuid4().hex[:8]}"
        _logger.info("パイプライン開始: run_id=%s, request=%s", run_id, user_request[:100])

        self._emit_audit(run_id, "pipeline_start", f"パイプライン開始: {user_request[:100]}")

        # 1. ガードレール事前チェック
        if self._guardrails is not None:
            pre_result = await self._guardrails.run_pre_check({"user_request": user_request, "context": context or {}})
            if not pre_result.all_passed and pre_result.short_circuited:
                reason = f"ガードレール事前チェック失敗: {pre_result.summary()}"
                self._emit_audit(run_id, "pipeline_rejected", reason)
                _logger.warning("パイプライン拒否: %s", reason)
                return {
                    "status": "rejected",
                    "reason": reason,
                    "run_id": run_id,
                    "guardrail_summary": pre_result.summary(),
                }

        # 2. タスク分解
        planner_output = await self._plan(
            user_request,
            context=context,
            constraints=constraints,
            available_agents=available_agents,
        )
        plan = planner_output.plan

        self._emit_audit(
            run_id,
            "plan_generated",
            f"計画生成完了: {len(plan.steps)} ステップ, リスク={plan.overall_risk.value}",
        )

        # 3-4. フロー実行（再計画ループ付き）
        flow_result: dict[str, Any] = {}
        replan_count = 0

        while replan_count <= self._max_replans:
            flow_result = await self._execute_flow(plan, run_id)

            # 再計画が必要か判定
            replan_meta = flow_result.get("_replan_metadata")
            if replan_meta is None:
                break  # 正常完了または再計画不要

            replan_count += 1
            if replan_count > self._max_replans:
                _logger.warning("再計画上限到達: max_replans=%d", self._max_replans)
                self._emit_audit(run_id, "replan_limit", "再計画上限に到達")
                break

            _logger.info("再計画実行: %d/%d", replan_count, self._max_replans)
            self._emit_audit(
                run_id,
                "replan_start",
                f"再計画 {replan_count}/{self._max_replans}",
            )

            planner_output = await self._planner.replan(
                ReplanRequest(
                    original_plan=plan,
                    failed_step_id=replan_meta.get("step_id", ""),
                    failure_reason=replan_meta.get("failure_reason", ""),
                    completed_results=flow_result.get("_completed_results", {}),
                ),
            )
            plan = planner_output.plan

        # 5. ガードレール事後チェック
        if self._guardrails is not None:
            post_result = await self._guardrails.run_post_check(
                {"user_request": user_request},
                flow_result,
            )
            if not post_result.all_passed:
                flow_result["_post_check_warnings"] = post_result.summary()
                self._emit_audit(
                    run_id,
                    "post_check_warning",
                    f"事後チェック警告: {post_result.summary()}",
                )

        self._emit_audit(run_id, "pipeline_complete", "パイプライン完了")
        _logger.info("パイプライン完了: run_id=%s", run_id)

        return {
            "status": "completed",
            "result": flow_result,
            "plan": plan.model_dump(),
            "run_id": run_id,
            "warnings": planner_output.warnings,
            "replan_count": replan_count,
        }

    async def execute_plan(
        self,
        plan: ExecutionPlan,
        *,
        context: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """事前構築済みの ExecutionPlan を直接実行（PlannerAgent をスキップ）.

        App が独自に計画を生成済みの場合に使用する。
        ガードレール事前/事後チェック + フロー実行 + 監査を行う。

        Args:
            plan: 事前構築済みの実行計画
            context: 追加コンテキスト（ガードレールに渡す）

        Returns:
            実行結果
        """
        run_id = f"run-{uuid.uuid4().hex[:8]}"
        self._emit_audit(run_id, "execute_plan_start", f"計画直接実行: {plan.goal[:100]}")

        # ガードレール事前チェック
        if self._guardrails is not None:
            pre_result = await self._guardrails.run_pre_check({"goal": plan.goal, **(context or {})})
            if not pre_result.all_passed and pre_result.short_circuited:
                reason = f"ガードレール事前チェック失敗: {pre_result.summary()}"
                self._emit_audit(run_id, "pipeline_rejected", reason)
                return {"status": "rejected", "reason": reason, "run_id": run_id}

        # フロー実行
        flow_result = await self._execute_flow(plan, run_id)

        # ガードレール事後チェック
        if self._guardrails is not None:
            post_result = await self._guardrails.run_post_check(
                {"goal": plan.goal},
                flow_result,
            )
            if not post_result.all_passed:
                flow_result["_post_check_warnings"] = post_result.summary()

        self._emit_audit(run_id, "execute_plan_complete", "計画直接実行完了")

        return {
            "status": "completed",
            "result": flow_result,
            "plan": plan.model_dump(),
            "run_id": run_id,
        }

    async def _plan(
        self,
        user_request: str,
        *,
        context: dict[str, Any] | None = None,
        constraints: list[str] | None = None,
        available_agents: list[str] | None = None,
    ) -> PlannerOutput:
        """PlannerAgent でタスク分解."""
        planner_input = PlannerInput(
            user_request=user_request,
            context=context or {},
            constraints=constraints or [],
            available_agents=available_agents or [],
        )
        result = await self._planner.run(planner_input.model_dump())
        return PlannerOutput.model_validate(result)

    async def _execute_flow(
        self,
        plan: Any,
        run_id: str,
    ) -> dict[str, Any]:
        """ミドルウェア付きフローを実行."""
        # ミドルウェア構築
        audit_mw = AuditMiddleware(
            self._audit_logger,
            flow_id=plan.plan_id,
            run_id=run_id,
        )
        risk_mw = RiskGateMiddleware(
            plan=plan,
            audit_logger=self._audit_logger,
        )
        verifier_mw = StepVerifierMiddleware(
            plan=plan,
            dual_verifier=self._verifier,
            audit_logger=self._audit_logger,
            max_retries=self._max_step_retries,
        )

        # 順序: Audit → RiskGate → StepVerifier
        middlewares = [audit_mw, risk_mw, verifier_mw]

        flow = self._flow_generator.generate_flow(plan, middlewares=middlewares)

        return await flow.run({"_plan": plan.model_dump(), "_run_id": run_id})

    def _emit_audit(self, run_id: str, decision: str, reason: str) -> None:
        """パイプラインレベルの監査イベントを emit."""
        event = AuditEvent(
            tool_name="AutonomousPipeline",
            decision=decision,
            reason=reason,
            operation_type="pipeline",
            run_id=run_id,
        )
        self._audit_logger.log_event(event)


__all__ = ["AutonomousPipeline"]
