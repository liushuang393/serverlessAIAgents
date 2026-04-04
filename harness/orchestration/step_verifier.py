"""ステップ検証ミドルウェア - ノード実行後の品質検証ループ.

FlowMiddleware プロトコルを実装し、各ノード実行後に DualVerifier で
出力を検証する。失敗時は RetryAdvisor の判定に基づきリトライまたは
再計画を要求する。
"""

from __future__ import annotations

import logging
from typing import Any

from contracts.flow.contracts import MiddlewareDecision, MiddlewareResult
from harness.governance.audit import AuditEvent, AuditLogger
from harness.orchestration.models import ExecutionPlan, PlanStep
from harness.risk.service import RiskLevel
from kernel.agents.dual_verifier import DualVerifier, VerifyStatus

_logger = logging.getLogger(__name__)


class StepVerifierMiddleware:
    """ステップ検証ミドルウェア.

    各ノード実行後に DualVerifier で出力品質を検証する。

    検証結果に基づく判定:
    - PASS: ALLOW（次ステップへ進行）
    - WARNING: ALLOW + 警告 metadata
    - FAIL: リトライ残あり → DENY（再実行シグナル）、超過 → 再計画シグナル
    - NEED_HUMAN: APPROVAL_REQUIRED（人間確認へエスカレート）

    全判定に AuditEvent を emit する。
    """

    def __init__(
        self,
        plan: ExecutionPlan,
        dual_verifier: DualVerifier,
        audit_logger: AuditLogger,
        *,
        max_retries: int = 2,
    ) -> None:
        """初期化.

        Args:
            plan: 実行計画（ステップ情報参照用）
            dual_verifier: 品質検証器
            audit_logger: 監査ロガー
            max_retries: ステップ毎の最大リトライ回数
        """
        self._plan = plan
        self._verifier = dual_verifier
        self._audit_logger = audit_logger
        self._max_retries = max_retries

        # ノード毎のリトライカウンター
        self._retry_counts: dict[str, int] = {}

        # step_id → PlanStep の高速ルックアップ
        self._step_map: dict[str, PlanStep] = {
            step.step_id: step for step in plan.steps
        }

    @property
    def name(self) -> str:
        """ミドルウェア名."""
        return "StepVerifierMiddleware"

    def _find_step(self, node_id: str, node_name: str) -> PlanStep | None:
        """ノードIDまたはノード名からステップを検索."""
        step = self._step_map.get(node_id)
        if step is not None:
            return step
        for s in self._plan.steps:
            if s.agent_id == node_name or s.description == node_name:
                return s
        return None

    def _emit_audit(
        self,
        node_id: str,
        node_name: str,
        decision: str,
        reason: str,
        *,
        verify_status: str = "",
        **extra_metadata: Any,
    ) -> None:
        """監査イベントを emit."""
        step = self._find_step(node_id, node_name)
        risk_level = step.risk_level.value if step is not None else RiskLevel.LOW.value

        event = AuditEvent(
            tool_name=node_name,
            decision=decision,
            reason=reason,
            operation_type="step_verification",
            risk_level=risk_level,
            flow_id=self._plan.plan_id,
            metadata={
                "node_id": node_id,
                "verify_status": verify_status,
                **extra_metadata,
            },
        )
        self._audit_logger.log_event(event)

    async def before_node(
        self,
        node_id: str,
        node_name: str,
        inputs: dict[str, Any],
    ) -> MiddlewareResult:
        """ノード実行前: 何もしない（前処理は RiskGate が担当）.

        Args:
            node_id: ノード識別子
            node_name: ノード表示名
            inputs: ノードへの入力データ

        Returns:
            常に ALLOW
        """
        return MiddlewareResult(decision=MiddlewareDecision.ALLOW)

    async def after_node(
        self,
        node_id: str,
        node_name: str,
        result: dict[str, Any],
        success: bool,
    ) -> MiddlewareResult:
        """ノード実行後: DualVerifier で出力を検証.

        Args:
            node_id: ノード識別子
            node_name: ノード表示名
            result: ノード実行結果
            success: 実行成功フラグ

        Returns:
            検証結果に基づく判定
        """
        # 実行失敗はそのまま通す（FlowExecutor がエラー処理）
        if not success:
            self._emit_audit(
                node_id, node_name,
                "skip_verification",
                "ノード実行失敗のため検証スキップ",
                verify_status="skipped",
            )
            return MiddlewareResult(decision=MiddlewareDecision.ALLOW)

        # DualVerifier で検証
        step = self._find_step(node_id, node_name)
        context: dict[str, Any] = {"node_id": node_id, "node_name": node_name}
        if step is not None:
            context["step_id"] = step.step_id
            context["agent_id"] = step.agent_id

        verify_result = await self._verifier.verify(result, context)

        # PASS: 次ステップへ
        if verify_result.status == VerifyStatus.PASS:
            self._emit_audit(
                node_id, node_name,
                "verification_passed",
                f"検証合格: {verify_result.message}",
                verify_status="pass",
                confidence=verify_result.confidence,
            )
            _logger.debug("検証合格: node_id=%s", node_id)
            return MiddlewareResult(decision=MiddlewareDecision.ALLOW)

        # WARNING: 警告付きで続行
        if verify_result.status == VerifyStatus.WARNING:
            self._emit_audit(
                node_id, node_name,
                "verification_warning",
                f"検証警告: {verify_result.message}",
                verify_status="warning",
                confidence=verify_result.confidence,
                suggestions=verify_result.suggestions,
            )
            _logger.info("検証警告: node_id=%s, message=%s", node_id, verify_result.message)
            return MiddlewareResult(
                decision=MiddlewareDecision.ALLOW,
                metadata={
                    "verification_warning": verify_result.message,
                    "suggestions": verify_result.suggestions,
                },
            )

        # NEED_HUMAN: 人間確認へエスカレート
        if verify_result.status == VerifyStatus.NEED_HUMAN:
            reason = f"人間確認が必要: {verify_result.message}"
            self._emit_audit(
                node_id, node_name,
                "approval_required",
                reason,
                verify_status="need_human",
            )
            _logger.info("人間確認要求: node_id=%s, message=%s", node_id, verify_result.message)
            return MiddlewareResult(
                decision=MiddlewareDecision.APPROVAL_REQUIRED,
                reason=reason,
                metadata={
                    "verify_message": verify_result.message,
                    "suggestions": verify_result.suggestions,
                },
            )

        # FAIL: リトライまたは再計画
        return self._handle_verification_failure(node_id, node_name, verify_result)

    def _handle_verification_failure(
        self,
        node_id: str,
        node_name: str,
        verify_result: Any,
    ) -> MiddlewareResult:
        """検証失敗時の処理: リトライまたは再計画シグナル."""
        current_retries = self._retry_counts.get(node_id, 0)

        if current_retries < self._max_retries:
            # リトライ残あり → DENY で再実行シグナル
            self._retry_counts[node_id] = current_retries + 1
            reason = (
                f"検証失敗 (リトライ {current_retries + 1}/{self._max_retries}): "
                f"{verify_result.message}"
            )
            self._emit_audit(
                node_id, node_name,
                "retry_requested",
                reason,
                verify_status="fail",
                retry_count=current_retries + 1,
                max_retries=self._max_retries,
            )
            _logger.warning("検証失敗→リトライ: node_id=%s, %s", node_id, reason)
            return MiddlewareResult(
                decision=MiddlewareDecision.DENY,
                reason=reason,
                metadata={
                    "retry_requested": True,
                    "retry_count": current_retries + 1,
                    "max_retries": self._max_retries,
                },
            )

        # リトライ超過 → 再計画シグナル
        reason = (
            f"検証失敗 (リトライ上限到達): {verify_result.message}"
        )
        self._emit_audit(
            node_id, node_name,
            "replan_requested",
            reason,
            verify_status="fail",
            retry_count=current_retries,
            suggestions=verify_result.suggestions,
        )
        _logger.warning("検証失敗→再計画要求: node_id=%s, %s", node_id, reason)
        return MiddlewareResult(
            decision=MiddlewareDecision.DENY,
            reason=reason,
            metadata={
                "replan_requested": True,
                "failure_reason": verify_result.message,
                "step_id": node_id,
            },
        )


__all__ = ["StepVerifierMiddleware"]
