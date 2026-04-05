"""リスクゲートミドルウェア - 段階的信頼に基づくフロー制御.

FlowMiddleware プロトコルを実装し、各ノード実行前にリスクレベルを評価。
LOW=自動実行, MEDIUM=ログ強化+自動, HIGH=人間承認, CRITICAL=停止。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from contracts.flow.contracts import MiddlewareDecision, MiddlewareResult
from harness.approval.types import ApprovalRequest, ApprovalStatus
from harness.governance.audit import AuditEvent, AuditLogger
from harness.risk.service import RiskLevel
from harness.security.policy_engine import AuthContext, AuthResult, PolicyEngine


if TYPE_CHECKING:
    from harness.approval.approval_manager import ApprovalManager
    from harness.orchestration.models import ExecutionPlan, PlanStep


_logger = logging.getLogger(__name__)


class RiskGateMiddleware:
    """段階的信頼ゲート.

    ExecutionPlan の各ステップに付与された risk_level に基づき、
    ノード実行の許可/拒否/承認要求を判定する。

    判定ルール:
    - LOW: 自動実行（ALLOW）
    - MEDIUM: ログ強化 + 自動実行（ALLOW + enhanced_logging metadata）
    - HIGH: PolicyEngine 認可チェック → APPROVAL_REQUIRED
    - CRITICAL: 即停止（DENY）

    全判定に AuditEvent を emit する。
    """

    def __init__(
        self,
        plan: ExecutionPlan,
        audit_logger: AuditLogger,
        *,
        policy_engine: PolicyEngine | None = None,
        approval_manager: ApprovalManager | None = None,
        user_id: str | None = None,
    ) -> None:
        """初期化.

        Args:
            plan: 実行計画（ステップ→リスクのマッピング源）
            audit_logger: 監査ロガー
            policy_engine: ポリシーエンジン（HIGH リスク時の認可チェック用）
            approval_manager: 承認マネージャー（HIGH リスク時の承認要求用）
            user_id: ユーザーID（監査・認可コンテキスト用）
        """
        self._plan = plan
        self._audit_logger = audit_logger
        self._policy_engine = policy_engine
        self._approval_manager = approval_manager
        self._user_id = user_id

        # step_id → PlanStep の高速ルックアップ用
        self._step_map: dict[str, PlanStep] = {step.step_id: step for step in plan.steps}

    @property
    def name(self) -> str:
        """ミドルウェア名."""
        return "RiskGateMiddleware"

    def _find_step(self, node_id: str, node_name: str) -> PlanStep | None:
        """ノードIDまたはノード名からステップを検索."""
        step = self._step_map.get(node_id)
        if step is not None:
            return step
        # node_name でフォールバック検索
        for s in self._plan.steps:
            if node_name in (s.agent_id, s.description):
                return s
        return None

    def _emit_audit(
        self,
        node_id: str,
        node_name: str,
        decision: str,
        reason: str,
        risk_level: RiskLevel,
        **extra_metadata: Any,
    ) -> None:
        """監査イベントを emit."""
        event = AuditEvent(
            tool_name=node_name,
            decision=decision,
            reason=reason,
            operation_type="risk_gate",
            risk_level=risk_level.value,
            user_id=self._user_id,
            flow_id=self._plan.plan_id,
            metadata={
                "node_id": node_id,
                "risk_level": risk_level.value,
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
        """ノード実行前: リスクレベルに基づき判定.

        Args:
            node_id: ノード識別子
            node_name: ノード表示名
            inputs: ノードへの入力データ

        Returns:
            ALLOW / APPROVAL_REQUIRED / DENY
        """
        step = self._find_step(node_id, node_name)
        if step is None:
            # 計画外ノード（ReviewNode 等）は ALLOW
            _logger.debug("リスクゲート: 計画外ノード %s → ALLOW", node_id)
            return MiddlewareResult(decision=MiddlewareDecision.ALLOW)

        risk = step.risk_level

        # CRITICAL: 即停止
        if risk == RiskLevel.CRITICAL:
            reason = f"CRITICAL リスク: ステップ '{step.description}' の実行を拒否"
            self._emit_audit(node_id, node_name, "deny", reason, risk)
            _logger.warning("リスクゲート: %s", reason)
            return MiddlewareResult(
                decision=MiddlewareDecision.DENY,
                reason=reason,
                metadata={"risk_level": risk.value, "step_id": step.step_id},
            )

        # HIGH: 認可チェック → 承認要求
        if risk == RiskLevel.HIGH:
            return await self._handle_high_risk(node_id, node_name, step)

        # MEDIUM: ログ強化 + 自動実行
        if risk == RiskLevel.MEDIUM:
            reason = f"MEDIUM リスク: ステップ '{step.description}' をログ強化で実行"
            self._emit_audit(node_id, node_name, "allow_enhanced", reason, risk)
            _logger.info("リスクゲート: %s", reason)
            return MiddlewareResult(
                decision=MiddlewareDecision.ALLOW,
                metadata={
                    "enhanced_logging": True,
                    "risk_level": risk.value,
                    "step_id": step.step_id,
                },
            )

        # LOW: 自動実行
        reason = f"LOW リスク: ステップ '{step.description}' を自動実行"
        self._emit_audit(node_id, node_name, "allow", reason, risk)
        _logger.debug("リスクゲート: %s", reason)
        return MiddlewareResult(
            decision=MiddlewareDecision.ALLOW,
            metadata={"risk_level": risk.value, "step_id": step.step_id},
        )

    async def _handle_high_risk(
        self,
        node_id: str,
        node_name: str,
        step: PlanStep,
    ) -> MiddlewareResult:
        """HIGH リスクステップの処理: 認可チェック → 承認要求."""
        # PolicyEngine による認可チェック
        if self._policy_engine is not None:
            auth_context = AuthContext(
                subject={"user_id": self._user_id or "system"},
                resource={"type": "agent_step", "step_id": step.step_id, "agent_id": step.agent_id},
                action="execute",
            )
            auth_result: AuthResult = await self._policy_engine.authorize(auth_context)

            if not auth_result.allowed:
                reason = f"HIGH リスク + 認可拒否: {auth_result.reason}"
                self._emit_audit(
                    node_id,
                    node_name,
                    "deny",
                    reason,
                    RiskLevel.HIGH,
                    auth_decision=auth_result.decision.value,
                )
                _logger.warning("リスクゲート: %s", reason)
                return MiddlewareResult(
                    decision=MiddlewareDecision.DENY,
                    reason=reason,
                )

        # 承認マネージャーへの承認要求
        reason = f"HIGH リスク: ステップ '{step.description}' は人間の承認が必要"
        self._emit_audit(node_id, node_name, "approval_required", reason, RiskLevel.HIGH)
        _logger.info("リスクゲート: %s", reason)

        if self._approval_manager is not None:
            approval_request = ApprovalRequest(
                action="execute",
                reason=f"[HIGH リスク] {step.description}",
                resource_id=step.step_id,
                resource_type="agent_step",
                context={
                    "node_id": node_id,
                    "node_name": node_name,
                },
                metadata={
                    "step_id": step.step_id,
                    "agent_id": step.agent_id,
                    "risk_level": RiskLevel.HIGH.value,
                    "plan_id": self._plan.plan_id,
                },
            )
            response = await self._approval_manager.request_approval(approval_request)

            if response.status == ApprovalStatus.APPROVED:
                self._emit_audit(
                    node_id,
                    node_name,
                    "allow_after_approval",
                    "人間が承認済み",
                    RiskLevel.HIGH,
                )
                return MiddlewareResult(decision=MiddlewareDecision.ALLOW)

            # 拒否またはタイムアウト
            deny_reason = f"承認拒否/期限切れ: {response.status.value}"
            self._emit_audit(node_id, node_name, "deny", deny_reason, RiskLevel.HIGH)
            return MiddlewareResult(
                decision=MiddlewareDecision.DENY,
                reason=deny_reason,
            )

        # ApprovalManager 未設定の場合は APPROVAL_REQUIRED を返す
        return MiddlewareResult(
            decision=MiddlewareDecision.APPROVAL_REQUIRED,
            reason=reason,
            metadata={"risk_level": RiskLevel.HIGH.value, "step_id": step.step_id},
        )

    async def after_node(
        self,
        node_id: str,
        node_name: str,
        result: dict[str, Any],
        success: bool,
    ) -> MiddlewareResult:
        """ノード実行後: 結果をログに記録.

        Args:
            node_id: ノード識別子
            node_name: ノード表示名
            result: ノード実行結果
            success: 実行成功フラグ

        Returns:
            常に ALLOW（後処理は StepVerifier が担当）
        """
        step = self._find_step(node_id, node_name)
        risk = step.risk_level if step is not None else RiskLevel.LOW

        self._emit_audit(
            node_id,
            node_name,
            "node_result",
            f"ノード実行{'成功' if success else '失敗'}",
            risk,
            success=success,
        )

        return MiddlewareResult(decision=MiddlewareDecision.ALLOW)


__all__ = ["RiskGateMiddleware"]
