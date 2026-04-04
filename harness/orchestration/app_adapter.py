"""App 統合アダプタ — 既存 App が AutonomousPipeline を利用するためのブリッジ.

App 側の独自計画モデル（HarnessPlan 等）を harness の ExecutionPlan に変換し、
AutonomousPipeline 経由で実行する。App は「何を実行するか」のみ担当し、
「どう実行するか」はフレームワークに委譲する。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable

from harness.governance.audit import AuditLogger, LoggingAuditLogger
from harness.orchestration.dynamic_flow import DynamicFlowGenerator
from harness.orchestration.models import ExecutionPlan, PlanStep
from harness.orchestration.pipeline import AutonomousPipeline
from harness.orchestration.planner import PlannerAgent
from harness.risk.service import RiskLevel


if TYPE_CHECKING:
    from harness.guardrails.pipeline import GuardrailPipeline


_logger = logging.getLogger(__name__)


# === App 側のモデルを受け入れるためのプロトコル ===


@runtime_checkable
class AppPlanLike(Protocol):
    """App 側の計画モデルが最低限持つべきインターフェース."""

    @property
    def goal(self) -> str: ...

    @property
    def blueprint_id(self) -> str: ...

    @property
    def task_kind(self) -> str: ...

    @property
    def execution_plan(self) -> dict[str, Any]: ...


# === 変換ロジック ===


def convert_app_plan_to_execution_plan(
    app_plan: AppPlanLike,
    *,
    available_agents: list[str] | None = None,
    default_risk: RiskLevel = RiskLevel.LOW,
) -> ExecutionPlan:
    """App 側の計画モデルを harness の ExecutionPlan に変換.

    App の execution_plan dict から steps を抽出し、
    harness の PlanStep モデルに変換する。

    Args:
        app_plan: App 側の計画（HarnessPlan 等）
        available_agents: 利用可能エージェントID リスト
        default_risk: デフォルトリスクレベル

    Returns:
        ExecutionPlan
    """
    raw_steps = app_plan.execution_plan.get("steps", [])
    steps: list[PlanStep] = []

    for i, raw_step in enumerate(raw_steps):
        if isinstance(raw_step, dict):
            # mypy --strict 対応: .get() のネスト返り値を明示的に型安全にする
            agent_id: str = raw_step.get("agent_id") or raw_step.get("capability") or "unknown"
            description: str = raw_step.get("description") or raw_step.get("label") or ""
            input_spec: dict[str, Any] = raw_step.get("input_spec") or raw_step.get("inputs") or {}
            step = PlanStep(
                step_id=raw_step.get("step_id", f"step-{i + 1}"),
                agent_id=str(agent_id),
                description=str(description),
                input_spec=dict(input_spec) if isinstance(input_spec, dict) else {},
                risk_level=RiskLevel(raw_step.get("risk_level", default_risk.value)),
                dependencies=raw_step.get("dependencies", []),
                timeout_seconds=raw_step.get("timeout_seconds", 300),
                max_retries=raw_step.get("max_retries", 2),
                metadata=raw_step.get("metadata", {}),
            )
            steps.append(step)
        elif isinstance(raw_step, str):
            # 文字列形式（"search → rank → subscribe"）
            steps.append(
                PlanStep(
                    step_id=f"step-{i + 1}",
                    agent_id=raw_step,
                    description=raw_step,
                    risk_level=default_risk,
                )
            )

    plan = ExecutionPlan(
        goal=app_plan.goal,
        steps=steps,
        metadata={
            "blueprint_id": app_plan.blueprint_id,
            "task_kind": app_plan.task_kind,
            "source": "app_adapter",
        },
    )
    return plan.model_copy(update={"overall_risk": plan.compute_overall_risk()})


# === App 統合アダプタ ===


class AppOrchestrationAdapter:
    """既存 App が AutonomousPipeline を利用するためのアダプタ.

    App の OrchestrationService から呼び出され、
    フレームワークの AutonomousPipeline に処理を委譲する。

    使い方:
        >>> adapter = AppOrchestrationAdapter(
        ...     agent_map={"flight_search": flight_search_agent, ...},
        ... )
        >>> # App の HarnessPlan が既にある場合
        >>> result = await adapter.execute_from_app_plan(harness_plan)
        >>>
        >>> # 自然言語リクエストを直接渡す場合
        >>> result = await adapter.execute(
        ...     user_request="NRT→LAX の便を探して",
        ...     available_agents=["flight_search", "flight_ranking"],
        ... )
    """

    def __init__(
        self,
        *,
        agent_map: dict[str, Any] | None = None,
        registry: Any = None,
        guardrail_pipeline: GuardrailPipeline | None = None,
        audit_logger: AuditLogger | None = None,
        planner: PlannerAgent | None = None,
        max_replans: int = 2,
    ) -> None:
        """初期化.

        Args:
            agent_map: agent_id → AgentProtocol の事前マッピング
            registry: AgentRegistry インスタンス
            guardrail_pipeline: ガードレールパイプライン
            audit_logger: 監査ロガー
            planner: PlannerAgent（省略時はデフォルト構成）
            max_replans: 最大再計画回数
        """
        self._audit_logger = audit_logger or LoggingAuditLogger()
        self._planner = planner or PlannerAgent()
        self._flow_generator = DynamicFlowGenerator(
            agent_map=agent_map,
            registry=registry,
        )
        self._pipeline = AutonomousPipeline(
            planner=self._planner,
            flow_generator=self._flow_generator,
            guardrail_pipeline=guardrail_pipeline,
            audit_logger=self._audit_logger,
            max_replans=max_replans,
        )

    async def execute(
        self,
        user_request: str,
        *,
        context: dict[str, Any] | None = None,
        constraints: list[str] | None = None,
        available_agents: list[str] | None = None,
    ) -> dict[str, Any]:
        """自然言語リクエストを AutonomousPipeline 経由で実行.

        Args:
            user_request: ユーザーの要求テキスト
            context: 追加コンテキスト
            constraints: 制約条件リスト
            available_agents: 利用可能エージェントID リスト

        Returns:
            パイプライン実行結果
        """
        return await self._pipeline.execute(
            user_request,
            context=context,
            constraints=constraints,
            available_agents=available_agents,
        )

    async def execute_from_app_plan(
        self,
        app_plan: AppPlanLike,
        *,
        available_agents: list[str] | None = None,
    ) -> dict[str, Any]:
        """App 側の既存計画を ExecutionPlan に変換してフロー実行.

        PlannerAgent をスキップし、App が既に生成した計画を
        直接 DynamicFlowGenerator → FlowExecutor で実行する。

        Args:
            app_plan: App 側の計画（HarnessPlan 等）
            available_agents: 利用可能エージェントID リスト

        Returns:
            実行結果
        """
        plan = convert_app_plan_to_execution_plan(
            app_plan,
            available_agents=available_agents,
        )

        _logger.info(
            "App 計画変換完了: blueprint=%s, steps=%d, risk=%s",
            app_plan.blueprint_id,
            len(plan.steps),
            plan.overall_risk.value,
        )

        # 公開 API 経由で実行（private メンバーにアクセスしない）
        return await self._pipeline.execute_plan(
            plan,
            context={"blueprint_id": app_plan.blueprint_id},
        )


__all__ = [
    "AppOrchestrationAdapter",
    "AppPlanLike",
    "convert_app_plan_to_execution_plan",
]
