# -*- coding: utf-8 -*-
"""PEVエンジン - 計画・実行・検証の統合エンジン.

Hassabisの「計画・実行・検証の分離」に基づく自己修正可能なエンジン。

設計原則:
- Plan-Execute-Verifyサイクルの自動化
- 検証失敗時の自動再計画
- WorldModelとの統合（因果推論・制約検証）
- ストリーミング実行イベント

使用例:
    >>> engine = PEVEngine(
    ...     agents={"analyzer": AnalyzerAgent(), "executor": ExecutorAgent()},
    ...     max_replans=3,
    ... )
    >>> result = await engine.run({"goal": "レポート作成"})
    >>> if result["success"]:
    ...     print("完了!")
"""

from __future__ import annotations

import logging
from collections.abc import AsyncIterator
from dataclasses import dataclass, field
from typing import Any

from pydantic import BaseModel, Field

from agentflow.pev.hierarchical_planner import (
    GoalStatus,
    HierarchicalPlan,
    HierarchicalPlanner,
)
from agentflow.pev.monitored_executor import (
    ExecutionEvent,
    ExecutionEventType,
    MonitoredExecutor,
)
from agentflow.pev.result_verifier import (
    ResultVerifier,
    VerificationResult,
    VerificationStrategy,
)


class PEVEngineConfig(BaseModel):
    """PEVエンジン設定.

    Attributes:
        max_replans: 最大再計画回数
        timeout_seconds: タイムアウト秒数
        max_concurrent: 最大同時実行数
        verification_strategy: 検証戦略
        acceptance_threshold: 合格閾値
        auto_replan: 検証失敗時に自動再計画するか
    """

    max_replans: int = Field(default=3, ge=0, description="最大再計画回数")
    timeout_seconds: float = Field(default=600.0, gt=0, description="タイムアウト")
    max_concurrent: int = Field(default=5, ge=1, description="最大同時実行")
    verification_strategy: VerificationStrategy = Field(
        default=VerificationStrategy.THRESHOLD
    )
    acceptance_threshold: float = Field(default=0.7, ge=0.0, le=1.0)
    auto_replan: bool = Field(default=True, description="自動再計画")


class PEVResult(BaseModel):
    """PEV実行結果.

    Attributes:
        success: 成功フラグ
        goal: 目標
        plan_id: 計画ID
        replan_count: 再計画回数
        verification: 最終検証結果
        results: 目標ごとの結果
        events: 実行イベント
    """

    success: bool = Field(default=False)
    goal: str = Field(default="")
    plan_id: str = Field(default="")
    replan_count: int = Field(default=0)
    verification: VerificationResult | None = Field(default=None)
    results: dict[str, Any] = Field(default_factory=dict)
    events: list[dict[str, Any]] = Field(default_factory=list)
    error: str | None = Field(default=None)


@dataclass
class PEVEngine:
    """PEVエンジン - 計画・実行・検証の統合.

    HierarchicalPlanner, MonitoredExecutor, ResultVerifierを統合し、
    自己修正可能なエージェント実行を実現する。

    主な機能:
    - 目標の階層的計画
    - 監視付き実行
    - 結果検証と自動再計画
    """

    agents: dict[str, Any] = field(default_factory=dict)
    default_agent: Any = None
    llm_client: Any = None
    config: PEVEngineConfig = field(default_factory=PEVEngineConfig)
    world_model: Any = None
    _planner: HierarchicalPlanner | None = None
    _executor: MonitoredExecutor | None = None
    _verifier: ResultVerifier | None = None
    _logger: logging.Logger = field(
        default_factory=lambda: logging.getLogger("agentflow.pev.engine")
    )

    def __post_init__(self) -> None:
        """初期化後処理."""
        self._planner = HierarchicalPlanner(llm_client=self.llm_client)
        self._executor = MonitoredExecutor(
            agents=self.agents,
            default_agent=self.default_agent,
            timeout_seconds=self.config.timeout_seconds,
            max_concurrent=self.config.max_concurrent,
        )
        self._verifier = ResultVerifier(
            llm_client=self.llm_client,
            default_strategy=self.config.verification_strategy,
            acceptance_threshold=self.config.acceptance_threshold,
        )

    async def run(
        self,
        inputs: dict[str, Any],
    ) -> PEVResult:
        """PEVサイクルを実行.

        Args:
            inputs: 入力データ（goal必須）

        Returns:
            PEVResult
        """
        goal = inputs.get("goal", "")
        context = inputs.get("context", {})
        expected = inputs.get("expected", {})
        available_agents = list(self.agents.keys())

        result = PEVResult(goal=goal)

        try:
            # 1. Plan - 計画作成
            self._logger.info(f"計画作成開始: {goal}")
            plan = await self._planner.create_plan(
                goal=goal,
                context=context,
                available_agents=available_agents,
            )
            result.plan_id = plan.id

            # PEVサイクル（再計画込み）
            while result.replan_count <= self.config.max_replans:
                # 2. Execute - 監視付き実行
                self._logger.info(f"実行開始 (試行 {result.replan_count + 1})")
                execution_results: dict[str, Any] = {}
                failed_goal = None

                async for event in self._executor.execute(plan, context):
                    result.events.append(event.model_dump())

                    if event.type == ExecutionEventType.GOAL_FAILED:
                        failed_goal = plan.get_goal(event.goal_id) if event.goal_id else None

                    if event.type == ExecutionEventType.COMPLETED:
                        execution_results = event.data.get("results", {})

                # 3. Verify - 結果検証
                self._logger.info("検証開始")
                verification = await self._verifier.verify(
                    goal=goal,
                    result=execution_results,
                    expected=expected,
                    strategy=self.config.verification_strategy,
                )
                result.verification = verification
                result.results = execution_results

                # 検証成功
                if verification.is_acceptable:
                    result.success = True
                    self._logger.info(f"検証成功: スコア={verification.score:.2f}")
                    break

                # 自動再計画が無効または上限到達
                if not self.config.auto_replan:
                    self._logger.info("自動再計画無効、終了")
                    break

                if result.replan_count >= self.config.max_replans:
                    self._logger.warning(f"再計画上限到達: {self.config.max_replans}")
                    break

                # 4. Replan - 再計画
                self._logger.info(f"再計画開始 ({result.replan_count + 1}/{self.config.max_replans})")

                if failed_goal:
                    plan = await self._planner.replan(
                        plan=plan,
                        failed_goal=failed_goal,
                        error=verification.feedback,
                        context={"verification": verification.model_dump()},
                    )
                else:
                    # 失敗した目標が特定できない場合は全体を再計画
                    plan = await self._planner.create_plan(
                        goal=goal,
                        context={**context, "previous_feedback": verification.feedback},
                        available_agents=available_agents,
                    )

                result.replan_count += 1

        except Exception as e:
            self._logger.error(f"PEVエンジンエラー: {e}")
            result.error = str(e)

        return result

    async def run_stream(
        self,
        inputs: dict[str, Any],
    ) -> AsyncIterator[ExecutionEvent]:
        """ストリーミング実行.

        Args:
            inputs: 入力データ

        Yields:
            ExecutionEvent
        """
        goal = inputs.get("goal", "")
        context = inputs.get("context", {})
        available_agents = list(self.agents.keys())

        # 計画作成
        plan = await self._planner.create_plan(
            goal=goal,
            context=context,
            available_agents=available_agents,
        )

        yield ExecutionEvent(
            type=ExecutionEventType.STARTED,
            message=f"計画作成完了: {len(plan.levels)}レベル",
            data={"plan_id": plan.id},
        )

        # 実行
        async for event in self._executor.execute(plan, context):
            yield event

    def get_planner(self) -> HierarchicalPlanner:
        """プランナーを取得."""
        return self._planner

    def get_executor(self) -> MonitoredExecutor:
        """エグゼキューターを取得."""
        return self._executor

    def get_verifier(self) -> ResultVerifier:
        """検証器を取得."""
        return self._verifier


__all__ = [
    "PEVEngineConfig",
    "PEVResult",
    "PEVEngine",
]

