"""監視付き実行 - 実行監視と異常検知.

計画を監視しながら実行し、異常を検知して報告する。

設計原則:
- 実行の進捗を継続的に監視
- 異常（タイムアウト、エラー、予期しない結果）を検知
- 早期警告と自動停止

使用例:
    >>> executor = MonitoredExecutor(
    ...     agents={"analyzer": AnalyzerAgent()},
    ...     timeout=300,
    ... )
    >>> async for event in executor.execute(plan):
    ...     if event.type == "progress":
    ...         print(f"進捗: {event.progress}%")
    ...     elif event.type == "anomaly":
    ...         print(f"異常検知: {event.message}")
"""

from __future__ import annotations

import asyncio
import logging
import time
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from agentflow.pev.hierarchical_planner import GoalStatus, HierarchicalPlan, SubGoal


if TYPE_CHECKING:
    from collections.abc import AsyncIterator, Callable


class ExecutionEventType(str, Enum):
    """実行イベントタイプ."""

    STARTED = "started"
    PROGRESS = "progress"
    GOAL_STARTED = "goal_started"
    GOAL_COMPLETED = "goal_completed"
    GOAL_FAILED = "goal_failed"
    ANOMALY = "anomaly"
    TIMEOUT = "timeout"
    COMPLETED = "completed"
    ABORTED = "aborted"


class ExecutionEvent(BaseModel):
    """実行イベント.

    Attributes:
        id: イベントID
        type: イベントタイプ
        timestamp: タイムスタンプ
        goal_id: 関連する目標ID
        progress: 進捗率（0.0-1.0）
        message: メッセージ
        data: 追加データ
    """

    id: str = Field(default_factory=lambda: f"evt-{uuid.uuid4().hex[:8]}")
    type: ExecutionEventType = Field(...)
    timestamp: datetime = Field(default_factory=datetime.now)
    goal_id: str | None = Field(default=None)
    progress: float = Field(default=0.0, ge=0.0, le=1.0)
    message: str = Field(default="")
    data: dict[str, Any] = Field(default_factory=dict)


class ExecutionResult(BaseModel):
    """実行結果.

    Attributes:
        success: 成功フラグ
        plan_id: 計画ID
        completed_goals: 完了した目標数
        total_goals: 総目標数
        duration: 実行時間（秒）
        results: 目標ごとの結果
        errors: エラーリスト
    """

    success: bool = Field(default=True)
    plan_id: str = Field(default="")
    completed_goals: int = Field(default=0)
    total_goals: int = Field(default=0)
    duration: float = Field(default=0.0)
    results: dict[str, Any] = Field(default_factory=dict)
    errors: list[str] = Field(default_factory=list)


@dataclass
class ExecutionMonitor:
    """実行監視.

    実行状態を監視し、異常を検知する。
    """

    timeout_seconds: float = 300.0
    progress_interval: float = 5.0
    anomaly_threshold: float = 0.3
    _start_time: float | None = None
    _last_progress: float = 0.0
    _stall_count: int = 0
    _logger: logging.Logger = field(default_factory=lambda: logging.getLogger("agentflow.pev.monitor"))

    def start(self) -> None:
        """監視開始."""
        self._start_time = time.time()
        self._last_progress = 0.0
        self._stall_count = 0

    def check_timeout(self) -> bool:
        """タイムアウトをチェック."""
        if self._start_time is None:
            return False
        elapsed = time.time() - self._start_time
        return elapsed > self.timeout_seconds

    def check_stall(self, current_progress: float) -> bool:
        """進捗停滞をチェック."""
        if current_progress <= self._last_progress:
            self._stall_count += 1
        else:
            self._stall_count = 0
            self._last_progress = current_progress

        return self._stall_count > 3  # 3回連続で停滞

    def get_elapsed(self) -> float:
        """経過時間を取得."""
        if self._start_time is None:
            return 0.0
        return time.time() - self._start_time


@dataclass
class MonitoredExecutor:
    """監視付き実行器.

    計画を監視しながら実行する。
    """

    agents: dict[str, Any] = field(default_factory=dict)
    default_agent: Any = None
    timeout_seconds: float = 300.0
    max_concurrent: int = 5
    on_anomaly: Callable[[ExecutionEvent], None] | None = None
    _monitor: ExecutionMonitor = field(default_factory=ExecutionMonitor)
    _logger: logging.Logger = field(default_factory=lambda: logging.getLogger("agentflow.pev.executor"))

    async def execute(
        self,
        plan: HierarchicalPlan,
        context: dict[str, Any] | None = None,
    ) -> AsyncIterator[ExecutionEvent]:
        """計画を実行.

        Args:
            plan: 実行する計画
            context: 実行コンテキスト

        Yields:
            ExecutionEvent
        """
        context = context or {}
        self._monitor.start()

        yield ExecutionEvent(
            type=ExecutionEventType.STARTED,
            message=f"計画実行開始: {plan.root_goal}",
            data={"plan_id": plan.id},
        )

        results: dict[str, Any] = {}

        try:
            while True:
                # タイムアウトチェック
                if self._monitor.check_timeout():
                    yield ExecutionEvent(
                        type=ExecutionEventType.TIMEOUT,
                        message=f"タイムアウト: {self._monitor.get_elapsed():.0f}秒",
                    )
                    break

                # 実行可能な目標を取得
                ready_goals = plan.get_ready_goals()

                if not ready_goals:
                    # 全て完了または全てブロック
                    if plan.get_progress() >= 1.0:
                        break
                    # 進捗停滞チェック
                    if self._monitor.check_stall(plan.get_progress()):
                        yield ExecutionEvent(
                            type=ExecutionEventType.ANOMALY,
                            message="進捗停滞検知",
                            progress=plan.get_progress(),
                        )
                        break
                    await asyncio.sleep(0.5)
                    continue

                # 並列実行（最大同時実行数まで）
                batch = ready_goals[: self.max_concurrent]

                tasks = [self._execute_goal(goal, plan, context, results) for goal in batch]

                for task_result in await asyncio.gather(*tasks, return_exceptions=True):
                    if isinstance(task_result, BaseException):
                        self._logger.error(f"目標実行エラー: {task_result}")
                        continue

                    event = task_result
                    yield event

                # 進捗イベント
                yield ExecutionEvent(
                    type=ExecutionEventType.PROGRESS,
                    progress=plan.get_progress(),
                    message=f"進捗: {plan.get_progress() * 100:.0f}%",
                )

        except Exception as e:
            self._logger.exception(f"実行エラー: {e}")
            yield ExecutionEvent(
                type=ExecutionEventType.ABORTED,
                message=str(e),
            )

        # 完了イベント
        yield ExecutionEvent(
            type=ExecutionEventType.COMPLETED,
            progress=plan.get_progress(),
            message="計画実行完了",
            data={"results": results, "duration": self._monitor.get_elapsed()},
        )

    async def _execute_goal(
        self,
        goal: SubGoal,
        plan: HierarchicalPlan,
        context: dict[str, Any],
        results: dict[str, Any],
    ) -> ExecutionEvent:
        """単一の目標を実行."""
        self._logger.info(f"目標実行開始: {goal.name}")
        plan.update_goal_status(goal.id, GoalStatus.IN_PROGRESS)

        try:
            # 担当Agentを取得
            agent = self._get_agent(goal.assigned_agent)

            if agent is None:
                msg = f"Agentが見つかりません: {goal.assigned_agent}"
                raise ValueError(msg)

            # 入力を準備
            input_data = {
                "goal": goal.name,
                "description": goal.description,
                "context": context,
                "dependencies_results": {dep_id: results.get(dep_id) for dep_id in goal.dependencies},
            }

            # Agent実行
            if hasattr(agent, "run"):
                result = await agent.run(input_data)
            elif hasattr(agent, "invoke"):
                result = await agent.invoke(input_data)
            elif callable(agent):
                result = await agent(input_data)
            else:
                result = {"status": "completed", "goal": goal.name}

            # 結果を記録
            results[goal.id] = result
            plan.update_goal_status(goal.id, GoalStatus.COMPLETED, result=result)

            self._logger.info(f"目標完了: {goal.name}")

            return ExecutionEvent(
                type=ExecutionEventType.GOAL_COMPLETED,
                goal_id=goal.id,
                message=f"目標完了: {goal.name}",
                data={"result": result},
            )

        except Exception as e:
            self._logger.exception(f"目標失敗: {goal.name} - {e}")
            plan.update_goal_status(goal.id, GoalStatus.FAILED, error=str(e))

            return ExecutionEvent(
                type=ExecutionEventType.GOAL_FAILED,
                goal_id=goal.id,
                message=f"目標失敗: {goal.name}",
                data={"error": str(e)},
            )

    def _get_agent(self, agent_name: str | None) -> Any:
        """Agentを取得."""
        if agent_name and agent_name in self.agents:
            return self.agents[agent_name]
        return self.default_agent

    def get_result(
        self,
        plan: HierarchicalPlan,
        results: dict[str, Any],
    ) -> ExecutionResult:
        """実行結果を取得."""
        total = sum(len(level.goals) for level in plan.levels)
        completed = sum(1 for level in plan.levels for goal in level.goals if goal.status == GoalStatus.COMPLETED)
        errors = [
            f"{goal.name}: {goal.error}"
            for level in plan.levels
            for goal in level.goals
            if goal.status == GoalStatus.FAILED and goal.error
        ]

        return ExecutionResult(
            success=len(errors) == 0,
            plan_id=plan.id,
            completed_goals=completed,
            total_goals=total,
            duration=self._monitor.get_elapsed(),
            results=results,
            errors=errors,
        )


__all__ = [
    "ExecutionEvent",
    "ExecutionEventType",
    "ExecutionMonitor",
    "ExecutionResult",
    "MonitoredExecutor",
]
