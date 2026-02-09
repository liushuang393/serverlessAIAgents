"""オーケストレーター - 統合編排システム.

Planner/Executor/Monitorを統合し、タスクの計画から実行、監視までを管理。

設計原則:
- 計画駆動型実行
- リアルタイム監視
- 自動リカバリ
- 状態管理

使用例:
    >>> from agentflow.orchestration import Orchestrator
    >>>
    >>> orchestrator = Orchestrator(llm_client=my_llm, tool_provider=my_tools)
    >>> await orchestrator.initialize()
    >>>
    >>> # タスクを実行
    >>> result = await orchestrator.execute("競合分析レポートを作成")
    >>>
    >>> # ストリーム実行
    >>> async for event in orchestrator.execute_stream("...", context):
    ...     print(event)
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any

from pydantic import BaseModel, Field

from agentflow.orchestration.executor import ExecutorAgent, ExecutorConfig, StepResult
from agentflow.orchestration.monitor import (
    AlertSeverity,
    MonitorAgent,
    MonitorEvent,
    MonitorThresholds,
)
from agentflow.orchestration.planner import (
    ExecutionPlan,
    PlannerAgent,
    PlannerConfig,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


class ExecutionPhase(str, Enum):
    """実行フェーズ."""

    PLANNING = "planning"      # 計画中
    EXECUTING = "executing"    # 実行中
    MONITORING = "monitoring"  # 監視中
    COMPLETED = "completed"    # 完了
    FAILED = "failed"          # 失敗
    CANCELLED = "cancelled"    # キャンセル


class ExecutionStatus(str, Enum):
    """実行状態."""

    PENDING = "pending"
    RUNNING = "running"
    PAUSED = "paused"
    COMPLETED = "completed"
    FAILED = "failed"
    CANCELLED = "cancelled"


@dataclass
class ExecutionContext:
    """実行コンテキスト.

    実行中の状態とデータを保持。

    Attributes:
        execution_id: 実行ID
        task: 元のタスク
        plan: 実行計画
        phase: 現在のフェーズ
        status: 実行状態
        progress: 進捗率
        results: ステップ結果
        context: 共有コンテキスト
        started_at: 開始時刻
        completed_at: 完了時刻
    """

    execution_id: str
    task: str
    plan: ExecutionPlan | None = None
    phase: ExecutionPhase = ExecutionPhase.PLANNING
    status: ExecutionStatus = ExecutionStatus.PENDING
    progress: float = 0.0
    results: dict[str, StepResult] = field(default_factory=dict)
    context: dict[str, Any] = field(default_factory=dict)
    started_at: datetime = field(default_factory=datetime.now)
    completed_at: datetime | None = None
    error: str | None = None

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "execution_id": self.execution_id,
            "task": self.task,
            "phase": self.phase.value,
            "status": self.status.value,
            "progress": self.progress,
            "results_count": len(self.results),
            "started_at": self.started_at.isoformat(),
            "completed_at": self.completed_at.isoformat() if self.completed_at else None,
            "error": self.error,
        }


@dataclass
class ExecutionResult:
    """実行結果.

    Attributes:
        execution_id: 実行ID
        success: 成功したかどうか
        output: 最終出力
        error: エラーメッセージ
        plan: 実行計画
        step_results: ステップ結果
        duration_ms: 実行時間
        metadata: メタデータ
    """

    execution_id: str
    success: bool
    output: Any = None
    error: str | None = None
    plan: ExecutionPlan | None = None
    step_results: list[StepResult] = field(default_factory=list)
    duration_ms: float = 0.0
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "execution_id": self.execution_id,
            "success": self.success,
            "output": self.output,
            "error": self.error,
            "step_count": len(self.step_results),
            "duration_ms": self.duration_ms,
        }


class OrchestratorConfig(BaseModel):
    """オーケストレーター設定.

    Attributes:
        max_steps: 最大ステップ数
        max_retries: 最大リトライ回数
        enable_monitoring: 監視を有効化
        enable_replan: 再計画を有効化
        auto_recovery: 自動リカバリを有効化
    """

    max_steps: int = Field(default=20, ge=1)
    max_retries: int = Field(default=3, ge=0)
    enable_monitoring: bool = Field(default=True)
    enable_replan: bool = Field(default=True)
    auto_recovery: bool = Field(default=True)
    default_timeout: float = Field(default=300.0, ge=10.0)


class Orchestrator:
    """オーケストレーター.

    Planner/Executor/Monitorを統合し、タスク実行を管理。

    主な機能:
    - タスクの計画作成
    - 計画に基づく実行
    - リアルタイム監視
    - 失敗時の再計画とリカバリ
    - 状態管理

    Example:
        >>> orchestrator = Orchestrator(
        ...     llm_client=my_llm,
        ...     tool_provider=my_tools,
        ... )
        >>> await orchestrator.initialize()
        >>>
        >>> # 同期実行
        >>> result = await orchestrator.execute("競合分析")
        >>>
        >>> # ストリーム実行
        >>> async for event in orchestrator.execute_stream("..."):
        ...     print(f"{event['type']}: {event['message']}")
    """

    def __init__(
        self,
        llm_client: Any = None,
        tool_provider: Any = None,
        config: OrchestratorConfig | None = None,
        planner_config: PlannerConfig | None = None,
        executor_config: ExecutorConfig | None = None,
        monitor_thresholds: MonitorThresholds | None = None,
    ) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント
            tool_provider: ツールプロバイダー
            config: オーケストレーター設定
            planner_config: 計画Agent設定
            executor_config: 実行Agent設定
            monitor_thresholds: 監視閾値
        """
        self._llm = llm_client
        self._tool_provider = tool_provider
        self._config = config or OrchestratorConfig()

        # サブコンポーネント
        self._planner = PlannerAgent(
            llm_client=llm_client,
            config=planner_config,
        )
        self._executor = ExecutorAgent(
            tool_provider=tool_provider,
            llm_client=llm_client,
            config=executor_config,
        )
        self._monitor = MonitorAgent(
            thresholds=monitor_thresholds,
            alert_callback=self._on_alert,
        )

        # 状態
        self._executions: dict[str, ExecutionContext] = {}
        self._initialized = False
        self._logger = logging.getLogger(__name__)

    async def initialize(self) -> None:
        """初期化."""
        self._logger.info("オーケストレーターを初期化中...")

        # ツールプロバイダーを初期化
        if self._tool_provider and hasattr(self._tool_provider, "initialize"):
            await self._tool_provider.initialize()

        self._initialized = True
        self._logger.info("オーケストレーター初期化完了")

    async def execute(
        self,
        task: str,
        context: dict[str, Any] | None = None,
        available_tools: list[str] | None = None,
    ) -> ExecutionResult:
        """タスクを実行.

        Args:
            task: タスク説明
            context: コンテキスト
            available_tools: 利用可能なツール

        Returns:
            ExecutionResult
        """
        execution_id = f"exec-{uuid.uuid4().hex[:8]}"
        context = context or {}

        self._logger.info(f"タスク実行開始: {execution_id} - {task[:50]}...")

        # 実行コンテキストを作成
        exec_ctx = ExecutionContext(
            execution_id=execution_id,
            task=task,
            context=context,
        )
        self._executions[execution_id] = exec_ctx

        start_time = datetime.now()

        try:
            # 1. 計画作成
            exec_ctx.phase = ExecutionPhase.PLANNING
            exec_ctx.status = ExecutionStatus.RUNNING

            plan = await self._planner.create_plan(
                goal=task,
                context=context,
                available_tools=available_tools,
            )
            exec_ctx.plan = plan

            # 2. 監視開始
            if self._config.enable_monitoring:
                self._monitor.start_monitoring(execution_id)

            # 3. 実行
            exec_ctx.phase = ExecutionPhase.EXECUTING
            step_results: list[StepResult] = []

            async for result in self._executor.execute_plan(plan, context):
                step_results.append(result)
                exec_ctx.results[result.step_id] = result
                exec_ctx.progress = plan.get_progress()

                # 監視に報告
                if self._config.enable_monitoring:
                    step = plan.get_step(result.step_id)
                    step_name = step.name if step else result.step_id

                    if result.success:
                        self._monitor.report_step_completed(
                            execution_id,
                            result.step_id,
                            step_name,
                            result.duration_ms,
                        )
                    else:
                        self._monitor.report_step_failed(
                            execution_id,
                            result.step_id,
                            step_name,
                            result.error or "不明なエラー",
                        )

                        # 再計画
                        if self._config.enable_replan and step:
                            plan = await self._planner.replan(
                                plan,
                                step,
                                result.error or "",
                            )

            # 4. 完了
            exec_ctx.phase = ExecutionPhase.COMPLETED
            exec_ctx.status = ExecutionStatus.COMPLETED
            exec_ctx.completed_at = datetime.now()
            exec_ctx.progress = 1.0

            if self._config.enable_monitoring:
                self._monitor.report_completed(execution_id)

            duration_ms = (datetime.now() - start_time).total_seconds() * 1000

            # 最終出力を構築
            output = self._build_output(step_results, context)

            self._logger.info(
                f"タスク完了: {execution_id} ({duration_ms:.0f}ms, "
                f"{len(step_results)}ステップ)"
            )

            return ExecutionResult(
                execution_id=execution_id,
                success=True,
                output=output,
                plan=plan,
                step_results=step_results,
                duration_ms=duration_ms,
            )

        except Exception as e:
            exec_ctx.phase = ExecutionPhase.FAILED
            exec_ctx.status = ExecutionStatus.FAILED
            exec_ctx.error = str(e)
            exec_ctx.completed_at = datetime.now()

            if self._config.enable_monitoring:
                self._monitor.report_failed(execution_id, str(e))

            duration_ms = (datetime.now() - start_time).total_seconds() * 1000

            self._logger.exception(f"タスク失敗: {execution_id} - {e}")

            return ExecutionResult(
                execution_id=execution_id,
                success=False,
                error=str(e),
                duration_ms=duration_ms,
            )

    async def execute_stream(
        self,
        task: str,
        context: dict[str, Any] | None = None,
        available_tools: list[str] | None = None,
    ) -> AsyncIterator[dict[str, Any]]:
        """タスクをストリーム実行.

        進捗イベントをリアルタイムで返す。

        Args:
            task: タスク説明
            context: コンテキスト
            available_tools: 利用可能なツール

        Yields:
            イベント辞書
        """
        execution_id = f"exec-{uuid.uuid4().hex[:8]}"
        context = context or {}

        # 実行コンテキストを作成
        exec_ctx = ExecutionContext(
            execution_id=execution_id,
            task=task,
            context=context,
        )
        self._executions[execution_id] = exec_ctx

        start_time = datetime.now()

        try:
            # 1. 計画作成
            yield {
                "type": "phase_change",
                "phase": "planning",
                "message": "計画を作成中...",
                "execution_id": execution_id,
            }

            exec_ctx.phase = ExecutionPhase.PLANNING
            exec_ctx.status = ExecutionStatus.RUNNING

            plan = await self._planner.create_plan(
                goal=task,
                context=context,
                available_tools=available_tools,
            )
            exec_ctx.plan = plan

            yield {
                "type": "plan_created",
                "plan_id": plan.id,
                "total_steps": plan.total_steps,
                "estimated_duration": plan.estimated_duration,
                "execution_id": execution_id,
            }

            # 2. 監視開始
            if self._config.enable_monitoring:
                self._monitor.start_monitoring(execution_id)

            # 3. 実行
            yield {
                "type": "phase_change",
                "phase": "executing",
                "message": "実行中...",
                "execution_id": execution_id,
            }

            exec_ctx.phase = ExecutionPhase.EXECUTING
            step_results: list[StepResult] = []

            async for result in self._executor.execute_plan(plan, context):
                step_results.append(result)
                exec_ctx.results[result.step_id] = result
                exec_ctx.progress = plan.get_progress()

                step = plan.get_step(result.step_id)
                step_name = step.name if step else result.step_id

                yield {
                    "type": "step_complete" if result.success else "step_failed",
                    "step_id": result.step_id,
                    "step_name": step_name,
                    "success": result.success,
                    "output": result.output if result.success else None,
                    "error": result.error,
                    "duration_ms": result.duration_ms,
                    "progress": exec_ctx.progress,
                    "execution_id": execution_id,
                }

                # 監視に報告
                if self._config.enable_monitoring:
                    if result.success:
                        self._monitor.report_step_completed(
                            execution_id, result.step_id, step_name, result.duration_ms
                        )
                    else:
                        self._monitor.report_step_failed(
                            execution_id, result.step_id, step_name, result.error or ""
                        )

            # 4. 完了
            exec_ctx.phase = ExecutionPhase.COMPLETED
            exec_ctx.status = ExecutionStatus.COMPLETED
            exec_ctx.completed_at = datetime.now()
            exec_ctx.progress = 1.0

            if self._config.enable_monitoring:
                self._monitor.report_completed(execution_id)

            duration_ms = (datetime.now() - start_time).total_seconds() * 1000
            output = self._build_output(step_results, context)

            yield {
                "type": "completed",
                "success": True,
                "output": output,
                "duration_ms": duration_ms,
                "total_steps": len(step_results),
                "execution_id": execution_id,
            }

        except Exception as e:
            exec_ctx.phase = ExecutionPhase.FAILED
            exec_ctx.status = ExecutionStatus.FAILED
            exec_ctx.error = str(e)
            exec_ctx.completed_at = datetime.now()

            if self._config.enable_monitoring:
                self._monitor.report_failed(execution_id, str(e))

            duration_ms = (datetime.now() - start_time).total_seconds() * 1000

            yield {
                "type": "failed",
                "success": False,
                "error": str(e),
                "duration_ms": duration_ms,
                "execution_id": execution_id,
            }

    def _build_output(
        self,
        step_results: list[StepResult],
        context: dict[str, Any],
    ) -> Any:
        """最終出力を構築.

        Args:
            step_results: ステップ結果
            context: コンテキスト

        Returns:
            最終出力
        """
        # 成功したステップの出力を集約
        outputs = []
        for result in step_results:
            if result.success and result.output:
                outputs.append({
                    "step_id": result.step_id,
                    "output": result.output,
                })

        # 最後の成功出力があればそれを返す
        if outputs:
            return outputs[-1]["output"]

        return None

    def _on_alert(self, event: MonitorEvent) -> None:
        """アラートハンドラー.

        Args:
            event: 監視イベント
        """
        self._logger.warning(
            f"アラート [{event.severity.value}]: {event.message}"
        )

        # 自動リカバリ
        if self._config.auto_recovery and event.severity == AlertSeverity.CRITICAL:
            self._logger.info(f"自動リカバリを検討: {event.execution_id}")
            # TODO: リカバリロジックを実装

    async def cancel(self, execution_id: str) -> bool:
        """実行をキャンセル.

        Args:
            execution_id: 実行ID

        Returns:
            成功したかどうか
        """
        exec_ctx = self._executions.get(execution_id)
        if not exec_ctx:
            return False

        exec_ctx.status = ExecutionStatus.CANCELLED
        exec_ctx.phase = ExecutionPhase.CANCELLED
        exec_ctx.completed_at = datetime.now()

        self._monitor.stop_monitoring(execution_id)

        self._logger.info(f"実行をキャンセル: {execution_id}")
        return True

    def get_status(self, execution_id: str) -> ExecutionContext | None:
        """実行状態を取得.

        Args:
            execution_id: 実行ID

        Returns:
            ExecutionContext or None
        """
        return self._executions.get(execution_id)

    def list_executions(self) -> list[ExecutionContext]:
        """全ての実行を一覧取得.

        Returns:
            ExecutionContextリスト
        """
        return list(self._executions.values())

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        status_counts = dict.fromkeys(ExecutionStatus, 0)
        for ctx in self._executions.values():
            status_counts[ctx.status] += 1

        return {
            "initialized": self._initialized,
            "total_executions": len(self._executions),
            "status_counts": {k.value: v for k, v in status_counts.items()},
            "config": {
                "max_steps": self._config.max_steps,
                "max_retries": self._config.max_retries,
                "enable_monitoring": self._config.enable_monitoring,
                "enable_replan": self._config.enable_replan,
            },
        }

    async def close(self) -> None:
        """リソースを解放."""
        # 全ての監視を停止
        for execution_id in list(self._executions.keys()):
            self._monitor.stop_monitoring(execution_id)

        # ツールプロバイダーをクローズ
        if self._tool_provider and hasattr(self._tool_provider, "close"):
            await self._tool_provider.close()

        self._initialized = False
        self._logger.info("オーケストレーターを終了しました")

    async def __aenter__(self) -> Orchestrator:
        """非同期コンテキストマネージャー."""
        await self.initialize()
        return self

    async def __aexit__(self, *args: Any) -> None:
        """非同期コンテキストマネージャー終了."""
        await self.close()


# エクスポート
__all__ = [
    "ExecutionContext",
    "ExecutionPhase",
    "ExecutionResult",
    "ExecutionStatus",
    "Orchestrator",
    "OrchestratorConfig",
]
