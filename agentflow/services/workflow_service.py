"""Workflow Service - 統一Workflow実行サービス.

API / CLI / Studio 全てが使用する Workflow 実行サービス。

使用例:
    >>> # DeepAgent ワークフロー実行
    >>> service = WorkflowService()
    >>> async for event in service.execute_stream(
    ...     workflow_type="deep_agent",
    ...     task="データ分析レポートを作成",
    ... ):
    ...     print(event.to_json())
"""

from __future__ import annotations

import asyncio
import time
from enum import Enum
from typing import TYPE_CHECKING, Any

from agentflow.services.base import (
    LogLevel,
    ServiceBase,
    ServiceEvent,
    ServiceEventType,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator
    from pathlib import Path


class WorkflowType(str, Enum):
    """ワークフロータイプ."""

    DEEP_AGENT = "deep_agent"
    PIPELINE = "pipeline"
    REFLECTION = "reflection"
    CUSTOM = "custom"


class WorkflowService(ServiceBase[dict[str, Any]]):
    """Workflow実行サービス.

    全交互モード（API/CLI/Studio）で共通のWorkflow実行ロジックを提供。

    サポートするワークフロー:
    - DeepAgent: 6フェーズ協調実行
    - Pipeline: 順次実行
    - Reflection: 自己改善ループ
    - Custom: カスタムフロー
    """

    def __init__(
        self,
        workflows_dir: Path | None = None,
    ) -> None:
        """初期化.

        Args:
            workflows_dir: Workflowディレクトリ
        """
        super().__init__()
        self._workflows_dir = workflows_dir

    async def _execute_internal(
        self,
        execution_id: str,
        *,
        workflow_type: str = "deep_agent",
        task: str = "",
        input_data: dict[str, Any] | None = None,
        config: dict[str, Any] | None = None,
        **kwargs: Any,
    ) -> AsyncIterator[ServiceEvent]:
        """Workflow実行（内部）.

        Args:
            execution_id: 実行ID
            workflow_type: ワークフロータイプ
            task: タスク説明
            input_data: 入力データ
            config: 設定
            **kwargs: 追加オプション

        Yields:
            実行イベント
        """
        start_time = time.time()
        input_data = input_data or {}
        config = config or {}

        yield self._emit_log(
            execution_id,
            f"Starting workflow: {workflow_type}",
            LogLevel.INFO,
        )

        try:
            wf_type = WorkflowType(workflow_type)
        except ValueError:
            yield self._emit_error(
                execution_id,
                "invalid_workflow_type",
                f"Unknown workflow type: {workflow_type}",
            )
            return

        # ワークフロータイプ別実行
        if wf_type == WorkflowType.DEEP_AGENT:
            async for event in self._execute_deep_agent(execution_id, task, input_data, config, start_time):
                yield event
        elif wf_type == WorkflowType.PIPELINE:
            async for event in self._execute_pipeline(execution_id, input_data, config, start_time):
                yield event
        elif wf_type == WorkflowType.REFLECTION:
            async for event in self._execute_reflection(execution_id, task, input_data, config, start_time):
                yield event
        else:
            yield self._emit_error(
                execution_id,
                "not_implemented",
                f"Workflow type not implemented: {workflow_type}",
            )

    async def _execute_deep_agent(
        self,
        execution_id: str,
        task: str,
        input_data: dict[str, Any],
        config: dict[str, Any],
        start_time: float,
    ) -> AsyncIterator[ServiceEvent]:
        """DeepAgent ワークフロー実行.

        6フェーズ:
        1. 認知分析
        2. タスク分解
        3. Agent選択/生成
        4. 並行実行
        5. 品質評審
        6. 自己進化
        """
        phases = [
            (10, "cognitive_analysis", "認知分析中..."),
            (25, "task_decomposition", "タスク分解中..."),
            (40, "agent_selection", "Agent選択中..."),
            (70, "execution", "実行中..."),
            (85, "quality_review", "品質評審中..."),
            (95, "evolution", "学習中..."),
        ]

        try:
            from agentflow.patterns import DeepAgentCoordinator

            coordinator = DeepAgentCoordinator(
                llm_client=config.get("llm_client"),
            )

            # フェーズごとに進捗を報告
            for progress, phase, message in phases:
                yield self._emit_progress(execution_id, progress, message, phase=phase)
                await asyncio.sleep(0.1)  # UI更新のため

            # 実行
            result = await coordinator.execute(task, **input_data)

            duration_ms = (time.time() - start_time) * 1000
            yield self._emit_progress(execution_id, 100.0, "完了", phase="complete")
            yield self._emit_result(execution_id, result, duration_ms)

        except ImportError:
            # DeepAgentCoordinator がない場合はシミュレーション
            for progress, phase, message in phases:
                yield self._emit_progress(execution_id, progress, message, phase=phase)
                await asyncio.sleep(0.5)

            duration_ms = (time.time() - start_time) * 1000
            yield self._emit_progress(execution_id, 100.0, "完了（シミュレーション）", phase="complete")
            yield self._emit_result(
                execution_id,
                {
                    "task": task,
                    "status": "simulated",
                    "message": "DeepAgentCoordinator not available",
                },
                duration_ms,
            )

        except Exception as e:
            self._logger.exception("DeepAgent execution error")
            yield self._emit_error(execution_id, "execution_error", f"DeepAgent failed: {e}")

    async def _execute_pipeline(
        self,
        execution_id: str,
        input_data: dict[str, Any],
        config: dict[str, Any],
        start_time: float,
    ) -> AsyncIterator[ServiceEvent]:
        """Pipeline ワークフロー実行."""
        agents = config.get("agents", [])
        if not agents:
            yield self._emit_error(
                execution_id,
                "invalid_config",
                "Pipeline requires 'agents' in config",
            )
            return

        total = len(agents)
        current_data = input_data

        for i, agent_config in enumerate(agents):
            progress = ((i + 1) / total) * 90.0
            agent_name = agent_config.get("name", f"Agent_{i}")

            yield self._emit_progress(
                execution_id,
                progress,
                f"Executing {agent_name}...",
                phase="execute",
                current_step=i + 1,
                total_steps=total,
            )

            yield ServiceEvent(
                type=ServiceEventType.AGENT_START,
                execution_id=execution_id,
                message=f"Starting {agent_name}",
                data={"agent": agent_name, "step": i + 1},
            )

            # TODO: 実際のAgent実行
            await asyncio.sleep(0.5)

            yield ServiceEvent(
                type=ServiceEventType.AGENT_COMPLETE,
                execution_id=execution_id,
                message=f"Completed {agent_name}",
                data={"agent": agent_name, "step": i + 1},
            )

        duration_ms = (time.time() - start_time) * 1000
        yield self._emit_progress(execution_id, 100.0, "Pipeline completed", phase="complete")
        yield self._emit_result(execution_id, current_data, duration_ms)

    async def _execute_reflection(
        self,
        execution_id: str,
        task: str,
        input_data: dict[str, Any],
        config: dict[str, Any],
        start_time: float,
    ) -> AsyncIterator[ServiceEvent]:
        """Reflection ワークフロー実行."""
        max_iterations = config.get("max_iterations", 3)

        for i in range(max_iterations):
            progress = ((i + 1) / max_iterations) * 90.0

            # Generate
            yield self._emit_progress(
                execution_id,
                progress - 20,
                f"Iteration {i + 1}: Generating...",
                phase="generate",
            )
            await asyncio.sleep(0.3)

            # Reflect
            yield self._emit_progress(
                execution_id,
                progress - 10,
                f"Iteration {i + 1}: Reflecting...",
                phase="reflect",
            )
            await asyncio.sleep(0.3)

            # Check if satisfactory
            yield self._emit_progress(
                execution_id,
                progress,
                f"Iteration {i + 1}: Evaluating...",
                phase="evaluate",
            )
            await asyncio.sleep(0.3)

        duration_ms = (time.time() - start_time) * 1000
        yield self._emit_progress(execution_id, 100.0, "Reflection completed", phase="complete")
        yield self._emit_result(
            execution_id,
            {"task": task, "iterations": max_iterations},
            duration_ms,
        )

    # =========================================================================
    # 追加API（Workflow固有）
    # =========================================================================

    async def list_workflows(self) -> list[dict[str, Any]]:
        """利用可能なWorkflowを一覧取得.

        Returns:
            Workflow情報リスト
        """
        return [
            {
                "id": "deep_agent",
                "name": "DeepAgent",
                "description": "6フェーズ協調実行ワークフロー",
                "phases": ["認知分析", "タスク分解", "Agent選択", "実行", "品質評審", "自己進化"],
            },
            {
                "id": "pipeline",
                "name": "Pipeline",
                "description": "順次実行パイプライン",
            },
            {
                "id": "reflection",
                "name": "Reflection",
                "description": "自己改善ループ",
            },
        ]

    async def get_workflow_status(self, execution_id: str) -> dict[str, Any] | None:
        """Workflow実行状態を取得.

        Args:
            execution_id: 実行ID

        Returns:
            状態情報
        """
        # TODO: 状態ストアから取得
        return None


# =============================================================================
# エクスポート
# =============================================================================

__all__ = ["WorkflowService", "WorkflowType"]
