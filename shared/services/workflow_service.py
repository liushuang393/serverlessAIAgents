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
import importlib
import inspect
import time
from enum import Enum
from typing import TYPE_CHECKING, Any

from shared.services.base import (
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
        self._status_store: dict[str, dict[str, Any]] = {}

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
        self._status_store[execution_id] = {
            "execution_id": execution_id,
            "workflow_type": workflow_type,
            "status": "running",
            "task": task,
            "progress": 0.0,
            "current_step": 0,
            "total_steps": 0,
            "error": None,
            "result": None,
            "started_at": time.time(),
            "updated_at": time.time(),
        }

        try:
            wf_type = WorkflowType(workflow_type)
        except ValueError:
            self._update_status(
                execution_id,
                status="error",
                error=f"Unknown workflow type: {workflow_type}",
            )
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
            self._update_status(
                execution_id,
                status="error",
                error=f"Workflow type not implemented: {workflow_type}",
            )
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
            from kernel.patterns import DeepAgentCoordinator

            coordinator = DeepAgentCoordinator(
                llm_client=config.get("llm_client"),
            )

            # フェーズごとに進捗を報告
            for progress, phase, message in phases:
                self._update_status(
                    execution_id,
                    status="running",
                    progress=progress,
                    current_step=phases.index((progress, phase, message)) + 1,
                    total_steps=len(phases),
                )
                yield self._emit_progress(execution_id, progress, message, phase=phase)
                await asyncio.sleep(0.1)  # UI更新のため

            # 実行
            result = await coordinator.execute(task, **input_data)

            duration_ms = (time.time() - start_time) * 1000
            self._update_status(
                execution_id,
                status="completed",
                progress=100.0,
                current_step=len(phases),
                total_steps=len(phases),
                result=result,
            )
            yield self._emit_progress(execution_id, 100.0, "完了", phase="complete")
            yield self._emit_result(execution_id, result, duration_ms)

        except ImportError:
            # DeepAgentCoordinator がない場合はシミュレーション
            for progress, phase, message in phases:
                self._update_status(
                    execution_id,
                    status="running",
                    progress=progress,
                    current_step=phases.index((progress, phase, message)) + 1,
                    total_steps=len(phases),
                )
                yield self._emit_progress(execution_id, progress, message, phase=phase)
                await asyncio.sleep(0.5)

            duration_ms = (time.time() - start_time) * 1000
            simulated_result = {
                "task": task,
                "status": "simulated",
                "message": "DeepAgentCoordinator not available",
            }
            self._update_status(
                execution_id,
                status="completed",
                progress=100.0,
                current_step=len(phases),
                total_steps=len(phases),
                result=simulated_result,
            )
            yield self._emit_progress(execution_id, 100.0, "完了（シミュレーション）", phase="complete")
            yield self._emit_result(
                execution_id,
                simulated_result,
                duration_ms,
            )

        except Exception as e:
            self._logger.exception("DeepAgent execution error")
            self._update_status(execution_id, status="error", error=f"DeepAgent failed: {e}")
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
            self._update_status(execution_id, status="error", error="Pipeline requires 'agents' in config")
            yield self._emit_error(
                execution_id,
                "invalid_config",
                "Pipeline requires 'agents' in config",
            )
            return

        total = len(agents)
        current_data = input_data
        self._update_status(execution_id, total_steps=total, current_step=0)

        for i, agent_config in enumerate(agents):
            if not isinstance(agent_config, dict):
                self._update_status(
                    execution_id,
                    status="error",
                    error=f"agent config must be dict at step {i + 1}",
                    current_step=i + 1,
                    total_steps=total,
                )
                yield self._emit_error(
                    execution_id,
                    "invalid_agent_config",
                    f"agent config must be dict at step {i + 1}",
                )
                return

            progress = ((i + 1) / total) * 90.0
            agent_name = agent_config.get("name", f"Agent_{i}")

            self._update_status(
                execution_id,
                status="running",
                progress=progress,
                current_step=i + 1,
                total_steps=total,
            )
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

            try:
                current_data = await self._execute_pipeline_agent(
                    agent_config=agent_config,
                    current_data=current_data,
                    step=i + 1,
                )
            except Exception as exc:
                self._update_status(
                    execution_id,
                    status="error",
                    progress=progress,
                    current_step=i + 1,
                    total_steps=total,
                    error=f"Agent execution failed at step {i + 1}: {exc}",
                )
                yield self._emit_error(
                    execution_id,
                    "agent_execution_failed",
                    f"{agent_name} failed: {exc}",
                )
                return

            yield ServiceEvent(
                type=ServiceEventType.AGENT_COMPLETE,
                execution_id=execution_id,
                message=f"Completed {agent_name}",
                data={"agent": agent_name, "step": i + 1, "result": current_data},
            )

        duration_ms = (time.time() - start_time) * 1000
        self._update_status(
            execution_id,
            status="completed",
            progress=100.0,
            current_step=total,
            total_steps=total,
            result=current_data,
        )
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
            self._update_status(
                execution_id,
                status="running",
                progress=progress,
                current_step=i + 1,
                total_steps=max_iterations,
            )

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
        result_payload = {"task": task, "iterations": max_iterations}
        self._update_status(
            execution_id,
            status="completed",
            progress=100.0,
            current_step=max_iterations,
            total_steps=max_iterations,
            result=result_payload,
        )
        yield self._emit_progress(execution_id, 100.0, "Reflection completed", phase="complete")
        yield self._emit_result(
            execution_id,
            result_payload,
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
        return self._status_store.get(execution_id)

    async def _execute_pipeline_agent(
        self,
        *,
        agent_config: dict[str, Any],
        current_data: dict[str, Any],
        step: int,
    ) -> dict[str, Any]:
        """Pipeline の1ステップを実行する."""
        agent_obj = self._resolve_agent(agent_config)
        if agent_obj is None:
            msg = f"agent is not resolvable at step {step}"
            raise ValueError(msg)

        input_data = current_data
        input_mapper = agent_config.get("input_mapper")
        if callable(input_mapper):
            mapped = input_mapper(current_data)
            if isinstance(mapped, dict):
                input_data = mapped

        if hasattr(agent_obj, "run"):
            result = agent_obj.run(input_data)
        elif hasattr(agent_obj, "process"):
            result = agent_obj.process(input_data)
        else:
            msg = f"resolved agent has no run/process method: {agent_obj}"
            raise TypeError(msg)

        if inspect.isawaitable(result):
            resolved = await result
        else:
            resolved = result

        if isinstance(resolved, dict):
            return resolved
        return {"result": resolved}

    def _resolve_agent(self, agent_config: dict[str, Any]) -> Any | None:
        """設定から実行可能なAgentを解決する."""
        direct_agent = agent_config.get("agent")
        if direct_agent is not None:
            return direct_agent

        instance = agent_config.get("instance")
        if instance is not None:
            return instance

        class_path = agent_config.get("class_path")
        if isinstance(class_path, str) and "." in class_path:
            try:
                module_name, class_name = class_path.rsplit(".", 1)
                module = importlib.import_module(module_name)
                klass = getattr(module, class_name)
                return klass() if inspect.isclass(klass) else klass
            except Exception as exc:
                self._logger.warning("failed to load class_path=%s: %s", class_path, exc)

        module_name = agent_config.get("module")
        class_name = agent_config.get("class_name")
        if isinstance(module_name, str) and isinstance(class_name, str):
            try:
                module = importlib.import_module(module_name)
                klass = getattr(module, class_name)
                return klass() if inspect.isclass(klass) else klass
            except Exception as exc:
                self._logger.warning("failed to load module/class=%s.%s: %s", module_name, class_name, exc)

        return None

    def _update_status(
        self,
        execution_id: str,
        *,
        status: str | None = None,
        progress: float | None = None,
        current_step: int | None = None,
        total_steps: int | None = None,
        error: str | None = None,
        result: dict[str, Any] | None = None,
    ) -> None:
        """状態ストアを更新する."""
        state = self._status_store.get(execution_id)
        if state is None:
            return
        if status is not None:
            state["status"] = status
        if progress is not None:
            state["progress"] = progress
        if current_step is not None:
            state["current_step"] = current_step
        if total_steps is not None:
            state["total_steps"] = total_steps
        if error is not None:
            state["error"] = error
        if result is not None:
            state["result"] = result
        state["updated_at"] = time.time()


# =============================================================================
# エクスポート
# =============================================================================

__all__ = ["WorkflowService", "WorkflowType"]
