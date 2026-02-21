"""PreviewService - 統一プレビューサービス.

ワークフローの実行とデバッグを統合したサービス層。
Studio / CLI / API 全てが使用します。
"""

from __future__ import annotations

import asyncio
import logging
import time
from typing import TYPE_CHECKING, Any

from agentflow.core.interfaces import (
    DebugEvent,
    ExecutionEvent,
    IWorkflowRunner,
    WorkflowDefinition,
)


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


logger = logging.getLogger(__name__)


class PreviewService(IWorkflowRunner):
    """プレビューサービス.

    ワークフローの実行、ストリーム実行、デバッグ実行を提供します。

    使用例:
        >>> service = PreviewService()
        >>> # 同期実行
        >>> result = await service.run(workflow, {"input": "..."})
        >>> # ストリーム実行
        >>> async for event in service.run_stream(workflow, inputs):
        ...     print(f"{event.progress}%: {event.message}")
    """

    def __init__(self) -> None:
        """初期化."""
        self._executions: dict[str, dict[str, Any]] = {}

    async def run(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        inputs: dict[str, Any],
    ) -> dict[str, Any]:
        """ワークフローを同期実行.

        Args:
            workflow: ワークフロー定義
            inputs: 入力データ

        Returns:
            実行結果
        """
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)

        result: dict[str, Any] = {"inputs": inputs}

        async for event in self.run_stream(workflow, inputs):
            if event.type == "complete":
                result = event.data or result

        return result

    async def run_stream(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        inputs: dict[str, Any],
    ) -> AsyncIterator[ExecutionEvent]:
        """ワークフローをストリーム実行.

        Args:
            workflow: ワークフロー定義
            inputs: 入力データ

        Yields:
            実行イベント
        """
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)

        start_time = time.time()
        execution_id = f"exec_{int(start_time * 1000)}"

        # 実行開始
        yield ExecutionEvent(
            type="start",
            message=f"Starting workflow: {workflow.name}",
            progress=0,
            data={"execution_id": execution_id, "workflow_id": workflow.id},
        )

        # 実行状態を保存
        self._executions[execution_id] = {
            "workflow": workflow,
            "inputs": inputs,
            "results": {},
            "status": "running",
        }

        try:
            # 実際の FlowExecutor を使用して実行
            try:
                from agentflow.flow import FlowBuilder

                # ワークフローからフローを構築
                FlowBuilder(workflow.id)

                # ノードを追加（実際のエージェントクラスが必要）
                # ここでは簡易実装として各ノードを順次実行

                for i, node in enumerate(workflow.nodes):
                    progress = ((i + 1) / len(workflow.nodes)) * 90

                    yield ExecutionEvent(
                        type="node_start",
                        node_id=node.id,
                        message=f"Executing node: {node.agent_type}",
                        progress=progress - 10,
                    )

                    # ノード実行をシミュレート
                    # TODO: 実際のエージェント実行
                    await asyncio.sleep(0.1)

                    node_result = {
                        "node_id": node.id,
                        "agent_type": node.agent_type,
                        "output": f"Result from {node.agent_type}",
                    }

                    self._executions[execution_id]["results"][node.id] = node_result

                    yield ExecutionEvent(
                        type="node_complete",
                        node_id=node.id,
                        message=f"Completed: {node.agent_type}",
                        progress=progress,
                        data=node_result,
                    )

            except ImportError:
                # FlowExecutor がない場合は簡易実行
                for i, node in enumerate(workflow.nodes):
                    progress = ((i + 1) / len(workflow.nodes)) * 90

                    yield ExecutionEvent(
                        type="node_start",
                        node_id=node.id,
                        message=f"Executing: {node.agent_type}",
                        progress=progress - 10,
                    )

                    await asyncio.sleep(0.2)

                    yield ExecutionEvent(
                        type="node_complete",
                        node_id=node.id,
                        message=f"Completed: {node.agent_type}",
                        progress=progress,
                        data={"node_id": node.id, "status": "simulated"},
                    )

            # 完了
            duration_ms = (time.time() - start_time) * 1000
            self._executions[execution_id]["status"] = "completed"

            yield ExecutionEvent(
                type="complete",
                message="Workflow completed successfully",
                progress=100,
                data={
                    "execution_id": execution_id,
                    "duration_ms": duration_ms,
                    "results": self._executions[execution_id]["results"],
                },
            )

        except Exception as e:
            self._executions[execution_id]["status"] = "error"
            self._executions[execution_id]["error"] = str(e)

            yield ExecutionEvent(
                type="error",
                message=f"Workflow failed: {e}",
                data={"error": str(e)},
            )

    async def run_debug(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
        inputs: dict[str, Any],
        breakpoints: list[str] | None = None,
    ) -> AsyncIterator[DebugEvent]:
        """ワークフローをデバッグモードで実行.

        Args:
            workflow: ワークフロー定義
            inputs: 入力データ
            breakpoints: ブレークポイントノード ID

        Yields:
            デバッグイベント
        """
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)

        breakpoints = breakpoints or []
        start_time = time.time()
        f"debug_{int(start_time * 1000)}"

        yield DebugEvent(
            type="start",
            message=f"Starting debug session: {workflow.name}",
            progress=0,
            breakpoint_hit=False,
            variables={"inputs": inputs},
        )

        for i, node in enumerate(workflow.nodes):
            progress = ((i + 1) / len(workflow.nodes)) * 90

            # ブレークポイントチェック
            if node.id in breakpoints:
                yield DebugEvent(
                    type="progress",
                    node_id=node.id,
                    message=f"Breakpoint hit: {node.id}",
                    progress=progress - 10,
                    breakpoint_hit=True,
                    variables={"current_node": node.id, "inputs": inputs},
                    call_stack=[f"{workflow.name}.{node.id}"],
                )
                # ここで一時停止（実際の実装ではユーザー入力を待つ）
                await asyncio.sleep(1)

            yield DebugEvent(
                type="node_start",
                node_id=node.id,
                message=f"Executing: {node.agent_type}",
                progress=progress - 10,
                breakpoint_hit=False,
            )

            await asyncio.sleep(0.2)

            yield DebugEvent(
                type="node_complete",
                node_id=node.id,
                message=f"Completed: {node.agent_type}",
                progress=progress,
                breakpoint_hit=False,
                variables={f"result_{node.id}": {"status": "ok"}},
            )

        duration_ms = (time.time() - start_time) * 1000

        yield DebugEvent(
            type="complete",
            message="Debug session completed",
            progress=100,
            breakpoint_hit=False,
            variables={"duration_ms": duration_ms},
        )

    async def validate(
        self,
        workflow: WorkflowDefinition | dict[str, Any],
    ) -> list[str]:
        """ワークフローを検証.

        Args:
            workflow: ワークフロー定義

        Returns:
            エラーメッセージのリスト
        """
        if isinstance(workflow, dict):
            workflow = WorkflowDefinition.from_dict(workflow)

        errors: list[str] = []

        # 基本検証
        if not workflow.id:
            errors.append("Workflow ID is required")

        if not workflow.name:
            errors.append("Workflow name is required")

        if not workflow.nodes:
            errors.append("Workflow must have at least one node")

        # ノード検証
        node_ids = set()
        for node in workflow.nodes:
            if not node.id:
                errors.append("All nodes must have an ID")
            elif node.id in node_ids:
                errors.append(f"Duplicate node ID: {node.id}")
            else:
                node_ids.add(node.id)

            if not node.agent_type:
                errors.append(f"Node {node.id} must have an agent type")

        # エッジ検証
        for edge in workflow.edges:
            if edge.source not in node_ids:
                errors.append(f"Edge source not found: {edge.source}")
            if edge.target not in node_ids:
                errors.append(f"Edge target not found: {edge.target}")

        return errors

    async def get_node_result(
        self,
        execution_id: str,
        node_id: str,
    ) -> dict[str, Any] | None:
        """特定ノードの実行結果を取得.

        Args:
            execution_id: 実行 ID
            node_id: ノード ID

        Returns:
            ノードの実行結果
        """
        execution = self._executions.get(execution_id)
        if execution is None:
            return None

        results = execution.get("results", {})
        if not isinstance(results, dict):
            return None
        node_result = results.get(node_id)
        return node_result if isinstance(node_result, dict) else None

    def get_execution_status(self, execution_id: str) -> dict[str, Any] | None:
        """実行状態を取得.

        Args:
            execution_id: 実行 ID

        Returns:
            実行状態
        """
        execution = self._executions.get(execution_id)
        if execution is None:
            return None

        return {
            "execution_id": execution_id,
            "workflow_id": execution.get("workflow", {}).id if execution.get("workflow") else None,
            "status": execution.get("status"),
            "results": execution.get("results"),
            "error": execution.get("error"),
        }


__all__ = ["PreviewService"]
