"""AgentFlow エンジン — kernel 層.

PocketFlow ラッパーとライフサイクルフック実装。
legacy core surface/engine.py から移行。
"""

import asyncio
import logging
import time
import uuid
from collections.abc import Awaitable, Callable
from typing import Any, cast

from pocketflow import AsyncFlow, AsyncNode

from kernel.core.exceptions import WorkflowNotFoundError
from kernel.core.hooks import HookType, LifecycleHooks
from kernel.core.types import ExecutionContext, ExecutionResult, WorkflowConfig


class HookedNode(AsyncNode):  # type: ignore[misc]
    """PocketFlow ノードにフック機能を追加したラッパークラス."""

    def __init__(
        self,
        node_id: str,
        func: Any,
        hooks: LifecycleHooks,
        context: ExecutionContext,
        logger: logging.Logger,
    ) -> None:
        """HookedNode を初期化.

        Args:
            node_id: ノードの一意識別子
            func: 実行する関数
            hooks: ライフサイクルフックマネージャー
            context: 実行コンテキスト
            logger: ロガーインスタンス
        """
        super().__init__()
        self.node_id = node_id
        self.func = func
        self.hooks = hooks
        self.context = context
        self.logger = logger

    async def prep_async(self, shared: dict[str, Any]) -> dict[str, Any]:
        """ノード実行前の準備処理.

        Args:
            shared: 共有状態辞書

        Returns:
            準備結果
        """
        self.logger.debug(f"Preparing node: {self.node_id}")
        return shared

    async def exec_async(self, prep_res: dict[str, Any]) -> dict[str, Any]:
        """ノードのメイン実行処理.

        Args:
            prep_res: 準備処理の結果

        Returns:
            実行結果
        """
        await self.hooks.trigger(HookType.ON_NODE_EXEC, self.context, self.node_id)
        self.logger.debug(f"Executing node: {self.node_id}")

        if callable(self.func):
            result = await self.func(prep_res)
        else:
            result = prep_res

        await self.hooks.trigger(HookType.ON_NODE_COMPLETE, self.context, self.node_id, result)
        return result  # type: ignore[no-any-return]

    async def exec_fallback_async(self, _prep_res: dict[str, Any], exc: Exception) -> dict[str, Any]:
        """エラー発生時のフォールバック処理.

        Args:
            _prep_res: 準備処理の結果
            exc: 発生した例外

        Raises:
            Exception: 元の例外を再送出
        """
        self.logger.error(f"Node execution failed: {self.node_id}, error: {exc!s}")
        await self.hooks.trigger(HookType.ON_ERROR, self.context, exc)
        raise exc


class AgentFlowEngine:
    """AgentFlow メインエンジン.

    PocketFlow をラップし、ライフサイクルフック、プロトコルサポート、
    Agent ブロック管理機能を追加する。
    """

    def __init__(self, *, logger: logging.Logger | None = None) -> None:
        """AgentFlowEngine を初期化.

        Args:
            logger: ロガーインスタンス
        """
        self._logger = logger or logging.getLogger(__name__)
        self._hooks = LifecycleHooks()
        self._workflows: dict[str, WorkflowConfig] = {}
        self._execution_tokens: dict[str, dict[str, bool]] = {}
        self._execution_contexts: dict[str, ExecutionContext] = {}
        self._running_tasks: dict[str, asyncio.Task[Any]] = {}
        self._logger.info("AgentFlowEngine initialized")

    @property
    def hooks(self) -> LifecycleHooks:
        """ライフサイクルフックマネージャーを取得."""
        return self._hooks

    def register_hook(self, hook_type: HookType, callback: Callable[..., Awaitable[None]]) -> None:
        """フックコールバックを登録.

        Args:
            hook_type: フックタイプ
            callback: 非同期コールバック関数
        """
        self._hooks.register(hook_type, callback)

    def unregister_hook(self, hook_type: HookType, callback: Callable[..., Awaitable[None]]) -> None:
        """フックコールバックを解除.

        Args:
            hook_type: フックタイプ
            callback: 解除するコールバック関数
        """
        self._hooks.unregister(hook_type, callback)



    def register_workflow(self, workflow: WorkflowConfig) -> None:
        """ワークフロー設定を登録.

        Args:
            workflow: ワークフロー設定
        """
        self._workflows[workflow.workflow_id] = workflow
        self._logger.info(f"Registered workflow: {workflow.workflow_id}")

    def unregister_workflow(self, workflow_id: str) -> None:
        """ワークフローを解除.

        Args:
            workflow_id: ワークフローID

        Raises:
            WorkflowNotFoundError: ワークフローが未登録の場合
        """
        if workflow_id not in self._workflows:
            raise WorkflowNotFoundError(workflow_id)
        del self._workflows[workflow_id]
        self._logger.info(f"Unregistered workflow: {workflow_id}")

    def get_workflow(self, workflow_id: str) -> WorkflowConfig:
        """登録済みワークフローを取得.

        Args:
            workflow_id: ワークフローID

        Returns:
            WorkflowConfig インスタンス

        Raises:
            WorkflowNotFoundError: ワークフローが未登録の場合
        """
        if workflow_id not in self._workflows:
            raise WorkflowNotFoundError(workflow_id)
        return self._workflows[workflow_id]

    def _build_pocketflow(self, workflow: WorkflowConfig, context: ExecutionContext) -> AsyncFlow:
        """WorkflowConfig から PocketFlow の AsyncFlow を構築.

        Args:
            workflow: ワークフロー設定
            context: 実行コンテキスト

        Returns:
            構築された AsyncFlow インスタンス
        """
        nodes: dict[str, HookedNode] = {}

        def create_node_func(node_config: dict[str, Any]) -> Any:
            if node_config.get("type") == "coordinator" and "coordinator" in node_config:
                coordinator = node_config["coordinator"]

                async def coordinator_func(data: dict[str, Any]) -> dict[str, Any]:
                    if self._is_cancelled(context.execution_id):
                        raise asyncio.CancelledError(f"execution cancelled: {context.execution_id}")
                    task_input = data.get("inputs", data)
                    if isinstance(task_input, dict):
                        result = await coordinator._execute_sequential(task_input)
                    else:
                        result = await coordinator.execute(str(task_input))
                    if "outputs" in data:
                        data["outputs"]["coordinator_result"] = result
                    if self._is_cancelled(context.execution_id):
                        raise asyncio.CancelledError(f"execution cancelled: {context.execution_id}")
                    return cast("dict[str, Any]", result)

                return coordinator_func

            async def node_func(data: dict[str, Any]) -> dict[str, Any]:
                if self._is_cancelled(context.execution_id):
                    raise asyncio.CancelledError(f"execution cancelled: {context.execution_id}")
                return data

            return node_func

        for node_config in workflow.nodes:
            node_id = node_config["id"]
            nodes[node_id] = HookedNode(
                node_id=node_id,
                func=create_node_func(node_config),
                hooks=self._hooks,
                context=context,
                logger=self._logger,
            )

        for edge in workflow.edges:
            source_id = edge.get("source") or edge.get("from")
            target_id = edge.get("target") or edge.get("to")
            if source_id and target_id and source_id in nodes and target_id in nodes:
                nodes[source_id].next(nodes[target_id])

        start_node = None
        for node_config in workflow.nodes:
            node_id = node_config["id"]
            is_start = node_config.get("type") == "start"
            is_not_target = not any(
                (e.get("target") or e.get("to")) == node_id for e in workflow.edges
            )
            if is_start or is_not_target:
                start_node = nodes[node_id]
                break

        return AsyncFlow(start=start_node)


    async def execute(self, workflow_id: str, inputs: dict[str, Any]) -> ExecutionResult:
        """ワークフローを実行.

        Args:
            workflow_id: ワークフローID
            inputs: ワークフローへの入力パラメータ

        Returns:
            ExecutionResult（ステータス、出力、メタデータを含む）

        Raises:
            WorkflowNotFoundError: ワークフローが未登録の場合
        """
        if workflow_id not in self._workflows:
            raise WorkflowNotFoundError(workflow_id)

        execution_id = str(uuid.uuid4())
        context = ExecutionContext(
            workflow_id=workflow_id,
            execution_id=execution_id,
            inputs=inputs,
        )

        start_time = time.time()
        self._logger.info(f"Starting execution: workflow={workflow_id}, execution={execution_id}")
        self._execution_tokens[execution_id] = {"cancelled": False}
        self._execution_contexts[execution_id] = context
        current_task = asyncio.current_task()
        if current_task is not None:
            self._running_tasks[execution_id] = current_task

        try:
            await self._hooks.trigger(HookType.ON_START, context)
            workflow = self._workflows[workflow_id]
            flow = self._build_pocketflow(workflow, context)
            shared_state: dict[str, Any] = {"inputs": inputs, "outputs": {}}
            result = await flow.run_async(shared_state)

            coordinator_result = shared_state.get("outputs", {}).get("coordinator_result")
            final_result = coordinator_result if coordinator_result is not None else result
            output: dict[str, Any] = {
                "workflow_id": workflow_id,
                "execution_id": execution_id,
                "result": final_result,
                "outputs": shared_state.get("outputs", {}),
            }

            await self._hooks.trigger(HookType.ON_COMPLETE, context, output)
            duration = time.time() - start_time
            self._logger.info(
                f"Execution completed: workflow={workflow_id}, "
                f"execution={execution_id}, duration={duration:.2f}s"
            )
            return ExecutionResult(
                status="success", output=output, error=None, duration=duration, context=context,
            )

        except asyncio.CancelledError:
            await self._hooks.trigger(HookType.ON_CANCEL, context)
            duration = time.time() - start_time
            return ExecutionResult(
                status="cancelled", output={}, error="execution_cancelled",
                duration=duration, context=context,
            )
        except Exception as e:
            await self._hooks.trigger(HookType.ON_ERROR, context, e)
            duration = time.time() - start_time
            self._logger.exception(
                f"Execution failed: workflow={workflow_id}, execution={execution_id}"
            )
            return ExecutionResult(
                status="error", output={}, error=str(e), duration=duration, context=context,
            )
        finally:
            self._running_tasks.pop(execution_id, None)
            self._execution_tokens.pop(execution_id, None)
            self._execution_contexts.pop(execution_id, None)

    async def cancel(self, execution_id: str) -> None:
        """実行中のワークフローをキャンセル.

        Args:
            execution_id: キャンセルする実行ID
        """
        self._logger.info(f"Cancelling execution: {execution_id}")
        token = self._execution_tokens.get(execution_id)
        if token is not None:
            token["cancelled"] = True

        task = self._running_tasks.get(execution_id)
        if task is not None and not task.done():
            task.cancel()

        context = self._execution_contexts.get(execution_id) or ExecutionContext(
            workflow_id="unknown", execution_id=execution_id, inputs={},
        )
        await self._hooks.trigger(HookType.ON_CANCEL, context)

    def _is_cancelled(self, execution_id: str) -> bool:
        """実行がキャンセルされたか確認."""
        token = self._execution_tokens.get(execution_id)
        return bool(token and token.get("cancelled"))

    def list_workflows(self) -> list[str]:
        """登録済みワークフローIDの一覧を取得.

        Returns:
            ワークフローIDのリスト
        """
        return list(self._workflows.keys())
