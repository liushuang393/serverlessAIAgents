"""AgentFlow engine - PocketFlowラッパーとライフサイクルフック実装."""

import logging
import time
import uuid
from collections.abc import Awaitable, Callable
from typing import Any, cast

from pocketflow import AsyncFlow, AsyncNode

from agentflow.core.exceptions import WorkflowNotFoundError
from agentflow.core.hooks import HookType, LifecycleHooks
from agentflow.core.types import ExecutionContext, ExecutionResult, WorkflowConfig


class HookedNode(AsyncNode):  # type: ignore[misc]
    """PocketFlow ノードにフック機能を追加したラッパークラス.

    このクラスは PocketFlow の AsyncNode を継承し、各ライフサイクルポイントで
    AgentFlow のフックをトリガーします。
    """

    def __init__(
        self,
        node_id: str,
        func: Any,
        hooks: LifecycleHooks,
        context: ExecutionContext,
        logger: logging.Logger,
    ) -> None:
        """HookedNodeを初期化.

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
        # ON_NODE_EXEC フックをトリガー
        await self.hooks.trigger(HookType.ON_NODE_EXEC, self.context, self.node_id)

        self.logger.debug(f"Executing node: {self.node_id}")

        # 関数を実行
        if callable(self.func):
            result = await self.func(prep_res)
        else:
            result = prep_res

        # ON_NODE_COMPLETE フックをトリガー
        await self.hooks.trigger(HookType.ON_NODE_COMPLETE, self.context, self.node_id, result)

        return result  # type: ignore[no-any-return]

    async def exec_fallback_async(self, _prep_res: dict[str, Any], exc: Exception) -> dict[str, Any]:
        """エラー発生時のフォールバック処理.

        Args:
            prep_res: 準備処理の結果
            exc: 発生した例外

        Returns:
            フォールバック結果

        Raises:
            Exception: 元の例外を再送出
        """
        self.logger.error(f"Node execution failed: {self.node_id}, error: {exc!s}")
        # ON_ERROR フックをトリガー
        await self.hooks.trigger(HookType.ON_ERROR, self.context, exc)
        raise exc


class AgentFlowEngine:
    """Main execution engine for AgentFlow.

    This class wraps PocketFlow and adds lifecycle hooks, protocol support,
    and agent block management capabilities.

    Example:
        >>> engine = AgentFlowEngine()
        >>> result = await engine.execute("my-workflow", {"input": "test"})
        >>> print(result.status)
        success
    """

    def __init__(
        self,
        *,
        logger: logging.Logger | None = None,
    ) -> None:
        """Initialize AgentFlow engine.

        Args:
            logger: Optional logger instance. If not provided, creates a default logger.
        """
        self._logger = logger or logging.getLogger(__name__)
        self._hooks = LifecycleHooks()
        self._workflows: dict[str, WorkflowConfig] = {}
        self._logger.info("AgentFlowEngine initialized")

    @property
    def hooks(self) -> LifecycleHooks:
        """Get lifecycle hooks manager.

        Returns:
            LifecycleHooks instance for registering callbacks.

        Example:
            >>> engine = AgentFlowEngine()
            >>> async def log_start(ctx: ExecutionContext) -> None:
            ...     print(f"Starting: {ctx.workflow_id}")
            >>> engine.hooks.register(HookType.ON_START, log_start)
        """
        return self._hooks

    def register_hook(
        self,
        hook_type: HookType,
        callback: Callable[..., Awaitable[None]],
    ) -> None:
        """フックコールバックを登録.

        Args:
            hook_type: フックタイプ
            callback: 非同期コールバック関数
        """
        self._hooks.register(hook_type, callback)

    def unregister_hook(
        self,
        hook_type: HookType,
        callback: Callable[..., Awaitable[None]],
    ) -> None:
        """フックコールバックを解除.

        Args:
            hook_type: フックタイプ
            callback: 解除するコールバック関数
        """
        self._hooks.unregister(hook_type, callback)

    def register_workflow(self, workflow: WorkflowConfig) -> None:
        """Register a workflow configuration.

        Args:
            workflow: Workflow configuration to register.

        Example:
            >>> engine = AgentFlowEngine()
            >>> workflow = WorkflowConfig(
            ...     workflow_id="test",
            ...     name="Test Workflow",
            ...     nodes=[],
            ...     edges=[],
            ... )
            >>> engine.register_workflow(workflow)
        """
        self._workflows[workflow.workflow_id] = workflow
        self._logger.info(f"Registered workflow: {workflow.workflow_id}")

    def unregister_workflow(self, workflow_id: str) -> None:
        """Unregister a workflow.

        Args:
            workflow_id: ID of workflow to unregister.

        Raises:
            WorkflowNotFoundError: If workflow is not registered.

        Example:
            >>> engine = AgentFlowEngine()
            >>> engine.unregister_workflow("test")
        """
        if workflow_id not in self._workflows:
            raise WorkflowNotFoundError(workflow_id)
        del self._workflows[workflow_id]
        self._logger.info(f"Unregistered workflow: {workflow_id}")

    def get_workflow(self, workflow_id: str) -> WorkflowConfig:
        """Get a registered workflow.

        Args:
            workflow_id: ID of workflow to retrieve.

        Returns:
            WorkflowConfig instance.

        Raises:
            WorkflowNotFoundError: If workflow is not registered.

        Example:
            >>> engine = AgentFlowEngine()
            >>> workflow = engine.get_workflow("test")
        """
        if workflow_id not in self._workflows:
            raise WorkflowNotFoundError(workflow_id)
        return self._workflows[workflow_id]

    def _build_pocketflow(self, workflow: WorkflowConfig, context: ExecutionContext) -> AsyncFlow:
        """WorkflowConfigからPocketFlowのAsyncFlowを構築.

        Args:
            workflow: ワークフロー設定
            context: 実行コンテキスト

        Returns:
            構築されたAsyncFlowインスタンス
        """
        # ノードマップを作成
        nodes: dict[str, HookedNode] = {}

        # ノード関数を定義
        def create_node_func(node_config: dict[str, Any]) -> Any:
            # coordinatorノードの場合は、coordinatorを直接実行
            if node_config.get("type") == "coordinator" and "coordinator" in node_config:
                coordinator = node_config["coordinator"]

                async def coordinator_func(data: dict[str, Any]) -> dict[str, Any]:
                    # inputsからtaskを取得、なければ全体を渡す
                    task_input = data.get("inputs", data)
                    # coordinatorがdict入力を期待する場合は直接渡す
                    if isinstance(task_input, dict):
                        # AgentCoordinatorのexecuteメソッドがstr型を期待しているため、
                        # _execute_sequentialを直接呼び出す
                        result = await coordinator._execute_sequential(task_input)
                    else:
                        result = await coordinator.execute(str(task_input))

                    # 結果をshared_stateに保存
                    if "outputs" in data:
                        data["outputs"]["coordinator_result"] = result

                    return cast("dict[str, Any]", result)

                return coordinator_func

            # 通常のノードはパススルー
            async def node_func(data: dict[str, Any]) -> dict[str, Any]:
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

        # エッジに基づいてノードを接続
        for edge in workflow.edges:
            source_id = edge.get("source") or edge.get("from")
            target_id = edge.get("target") or edge.get("to")
            if source_id and target_id and source_id in nodes and target_id in nodes:
                nodes[source_id].next(nodes[target_id])

        # 開始ノードを見つける
        start_node = None
        for node_config in workflow.nodes:
            node_id = node_config["id"]
            # startタイプのノード、またはどのedgeのターゲットにもなっていないノード
            is_start = node_config.get("type") == "start"
            is_not_target = not any((e.get("target") or e.get("to")) == node_id for e in workflow.edges)
            if is_start or is_not_target:
                start_node = nodes[node_id]
                break

        # AsyncFlow を作成
        return AsyncFlow(start=start_node)

    async def execute(
        self,
        workflow_id: str,
        inputs: dict[str, Any],
    ) -> ExecutionResult:
        """Execute a workflow.

        Args:
            workflow_id: ID of workflow to execute.
            inputs: Input parameters for the workflow.

        Returns:
            ExecutionResult containing status, output, and metadata.

        Raises:
            WorkflowNotFoundError: If workflow is not registered.

        Example:
            >>> engine = AgentFlowEngine()
            >>> result = await engine.execute("my-workflow", {"input": "test"})
            >>> print(result.status)
            success
        """
        # Validate workflow exists
        if workflow_id not in self._workflows:
            raise WorkflowNotFoundError(workflow_id)

        # Create execution context
        execution_id = str(uuid.uuid4())
        context = ExecutionContext(
            workflow_id=workflow_id,
            execution_id=execution_id,
            inputs=inputs,
        )

        start_time = time.time()
        self._logger.info(f"Starting execution: workflow={workflow_id}, execution={execution_id}")

        try:
            # ON_START フックをトリガー
            await self._hooks.trigger(HookType.ON_START, context)

            # PocketFlow ワークフローを構築
            workflow = self._workflows[workflow_id]
            flow = self._build_pocketflow(workflow, context)

            # PocketFlow で実行
            shared_state: dict[str, Any] = {"inputs": inputs, "outputs": {}}
            result = await flow.run_async(shared_state)

            # coordinatorの結果を取得
            coordinator_result = shared_state.get("outputs", {}).get("coordinator_result")
            final_result = coordinator_result if coordinator_result is not None else result

            output: dict[str, Any] = {
                "workflow_id": workflow_id,
                "execution_id": execution_id,
                "result": final_result,
                "outputs": shared_state.get("outputs", {}),
            }

            # ON_COMPLETE フックをトリガー
            await self._hooks.trigger(HookType.ON_COMPLETE, context, output)

            duration = time.time() - start_time
            self._logger.info(
                f"Execution completed: workflow={workflow_id}, execution={execution_id}, duration={duration:.2f}s"
            )

            return ExecutionResult(
                status="success",
                output=output,
                error=None,
                duration=duration,
                context=context,
            )

        except Exception as e:
            # Trigger ON_ERROR hooks
            await self._hooks.trigger(HookType.ON_ERROR, context, e)

            duration = time.time() - start_time
            self._logger.exception(f"Execution failed: workflow={workflow_id}, execution={execution_id}")

            return ExecutionResult(
                status="error",
                output={},
                error=str(e),
                duration=duration,
                context=context,
            )

    async def cancel(self, execution_id: str) -> None:
        """Cancel a running execution.

        Args:
            execution_id: ID of execution to cancel.

        Example:
            >>> engine = AgentFlowEngine()
            >>> await engine.cancel("execution-123")
        """
        # TODO: Implement cancellation logic
        self._logger.info(f"Cancelling execution: {execution_id}")
        # Trigger ON_CANCEL hooks
        context = ExecutionContext(
            workflow_id="unknown",
            execution_id=execution_id,
            inputs={},
        )
        await self._hooks.trigger(HookType.ON_CANCEL, context)

    def list_workflows(self) -> list[str]:
        """List all registered workflow IDs.

        Returns:
            List of workflow IDs.

        Example:
            >>> engine = AgentFlowEngine()
            >>> workflows = engine.list_workflows()
            >>> print(workflows)
            ['workflow-1', 'workflow-2']
        """
        return list(self._workflows.keys())
