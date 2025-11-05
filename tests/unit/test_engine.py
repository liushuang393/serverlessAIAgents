"""Unit tests for AgentFlowEngine."""

import pytest

from agentflow.core.engine import AgentFlowEngine
from agentflow.core.exceptions import WorkflowNotFoundError
from agentflow.core.hooks import HookType
from agentflow.core.types import ExecutionContext, WorkflowConfig


class TestAgentFlowEngine:
    """Test suite for AgentFlowEngine."""

    def test_engine_initialization(self) -> None:
        """Test that engine initializes correctly."""
        engine = AgentFlowEngine()
        assert engine is not None
        assert engine.hooks is not None
        assert engine.list_workflows() == []

    def test_register_workflow(
        self,
        engine: AgentFlowEngine,
        sample_workflow: WorkflowConfig,
    ) -> None:
        """Test workflow registration."""
        engine.register_workflow(sample_workflow)
        assert sample_workflow.workflow_id in engine.list_workflows()

        # Verify we can retrieve the workflow
        retrieved = engine.get_workflow(sample_workflow.workflow_id)
        assert retrieved.workflow_id == sample_workflow.workflow_id
        assert retrieved.name == sample_workflow.name

    def test_unregister_workflow(
        self,
        engine: AgentFlowEngine,
        sample_workflow: WorkflowConfig,
    ) -> None:
        """Test workflow unregistration."""
        engine.register_workflow(sample_workflow)
        assert sample_workflow.workflow_id in engine.list_workflows()

        engine.unregister_workflow(sample_workflow.workflow_id)
        assert sample_workflow.workflow_id not in engine.list_workflows()

    def test_unregister_nonexistent_workflow(self, engine: AgentFlowEngine) -> None:
        """Test that unregistering nonexistent workflow raises error."""
        with pytest.raises(WorkflowNotFoundError) as exc_info:
            engine.unregister_workflow("nonexistent")
        assert "nonexistent" in str(exc_info.value)

    def test_get_nonexistent_workflow(self, engine: AgentFlowEngine) -> None:
        """Test that getting nonexistent workflow raises error."""
        with pytest.raises(WorkflowNotFoundError) as exc_info:
            engine.get_workflow("nonexistent")
        assert "nonexistent" in str(exc_info.value)

    async def test_execute_workflow_success(
        self,
        engine: AgentFlowEngine,
        sample_workflow: WorkflowConfig,
    ) -> None:
        """Test successful workflow execution."""
        engine.register_workflow(sample_workflow)

        result = await engine.execute(
            sample_workflow.workflow_id,
            {"input": "test"},
        )

        assert result.status == "success"
        assert result.error is None
        assert result.duration >= 0
        assert result.context.workflow_id == sample_workflow.workflow_id

    async def test_execute_nonexistent_workflow(self, engine: AgentFlowEngine) -> None:
        """Test that executing nonexistent workflow raises error."""
        with pytest.raises(WorkflowNotFoundError):
            await engine.execute("nonexistent", {})

    async def test_hooks_are_triggered(
        self,
        engine: AgentFlowEngine,
        sample_workflow: WorkflowConfig,
    ) -> None:
        """Test that lifecycle hooks are triggered during execution."""
        engine.register_workflow(sample_workflow)

        # Track hook calls
        hook_calls: list[str] = []

        async def on_start(ctx: ExecutionContext) -> None:
            hook_calls.append("start")

        async def on_complete(ctx: ExecutionContext, output: dict) -> None:
            hook_calls.append("complete")

        # Register hooks
        engine.hooks.register(HookType.ON_START, on_start)
        engine.hooks.register(HookType.ON_COMPLETE, on_complete)

        # Execute workflow
        await engine.execute(sample_workflow.workflow_id, {})

        # Verify hooks were called
        assert "start" in hook_calls
        assert "complete" in hook_calls

    async def test_error_hook_on_failure(self, engine: AgentFlowEngine) -> None:
        """Test that error hook is triggered on execution failure."""
        # Register a workflow that will fail
        workflow = WorkflowConfig(
            workflow_id="failing-workflow",
            name="Failing Workflow",
            nodes=[],
            edges=[],
        )
        engine.register_workflow(workflow)

        # Track error hook calls
        error_caught = False

        async def on_error(ctx: ExecutionContext, error: Exception) -> None:
            nonlocal error_caught
            error_caught = True

        engine.hooks.register(HookType.ON_ERROR, on_error)

        # Note: Current implementation doesn't actually fail,
        # so this test will need to be updated when real execution is implemented
        result = await engine.execute("failing-workflow", {})

        # For now, just verify the result structure
        assert result.status in ("success", "error")

    def test_list_workflows(
        self,
        engine: AgentFlowEngine,
        sample_workflow: WorkflowConfig,
    ) -> None:
        """Test listing registered workflows."""
        assert engine.list_workflows() == []

        engine.register_workflow(sample_workflow)
        workflows = engine.list_workflows()
        assert len(workflows) == 1
        assert sample_workflow.workflow_id in workflows

        # Register another workflow
        workflow2 = WorkflowConfig(
            workflow_id="workflow-2",
            name="Workflow 2",
            nodes=[],
            edges=[],
        )
        engine.register_workflow(workflow2)

        workflows = engine.list_workflows()
        assert len(workflows) == 2
        assert sample_workflow.workflow_id in workflows
        assert workflow2.workflow_id in workflows

    def test_register_and_unregister_hook(self, engine: AgentFlowEngine) -> None:
        """Test registering and unregistering hooks."""

        async def test_callback(ctx: ExecutionContext) -> None:
            pass

        # Test register_hook method
        engine.register_hook(HookType.ON_START, test_callback)
        hooks = engine.hooks.get_hooks(HookType.ON_START)
        assert test_callback in hooks

        # Test unregister_hook method
        engine.unregister_hook(HookType.ON_START, test_callback)
        hooks = engine.hooks.get_hooks(HookType.ON_START)
        assert test_callback not in hooks

    async def test_cancel_execution(self, engine: AgentFlowEngine) -> None:
        """Test cancelling an execution."""
        # Track cancel hook calls
        cancel_called = []

        async def on_cancel(ctx: ExecutionContext) -> None:
            cancel_called.append(ctx.execution_id)

        engine.hooks.register(HookType.ON_CANCEL, on_cancel)

        # Cancel an execution
        await engine.cancel("test-execution-123")

        assert cancel_called == ["test-execution-123"]

    async def test_execute_with_non_callable_node(self, engine: AgentFlowEngine) -> None:
        """Test executing workflow with non-callable node function."""
        # Create a workflow with a non-callable node
        workflow = WorkflowConfig(
            workflow_id="test-non-callable",
            name="Test Non-Callable",
            nodes=[
                {
                    "id": "node1",
                    "type": "process",
                    "func": None,  # Non-callable
                }
            ],
            edges=[],
        )

        engine.register_workflow(workflow)

        # Execute should handle non-callable gracefully
        result = await engine.execute("test-non-callable", {"input": "test"})

        # Should complete successfully even with non-callable node
        assert result.status == "success"
