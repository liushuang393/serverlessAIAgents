"""LifecycleHooks のテスト."""

from typing import Any

import pytest

from agentflow.core.hooks import HookEvent, HookType, LifecycleHooks
from agentflow.core.types import ExecutionContext


class TestHookType:
    """HookType のテスト."""

    def test_hook_type_values(self) -> None:
        """HookType の値をテスト."""
        assert HookType.ON_START.value == "on_start"
        assert HookType.ON_NODE_EXEC.value == "on_node_exec"
        assert HookType.ON_NODE_COMPLETE.value == "on_node_complete"
        assert HookType.ON_ERROR.value == "on_error"
        assert HookType.ON_COMPLETE.value == "on_complete"
        assert HookType.ON_CANCEL.value == "on_cancel"

    def test_hook_type_enum_members(self) -> None:
        """HookType の全メンバーをテスト."""
        expected_members = {
            "ON_START",
            "ON_NODE_EXEC",
            "ON_NODE_COMPLETE",
            "ON_ERROR",
            "ON_COMPLETE",
            "ON_CANCEL",
        }
        actual_members = {member.name for member in HookType}
        assert actual_members == expected_members


class TestHookEvent:
    """HookEvent のテスト."""

    def test_hook_event_creation(self) -> None:
        """HookEvent の作成をテスト."""
        event = HookEvent(
            hook_type=HookType.ON_START,
            workflow_name="test-workflow",
            data={"key": "value"},
        )

        assert event.hook_type == HookType.ON_START
        assert event.workflow_name == "test-workflow"
        assert event.data == {"key": "value"}

    def test_hook_event_default_data(self) -> None:
        """HookEvent のデフォルトデータをテスト."""
        event = HookEvent(
            hook_type=HookType.ON_COMPLETE,
            workflow_name="test-workflow",
        )

        assert event.data == {}


class TestLifecycleHooks:
    """LifecycleHooks のテスト."""

    @pytest.fixture
    def hooks(self) -> LifecycleHooks:
        """テスト用の LifecycleHooks インスタンス."""
        return LifecycleHooks()

    @pytest.fixture
    def execution_context(self) -> ExecutionContext:
        """テスト用の ExecutionContext."""
        return ExecutionContext(
            workflow_id="test-workflow",
            execution_id="test-execution-123",
        )

    def test_initialization(self, hooks: LifecycleHooks) -> None:
        """LifecycleHooks の初期化をテスト."""
        assert hooks is not None
        # すべての HookType に対して空のリストが初期化されている
        for hook_type in HookType:
            assert hooks.get_hooks(hook_type) == []

    @pytest.mark.asyncio
    async def test_register_and_trigger_hook(
        self, hooks: LifecycleHooks, execution_context: ExecutionContext
    ) -> None:
        """フックの登録とトリガーをテスト."""
        called = []

        async def test_callback(ctx: ExecutionContext) -> None:
            called.append(ctx.workflow_id)

        hooks.register(HookType.ON_START, test_callback)
        await hooks.trigger(HookType.ON_START, execution_context)

        assert called == ["test-workflow"]

    @pytest.mark.asyncio
    async def test_register_multiple_hooks(
        self, hooks: LifecycleHooks, execution_context: ExecutionContext
    ) -> None:
        """複数のフックを登録してトリガーをテスト."""
        call_order = []

        async def callback1(ctx: ExecutionContext) -> None:
            call_order.append("callback1")

        async def callback2(ctx: ExecutionContext) -> None:
            call_order.append("callback2")

        async def callback3(ctx: ExecutionContext) -> None:
            call_order.append("callback3")

        hooks.register(HookType.ON_START, callback1)
        hooks.register(HookType.ON_START, callback2)
        hooks.register(HookType.ON_START, callback3)

        await hooks.trigger(HookType.ON_START, execution_context)

        assert call_order == ["callback1", "callback2", "callback3"]

    @pytest.mark.asyncio
    async def test_trigger_with_multiple_args(self, hooks: LifecycleHooks) -> None:
        """複数の引数でトリガーをテスト."""
        received_args = []

        async def callback(ctx: ExecutionContext, node_id: str, data: dict[str, Any]) -> None:
            received_args.append((ctx.workflow_id, node_id, data))

        ctx = ExecutionContext(workflow_id="test", execution_id="123")
        hooks.register(HookType.ON_NODE_EXEC, callback)
        await hooks.trigger(HookType.ON_NODE_EXEC, ctx, "node-1", {"status": "running"})

        assert received_args == [("test", "node-1", {"status": "running"})]

    @pytest.mark.asyncio
    async def test_trigger_with_kwargs(self, hooks: LifecycleHooks) -> None:
        """キーワード引数でトリガーをテスト."""
        received_kwargs = []

        async def callback(**kwargs: Any) -> None:
            received_kwargs.append(kwargs)

        hooks.register(HookType.ON_COMPLETE, callback)
        await hooks.trigger(HookType.ON_COMPLETE, result="success", duration=1.5)

        assert received_kwargs == [{"result": "success", "duration": 1.5}]

    @pytest.mark.asyncio
    async def test_unregister_hook(
        self, hooks: LifecycleHooks, execution_context: ExecutionContext
    ) -> None:
        """フックの登録解除をテスト."""
        called = []

        async def callback(ctx: ExecutionContext) -> None:
            called.append(ctx.workflow_id)

        hooks.register(HookType.ON_START, callback)
        hooks.unregister(HookType.ON_START, callback)

        await hooks.trigger(HookType.ON_START, execution_context)

        assert called == []

    def test_unregister_nonexistent_hook(self, hooks: LifecycleHooks) -> None:
        """存在しないフックの登録解除をテスト."""

        async def callback(ctx: ExecutionContext) -> None:
            pass

        with pytest.raises(ValueError, match="Callback not registered"):
            hooks.unregister(HookType.ON_START, callback)

    @pytest.mark.asyncio
    async def test_unregister_one_of_multiple_hooks(
        self, hooks: LifecycleHooks, execution_context: ExecutionContext
    ) -> None:
        """複数のフックのうち1つを登録解除をテスト."""
        call_order = []

        async def callback1(ctx: ExecutionContext) -> None:
            call_order.append("callback1")

        async def callback2(ctx: ExecutionContext) -> None:
            call_order.append("callback2")

        hooks.register(HookType.ON_START, callback1)
        hooks.register(HookType.ON_START, callback2)
        hooks.unregister(HookType.ON_START, callback1)

        await hooks.trigger(HookType.ON_START, execution_context)

        assert call_order == ["callback2"]

    @pytest.mark.asyncio
    async def test_clear_specific_hook_type(
        self, hooks: LifecycleHooks, execution_context: ExecutionContext
    ) -> None:
        """特定のフックタイプをクリアをテスト."""
        called = []

        async def callback(ctx: ExecutionContext) -> None:
            called.append(ctx.workflow_id)

        hooks.register(HookType.ON_START, callback)
        hooks.register(HookType.ON_COMPLETE, callback)

        hooks.clear(HookType.ON_START)

        await hooks.trigger(HookType.ON_START, execution_context)
        await hooks.trigger(HookType.ON_COMPLETE, execution_context)

        # ON_START はクリアされたので呼ばれない
        assert called == ["test-workflow"]

    @pytest.mark.asyncio
    async def test_clear_all_hooks(
        self, hooks: LifecycleHooks, execution_context: ExecutionContext
    ) -> None:
        """すべてのフックをクリアをテスト."""
        called = []

        async def callback(ctx: ExecutionContext) -> None:
            called.append(ctx.workflow_id)

        hooks.register(HookType.ON_START, callback)
        hooks.register(HookType.ON_COMPLETE, callback)
        hooks.register(HookType.ON_ERROR, callback)

        hooks.clear()

        await hooks.trigger(HookType.ON_START, execution_context)
        await hooks.trigger(HookType.ON_COMPLETE, execution_context)
        await hooks.trigger(HookType.ON_ERROR, execution_context)

        # すべてクリアされたので呼ばれない
        assert called == []

    def test_get_hooks(self, hooks: LifecycleHooks) -> None:
        """get_hooks メソッドをテスト."""

        async def callback1(ctx: ExecutionContext) -> None:
            pass

        async def callback2(ctx: ExecutionContext) -> None:
            pass

        hooks.register(HookType.ON_START, callback1)
        hooks.register(HookType.ON_START, callback2)

        callbacks = hooks.get_hooks(HookType.ON_START)

        assert len(callbacks) == 2
        assert callback1 in callbacks
        assert callback2 in callbacks

    def test_get_hooks_returns_copy(self, hooks: LifecycleHooks) -> None:
        """get_hooks がコピーを返すことをテスト."""

        async def callback(ctx: ExecutionContext) -> None:
            pass

        hooks.register(HookType.ON_START, callback)

        callbacks1 = hooks.get_hooks(HookType.ON_START)
        callbacks2 = hooks.get_hooks(HookType.ON_START)

        # 異なるリストインスタンスであることを確認
        assert callbacks1 is not callbacks2
        assert callbacks1 == callbacks2

    @pytest.mark.asyncio
    async def test_hook_exception_propagation(self, hooks: LifecycleHooks) -> None:
        """フック内の例外が伝播することをテスト."""

        async def failing_callback(ctx: ExecutionContext) -> None:
            msg = "Hook failed"
            raise RuntimeError(msg)

        hooks.register(HookType.ON_START, failing_callback)

        ctx = ExecutionContext(workflow_id="test", execution_id="123")

        with pytest.raises(RuntimeError, match="Hook failed"):
            await hooks.trigger(HookType.ON_START, ctx)

    @pytest.mark.asyncio
    async def test_different_hook_types_are_independent(
        self, hooks: LifecycleHooks, execution_context: ExecutionContext
    ) -> None:
        """異なるフックタイプが独立していることをテスト."""
        on_start_called = []
        on_complete_called = []

        async def on_start_callback(ctx: ExecutionContext) -> None:
            on_start_called.append(ctx.workflow_id)

        async def on_complete_callback(ctx: ExecutionContext, result: dict[str, Any]) -> None:
            on_complete_called.append(ctx.workflow_id)

        hooks.register(HookType.ON_START, on_start_callback)
        hooks.register(HookType.ON_COMPLETE, on_complete_callback)

        await hooks.trigger(HookType.ON_START, execution_context)
        await hooks.trigger(HookType.ON_COMPLETE, execution_context, {"status": "done"})

        assert on_start_called == ["test-workflow"]
        assert on_complete_called == ["test-workflow"]
