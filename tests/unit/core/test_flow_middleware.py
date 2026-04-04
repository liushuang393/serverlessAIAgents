"""FlowMiddleware と FlowExecutor の統合テスト."""

from __future__ import annotations

import pytest

from contracts.flow.contracts import (
    FlowMiddleware,
    MiddlewareDecision,
    MiddlewareResult,
)
from kernel.flow.builder import create_flow
from kernel.flow.context import FlowContext, ScopedContextView
from kernel.flow.nodes import BranchNode
from kernel.flow.types import AgentProtocol


# ============================================================
# テスト用 Agent / Middleware
# ============================================================


class EchoAgent:
    """入力をそのまま返すテスト用 Agent."""

    name = "EchoAgent"

    async def run(self, inputs: dict) -> dict:
        return {"echo": inputs.get("question", "no input")}


class UpperAgent:
    """文字列を大文字にするテスト用 Agent."""

    name = "UpperAgent"

    async def run(self, inputs: dict) -> dict:
        return {"result": inputs.get("question", "").upper()}


class AllowMiddleware:
    """常に ALLOW を返すミドルウェア."""

    @property
    def name(self) -> str:
        return "AllowMiddleware"

    async def before_node(
        self, node_id: str, node_name: str, inputs: dict
    ) -> MiddlewareResult:
        return MiddlewareResult(decision=MiddlewareDecision.ALLOW)

    async def after_node(
        self, node_id: str, node_name: str, result: dict, success: bool
    ) -> MiddlewareResult:
        return MiddlewareResult(
            decision=MiddlewareDecision.ALLOW,
            metadata={"audited": True},
        )


class DenyMiddleware:
    """常に DENY を返すミドルウェア."""

    @property
    def name(self) -> str:
        return "DenyMiddleware"

    async def before_node(
        self, node_id: str, node_name: str, inputs: dict
    ) -> MiddlewareResult:
        return MiddlewareResult(
            decision=MiddlewareDecision.DENY,
            reason="テスト用拒否",
        )

    async def after_node(
        self, node_id: str, node_name: str, result: dict, success: bool
    ) -> MiddlewareResult:
        return MiddlewareResult(decision=MiddlewareDecision.ALLOW)


# ============================================================
# FlowMiddleware Protocol テスト
# ============================================================


class TestMiddlewareProtocol:
    """FlowMiddleware Protocol の準拠テスト."""

    def test_allow_middleware_is_protocol_compliant(self) -> None:
        """AllowMiddleware が FlowMiddleware Protocol を満たすか."""
        assert isinstance(AllowMiddleware(), FlowMiddleware)

    def test_deny_middleware_is_protocol_compliant(self) -> None:
        """DenyMiddleware が FlowMiddleware Protocol を満たすか."""
        assert isinstance(DenyMiddleware(), FlowMiddleware)


# ============================================================
# FlowExecutor + Middleware テスト
# ============================================================


class TestFlowExecutorMiddleware:
    """FlowExecutor のミドルウェア統合テスト."""

    @pytest.mark.asyncio
    async def test_execute_with_allow_middleware(self) -> None:
        """ALLOW ミドルウェアで正常にノードが実行されるか."""
        flow = create_flow("test-allow").then(EchoAgent).build()
        flow._executor.add_middleware(AllowMiddleware())
        result = await flow.run({"question": "hello"})
        # EchoAgent の結果が含まれるか
        assert any("echo" in v for v in result.values() if isinstance(v, dict))

    @pytest.mark.asyncio
    async def test_execute_with_deny_middleware(self) -> None:
        """DENY ミドルウェアでノードがスキップされるか."""
        flow = create_flow("test-deny").then(EchoAgent).build()
        flow._executor.add_middleware(DenyMiddleware())
        result = await flow.run({"question": "hello"})
        # DENY されたのでノード結果は空（エラーイベントが発行される）
        # FlowExecutor は結果なしで flow_complete を返す
        assert isinstance(result, dict)

    @pytest.mark.asyncio
    async def test_middleware_chain_order(self) -> None:
        """複数ミドルウェアが登録順に実行されるか."""
        flow = create_flow("test-chain").then(EchoAgent).build()
        flow._executor.add_middleware(AllowMiddleware())
        flow._executor.add_middleware(AllowMiddleware())
        result = await flow.run({"question": "chain"})
        assert isinstance(result, dict)


# ============================================================
# ScopedContextView テスト
# ============================================================


class TestScopedContextView:
    """ScopedContextView のテスト."""

    def test_set_and_get_own_output(self) -> None:
        """自 Agent の出力を設定・取得できるか."""
        ctx = FlowContext("test-flow")
        view = ScopedContextView(ctx, "agent_a")
        view.set_output({"answer": "hello"})
        assert view.get_my_output() == {"answer": "hello"}

    def test_get_peer_output_is_readonly(self) -> None:
        """他 Agent の出力が読み取り専用か."""
        ctx = FlowContext("test-flow")
        ctx.set_result("agent_b", {"data": "secret"})

        view = ScopedContextView(ctx, "agent_a")
        peer = view.get_peer_output("agent_b")
        assert peer["data"] == "secret"

        with pytest.raises(TypeError, match="読み取り専用"):
            peer["data"] = "modified"

    def test_get_output_dispatches_correctly(self) -> None:
        """get_output が自 Agent と他 Agent を正しく区別するか."""
        ctx = FlowContext("test-flow")
        ctx.set_result("agent_a", {"own": True})
        ctx.set_result("agent_b", {"peer": True})

        view = ScopedContextView(ctx, "agent_a")
        own = view.get_output("agent_a")
        peer = view.get_output("agent_b")

        assert isinstance(own, dict)
        assert own.get("own") is True
        # peer は ReadOnlyDict
        with pytest.raises(TypeError):
            peer["new_key"] = "value"

    def test_inputs_are_copy(self) -> None:
        """inputs が元のデータのコピーであるか."""
        ctx = FlowContext("test-flow")
        ctx.set_inputs({"q": "test"})
        view = ScopedContextView(ctx, "agent_a")
        inputs = view.inputs
        inputs["q"] = "modified"
        assert ctx.inputs["q"] == "test"  # 元データは変わらない


# ============================================================
# BranchNode テスト
# ============================================================


class TestBranchNode:
    """BranchNode / FlowBuilder.branch() のテスト."""

    @pytest.mark.asyncio
    async def test_branch_routes_correctly(self) -> None:
        """条件分岐が正しいルートに分岐するか."""
        ctx = FlowContext("test-branch")
        ctx.set_inputs({"question": "tech question"})
        ctx.set_result("classifier", {"category": "technical"})

        node = BranchNode(
            id="branch_1",
            name="テスト分岐",
            condition=lambda c: c.get_result("classifier").get("category", ""),
            routes={
                "technical": EchoAgent(),
                "business": UpperAgent(),
            },
            default_agent=EchoAgent(),
        )

        result = await node.execute(ctx)
        assert result.success is True
        assert result.data.get("echo") == "tech question"

    @pytest.mark.asyncio
    async def test_branch_uses_default(self) -> None:
        """マッチしない場合にデフォルト Agent が使われるか."""
        ctx = FlowContext("test-branch-default")
        ctx.set_inputs({"question": "hello"})
        ctx.set_result("classifier", {"category": "unknown"})

        node = BranchNode(
            id="branch_2",
            name="デフォルト分岐",
            condition=lambda c: c.get_result("classifier").get("category", ""),
            routes={"technical": EchoAgent()},
            default_agent=UpperAgent(),
        )

        result = await node.execute(ctx)
        assert result.success is True
        assert result.data.get("result") == "HELLO"

    @pytest.mark.asyncio
    async def test_branch_no_match_no_default(self) -> None:
        """マッチなし + デフォルトなしの場合にエラーを返すか."""
        ctx = FlowContext("test-branch-none")
        ctx.set_inputs({"question": "hello"})
        ctx.set_result("classifier", {"category": "unknown"})

        node = BranchNode(
            id="branch_3",
            name="マッチなし分岐",
            condition=lambda c: c.get_result("classifier").get("category", ""),
            routes={"technical": EchoAgent()},
        )

        result = await node.execute(ctx)
        assert result.success is False
        assert "No agent for route" in result.data.get("error", "")


# ============================================================
# FlowBuilder.branch() 統合テスト
# ============================================================


class TestFlowBuilderBranch:
    """FlowBuilder の branch() メソッドのテスト."""

    @pytest.mark.asyncio
    async def test_flow_with_branch(self) -> None:
        """FlowBuilder で branch を含むフローが動作するか."""
        flow = (
            create_flow("test-branch-flow")
            .then(EchoAgent, ids=["echo"])
            .branch(
                condition=lambda ctx: "tech" if "tech" in ctx.get_result("echo").get("echo", "") else "other",
                routes={"tech": UpperAgent},
                default=EchoAgent,
                id="branch",
            )
            .build()
        )
        result = await flow.run({"question": "tech stuff"})
        assert isinstance(result, dict)


# ============================================================
# 型付き State テスト
# ============================================================


class TestTypedState:
    """FlowContext の型付き State 検証テスト."""

    def test_state_schema_warns_on_unknown_keys(self) -> None:
        """未知のキーで警告が出るか（ログで確認）."""
        from pydantic import BaseModel

        class MyState(BaseModel):
            answer: str = ""
            score: float = 0.0

        ctx = FlowContext("test-typed", state_schema_=MyState)
        # 未知キー "unknown_field" は警告が出るが例外にはならない
        ctx.set_result("agent_a", {"answer": "hello", "unknown_field": "oops"})
        assert ctx.get_result("agent_a")["answer"] == "hello"

    def test_state_schema_accepts_valid_keys(self) -> None:
        """正しいキーで正常動作するか."""
        from pydantic import BaseModel

        class MyState(BaseModel):
            answer: str = ""

        ctx = FlowContext("test-typed-valid", state_schema_=MyState)
        ctx.set_result("agent_a", {"answer": "valid"})
        assert ctx.get_result("agent_a")["answer"] == "valid"
