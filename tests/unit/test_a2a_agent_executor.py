"""A2A AgentExecutor のユニットテスト."""

from typing import Any

import pytest
from pydantic import BaseModel

from agentflow.protocols.a2a.agent_executor import (
    A2ARequestContext,
    ResilientAgentExecutor,
)
from agentflow.protocols.a2a.task_manager import A2ATaskManager
from agentflow.protocols.a2a.types import (
    A2ATaskState,
    DataPart,
    Message,
    Role,
    TextPart,
)


# ============================================================
# A2ARequestContext テスト
# ============================================================


@pytest.fixture
def task_manager() -> A2ATaskManager:
    """テスト用タスクマネージャー."""
    return A2ATaskManager()


@pytest.fixture
async def context(task_manager: A2ATaskManager) -> A2ARequestContext:
    """テスト用コンテキスト."""
    task = await task_manager.create_task()
    msg = Message.from_text("テスト入力")
    return A2ARequestContext(
        task_id=task.id,
        context_id=task.context_id,
        message=msg,
        task_manager=task_manager,
    )


class TestA2ARequestContext:
    """A2ARequestContext のテスト."""

    async def test_初期状態(self, context: A2ARequestContext) -> None:
        """初期状態の確認."""
        assert context.task_id
        assert context.context_id
        assert not context.is_cancelled

    async def test_emit_status(self, context: A2ARequestContext, task_manager: A2ATaskManager) -> None:
        """状態更新."""
        await context.emit_status(A2ATaskState.WORKING, "処理中")
        task = await task_manager.get_task(context.task_id)
        assert task is not None
        assert task.status.state == A2ATaskState.WORKING

    async def test_emit_text(self, context: A2ARequestContext, task_manager: A2ATaskManager) -> None:
        """テキストアーティファクト発行."""
        await context.emit_text("テキスト結果")
        task = await task_manager.get_task(context.task_id)
        assert task is not None
        assert len(task.artifacts) == 1

    async def test_emit_data(self, context: A2ARequestContext, task_manager: A2ATaskManager) -> None:
        """データアーティファクト発行."""
        await context.emit_data({"key": "value"})
        task = await task_manager.get_task(context.task_id)
        assert task is not None
        assert len(task.artifacts) == 1

    async def test_キャンセル要求(self, context: A2ARequestContext) -> None:
        """キャンセルフラグの設定."""
        assert not context.is_cancelled
        context.request_cancel()
        assert context.is_cancelled


# ============================================================
# ResilientAgentExecutor テスト
# ============================================================


class MockInput(BaseModel):
    """テスト用入力モデル."""

    text: str = ""


class MockOutput(BaseModel):
    """テスト用出力モデル."""

    result: str = ""


class MockResilientAgent:
    """テスト用モック Agent."""

    name: str = "MockAgent"

    def __init__(self, *, should_fail: bool = False) -> None:
        self.should_fail = should_fail
        self.last_context: Any = None

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """通常実行."""
        if self.should_fail:
            msg = "テストエラー"
            raise RuntimeError(msg)
        return {"result": f"processed: {input_data.get('text', '')}"}

    async def run_with_context(self, input_data: dict[str, Any], context: Any) -> dict[str, Any]:
        """コンテキスト付き実行."""
        self.last_context = context
        return await self.run(input_data)


class TestResilientAgentExecutor:
    """ResilientAgentExecutor のテスト."""

    async def test_正常実行(self, task_manager: A2ATaskManager) -> None:
        """正常な実行フロー: working → artifact → completed."""
        agent = MockResilientAgent()
        executor = ResilientAgentExecutor(agent)  # type: ignore[arg-type]

        task = await task_manager.create_task()
        msg = Message.from_text("テスト入力")
        ctx = A2ARequestContext(
            task_id=task.id,
            context_id=task.context_id,
            message=msg,
            task_manager=task_manager,
        )

        await executor.execute(ctx)

        # 最終状態が completed
        result_task = await task_manager.get_task(task.id)
        assert result_task is not None
        assert result_task.status.state == A2ATaskState.COMPLETED
        assert len(result_task.artifacts) == 1

    async def test_コンテキストが注入される(self, task_manager: A2ATaskManager) -> None:
        """run_with_context にコンテキストが渡される."""
        agent = MockResilientAgent()
        executor = ResilientAgentExecutor(agent)  # type: ignore[arg-type]

        task = await task_manager.create_task()
        msg = Message.from_text("テスト")
        ctx = A2ARequestContext(
            task_id=task.id,
            context_id=task.context_id,
            message=msg,
            task_manager=task_manager,
        )

        await executor.execute(ctx)
        assert agent.last_context is ctx

    async def test_失敗時にfailed状態(self, task_manager: A2ATaskManager) -> None:
        """Agent 実行失敗時は failed 状態になる."""
        agent = MockResilientAgent(should_fail=True)
        executor = ResilientAgentExecutor(agent)  # type: ignore[arg-type]

        task = await task_manager.create_task()
        msg = Message.from_text("テスト")
        ctx = A2ARequestContext(
            task_id=task.id,
            context_id=task.context_id,
            message=msg,
            task_manager=task_manager,
        )

        with pytest.raises(RuntimeError, match="テストエラー"):
            await executor.execute(ctx)

        result_task = await task_manager.get_task(task.id)
        assert result_task is not None
        assert result_task.status.state == A2ATaskState.FAILED

    async def test_キャンセル(self, task_manager: A2ATaskManager) -> None:
        """cancel でキャンセルフラグが設定される."""
        agent = MockResilientAgent()
        executor = ResilientAgentExecutor(agent)  # type: ignore[arg-type]

        task = await task_manager.create_task()
        msg = Message.from_text("テスト")
        ctx = A2ARequestContext(
            task_id=task.id,
            context_id=task.context_id,
            message=msg,
            task_manager=task_manager,
        )

        assert not ctx.is_cancelled
        await executor.cancel(ctx)
        assert ctx.is_cancelled


class TestMessageToDict:
    """Message → dict 変換のテスト."""

    def test_テキストメッセージ変換(self) -> None:
        """TextPart がテキストキーに変換される."""
        msg = Message(role=Role.USER, parts=[TextPart(text="質問")])
        result = ResilientAgentExecutor._message_to_dict(msg)
        assert result["text"] == "質問"

    def test_データメッセージ変換(self) -> None:
        """DataPart がマージされる."""
        msg = Message(role=Role.USER, parts=[DataPart(data={"key": "val"})])
        result = ResilientAgentExecutor._message_to_dict(msg)
        assert result["key"] == "val"

    def test_複合メッセージ変換(self) -> None:
        """TextPart + DataPart の混合."""
        msg = Message(
            role=Role.USER,
            parts=[TextPart(text="入力"), DataPart(data={"mode": "test"})],
        )
        result = ResilientAgentExecutor._message_to_dict(msg)
        assert result["text"] == "入力"
        assert result["mode"] == "test"

    def test_メタデータ付き(self) -> None:
        """メタデータが _metadata キーに格納される."""
        msg = Message(
            role=Role.USER,
            parts=[TextPart(text="テスト")],
            metadata={"source": "api"},
        )
        result = ResilientAgentExecutor._message_to_dict(msg)
        assert result["_metadata"]["source"] == "api"
