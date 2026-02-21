"""ResilientAgent 単体テスト.

agentflow/core/resilient_agent.py のテスト。
"""

import asyncio
from typing import Any

import pytest
from pydantic import BaseModel

from agentflow.core.exceptions import (
    AgentExecutionError,
    AgentOutputValidationError,
    AgentRetryExhaustedError,
    AgentTimeoutError,
)
from agentflow.core.resilient_agent import ResilientAgent


class TestInput(BaseModel):
    """テスト用入力モデル."""

    query: str


class TestOutput(BaseModel):
    """テスト用出力モデル."""

    answer: str


class ConcreteAgent(ResilientAgent[TestInput, TestOutput]):
    """テスト用の具象 Agent."""

    name = "ConcreteAgent"
    timeout_seconds = 5
    max_retries = 2
    retry_delay = 0.1

    def _parse_input(self, input_data: dict[str, Any]) -> TestInput:
        """入力パース."""
        return TestInput(**input_data)

    async def process(self, input_data: TestInput) -> TestOutput:
        """処理実行."""
        return TestOutput(answer=f"Echo: {input_data.query}")


class TestResilientAgent:
    """ResilientAgent のテストクラス."""

    def test_agent_creation(self) -> None:
        """Agent が正しく作成されることをテスト."""
        agent = ConcreteAgent()

        assert agent.name == "ConcreteAgent"
        assert agent.timeout_seconds == 5
        assert agent.max_retries == 2
        assert agent.retry_delay == 0.1

    @pytest.mark.asyncio
    async def test_run_success(self) -> None:
        """正常実行のテスト."""
        agent = ConcreteAgent()
        result = await agent.run({"query": "hello"})

        assert result["answer"] == "Echo: hello"

    @pytest.mark.asyncio
    async def test_run_with_retry_success(self) -> None:
        """リトライ後成功のテスト."""
        agent = ConcreteAgent()
        call_count = 0

        original_process = agent.process

        async def flaky_process(input_data: TestInput) -> TestOutput:
            nonlocal call_count
            call_count += 1
            if call_count < 2:
                msg = "Temporary error"
                raise ValueError(msg)
            return await original_process(input_data)

        agent.process = flaky_process  # type: ignore

        result = await agent.run({"query": "hello"})

        assert result["answer"] == "Echo: hello"
        assert call_count == 2

    @pytest.mark.asyncio
    async def test_run_output_validation_error_repairs_and_retries(self) -> None:
        """出力検証エラー時は repair 判定で再実行される."""
        agent = ConcreteAgent()
        call_count = 0

        async def flaky_process(input_data: TestInput) -> TestOutput:
            nonlocal call_count
            call_count += 1
            if call_count == 1:
                raise AgentOutputValidationError(
                    agent_name="ConcreteAgent",
                    field_name="answer",
                    expected="non-empty",
                    actual="",
                )
            return TestOutput(answer="fixed")

        agent.process = flaky_process  # type: ignore

        result = await agent.run({"query": "hello"})
        assert result["answer"] == "fixed"
        assert call_count == 2

    @pytest.mark.asyncio
    async def test_run_retry_exhausted(self) -> None:
        """リトライ上限到達のテスト."""
        agent = ConcreteAgent()
        agent.max_retries = 2

        async def always_fail(input_data: TestInput) -> TestOutput:
            msg = "Always fail"
            raise ValueError(msg)

        agent.process = always_fail  # type: ignore

        with pytest.raises(AgentRetryExhaustedError) as exc_info:
            await agent.run({"query": "hello"})

        assert "ConcreteAgent" in str(exc_info.value)
        assert exc_info.value.max_retries == 3  # max_retries + 1

    @pytest.mark.asyncio
    async def test_run_validation_error_is_not_retried(self) -> None:
        """Pydantic 検証エラーは無駄なリトライをしない."""
        agent = ConcreteAgent()
        agent.max_retries = 3

        with pytest.raises(AgentRetryExhaustedError) as exc_info:
            await agent.run({})

        assert exc_info.value.max_retries == 1

    @pytest.mark.asyncio
    async def test_run_timeout(self) -> None:
        """タイムアウトのテスト."""
        agent = ConcreteAgent()
        agent.timeout_seconds = 1
        agent.max_retries = 0

        async def slow_process(input_data: TestInput) -> TestOutput:
            await asyncio.sleep(10)
            return TestOutput(answer="never")

        agent.process = slow_process  # type: ignore

        with pytest.raises(AgentTimeoutError) as exc_info:
            await agent.run({"query": "hello"})

        assert exc_info.value.timeout_seconds == 1

    def test_calculate_retry_delay_fixed(self) -> None:
        """固定遅延計算のテスト."""
        agent = ConcreteAgent()
        agent.retry_backoff = "fixed"
        agent.retry_delay = 1.0

        assert agent._calculate_retry_delay(0) == 1.0
        assert agent._calculate_retry_delay(1) == 1.0
        assert agent._calculate_retry_delay(2) == 1.0

    def test_calculate_retry_delay_exponential(self) -> None:
        """指数遅延計算のテスト."""
        agent = ConcreteAgent()
        agent.retry_backoff = "exponential"
        agent.retry_delay = 1.0

        assert agent._calculate_retry_delay(0) == 1.0
        assert agent._calculate_retry_delay(1) == 2.0
        assert agent._calculate_retry_delay(2) == 4.0

    def test_validate_output_default(self) -> None:
        """デフォルト出力検証のテスト."""
        agent = ConcreteAgent()
        output = TestOutput(answer="test")

        assert agent.validate_output(output) is True

    def test_get_agent_info(self) -> None:
        """Agent 情報取得のテスト."""
        agent = ConcreteAgent()
        info = agent.get_agent_info()

        assert info["name"] == "ConcreteAgent"
        assert info["timeout_seconds"] == 5
        assert info["max_retries"] == 2
        assert info["retry_delay"] == 0.1


class TestAgentExceptions:
    """Agent 例外クラスのテスト."""

    def test_agent_execution_error(self) -> None:
        """AgentExecutionError のテスト."""
        error = AgentExecutionError("TestAgent", "Test error")

        assert "TestAgent" in str(error)
        assert "Test error" in str(error)
        assert error.agent_name == "TestAgent"

    def test_agent_execution_error_with_original(self) -> None:
        """AgentExecutionError with original error のテスト."""
        original = ValueError("Original")
        error = AgentExecutionError("TestAgent", "Test error", original)

        assert error.original_error is original

    def test_agent_timeout_error(self) -> None:
        """AgentTimeoutError のテスト."""
        error = AgentTimeoutError("TestAgent", 30, 3)

        assert error.timeout_seconds == 30
        assert error.attempts == 3
        assert "30" in str(error)

    def test_agent_retry_exhausted_error(self) -> None:
        """AgentRetryExhaustedError のテスト."""
        last_error = ValueError("Last")
        error = AgentRetryExhaustedError("TestAgent", 3, last_error)

        assert error.max_retries == 3
        assert "3" in str(error)
