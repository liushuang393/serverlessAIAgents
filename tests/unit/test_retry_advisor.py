# -*- coding: utf-8 -*-
"""RetryAdvisor 単体テスト."""

from pydantic import BaseModel, ValidationError
import pytest

from agentflow.core.exceptions import AgentOutputValidationError
from agentflow.core.retry_advisor import (
    RetryAction,
    RetryAdvisor,
    RetryContext,
)


class _InputModel(BaseModel):
    """テスト用入力モデル."""

    query: str


class _HttpError(Exception):
    """status_code を持つ疑似HTTPエラー."""

    def __init__(self, status_code: int) -> None:
        self.status_code = status_code
        super().__init__(f"status={status_code}")


class TestRetryAdvisor:
    """RetryAdvisor の判定ロジックテスト."""

    def test_input_validation_error_is_skip(self) -> None:
        """入力検証エラーは即停止."""
        advisor = RetryAdvisor()
        try:
            _InputModel.model_validate({})
            pytest.fail("ValidationError expected")
        except ValidationError as e:
            advice = advisor.advise(RetryContext(
                agent_name="TestAgent",
                attempt=0,
                max_retries=3,
                stage="input_parse",
                error=e,
                has_llm=True,
            ))
        assert advice.action == RetryAction.SKIP

    def test_output_validation_error_is_repair(self) -> None:
        """業務出力検証エラーは repair."""
        advisor = RetryAdvisor()
        error = AgentOutputValidationError(
            agent_name="TestAgent",
            field_name="essence",
            expected="non-empty",
            actual="",
        )
        advice = advisor.advise(RetryContext(
            agent_name="TestAgent",
            attempt=0,
            max_retries=3,
            stage="output_validation",
            error=error,
            has_llm=True,
        ))
        assert advice.action == RetryAction.REPAIR
        assert advice.temperature_override == 0.2

    def test_timeout_is_retry(self) -> None:
        """タイムアウトは retry."""
        advisor = RetryAdvisor()
        advice = advisor.advise(RetryContext(
            agent_name="TestAgent",
            attempt=0,
            max_retries=3,
            stage="process",
            error=TimeoutError("timeout"),
            has_llm=False,
        ))
        assert advice.action == RetryAction.RETRY

    def test_non_retryable_http_4xx_is_skip(self) -> None:
        """HTTP 4xx(429除外) は skip."""
        advisor = RetryAdvisor()
        advice = advisor.advise(RetryContext(
            agent_name="TestAgent",
            attempt=0,
            max_retries=3,
            stage="process",
            error=_HttpError(400),
            has_llm=True,
        ))
        assert advice.action == RetryAction.SKIP

    def test_retryable_http_429_is_retry(self) -> None:
        """HTTP 429 は retry."""
        advisor = RetryAdvisor()
        advice = advisor.advise(RetryContext(
            agent_name="TestAgent",
            attempt=0,
            max_retries=3,
            stage="process",
            error=_HttpError(429),
            has_llm=True,
        ))
        assert advice.action == RetryAction.RETRY

