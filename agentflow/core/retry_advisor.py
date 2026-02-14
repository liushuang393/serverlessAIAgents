"""RetryAdvisor - リトライ方針判定.

エラー種別と実行コンテキストから、次のアクションを判定する。
- retry: 通常リトライ
- repair: 修復付きリトライ（低温度・追加ヒント）
- skip: リトライせず終了
"""

from dataclasses import dataclass
from enum import Enum

from pydantic import ValidationError

from agentflow.core.exceptions import AgentOutputValidationError


class RetryAction(str, Enum):
    """リトライ時の次アクション."""

    RETRY = "retry"
    REPAIR = "repair"
    SKIP = "skip"


@dataclass(slots=True, frozen=True)
class RetryContext:
    """リトライ判定コンテキスト."""

    agent_name: str
    attempt: int
    max_retries: int
    stage: str
    error: Exception
    has_llm: bool


@dataclass(slots=True, frozen=True)
class RetryAdvice:
    """RetryAdvisor の判定結果."""

    action: RetryAction
    reason: str
    delay_override: float | None = None
    temperature_override: float | None = None
    repair_prompt_hint: str | None = None


class RetryAdvisor:
    """エラー分類に基づくリトライ方針決定器."""

    _REPAIR_HINT = (
        "前回の出力はスキーマ/形式検証に失敗しました。"
        "必ずJSON形式を守り、各フィールドの型・長さ制約を厳守してください。"
    )

    def advise(self, context: RetryContext) -> RetryAdvice:
        """次アクションを判定."""
        error = context.error
        status_code = getattr(error, "status_code", None)

        if context.attempt >= context.max_retries:
            return RetryAdvice(
                action=RetryAction.SKIP,
                reason="retry budget exhausted",
            )

        if isinstance(status_code, int):
            if status_code == 429 or status_code >= 500:
                return RetryAdvice(action=RetryAction.RETRY, reason=f"http {status_code}")
            return RetryAdvice(action=RetryAction.SKIP, reason=f"non-retryable http {status_code}")

        if context.stage == "input_parse" and isinstance(error, ValidationError):
            return RetryAdvice(
                action=RetryAction.SKIP,
                reason="input schema validation failed",
            )

        if isinstance(error, AgentOutputValidationError):
            return RetryAdvice(
                action=RetryAction.REPAIR,
                reason="output business validation failed",
                delay_override=0.0,
                temperature_override=0.2 if context.has_llm else None,
                repair_prompt_hint=self._REPAIR_HINT if context.has_llm else None,
            )

        if isinstance(error, ValidationError):
            if context.has_llm:
                return RetryAdvice(
                    action=RetryAction.REPAIR,
                    reason="output schema validation failed",
                    delay_override=0.0,
                    temperature_override=0.2,
                    repair_prompt_hint=self._REPAIR_HINT,
                )
            return RetryAdvice(action=RetryAction.SKIP, reason="deterministic schema validation failed")

        if isinstance(error, (TimeoutError, ConnectionError)):
            return RetryAdvice(action=RetryAction.RETRY, reason=type(error).__name__)

        return RetryAdvice(action=RetryAction.RETRY, reason="default-retry")

