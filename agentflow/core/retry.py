"""リトライ機構.

このモジュールは、エージェント実行時のリトライ機構を提供します。

使用例:
    ```python
    from agentflow.core.retry import RetryableAgent, RetryConfig

    class MyAgent(RetryableAgent):
        async def _run_impl(self, input_data: dict[str, Any]) -> dict[str, Any]:
            # 実際の処理
            return {"result": "success"}

    config = RetryConfig(max_attempts=3, wait_multiplier=2)
    agent = MyAgent(retry_config=config)
    result = await agent.run({"input": "data"})
    ```
"""

import asyncio
import logging
from typing import Any

from pydantic import BaseModel, Field

from agentflow.core.agent_block import AgentBlock


class RetryConfig(BaseModel):
    """リトライ設定.

    Args:
        max_attempts: 最大試行回数
        wait_multiplier: 待機時間の倍率
        wait_min: 最小待機時間（秒）
        wait_max: 最大待機時間（秒）
        retry_on_exceptions: リトライ対象の例外クラスリスト
    """

    max_attempts: int = Field(default=3, ge=1, description="最大試行回数")
    wait_multiplier: float = Field(default=1.0, ge=0.0, description="待機時間の倍率")
    wait_min: float = Field(default=1.0, ge=0.0, description="最小待機時間（秒）")
    wait_max: float = Field(default=60.0, ge=0.0, description="最大待機時間（秒）")
    retry_on_exceptions: list[type[Exception]] = Field(
        default_factory=lambda: [Exception], description="リトライ対象の例外クラスリスト"
    )


class RetryableAgent(AgentBlock):
    """リトライ機能付きエージェント.

    このクラスは、エージェント実行時のリトライ機構を提供します。
    サブクラスは`_run_impl`メソッドを実装する必要があります。

    使用例:
        ```python
        class MyAgent(RetryableAgent):
            async def _run_impl(self, input_data: dict[str, Any]) -> dict[str, Any]:
                # 実際の処理
                return {"result": "success"}

        config = RetryConfig(max_attempts=3)
        agent = MyAgent(retry_config=config)
        result = await agent.run({"input": "data"})
        ```
    """

    def __init__(self, retry_config: RetryConfig | None = None) -> None:
        """初期化.

        Args:
            retry_config: リトライ設定（Noneの場合はデフォルト設定）
        """
        super().__init__()
        self._retry_config = retry_config or RetryConfig()
        self._logger = logging.getLogger(__name__)

    async def run(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """リトライ付き実行.

        Args:
            input_data: 入力データ

        Returns:
            出力データ

        Raises:
            Exception: 最大試行回数を超えた場合
        """
        last_exception: Exception | None = None

        for attempt in range(1, self._retry_config.max_attempts + 1):
            try:
                self._logger.info(f"Attempt {attempt}/{self._retry_config.max_attempts}")
                result = await self._run_impl(input_data)
                self._logger.info(f"Attempt {attempt} succeeded")
                return result

            except Exception as e:
                last_exception = e
                self._logger.warning(f"Attempt {attempt} failed: {type(e).__name__}: {e}")

                # リトライ対象の例外かチェック
                if not any(isinstance(e, exc_type) for exc_type in self._retry_config.retry_on_exceptions):
                    self._logger.exception(f"Non-retryable exception: {type(e).__name__}")
                    raise

                # 最後の試行の場合は例外を再送出
                if attempt >= self._retry_config.max_attempts:
                    self._logger.exception(f"Max attempts ({self._retry_config.max_attempts}) reached")
                    raise

                # 待機時間を計算
                wait_time = min(
                    self._retry_config.wait_min * (self._retry_config.wait_multiplier ** (attempt - 1)),
                    self._retry_config.wait_max,
                )
                self._logger.info(f"Waiting {wait_time:.2f}s before retry...")
                await asyncio.sleep(wait_time)

        # ここには到達しないはずだが、念のため
        if last_exception:
            raise last_exception
        msg = "Unexpected error in retry logic"
        raise RuntimeError(msg)

    async def _run_impl(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """実際の処理実装.

        サブクラスでこのメソッドを実装してください。

        Args:
            input_data: 入力データ

        Returns:
            出力データ

        Raises:
            NotImplementedError: サブクラスで実装されていない場合
        """
        msg = "Subclass must implement _run_impl method"
        raise NotImplementedError(msg)
