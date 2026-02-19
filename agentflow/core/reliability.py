"""信頼性強化ユーティリティ.

リトライ、サーキットブレーカー、メトリクスを統合したデコレータ。

使用例:
    >>> from agentflow.core.reliability import reliable
    >>>
    >>> @reliable(max_retries=3, circuit_breaker="api")
    ... async def call_api():
    ...     return await external_api.call()
    >>>
    >>> # または詳細設定
    >>> @reliable(
    ...     max_retries=5,
    ...     retry_delay=1.0,
    ...     circuit_breaker="api",
    ...     failure_threshold=10,
    ...     record_metrics=True,
    ... )
    ... async def call_api():
    ...     return await external_api.call()
"""

from __future__ import annotations

import asyncio
import logging
import time
from collections.abc import Callable
from functools import wraps
from typing import Any, TypeVar

from agentflow.core.circuit_breaker import (
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitBreakerOpenError,
)
from agentflow.observability.metrics import get_metrics


logger = logging.getLogger(__name__)

F = TypeVar("F", bound=Callable[..., Any])

# グローバルサーキットブレーカーレジストリ
_circuit_breakers: dict[str, CircuitBreaker] = {}


def get_circuit_breaker(
    name: str,
    config: CircuitBreakerConfig | None = None,
) -> CircuitBreaker:
    """サーキットブレーカーを取得または作成.

    Args:
        name: ブレーカー名
        config: 設定

    Returns:
        CircuitBreaker インスタンス
    """
    if name not in _circuit_breakers:
        _circuit_breakers[name] = CircuitBreaker(name, config)
    return _circuit_breakers[name]


def reliable(
    *,
    max_retries: int = 3,
    retry_delay: float = 1.0,
    retry_backoff: str = "exponential",
    circuit_breaker: str | None = None,
    failure_threshold: int = 5,
    recovery_timeout: float = 30.0,
    record_metrics: bool = True,
    metric_name: str | None = None,
) -> Callable[[F], F]:
    """信頼性強化デコレータ.

    リトライ、サーキットブレーカー、メトリクスを統合。

    Args:
        max_retries: 最大リトライ回数
        retry_delay: 初期リトライ遅延秒
        retry_backoff: 退避方式 "fixed" | "exponential"
        circuit_breaker: サーキットブレーカー名（None で無効）
        failure_threshold: 遮断までの連続失敗回数
        recovery_timeout: 遮断から半開までの秒数
        record_metrics: メトリクス記録有無
        metric_name: メトリクス名（None で関数名使用）

    Returns:
        デコレータ
    """

    def decorator(func: F) -> F:
        func_name = metric_name or func.__name__

        # サーキットブレーカー取得
        breaker: CircuitBreaker | None = None
        if circuit_breaker:
            config = CircuitBreakerConfig(
                failure_threshold=failure_threshold,
                recovery_timeout=recovery_timeout,
            )
            breaker = get_circuit_breaker(circuit_breaker, config)

        # メトリクス取得
        metrics = get_metrics() if record_metrics else None
        if metrics:
            call_counter = metrics.counter(
                f"{func_name}_calls_total",
                f"Total calls to {func_name}",
                ["status"],
            )
            duration_histogram = metrics.histogram(
                f"{func_name}_duration_seconds",
                f"Duration of {func_name} calls",
            )
            retry_counter = metrics.counter(
                f"{func_name}_retries_total",
                f"Total retries for {func_name}",
            )

        @wraps(func)
        async def wrapper(*args: Any, **kwargs: Any) -> Any:
            start_time = time.time()
            last_error: Exception | None = None

            for attempt in range(max_retries + 1):
                try:
                    # サーキットブレーカーチェック
                    if breaker:
                        await breaker._check_state()

                    # 関数実行
                    result = await func(*args, **kwargs)

                    # 成功記録
                    if breaker:
                        await breaker._record_success()
                    if metrics:
                        call_counter.inc(labels={"status": "success"})
                        duration_histogram.observe(time.time() - start_time)

                    return result

                except CircuitBreakerOpenError:
                    # サーキットブレーカー開放中は即座に失敗
                    if metrics:
                        call_counter.inc(labels={"status": "circuit_open"})
                    raise

                except Exception as e:
                    last_error = e
                    logger.warning(
                        f"{func_name} failed (attempt {attempt + 1}/{max_retries + 1}): {e}"
                    )

                    # 失敗記録
                    if breaker:
                        await breaker._record_failure()
                    if metrics and attempt < max_retries:
                        retry_counter.inc()

                    # 最後の試行なら例外を再送出
                    if attempt >= max_retries:
                        if metrics:
                            call_counter.inc(labels={"status": "failure"})
                            duration_histogram.observe(time.time() - start_time)
                        raise

                    # リトライ遅延
                    delay = retry_delay * (2**attempt if retry_backoff == "exponential" else 1)
                    await asyncio.sleep(delay)

            # ここには到達しないはず
            if last_error:
                raise last_error
            msg = "Unexpected error in reliable decorator"
            raise RuntimeError(msg)

        return wrapper  # type: ignore[return-value]

    return decorator
