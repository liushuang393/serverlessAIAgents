"""サーキットブレーカー.

連続失敗時にサービスを一時的に遮断し、システム全体の安定性を保護。

使用例:
    >>> from agentflow.core.circuit_breaker import CircuitBreaker
    >>>
    >>> breaker = CircuitBreaker(failure_threshold=5, recovery_timeout=30.0)
    >>>
    >>> async def call_external_service():
    ...     async with breaker:
    ...         return await external_api.call()
    >>>
    >>> # または関数デコレータとして
    >>> @breaker.protect
    ... async def protected_call():
    ...     return await external_api.call()
"""

from __future__ import annotations

import asyncio
import logging
import time
from collections.abc import Callable
from dataclasses import dataclass, field
from enum import Enum
from functools import wraps
from typing import Any, TypeVar


logger = logging.getLogger(__name__)

F = TypeVar("F", bound=Callable[..., Any])


class CircuitState(Enum):
    """サーキットブレーカーの状態."""

    CLOSED = "closed"  # 正常動作
    OPEN = "open"  # 遮断中
    HALF_OPEN = "half_open"  # 回復テスト中


@dataclass
class CircuitBreakerConfig:
    """サーキットブレーカー設定.

    Attributes:
        failure_threshold: 遮断までの連続失敗回数
        recovery_timeout: 遮断から半開までの秒数
        success_threshold: 半開から閉じるまでの連続成功回数
        excluded_exceptions: 失敗カウントしない例外
    """

    failure_threshold: int = 5
    recovery_timeout: float = 30.0
    success_threshold: int = 2
    excluded_exceptions: tuple[type[Exception], ...] = field(default_factory=tuple)


class CircuitBreakerOpenError(Exception):
    """サーキットブレーカーが開いている時の例外."""

    def __init__(self, breaker_name: str, remaining_seconds: float) -> None:
        """初期化."""
        self.breaker_name = breaker_name
        self.remaining_seconds = remaining_seconds
        super().__init__(
            f"Circuit breaker '{breaker_name}' is open. "
            f"Retry after {remaining_seconds:.1f}s"
        )


class CircuitBreaker:
    """サーキットブレーカー.

    連続失敗時にサービスを一時的に遮断。

    Attributes:
        name: ブレーカー名
        config: 設定
        state: 現在の状態
    """

    def __init__(
        self,
        name: str = "default",
        config: CircuitBreakerConfig | None = None,
        *,
        failure_threshold: int | None = None,
        recovery_timeout: float | None = None,
    ) -> None:
        """初期化.

        Args:
            name: ブレーカー名
            config: 設定（個別パラメータより優先）
            failure_threshold: 遮断までの連続失敗回数
            recovery_timeout: 遮断から半開までの秒数
        """
        self._name = name
        self._config = config or CircuitBreakerConfig(
            failure_threshold=failure_threshold or 5,
            recovery_timeout=recovery_timeout or 30.0,
        )
        self._state = CircuitState.CLOSED
        self._failure_count = 0
        self._success_count = 0
        self._last_failure_time: float | None = None
        self._lock = asyncio.Lock()

    @property
    def name(self) -> str:
        """ブレーカー名."""
        return self._name

    @property
    def state(self) -> CircuitState:
        """現在の状態."""
        return self._state

    @property
    def is_closed(self) -> bool:
        """閉じているか（正常動作）."""
        return self._state == CircuitState.CLOSED

    @property
    def is_open(self) -> bool:
        """開いているか（遮断中）."""
        return self._state == CircuitState.OPEN

    async def __aenter__(self) -> CircuitBreaker:
        """async with サポート."""
        await self._check_state()
        return self

    async def __aexit__(
        self,
        exc_type: type[BaseException] | None,
        exc_val: BaseException | None,
        exc_tb: Any,
    ) -> None:
        """async with サポート."""
        if exc_type is None:
            await self._record_success()
        elif exc_type and not issubclass(exc_type, self._config.excluded_exceptions):
            await self._record_failure()

    async def _check_state(self) -> None:
        """状態をチェックし、必要なら遷移."""
        async with self._lock:
            if self._state == CircuitState.OPEN:
                # 回復タイムアウト経過をチェック
                if self._last_failure_time is not None:
                    elapsed = time.time() - self._last_failure_time
                    if elapsed >= self._config.recovery_timeout:
                        self._state = CircuitState.HALF_OPEN
                        self._success_count = 0
                        logger.info(f"Circuit '{self._name}' → HALF_OPEN")
                    else:
                        remaining = self._config.recovery_timeout - elapsed
                        raise CircuitBreakerOpenError(self._name, remaining)
                else:
                    raise CircuitBreakerOpenError(self._name, self._config.recovery_timeout)

    async def _record_success(self) -> None:
        """成功を記録."""
        async with self._lock:
            if self._state == CircuitState.HALF_OPEN:
                self._success_count += 1
                if self._success_count >= self._config.success_threshold:
                    self._state = CircuitState.CLOSED
                    self._failure_count = 0
                    logger.info(f"Circuit '{self._name}' → CLOSED (recovered)")
            elif self._state == CircuitState.CLOSED:
                self._failure_count = 0  # 成功でリセット

    async def _record_failure(self) -> None:
        """失敗を記録."""
        async with self._lock:
            self._last_failure_time = time.time()

            if self._state == CircuitState.HALF_OPEN:
                # 半開中の失敗は即座に開く
                self._state = CircuitState.OPEN
                logger.warning(f"Circuit '{self._name}' → OPEN (half-open failed)")
            elif self._state == CircuitState.CLOSED:
                self._failure_count += 1
                if self._failure_count >= self._config.failure_threshold:
                    self._state = CircuitState.OPEN
                    logger.warning(
                        f"Circuit '{self._name}' → OPEN "
                        f"(failures: {self._failure_count})"
                    )

    def protect(self, func: F) -> F:
        """関数をサーキットブレーカーで保護するデコレータ.

        Args:
            func: 保護する非同期関数

        Returns:
            ラップされた関数
        """
        @wraps(func)
        async def wrapper(*args: Any, **kwargs: Any) -> Any:
            async with self:
                return await func(*args, **kwargs)

        return wrapper  # type: ignore[return-value]

    def reset(self) -> None:
        """状態をリセット（テスト用）."""
        self._state = CircuitState.CLOSED
        self._failure_count = 0
        self._success_count = 0
        self._last_failure_time = None

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "name": self._name,
            "state": self._state.value,
            "failure_count": self._failure_count,
            "success_count": self._success_count,
            "last_failure_time": self._last_failure_time,
        }

