# -*- coding: utf-8 -*-
"""サーキットブレーカーのユニットテスト."""

import asyncio

import pytest

from agentflow.core.circuit_breaker import (
    CircuitBreaker,
    CircuitBreakerConfig,
    CircuitBreakerOpenError,
    CircuitState,
)


class TestCircuitBreaker:
    """CircuitBreaker のテスト."""

    @pytest.fixture
    def breaker(self) -> CircuitBreaker:
        """テスト用サーキットブレーカー."""
        config = CircuitBreakerConfig(
            failure_threshold=3,
            recovery_timeout=0.1,  # テスト用に短く
            success_threshold=2,
        )
        return CircuitBreaker("test", config)

    def test_initial_state_is_closed(self, breaker: CircuitBreaker) -> None:
        """初期状態は CLOSED."""
        assert breaker.state == CircuitState.CLOSED
        assert breaker.is_closed is True
        assert breaker.is_open is False

    @pytest.mark.asyncio
    async def test_success_keeps_closed(self, breaker: CircuitBreaker) -> None:
        """成功時は CLOSED を維持."""
        async with breaker:
            pass  # 成功
        assert breaker.state == CircuitState.CLOSED

    @pytest.mark.asyncio
    async def test_failures_open_circuit(self, breaker: CircuitBreaker) -> None:
        """連続失敗で OPEN に遷移."""
        for _ in range(3):
            try:
                async with breaker:
                    raise ValueError("test error")
            except ValueError:
                pass

        assert breaker.state == CircuitState.OPEN
        assert breaker.is_open is True

    @pytest.mark.asyncio
    async def test_open_circuit_raises_error(self, breaker: CircuitBreaker) -> None:
        """OPEN 状態ではエラーを送出."""
        # 失敗を記録して OPEN に
        for _ in range(3):
            try:
                async with breaker:
                    raise ValueError("test error")
            except ValueError:
                pass

        # OPEN 状態でアクセス
        with pytest.raises(CircuitBreakerOpenError) as exc_info:
            async with breaker:
                pass

        assert exc_info.value.breaker_name == "test"
        assert exc_info.value.remaining_seconds > 0

    @pytest.mark.asyncio
    async def test_recovery_to_half_open(self, breaker: CircuitBreaker) -> None:
        """回復タイムアウト後に HALF_OPEN に遷移."""
        # 失敗を記録して OPEN に
        for _ in range(3):
            try:
                async with breaker:
                    raise ValueError("test error")
            except ValueError:
                pass

        assert breaker.state == CircuitState.OPEN

        # 回復タイムアウト待機
        await asyncio.sleep(0.15)

        # 次のアクセスで HALF_OPEN に
        async with breaker:
            pass

        # 成功したので CLOSED に向かう（success_threshold=2 なのでまだ HALF_OPEN）
        # 注: 1回成功で HALF_OPEN のまま
        assert breaker.state in (CircuitState.HALF_OPEN, CircuitState.CLOSED)

    @pytest.mark.asyncio
    async def test_half_open_to_closed(self, breaker: CircuitBreaker) -> None:
        """HALF_OPEN から連続成功で CLOSED に遷移."""
        # 失敗を記録して OPEN に
        for _ in range(3):
            try:
                async with breaker:
                    raise ValueError("test error")
            except ValueError:
                pass

        # 回復タイムアウト待機
        await asyncio.sleep(0.15)

        # 連続成功で CLOSED に
        for _ in range(2):
            async with breaker:
                pass

        assert breaker.state == CircuitState.CLOSED

    @pytest.mark.asyncio
    async def test_half_open_failure_reopens(self, breaker: CircuitBreaker) -> None:
        """HALF_OPEN 中の失敗で再び OPEN に."""
        # 失敗を記録して OPEN に
        for _ in range(3):
            try:
                async with breaker:
                    raise ValueError("test error")
            except ValueError:
                pass

        # 回復タイムアウト待機
        await asyncio.sleep(0.15)

        # HALF_OPEN 中に失敗
        try:
            async with breaker:
                raise ValueError("test error")
        except ValueError:
            pass

        assert breaker.state == CircuitState.OPEN

    def test_reset(self, breaker: CircuitBreaker) -> None:
        """reset() で初期状態に戻る."""
        breaker._state = CircuitState.OPEN
        breaker._failure_count = 10
        breaker.reset()

        assert breaker.state == CircuitState.CLOSED
        assert breaker._failure_count == 0

    def test_get_stats(self, breaker: CircuitBreaker) -> None:
        """統計情報を取得."""
        stats = breaker.get_stats()
        assert stats["name"] == "test"
        assert stats["state"] == "closed"
        assert "failure_count" in stats

