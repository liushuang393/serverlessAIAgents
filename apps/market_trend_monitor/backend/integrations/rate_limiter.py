"""APIレート制限.

Phase 13: トークンバケットアルゴリズムによるレート制限。
"""

from __future__ import annotations

import asyncio
import logging
import time
from dataclasses import dataclass


@dataclass
class RateLimitConfig:
    """レート制限設定."""

    max_tokens: float  # バケット容量
    refill_rate: float  # トークン/秒のリフィルレート
    name: str = ""


class TokenBucketRateLimiter:
    """トークンバケットレート制限.

    各API向けの設定:
    - NewsAPI: 100 req/day ≈ 1 req/864s
    - GitHub: 10 req/min
    - arXiv: 1 req/3s
    - StackOverflow: 300 req/day ≈ 1 req/288s (匿名)
    - DEV.to: 30 req/30s = 1 req/s
    """

    PRESETS: dict[str, RateLimitConfig] = {
        "news": RateLimitConfig(max_tokens=5, refill_rate=1 / 864, name="NewsAPI"),
        "github": RateLimitConfig(max_tokens=10, refill_rate=10 / 60, name="GitHub"),
        "arxiv": RateLimitConfig(max_tokens=3, refill_rate=1 / 3, name="arXiv"),
        "stackoverflow": RateLimitConfig(
            max_tokens=5,
            refill_rate=1 / 288,
            name="StackOverflow",
        ),
        "devto": RateLimitConfig(max_tokens=10, refill_rate=1.0, name="DEV.to"),
    }

    def __init__(self) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)
        self._buckets: dict[str, _Bucket] = {}

    def _get_bucket(self, source: str) -> _Bucket:
        if source not in self._buckets:
            config = self.PRESETS.get(
                source,
                RateLimitConfig(
                    max_tokens=10,
                    refill_rate=1.0,
                    name=source,
                ),
            )
            self._buckets[source] = _Bucket(
                tokens=config.max_tokens,
                max_tokens=config.max_tokens,
                refill_rate=config.refill_rate,
                last_refill=time.monotonic(),
            )
        return self._buckets[source]

    async def acquire(self, source: str) -> None:
        """トークンを取得。利用可能になるまで待機。"""
        bucket = self._get_bucket(source)
        while True:
            bucket.refill()
            if bucket.tokens >= 1.0:
                bucket.tokens -= 1.0
                return
            # 1トークンがリフィルされるまでの待機時間
            wait_time = (1.0 - bucket.tokens) / bucket.refill_rate
            self._logger.debug(
                "レート制限: %s - %.1f秒待機",
                source,
                wait_time,
            )
            await asyncio.sleep(min(wait_time, 5.0))

    def can_acquire(self, source: str) -> bool:
        """トークンが利用可能かチェック（待機なし）。"""
        bucket = self._get_bucket(source)
        bucket.refill()
        return bucket.tokens >= 1.0


@dataclass
class _Bucket:
    tokens: float
    max_tokens: float
    refill_rate: float
    last_refill: float

    def refill(self) -> None:
        now = time.monotonic()
        elapsed = now - self.last_refill
        self.tokens = min(self.max_tokens, self.tokens + elapsed * self.refill_rate)
        self.last_refill = now


# シングルトンインスタンス
rate_limiter = TokenBucketRateLimiter()
