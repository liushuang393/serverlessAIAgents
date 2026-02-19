"""レート制限モジュール.

API リクエストのレート制限を提供します。

特徴:
- スライディングウィンドウ方式
- ユーザー/IP/API Key 単位の制限
- バースト許容
- 複数の制限ルール
"""

from __future__ import annotations

import logging
import threading
import time
from collections import defaultdict
from dataclasses import dataclass, field
from typing import Any


logger = logging.getLogger(__name__)


class RateLimitExceeded(Exception):
    """レート制限超過例外.

    Attributes:
        limit: 制限値
        reset_at: リセット時刻（Unix タイムスタンプ）
        retry_after: リトライまでの秒数
    """

    def __init__(
        self,
        message: str = "Rate limit exceeded",
        limit: int = 0,
        reset_at: float = 0,
        retry_after: float = 0,
    ) -> None:
        """初期化."""
        super().__init__(message)
        self.limit = limit
        self.reset_at = reset_at
        self.retry_after = retry_after


@dataclass
class RateLimitConfig:
    """レート制限設定.

    Attributes:
        requests_per_minute: 1分あたりのリクエスト数
        requests_per_hour: 1時間あたりのリクエスト数
        requests_per_day: 1日あたりのリクエスト数
        burst_size: バーストサイズ
        enable_sliding_window: スライディングウィンドウを使用
    """

    requests_per_minute: int = 60
    requests_per_hour: int = 1000
    requests_per_day: int = 10000
    burst_size: int = 10
    enable_sliding_window: bool = True


@dataclass
class RateLimitInfo:
    """レート制限情報.

    Attributes:
        remaining: 残りリクエスト数
        limit: 制限値
        reset_at: リセット時刻
        retry_after: リトライまでの秒数
    """

    remaining: int
    limit: int
    reset_at: float
    retry_after: float = 0


@dataclass
class RequestRecord:
    """リクエスト記録.

    Attributes:
        timestamps: リクエストのタイムスタンプリスト
    """

    timestamps: list[float] = field(default_factory=list)


class RateLimiter:
    """レート制限器.

    スライディングウィンドウ方式でリクエストを制限します。

    Example:
        >>> limiter = RateLimiter(requests_per_minute=60)
        >>> if await limiter.check("user-123"):
        ...     # リクエスト処理
        ...     pass
        >>> else:
        ...     # レート制限超過
        ...     pass
    """

    def __init__(
        self,
        config: RateLimitConfig | None = None,
        *,
        requests_per_minute: int | None = None,
        requests_per_hour: int | None = None,
        requests_per_day: int | None = None,
    ) -> None:
        """初期化.

        Args:
            config: レート制限設定
            requests_per_minute: 1分あたりのリクエスト数
            requests_per_hour: 1時間あたりのリクエスト数
            requests_per_day: 1日あたりのリクエスト数
        """
        self._config = config or RateLimitConfig()

        # 個別指定があれば上書き
        if requests_per_minute is not None:
            self._config.requests_per_minute = requests_per_minute
        if requests_per_hour is not None:
            self._config.requests_per_hour = requests_per_hour
        if requests_per_day is not None:
            self._config.requests_per_day = requests_per_day

        self._records: dict[str, RequestRecord] = defaultdict(RequestRecord)
        self._lock = threading.Lock()
        self._logger = logging.getLogger(__name__)

    async def check(
        self,
        key: str,
        cost: int = 1,
    ) -> bool:
        """リクエストが許可されるかをチェック.

        Args:
            key: 識別キー（ユーザー ID、IP アドレスなど）
            cost: リクエストのコスト

        Returns:
            許可される場合 True
        """
        return self._check_sync(key, cost)

    def _check_sync(self, key: str, cost: int = 1) -> bool:
        """同期版チェック."""
        now = time.time()

        with self._lock:
            record = self._records[key]

            # 古いタイムスタンプを削除
            self._cleanup_old_timestamps(record, now)

            # 各ウィンドウでチェック
            minute_count = self._count_in_window(record, now, 60)
            hour_count = self._count_in_window(record, now, 3600)
            day_count = self._count_in_window(record, now, 86400)

            # 制限チェック
            if minute_count + cost > self._config.requests_per_minute:
                return False
            if hour_count + cost > self._config.requests_per_hour:
                return False
            if day_count + cost > self._config.requests_per_day:
                return False

            # リクエストを記録
            for _ in range(cost):
                record.timestamps.append(now)

            return True

    async def check_or_raise(
        self,
        key: str,
        cost: int = 1,
    ) -> None:
        """リクエストをチェックし、制限超過時は例外を発生.

        Args:
            key: 識別キー
            cost: リクエストのコスト

        Raises:
            RateLimitExceeded: レート制限超過時
        """
        if not await self.check(key, cost):
            info = await self.get_info(key)
            raise RateLimitExceeded(
                message=f"Rate limit exceeded for {key}",
                limit=info.limit,
                reset_at=info.reset_at,
                retry_after=info.retry_after,
            )

    async def get_info(self, key: str) -> RateLimitInfo:
        """レート制限情報を取得.

        Args:
            key: 識別キー

        Returns:
            RateLimitInfo
        """
        return self._get_info_sync(key)

    def _get_info_sync(self, key: str) -> RateLimitInfo:
        """同期版情報取得."""
        now = time.time()

        with self._lock:
            record = self._records[key]
            self._cleanup_old_timestamps(record, now)

            minute_count = self._count_in_window(record, now, 60)
            remaining = max(0, self._config.requests_per_minute - minute_count)

            # リセット時刻を計算
            if record.timestamps:
                oldest_in_minute = next(
                    (t for t in record.timestamps if now - t < 60),
                    now,
                )
                reset_at = oldest_in_minute + 60
            else:
                reset_at = now + 60

            retry_after = max(0, reset_at - now) if remaining == 0 else 0

            return RateLimitInfo(
                remaining=remaining,
                limit=self._config.requests_per_minute,
                reset_at=reset_at,
                retry_after=retry_after,
            )

    def _count_in_window(
        self,
        record: RequestRecord,
        now: float,
        window_seconds: float,
    ) -> int:
        """ウィンドウ内のリクエスト数をカウント."""
        cutoff = now - window_seconds
        return sum(1 for t in record.timestamps if t > cutoff)

    def _cleanup_old_timestamps(self, record: RequestRecord, now: float) -> None:
        """古いタイムスタンプを削除."""
        # 1日より古いものを削除
        cutoff = now - 86400
        record.timestamps = [t for t in record.timestamps if t > cutoff]

    def reset(self, key: str) -> None:
        """特定キーのレート制限をリセット.

        Args:
            key: 識別キー
        """
        with self._lock:
            if key in self._records:
                self._records[key].timestamps.clear()
                self._logger.debug(f"Reset rate limit for {key}")

    def reset_all(self) -> None:
        """全てのレート制限をリセット."""
        with self._lock:
            self._records.clear()
            self._logger.info("Reset all rate limits")


class MultiRateLimiter:
    """複数のレート制限を管理.

    異なる制限ルールを複数適用できます。

    Example:
        >>> limiter = MultiRateLimiter()
        >>> limiter.add_rule("api", requests_per_minute=60)
        >>> limiter.add_rule("premium", requests_per_minute=1000)
        >>> await limiter.check("user-123", rules=["api"])
    """

    def __init__(self) -> None:
        """初期化."""
        self._limiters: dict[str, RateLimiter] = {}
        self._logger = logging.getLogger(__name__)

    def add_rule(
        self,
        name: str,
        config: RateLimitConfig | None = None,
        **kwargs: Any,
    ) -> None:
        """レート制限ルールを追加.

        Args:
            name: ルール名
            config: レート制限設定
            **kwargs: RateLimitConfig への引数
        """
        if config:
            self._limiters[name] = RateLimiter(config)
        else:
            self._limiters[name] = RateLimiter(
                requests_per_minute=kwargs.get("requests_per_minute"),
                requests_per_hour=kwargs.get("requests_per_hour"),
                requests_per_day=kwargs.get("requests_per_day"),
            )
        self._logger.info(f"Added rate limit rule: {name}")

    async def check(
        self,
        key: str,
        rules: list[str] | None = None,
        cost: int = 1,
    ) -> bool:
        """全てのルールでチェック.

        Args:
            key: 識別キー
            rules: チェックするルール名（None で全て）
            cost: リクエストのコスト

        Returns:
            全てのルールで許可される場合 True
        """
        if rules is None:
            rules = list(self._limiters.keys())

        for rule_name in rules:
            limiter = self._limiters.get(rule_name)
            if limiter and not await limiter.check(key, cost):
                return False

        return True

    async def get_info(
        self,
        key: str,
        rule: str,
    ) -> RateLimitInfo | None:
        """特定ルールのレート制限情報を取得.

        Args:
            key: 識別キー
            rule: ルール名

        Returns:
            RateLimitInfo、または None
        """
        limiter = self._limiters.get(rule)
        if limiter:
            return await limiter.get_info(key)
        return None
