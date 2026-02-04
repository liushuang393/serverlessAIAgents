# -*- coding: utf-8 -*-
"""Quota Manager - 配额管理.

API调用配额和速率限制管理。

使用例:
    >>> quota = QuotaManager()
    >>> quota.set_limit("tenant-123", QuotaLimit(requests_per_minute=60))
    >>> if await quota.check_and_consume("tenant-123", "requests"):
    ...     result = await make_request()
"""

from __future__ import annotations

import asyncio
import logging
import time
from dataclasses import dataclass, field
from typing import Any
from enum import Enum

_logger = logging.getLogger(__name__)


class QuotaType(str, Enum):
    """配额类型."""

    REQUESTS = "requests"  # 请求数
    TOKENS = "tokens"  # Token数
    STORAGE = "storage"  # 存储空间
    BANDWIDTH = "bandwidth"  # 带宽


class QuotaPeriod(str, Enum):
    """配额周期."""

    MINUTE = "minute"
    HOUR = "hour"
    DAY = "day"
    MONTH = "month"


@dataclass
class QuotaLimit:
    """配额限制.

    Attributes:
        requests_per_minute: 每分钟请求数
        requests_per_hour: 每小时请求数
        requests_per_day: 每日请求数
        tokens_per_day: 每日Token数
        tokens_per_month: 每月Token数
        storage_mb: 存储空间（MB）
        bandwidth_mb_per_day: 每日带宽（MB）
    """

    requests_per_minute: int = 60
    requests_per_hour: int = 1000
    requests_per_day: int = 10000
    tokens_per_day: int = 100000
    tokens_per_month: int = 3000000
    storage_mb: int = 1000
    bandwidth_mb_per_day: int = 1000

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "requests_per_minute": self.requests_per_minute,
            "requests_per_hour": self.requests_per_hour,
            "requests_per_day": self.requests_per_day,
            "tokens_per_day": self.tokens_per_day,
            "tokens_per_month": self.tokens_per_month,
            "storage_mb": self.storage_mb,
            "bandwidth_mb_per_day": self.bandwidth_mb_per_day,
        }


@dataclass
class QuotaUsage:
    """配额使用情况.

    Attributes:
        quota_type: 配额类型
        period: 周期
        used: 已使用量
        limit: 限制
        reset_at: 重置时间
    """

    quota_type: QuotaType
    period: QuotaPeriod
    used: int = 0
    limit: int = 0
    reset_at: float = 0.0

    @property
    def remaining(self) -> int:
        """剩余配额."""
        return max(0, self.limit - self.used)

    @property
    def is_exceeded(self) -> bool:
        """是否超出配额."""
        return self.used >= self.limit

    @property
    def usage_percent(self) -> float:
        """使用百分比."""
        if self.limit == 0:
            return 0.0
        return min(100.0, self.used / self.limit * 100)

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "quota_type": self.quota_type.value,
            "period": self.period.value,
            "used": self.used,
            "limit": self.limit,
            "remaining": self.remaining,
            "is_exceeded": self.is_exceeded,
            "usage_percent": self.usage_percent,
            "reset_at": self.reset_at,
        }


class QuotaExceededError(Exception):
    """配额超出错误."""

    def __init__(
        self,
        quota_type: QuotaType,
        period: QuotaPeriod,
        usage: QuotaUsage,
    ) -> None:
        self.quota_type = quota_type
        self.period = period
        self.usage = usage
        super().__init__(
            f"Quota exceeded: {quota_type.value} per {period.value} "
            f"(used: {usage.used}, limit: {usage.limit})"
        )


@dataclass
class TenantQuota:
    """租户配额.

    Attributes:
        tenant_id: 租户ID
        limits: 配额限制
        usage: 使用情况
    """

    tenant_id: str
    limits: QuotaLimit = field(default_factory=QuotaLimit)
    usage: dict[str, QuotaUsage] = field(default_factory=dict)

    def _get_key(self, quota_type: QuotaType, period: QuotaPeriod) -> str:
        """获取使用情况键."""
        return f"{quota_type.value}:{period.value}"

    def _get_reset_time(self, period: QuotaPeriod) -> float:
        """获取重置时间."""
        now = time.time()
        if period == QuotaPeriod.MINUTE:
            return now + 60
        elif period == QuotaPeriod.HOUR:
            return now + 3600
        elif period == QuotaPeriod.DAY:
            return now + 86400
        elif period == QuotaPeriod.MONTH:
            return now + 2592000  # 30天
        return now + 60

    def _get_limit(self, quota_type: QuotaType, period: QuotaPeriod) -> int:
        """获取配额限制."""
        if quota_type == QuotaType.REQUESTS:
            if period == QuotaPeriod.MINUTE:
                return self.limits.requests_per_minute
            elif period == QuotaPeriod.HOUR:
                return self.limits.requests_per_hour
            elif period == QuotaPeriod.DAY:
                return self.limits.requests_per_day
        elif quota_type == QuotaType.TOKENS:
            if period == QuotaPeriod.DAY:
                return self.limits.tokens_per_day
            elif period == QuotaPeriod.MONTH:
                return self.limits.tokens_per_month
        elif quota_type == QuotaType.STORAGE:
            return self.limits.storage_mb
        elif quota_type == QuotaType.BANDWIDTH:
            if period == QuotaPeriod.DAY:
                return self.limits.bandwidth_mb_per_day
        return 0

    def get_usage(
        self,
        quota_type: QuotaType,
        period: QuotaPeriod,
    ) -> QuotaUsage:
        """获取使用情况."""
        key = self._get_key(quota_type, period)
        now = time.time()

        if key not in self.usage:
            self.usage[key] = QuotaUsage(
                quota_type=quota_type,
                period=period,
                limit=self._get_limit(quota_type, period),
                reset_at=self._get_reset_time(period),
            )

        usage = self.usage[key]

        # 检查是否需要重置
        if now >= usage.reset_at:
            usage.used = 0
            usage.reset_at = self._get_reset_time(period)

        return usage

    def consume(
        self,
        quota_type: QuotaType,
        period: QuotaPeriod,
        amount: int = 1,
    ) -> QuotaUsage:
        """消费配额.

        Args:
            quota_type: 配额类型
            period: 周期
            amount: 消费量

        Returns:
            更新后的使用情况

        Raises:
            QuotaExceededError: 配额超出
        """
        usage = self.get_usage(quota_type, period)

        if usage.used + amount > usage.limit:
            raise QuotaExceededError(quota_type, period, usage)

        usage.used += amount
        return usage


class QuotaManager:
    """配额管理器.

    管理多租户的API调用配额和速率限制。
    """

    def __init__(self) -> None:
        """初始化."""
        self._quotas: dict[str, TenantQuota] = {}
        self._default_limits = QuotaLimit()
        self._lock = asyncio.Lock()

    def set_default_limits(self, limits: QuotaLimit) -> None:
        """设置默认配额限制."""
        self._default_limits = limits

    def set_limits(self, tenant_id: str, limits: QuotaLimit) -> None:
        """设置租户配额限制.

        Args:
            tenant_id: 租户ID
            limits: 配额限制
        """
        if tenant_id not in self._quotas:
            self._quotas[tenant_id] = TenantQuota(tenant_id=tenant_id)
        self._quotas[tenant_id].limits = limits
        _logger.debug(f"Set quota limits for tenant {tenant_id}")

    def get_quota(self, tenant_id: str) -> TenantQuota:
        """获取租户配额.

        Args:
            tenant_id: 租户ID

        Returns:
            租户配额
        """
        if tenant_id not in self._quotas:
            self._quotas[tenant_id] = TenantQuota(
                tenant_id=tenant_id,
                limits=QuotaLimit(**self._default_limits.to_dict()),
            )
        return self._quotas[tenant_id]

    async def check(
        self,
        tenant_id: str,
        quota_type: QuotaType,
        period: QuotaPeriod = QuotaPeriod.MINUTE,
        amount: int = 1,
    ) -> bool:
        """检查配额是否充足.

        Args:
            tenant_id: 租户ID
            quota_type: 配额类型
            period: 周期
            amount: 消费量

        Returns:
            是否充足
        """
        async with self._lock:
            quota = self.get_quota(tenant_id)
            usage = quota.get_usage(quota_type, period)
            return usage.used + amount <= usage.limit

    async def consume(
        self,
        tenant_id: str,
        quota_type: QuotaType,
        period: QuotaPeriod = QuotaPeriod.MINUTE,
        amount: int = 1,
    ) -> QuotaUsage:
        """消费配额.

        Args:
            tenant_id: 租户ID
            quota_type: 配额类型
            period: 周期
            amount: 消费量

        Returns:
            更新后的使用情况

        Raises:
            QuotaExceededError: 配额超出
        """
        async with self._lock:
            quota = self.get_quota(tenant_id)
            return quota.consume(quota_type, period, amount)

    async def check_and_consume(
        self,
        tenant_id: str,
        quota_type: QuotaType,
        period: QuotaPeriod = QuotaPeriod.MINUTE,
        amount: int = 1,
        raise_on_exceeded: bool = False,
    ) -> bool:
        """检查并消费配额.

        Args:
            tenant_id: 租户ID
            quota_type: 配额类型
            period: 周期
            amount: 消费量
            raise_on_exceeded: 超出时是否抛出异常

        Returns:
            是否成功

        Raises:
            QuotaExceededError: 配额超出（仅当raise_on_exceeded=True）
        """
        try:
            await self.consume(tenant_id, quota_type, period, amount)
            return True
        except QuotaExceededError:
            if raise_on_exceeded:
                raise
            return False

    async def get_all_usage(self, tenant_id: str) -> dict[str, QuotaUsage]:
        """获取所有使用情况.

        Args:
            tenant_id: 租户ID

        Returns:
            使用情况字典
        """
        async with self._lock:
            quota = self.get_quota(tenant_id)
            return dict(quota.usage)

    async def reset_usage(
        self,
        tenant_id: str,
        quota_type: QuotaType | None = None,
        period: QuotaPeriod | None = None,
    ) -> None:
        """重置使用情况.

        Args:
            tenant_id: 租户ID
            quota_type: 配额类型（可选，None表示全部）
            period: 周期（可选，None表示全部）
        """
        async with self._lock:
            quota = self.get_quota(tenant_id)

            if quota_type is None and period is None:
                quota.usage.clear()
            else:
                keys_to_remove = []
                for key in quota.usage:
                    qt, p = key.split(":")
                    if (quota_type is None or qt == quota_type.value) and \
                       (period is None or p == period.value):
                        keys_to_remove.append(key)
                for key in keys_to_remove:
                    del quota.usage[key]

            _logger.debug(f"Reset quota usage for tenant {tenant_id}")


# 全局实例
_quota_manager = QuotaManager()


def get_quota_manager() -> QuotaManager:
    """获取全局配额管理器."""
    return _quota_manager


__all__ = [
    "QuotaType",
    "QuotaPeriod",
    "QuotaLimit",
    "QuotaUsage",
    "QuotaExceededError",
    "TenantQuota",
    "QuotaManager",
    "get_quota_manager",
]
