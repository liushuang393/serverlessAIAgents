"""Resource Pool - 资源池管理.

Provider连接池和资源复用管理。

使用例:
    >>> pool = ResourcePool[LLMProvider](max_size=10)
    >>> async with pool.acquire() as provider:
    ...     result = await provider.chat(messages)
"""

from __future__ import annotations

import asyncio
import logging
import time
from abc import ABC, abstractmethod
from contextlib import asynccontextmanager, suppress
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any, TypeVar


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


_logger = logging.getLogger(__name__)

T = TypeVar("T")


@dataclass
class PoolStats:
    """连接池统计.

    Attributes:
        total_created: 总创建数
        total_destroyed: 总销毁数
        current_size: 当前大小
        available: 可用数
        in_use: 使用中数
        wait_count: 等待数
        total_acquires: 总获取次数
        total_releases: 总释放次数
        avg_wait_ms: 平均等待时间
    """

    total_created: int = 0
    total_destroyed: int = 0
    current_size: int = 0
    available: int = 0
    in_use: int = 0
    wait_count: int = 0
    total_acquires: int = 0
    total_releases: int = 0
    total_wait_ms: float = 0.0

    @property
    def avg_wait_ms(self) -> float:
        """平均等待时间."""
        if self.total_acquires == 0:
            return 0.0
        return self.total_wait_ms / self.total_acquires

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "total_created": self.total_created,
            "total_destroyed": self.total_destroyed,
            "current_size": self.current_size,
            "available": self.available,
            "in_use": self.in_use,
            "wait_count": self.wait_count,
            "total_acquires": self.total_acquires,
            "total_releases": self.total_releases,
            "avg_wait_ms": self.avg_wait_ms,
        }


@dataclass
class PoolConfig:
    """连接池配置.

    Attributes:
        min_size: 最小连接数
        max_size: 最大连接数
        max_idle_time: 最大空闲时间（秒）
        acquire_timeout: 获取超时（秒）
        health_check_interval: 健康检查间隔（秒）
    """

    min_size: int = 1
    max_size: int = 10
    max_idle_time: float = 300.0  # 5分钟
    acquire_timeout: float = 30.0
    health_check_interval: float = 60.0


class ResourceFactory[T](ABC):
    """资源工厂接口."""

    @abstractmethod
    async def create(self) -> T:
        """创建资源."""

    @abstractmethod
    async def destroy(self, resource: T) -> None:
        """销毁资源."""

    async def validate(self, resource: T) -> bool:
        """验证资源是否有效."""
        return True

    async def reset(self, resource: T) -> T:
        """重置资源状态."""
        return resource


@dataclass
class PooledResource[T]:
    """池化资源包装器.

    Attributes:
        resource: 资源对象
        created_at: 创建时间
        last_used_at: 最后使用时间
        use_count: 使用次数
    """

    resource: T
    created_at: float = field(default_factory=time.time)
    last_used_at: float = field(default_factory=time.time)
    use_count: int = 0

    def mark_used(self) -> None:
        """标记为已使用."""
        self.last_used_at = time.time()
        self.use_count += 1

    @property
    def idle_time(self) -> float:
        """空闲时间（秒）."""
        return time.time() - self.last_used_at


class ResourcePool[T]:
    """通用资源池.

    管理资源的创建、复用和销毁。
    """

    def __init__(
        self,
        factory: ResourceFactory[T],
        config: PoolConfig | None = None,
        name: str = "default",
    ) -> None:
        """初始化.

        Args:
            factory: 资源工厂
            config: 连接池配置
            name: 池名称
        """
        self._factory = factory
        self._config = config or PoolConfig()
        self._name = name
        self._available: asyncio.Queue[PooledResource[T]] = asyncio.Queue()
        self._in_use: set[PooledResource[T]] = set()
        self._stats = PoolStats()
        self._lock = asyncio.Lock()
        self._closed = False
        self._maintenance_task: asyncio.Task[None] | None = None

    @property
    def stats(self) -> PoolStats:
        """获取统计信息."""
        self._stats.current_size = self._available.qsize() + len(self._in_use)
        self._stats.available = self._available.qsize()
        self._stats.in_use = len(self._in_use)
        return self._stats

    async def start(self) -> None:
        """启动连接池."""
        # 创建最小连接数
        for _ in range(self._config.min_size):
            pooled = await self._create_resource()
            await self._available.put(pooled)

        # 启动维护任务
        self._maintenance_task = asyncio.create_task(self._maintenance_loop())
        _logger.info(f"Resource pool '{self._name}' started with {self._config.min_size} connections")

    async def stop(self) -> None:
        """停止连接池."""
        self._closed = True

        if self._maintenance_task:
            self._maintenance_task.cancel()
            with suppress(asyncio.CancelledError):
                await self._maintenance_task

        # 销毁所有资源
        while not self._available.empty():
            pooled = await self._available.get()
            await self._destroy_resource(pooled)

        for pooled in list(self._in_use):
            await self._destroy_resource(pooled)
            self._in_use.discard(pooled)

        _logger.info(f"Resource pool '{self._name}' stopped")

    async def _create_resource(self) -> PooledResource[T]:
        """创建新资源."""
        resource = await self._factory.create()
        self._stats.total_created += 1
        return PooledResource(resource=resource)

    async def _destroy_resource(self, pooled: PooledResource[T]) -> None:
        """销毁资源."""
        try:
            await self._factory.destroy(pooled.resource)
            self._stats.total_destroyed += 1
        except Exception as e:
            _logger.warning(f"Failed to destroy resource: {e}")

    @asynccontextmanager
    async def acquire(self) -> AsyncIterator[T]:
        """获取资源.

        Yields:
            资源对象
        """
        if self._closed:
            msg = "Pool is closed"
            raise RuntimeError(msg)

        start_time = time.time()
        self._stats.wait_count += 1

        try:
            pooled = await self._acquire_pooled()
            self._stats.wait_count -= 1
            self._stats.total_wait_ms += (time.time() - start_time) * 1000
            self._stats.total_acquires += 1

            try:
                yield pooled.resource
            finally:
                await self._release_pooled(pooled)
                self._stats.total_releases += 1

        except TimeoutError:
            self._stats.wait_count -= 1
            msg = f"Acquire timeout after {self._config.acquire_timeout}s"
            raise TimeoutError(msg) from None

    async def _acquire_pooled(self) -> PooledResource[T]:
        """获取池化资源."""
        async with self._lock:
            # 尝试从可用队列获取
            while True:
                try:
                    pooled = self._available.get_nowait()
                    # 验证资源
                    if await self._factory.validate(pooled.resource):
                        pooled.mark_used()
                        self._in_use.add(pooled)
                        return pooled
                    await self._destroy_resource(pooled)
                except asyncio.QueueEmpty:
                    break

            # 如果未达到最大连接数，创建新资源
            if len(self._in_use) < self._config.max_size:
                pooled = await self._create_resource()
                pooled.mark_used()
                self._in_use.add(pooled)
                return pooled

        # 等待可用资源
        pooled = await asyncio.wait_for(
            self._available.get(),
            timeout=self._config.acquire_timeout,
        )
        pooled.mark_used()
        async with self._lock:
            self._in_use.add(pooled)
        return pooled

    async def _release_pooled(self, pooled: PooledResource[T]) -> None:
        """释放池化资源."""
        async with self._lock:
            self._in_use.discard(pooled)

        # 重置资源状态
        try:
            pooled.resource = await self._factory.reset(pooled.resource)
            await self._available.put(pooled)
        except Exception as e:
            _logger.warning(f"Failed to reset resource, destroying: {e}")
            await self._destroy_resource(pooled)

    async def _maintenance_loop(self) -> None:
        """维护循环."""
        while not self._closed:
            try:
                await asyncio.sleep(self._config.health_check_interval)
                await self._cleanup_idle()
            except asyncio.CancelledError:
                break
            except Exception as e:
                _logger.warning(f"Maintenance error: {e}")

    async def _cleanup_idle(self) -> None:
        """清理空闲资源."""
        to_destroy: list[PooledResource[T]] = []

        async with self._lock:
            # 收集超时的空闲资源
            temp_list: list[PooledResource[T]] = []
            while not self._available.empty():
                pooled = await self._available.get()
                if pooled.idle_time > self._config.max_idle_time:
                    # 保留最小连接数
                    current_size = len(temp_list) + len(self._in_use)
                    if current_size > self._config.min_size:
                        to_destroy.append(pooled)
                    else:
                        temp_list.append(pooled)
                else:
                    temp_list.append(pooled)

            # 放回可用队列
            for pooled in temp_list:
                await self._available.put(pooled)

        # 销毁超时资源
        for pooled in to_destroy:
            await self._destroy_resource(pooled)
            _logger.debug(f"Destroyed idle resource (idle {pooled.idle_time:.1f}s)")


__all__ = [
    "PoolConfig",
    "PoolStats",
    "PooledResource",
    "ResourceFactory",
    "ResourcePool",
]
