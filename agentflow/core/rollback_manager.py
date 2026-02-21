"""回滚管理器 - 状态快照与恢复系统.

Manus分析中提到的「回滚重试」机制：
- 每步执行前保存状态快照
- 失败时自动回滚到上一个稳定状态
- 支持多级回滚（回滚到任意检查点）
- 重试时使用不同策略

设计原则:
- 状态不可变，每次变更创建新快照
- 快照包含完整上下文
- 支持异步操作的回滚

使用例:
    >>> from agentflow.core.rollback_manager import RollbackManager
    >>>
    >>> manager = RollbackManager()
    >>>
    >>> # 创建检查点
    >>> checkpoint_id = await manager.create_checkpoint(state)
    >>>
    >>> try:
    ...     result = await risky_operation()
    ... except Exception:
    ...     # 回滚到检查点
    ...     state = await manager.rollback(checkpoint_id)
"""

from __future__ import annotations

import asyncio
import copy
import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import TYPE_CHECKING, Any, TypeVar


if TYPE_CHECKING:
    from collections.abc import Callable


logger = logging.getLogger(__name__)

T = TypeVar("T")


class CheckpointStatus(str, Enum):
    """检查点状态."""

    ACTIVE = "active"  # 活跃（可回滚）
    COMMITTED = "committed"  # 已提交（成功完成）
    ROLLED_BACK = "rolled_back"  # 已回滚
    EXPIRED = "expired"  # 已过期


@dataclass
class Checkpoint:
    """状态检查点.

    Attributes:
        id: 检查点ID
        state: 状态快照
        metadata: 元数据
        parent_id: 父检查点ID
        status: 状态
        created_at: 创建时间
        description: 描述
    """

    id: str
    state: dict[str, Any]
    metadata: dict[str, Any] = field(default_factory=dict)
    parent_id: str | None = None
    status: CheckpointStatus = CheckpointStatus.ACTIVE
    created_at: datetime = field(default_factory=datetime.now)
    description: str = ""

    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "id": self.id,
            "state": self.state,
            "metadata": self.metadata,
            "parent_id": self.parent_id,
            "status": self.status.value,
            "created_at": self.created_at.isoformat(),
            "description": self.description,
        }


@dataclass
class RollbackResult:
    """回滚结果.

    Attributes:
        success: 是否成功
        checkpoint_id: 回滚到的检查点ID
        restored_state: 恢复的状态
        message: 消息
        rolled_back_steps: 回滚的步骤数
    """

    success: bool
    checkpoint_id: str
    restored_state: dict[str, Any] | None = None
    message: str = ""
    rolled_back_steps: int = 0


class RetryStrategy(Enum):
    """重试策略."""

    SAME = "same"  # 相同方式重试
    ALTERNATIVE = "alternative"  # 替代方式
    SIMPLIFIED = "simplified"  # 简化方式
    HUMAN_ASSIST = "human_assist"  # 人工辅助


@dataclass
class RetryConfig:
    """重试配置.

    Attributes:
        max_retries: 最大重试次数
        strategies: 重试策略序列
        delay_seconds: 重试延迟
        backoff_multiplier: 退避乘数
    """

    max_retries: int = 3
    strategies: list[RetryStrategy] = field(
        default_factory=lambda: [
            RetryStrategy.SAME,
            RetryStrategy.ALTERNATIVE,
            RetryStrategy.SIMPLIFIED,
        ]
    )
    delay_seconds: float = 1.0
    backoff_multiplier: float = 2.0


class RollbackManager:
    """回滚管理器.

    管理状态检查点，支持失败时回滚和重试。

    核心功能:
    - 创建状态检查点
    - 回滚到指定检查点
    - 多策略重试
    - 检查点链追踪

    Example:
        >>> manager = RollbackManager(max_checkpoints=10)
        >>>
        >>> # 执行带回滚保护的操作
        >>> async with manager.protected_execution(current_state) as ctx:
        ...     result = await risky_operation()
        ...     ctx.update_state(result)
    """

    def __init__(
        self,
        max_checkpoints: int = 50,
        auto_cleanup: bool = True,
        retry_config: RetryConfig | None = None,
    ) -> None:
        """初始化.

        Args:
            max_checkpoints: 最大检查点数量
            auto_cleanup: 是否自动清理过期检查点
            retry_config: 重试配置
        """
        self._checkpoints: dict[str, Checkpoint] = {}
        self._checkpoint_order: list[str] = []  # 按时间顺序
        self._max_checkpoints = max_checkpoints
        self._auto_cleanup = auto_cleanup
        self._retry_config = retry_config or RetryConfig()
        self._current_checkpoint_id: str | None = None
        self._logger = logging.getLogger(__name__)

    async def create_checkpoint(
        self,
        state: dict[str, Any],
        description: str = "",
        metadata: dict[str, Any] | None = None,
    ) -> str:
        """创建检查点.

        Args:
            state: 当前状态
            description: 描述
            metadata: 元数据

        Returns:
            检查点ID
        """
        checkpoint_id = str(uuid.uuid4())

        # 深拷贝状态，确保不可变
        state_copy = copy.deepcopy(state)

        checkpoint = Checkpoint(
            id=checkpoint_id,
            state=state_copy,
            metadata=metadata or {},
            parent_id=self._current_checkpoint_id,
            description=description,
        )

        self._checkpoints[checkpoint_id] = checkpoint
        self._checkpoint_order.append(checkpoint_id)
        self._current_checkpoint_id = checkpoint_id

        # 自动清理
        if self._auto_cleanup:
            await self._cleanup_old_checkpoints()

        self._logger.info(f"创建检查点: {checkpoint_id[:8]}... - {description}")
        return checkpoint_id

    async def rollback(
        self,
        checkpoint_id: str | None = None,
        steps: int = 1,
    ) -> RollbackResult:
        """回滚到指定检查点.

        Args:
            checkpoint_id: 目标检查点ID（None则回滚指定步数）
            steps: 回滚步数（仅当checkpoint_id为None时有效）

        Returns:
            RollbackResult
        """
        if checkpoint_id:
            target_id = checkpoint_id
        else:
            # 按步数回滚
            if len(self._checkpoint_order) < steps:
                return RollbackResult(
                    success=False,
                    checkpoint_id="",
                    message=f"检查点不足，无法回滚{steps}步",
                )
            target_id = self._checkpoint_order[-(steps + 1)]

        if target_id not in self._checkpoints:
            return RollbackResult(
                success=False,
                checkpoint_id=target_id,
                message=f"检查点 {target_id} 不存在",
            )

        checkpoint = self._checkpoints[target_id]

        if checkpoint.status != CheckpointStatus.ACTIVE:
            return RollbackResult(
                success=False,
                checkpoint_id=target_id,
                message=f"检查点状态为 {checkpoint.status.value}，无法回滚",
            )

        # 标记被回滚的检查点
        rolled_back_count = 0
        target_index = self._checkpoint_order.index(target_id)
        for cp_id in self._checkpoint_order[target_index + 1 :]:
            if cp_id in self._checkpoints:
                self._checkpoints[cp_id].status = CheckpointStatus.ROLLED_BACK
                rolled_back_count += 1

        # 更新当前检查点
        self._current_checkpoint_id = target_id

        self._logger.info(f"回滚到检查点: {target_id[:8]}..., 回滚了{rolled_back_count}步")

        return RollbackResult(
            success=True,
            checkpoint_id=target_id,
            restored_state=copy.deepcopy(checkpoint.state),
            message=f"成功回滚到检查点 {checkpoint.description or target_id[:8]}",
            rolled_back_steps=rolled_back_count,
        )

    async def commit(
        self,
        checkpoint_id: str | None = None,
    ) -> bool:
        """提交检查点（标记为成功完成）.

        Args:
            checkpoint_id: 检查点ID（None则提交当前检查点）

        Returns:
            是否成功
        """
        target_id = checkpoint_id or self._current_checkpoint_id
        if not target_id or target_id not in self._checkpoints:
            return False

        self._checkpoints[target_id].status = CheckpointStatus.COMMITTED
        self._logger.info(f"提交检查点: {target_id[:8]}...")
        return True

    async def retry_with_rollback(
        self,
        operation: Callable[..., Any],
        state: dict[str, Any],
        *args: Any,
        **kwargs: Any,
    ) -> tuple[Any, dict[str, Any]]:
        """带回滚的重试执行.

        失败时自动回滚并使用不同策略重试。

        Args:
            operation: 要执行的操作
            state: 初始状态
            *args: 操作参数
            **kwargs: 操作关键字参数

        Returns:
            (操作结果, 最终状态)

        Raises:
            Exception: 所有重试都失败时抛出最后的异常
        """
        checkpoint_id = await self.create_checkpoint(
            state,
            description=f"重试保护: {operation.__name__}",
        )

        last_error: Exception | None = None
        current_state = copy.deepcopy(state)

        for attempt in range(self._retry_config.max_retries):
            strategy_index = min(attempt, len(self._retry_config.strategies) - 1)
            strategy = self._retry_config.strategies[strategy_index]

            try:
                self._logger.info(f"尝试 {attempt + 1}/{self._retry_config.max_retries}, 策略: {strategy.value}")

                # 根据策略调整参数
                adjusted_kwargs = self._adjust_for_strategy(strategy, kwargs)

                result = await operation(*args, **adjusted_kwargs)

                # 成功，提交检查点
                await self.commit(checkpoint_id)
                return result, current_state

            except Exception as e:
                last_error = e
                self._logger.warning(f"尝试 {attempt + 1} 失败: {e}")

                # 回滚状态
                rollback_result = await self.rollback(checkpoint_id)
                if rollback_result.success and rollback_result.restored_state:
                    current_state = rollback_result.restored_state

                # 等待后重试
                if attempt < self._retry_config.max_retries - 1:
                    delay = self._retry_config.delay_seconds * (self._retry_config.backoff_multiplier**attempt)
                    await asyncio.sleep(delay)

        # 所有重试都失败
        if last_error:
            raise last_error
        msg = "重试失败，但没有捕获到异常"
        raise RuntimeError(msg)

    def _adjust_for_strategy(
        self,
        strategy: RetryStrategy,
        kwargs: dict[str, Any],
    ) -> dict[str, Any]:
        """根据策略调整参数.

        Args:
            strategy: 重试策略
            kwargs: 原始参数

        Returns:
            调整后的参数
        """
        adjusted = copy.deepcopy(kwargs)

        if strategy == RetryStrategy.SIMPLIFIED:
            # 简化模式：减少复杂度
            if "max_tokens" in adjusted:
                adjusted["max_tokens"] = min(adjusted["max_tokens"], 1000)
            if "temperature" in adjusted:
                adjusted["temperature"] = 0.0
        elif strategy == RetryStrategy.ALTERNATIVE:
            # 替代模式：使用备选配置
            if "model" in adjusted:
                adjusted["_original_model"] = adjusted["model"]
                # 可以在这里切换到备选模型

        return adjusted

    async def _cleanup_old_checkpoints(self) -> None:
        """清理过期检查点."""
        while len(self._checkpoint_order) > self._max_checkpoints:
            oldest_id = self._checkpoint_order.pop(0)
            if oldest_id in self._checkpoints:
                self._checkpoints[oldest_id].status = CheckpointStatus.EXPIRED
                del self._checkpoints[oldest_id]

    def get_checkpoint(self, checkpoint_id: str) -> Checkpoint | None:
        """获取检查点."""
        return self._checkpoints.get(checkpoint_id)

    def get_checkpoint_chain(self) -> list[Checkpoint]:
        """获取检查点链（从最早到最新）."""
        return [self._checkpoints[cp_id] for cp_id in self._checkpoint_order if cp_id in self._checkpoints]

    def get_stats(self) -> dict[str, Any]:
        """获取统计信息."""
        status_counts = dict.fromkeys(CheckpointStatus, 0)
        for cp in self._checkpoints.values():
            status_counts[cp.status] += 1

        return {
            "total_checkpoints": len(self._checkpoints),
            "max_checkpoints": self._max_checkpoints,
            "current_checkpoint_id": self._current_checkpoint_id,
            "status_counts": {k.value: v for k, v in status_counts.items()},
        }
