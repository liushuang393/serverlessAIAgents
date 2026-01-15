# -*- coding: utf-8 -*-
"""增强记忆系统 - Manus分析启发的高级记忆功能.

基于Manus分析中提到的记忆系统特性:
- 记忆蒸馏: 将相似记忆抽象为高级知识
- 主动遗忘: 自动清理低价值记忆
- 强化学习: 基于任务结果调整记忆价值
- 上下文压缩: 智能压缩长对话历史

设计原则:
- 与现有MemoryManager兼容
- 支持渐进式采用
- 可配置的策略

使用例:
    >>> from agentflow.memory.enhanced_memory import EnhancedMemoryManager
    >>>
    >>> manager = EnhancedMemoryManager()
    >>> await manager.start()
    >>> 
    >>> # 记忆并自动蒸馏
    >>> await manager.remember_with_distillation("重要信息", topic="AI")
    >>> 
    >>> # 基于任务结果强化记忆
    >>> await manager.reinforce_memories(topic="AI", reward=1.0)
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field
from datetime import datetime, timedelta
from enum import Enum
from typing import Any, Callable

from agentflow.memory.memory_manager import MemoryManager
from agentflow.memory.types import MemoryEntry, MemoryType


class DistillationStrategy(str, Enum):
    """蒸馏策略."""
    
    SIMILARITY = "similarity"      # 基于相似度合并
    TEMPORAL = "temporal"          # 基于时间窗口合并
    SEMANTIC = "semantic"          # 基于语义关系合并
    HIERARCHICAL = "hierarchical"  # 层次化抽象


class ForgettingStrategy(str, Enum):
    """遗忘策略."""
    
    LRU = "lru"                    # 最近最少使用
    IMPORTANCE = "importance"      # 基于重要性
    DECAY = "decay"                # 时间衰减
    ADAPTIVE = "adaptive"          # 自适应（综合多因素）


@dataclass
class MemoryConfig:
    """增强记忆配置.
    
    Attributes:
        distillation_threshold: 触发蒸馏的相似记忆数量
        forgetting_threshold: 触发遗忘的记忆数量
        importance_decay_rate: 重要性衰减率
        min_importance: 最小重要性（低于此值将被遗忘）
        distillation_strategy: 蒸馏策略
        forgetting_strategy: 遗忘策略
    """
    
    distillation_threshold: int = 5
    forgetting_threshold: int = 1000
    importance_decay_rate: float = 0.01
    min_importance: float = 0.1
    distillation_strategy: DistillationStrategy = DistillationStrategy.SIMILARITY
    forgetting_strategy: ForgettingStrategy = ForgettingStrategy.ADAPTIVE
    auto_distillation: bool = True
    auto_forgetting: bool = True
    distillation_interval: int = 300  # 秒
    forgetting_interval: int = 600    # 秒


@dataclass
class DistilledKnowledge:
    """蒸馏后的知识.
    
    Attributes:
        id: 知识ID
        content: 抽象内容
        source_memories: 源记忆ID列表
        topic: 主题
        confidence: 置信度
        created_at: 创建时间
    """
    
    id: str
    content: str
    source_memories: list[str] = field(default_factory=list)
    topic: str = ""
    confidence: float = 1.0
    created_at: datetime = field(default_factory=datetime.now)
    access_count: int = 0
    last_accessed: datetime | None = None
    
    def to_dict(self) -> dict[str, Any]:
        """转换为字典."""
        return {
            "id": self.id,
            "content": self.content,
            "source_memories": self.source_memories,
            "topic": self.topic,
            "confidence": self.confidence,
            "created_at": self.created_at.isoformat(),
            "access_count": self.access_count,
        }


@dataclass
class MemoryStats:
    """记忆统计信息.
    
    Attributes:
        total_memories: 总记忆数
        distilled_count: 蒸馏知识数
        forgotten_count: 已遗忘数
        avg_importance: 平均重要性
        topic_distribution: 主题分布
    """
    
    total_memories: int = 0
    distilled_count: int = 0
    forgotten_count: int = 0
    avg_importance: float = 0.0
    topic_distribution: dict[str, int] = field(default_factory=dict)
    last_distillation: datetime | None = None
    last_forgetting: datetime | None = None


class MemoryImportanceTracker:
    """记忆重要性追踪器.

    追踪记忆的访问模式和任务相关性，动态调整重要性。
    """

    def __init__(self, decay_rate: float = 0.01) -> None:
        """初始化.

        Args:
            decay_rate: 衰减率
        """
        self._importance: dict[str, float] = {}
        self._access_count: dict[str, int] = {}
        self._last_access: dict[str, datetime] = {}
        self._decay_rate = decay_rate
        self._logger = logging.getLogger(__name__)

    def set_importance(self, memory_id: str, importance: float) -> None:
        """设置记忆重要性."""
        self._importance[memory_id] = max(0.0, min(1.0, importance))

    def get_importance(self, memory_id: str) -> float:
        """获取记忆重要性（含衰减）."""
        base = self._importance.get(memory_id, 0.5)

        # 应用时间衰减
        if memory_id in self._last_access:
            elapsed = (datetime.now() - self._last_access[memory_id]).total_seconds()
            decay = self._decay_rate * (elapsed / 3600)  # 每小时衰减
            base = max(0.0, base - decay)

        return base

    def record_access(self, memory_id: str, boost: float = 0.1) -> None:
        """记录访问并提升重要性."""
        self._access_count[memory_id] = self._access_count.get(memory_id, 0) + 1
        self._last_access[memory_id] = datetime.now()

        # 访问提升重要性
        current = self._importance.get(memory_id, 0.5)
        self._importance[memory_id] = min(1.0, current + boost)

    def reinforce(self, memory_id: str, reward: float) -> None:
        """基于任务结果强化记忆.

        Args:
            memory_id: 记忆ID
            reward: 奖励值（-1.0 到 1.0）
        """
        current = self._importance.get(memory_id, 0.5)
        adjustment = reward * 0.2  # 最大调整20%
        self._importance[memory_id] = max(0.0, min(1.0, current + adjustment))

    def get_low_importance_memories(self, threshold: float = 0.1) -> list[str]:
        """获取低重要性记忆列表."""
        return [
            mid for mid, imp in self._importance.items()
            if self.get_importance(mid) < threshold
        ]


class EnhancedMemoryManager:
    """增强记忆管理器.

    在基础MemoryManager上增加:
    - 记忆蒸馏
    - 主动遗忘
    - 强化学习
    - 统计分析

    Example:
        >>> manager = EnhancedMemoryManager()
        >>> await manager.start()
        >>> await manager.remember_with_distillation("信息", topic="AI")
        >>> await manager.reinforce_memories(topic="AI", reward=1.0)
        >>> await manager.stop()
    """

    def __init__(
        self,
        config: MemoryConfig | None = None,
        base_manager: MemoryManager | None = None,
        llm_client: Any = None,
    ) -> None:
        """初始化.

        Args:
            config: 增强记忆配置
            base_manager: 基础记忆管理器
            llm_client: LLM客户端（用于蒸馏）
        """
        self._config = config or MemoryConfig()
        self._base = base_manager or MemoryManager()
        self._llm = llm_client

        # 蒸馏知识存储
        self._distilled: dict[str, DistilledKnowledge] = {}

        # 重要性追踪
        self._tracker = MemoryImportanceTracker(
            decay_rate=self._config.importance_decay_rate
        )

        # 统计
        self._stats = MemoryStats()

        # 后台任务
        self._distillation_task: asyncio.Task[None] | None = None
        self._forgetting_task: asyncio.Task[None] | None = None

        self._logger = logging.getLogger(__name__)

    async def start(self) -> None:
        """启动记忆系统."""
        await self._base.start()

        # 启动自动蒸馏
        if self._config.auto_distillation:
            self._distillation_task = asyncio.create_task(
                self._auto_distillation_loop()
            )

        # 启动自动遗忘
        if self._config.auto_forgetting:
            self._forgetting_task = asyncio.create_task(
                self._auto_forgetting_loop()
            )

        self._logger.info("增强记忆系统已启动")

    async def stop(self) -> None:
        """停止记忆系统."""
        if self._distillation_task:
            self._distillation_task.cancel()
            try:
                await self._distillation_task
            except asyncio.CancelledError:
                pass

        if self._forgetting_task:
            self._forgetting_task.cancel()
            try:
                await self._forgetting_task
            except asyncio.CancelledError:
                pass

        await self._base.stop()
        self._logger.info("增强记忆系统已停止")

    async def remember(
        self,
        text: str,
        topic: str | None = None,
        metadata: dict[str, Any] | None = None,
        importance: float = 0.5,
    ) -> MemoryEntry:
        """记忆信息.

        Args:
            text: 记忆内容
            topic: 主题
            metadata: 元数据
            importance: 初始重要性

        Returns:
            MemoryEntry
        """
        entry = await self._base.remember(text, topic, metadata)

        # 设置重要性
        self._tracker.set_importance(entry.id, importance)

        # 更新统计
        self._stats.total_memories += 1
        if topic:
            self._stats.topic_distribution[topic] = (
                self._stats.topic_distribution.get(topic, 0) + 1
            )

        return entry

    async def remember_with_distillation(
        self,
        text: str,
        topic: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> MemoryEntry:
        """记忆并检查是否需要蒸馏.

        Args:
            text: 记忆内容
            topic: 主题
            metadata: 元数据

        Returns:
            MemoryEntry
        """
        entry = await self.remember(text, topic, metadata)

        # 检查是否需要蒸馏
        if topic and self._should_distill(topic):
            await self.distill_topic(topic)

        return entry

    def _should_distill(self, topic: str) -> bool:
        """检查是否应该蒸馏."""
        count = self._stats.topic_distribution.get(topic, 0)
        return count >= self._config.distillation_threshold

    async def distill_topic(self, topic: str) -> DistilledKnowledge | None:
        """蒸馏指定主题的记忆.

        Args:
            topic: 主题

        Returns:
            蒸馏后的知识
        """
        # 获取该主题的记忆
        memories = await self._base.recall(topic=topic, limit=100)

        if len(memories) < 2:
            return None

        # 使用LLM进行蒸馏
        if self._llm:
            distilled = await self._distill_with_llm(memories, topic)
        else:
            distilled = self._simple_distill(memories, topic)

        if distilled:
            self._distilled[distilled.id] = distilled
            self._stats.distilled_count += 1
            self._stats.last_distillation = datetime.now()
            self._logger.info(f"蒸馏完成: {topic} -> {distilled.id}")

        return distilled

    async def _distill_with_llm(
        self,
        memories: list[MemoryEntry],
        topic: str,
    ) -> DistilledKnowledge | None:
        """使用LLM蒸馏记忆."""
        contents = [m.content for m in memories]
        prompt = f"""请将以下关于"{topic}"的多条记忆抽象为一条高级知识:

记忆内容:
{chr(10).join(f'- {c}' for c in contents)}

要求:
1. 提取共同的核心信息
2. 去除冗余细节
3. 保持关键事实
4. 用简洁的语言表达

抽象知识:"""

        try:
            response = await self._llm.generate(prompt)
            content = response.get("content", str(response))

            import uuid
            return DistilledKnowledge(
                id=f"dk-{uuid.uuid4().hex[:8]}",
                content=content.strip(),
                source_memories=[m.id for m in memories],
                topic=topic,
                confidence=0.8,
            )
        except Exception as e:
            self._logger.warning(f"LLM蒸馏失败: {e}")
            return self._simple_distill(memories, topic)

    def _simple_distill(
        self,
        memories: list[MemoryEntry],
        topic: str,
    ) -> DistilledKnowledge:
        """简单蒸馏（无LLM）."""
        import uuid

        # 简单合并
        contents = [m.content for m in memories]
        combined = f"关于{topic}的综合知识: " + "; ".join(contents[:3])

        return DistilledKnowledge(
            id=f"dk-{uuid.uuid4().hex[:8]}",
            content=combined[:500],  # 限制长度
            source_memories=[m.id for m in memories],
            topic=topic,
            confidence=0.5,
        )

    async def forget_low_importance(self) -> int:
        """遗忘低重要性记忆.

        Returns:
            遗忘的记忆数量
        """
        low_importance = self._tracker.get_low_importance_memories(
            threshold=self._config.min_importance
        )

        forgotten = 0
        for memory_id in low_importance:
            # 从追踪器中移除
            if memory_id in self._tracker._importance:
                del self._tracker._importance[memory_id]
                forgotten += 1

        self._stats.forgotten_count += forgotten
        self._stats.last_forgetting = datetime.now()

        if forgotten > 0:
            self._logger.info(f"遗忘了 {forgotten} 条低重要性记忆")

        return forgotten

    async def reinforce_memories(
        self,
        topic: str | None = None,
        memory_ids: list[str] | None = None,
        reward: float = 1.0,
    ) -> None:
        """强化记忆.

        Args:
            topic: 主题（强化该主题下所有记忆）
            memory_ids: 记忆ID列表
            reward: 奖励值（-1.0 到 1.0）
        """
        if memory_ids:
            for mid in memory_ids:
                self._tracker.reinforce(mid, reward)
        elif topic:
            memories = await self._base.recall(topic=topic, limit=100)
            for m in memories:
                self._tracker.reinforce(m.id, reward)

        self._logger.debug(f"强化记忆: topic={topic}, reward={reward}")

    async def recall(
        self,
        query: str | None = None,
        topic: str | None = None,
        limit: int = 10,
        include_distilled: bool = True,
    ) -> list[MemoryEntry | DistilledKnowledge]:
        """召回记忆.

        Args:
            query: 查询文本
            topic: 主题
            limit: 最大返回数
            include_distilled: 是否包含蒸馏知识

        Returns:
            记忆和知识列表
        """
        results: list[MemoryEntry | DistilledKnowledge] = []

        # 召回基础记忆
        memories = await self._base.recall(query=query, topic=topic, limit=limit)
        results.extend(memories)

        # 记录访问
        for m in memories:
            self._tracker.record_access(m.id)

        # 包含蒸馏知识
        if include_distilled and topic:
            for dk in self._distilled.values():
                if dk.topic == topic:
                    dk.access_count += 1
                    dk.last_accessed = datetime.now()
                    results.append(dk)

        return results[:limit]

    async def _auto_distillation_loop(self) -> None:
        """自动蒸馏循环."""
        while True:
            try:
                await asyncio.sleep(self._config.distillation_interval)

                # 对每个主题检查是否需要蒸馏
                for topic in list(self._stats.topic_distribution.keys()):
                    if self._should_distill(topic):
                        await self.distill_topic(topic)

            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.error(f"自动蒸馏错误: {e}")

    async def _auto_forgetting_loop(self) -> None:
        """自动遗忘循环."""
        while True:
            try:
                await asyncio.sleep(self._config.forgetting_interval)

                # 检查是否超过阈值
                if self._stats.total_memories > self._config.forgetting_threshold:
                    await self.forget_low_importance()

            except asyncio.CancelledError:
                break
            except Exception as e:
                self._logger.error(f"自动遗忘错误: {e}")

    def get_stats(self) -> MemoryStats:
        """获取统计信息."""
        # 计算平均重要性
        if self._tracker._importance:
            self._stats.avg_importance = sum(
                self._tracker._importance.values()
            ) / len(self._tracker._importance)

        return self._stats

    def get_distilled_knowledge(self, topic: str | None = None) -> list[DistilledKnowledge]:
        """获取蒸馏知识.

        Args:
            topic: 主题过滤

        Returns:
            蒸馏知识列表
        """
        if topic:
            return [dk for dk in self._distilled.values() if dk.topic == topic]
        return list(self._distilled.values())

    async def __aenter__(self) -> "EnhancedMemoryManager":
        """async with支持."""
        await self.start()
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """async with支持."""
        await self.stop()

