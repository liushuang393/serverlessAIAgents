"""AgentFlow Memory System.

LightMemの思想に基づいた3段階記憶システム。

参考論文: LightMem - 効率的なLLM記憶システム
- Light1: 感覚記憶（予圧縮 + トピック分割）
- Light2: 短期記憶（トピックバッファ + 要約生成）
- Light3: 長期記憶（オンライン追加 + オフライン統合）

Example:
    >>> from agentflow.memory import MemoryManager
    >>> manager = MemoryManager()
    >>> await manager.start()
    >>> await manager.remember("重要な情報", topic="AI")
    >>> memories = await manager.recall(topic="AI")
    >>> await manager.stop()
"""

from agentflow.memory.long_term_memory import LongTermMemory
from agentflow.memory.memory_manager import MemoryManager
from agentflow.memory.sensory_memory import SensoryMemory
from agentflow.memory.short_term_memory import ShortTermMemory
from agentflow.memory.types import (
    CompressionConfig,
    MemoryEntry,
    MemoryType,
    TopicBuffer,
    UpdateQueue,
)

__all__ = [
    # Main Manager
    "MemoryManager",
    # Memory Layers
    "SensoryMemory",
    "ShortTermMemory",
    "LongTermMemory",
    # Types
    "MemoryEntry",
    "MemoryType",
    "TopicBuffer",
    "UpdateQueue",
    "CompressionConfig",
]

