"""AgentFlow Memory System.

LightMemの思想に基づいた3段階記憶システム。

参考論文: LightMem - 効率的なLLM記憶システム
- Light1: 感覚記憶（予圧縮 + トピック分割）
- Light2: 短期記憶（トピックバッファ + 要約生成）
- Light3: 長期記憶（オンライン追加 + オフライン統合）

拡張機能（HOPE/Evo-Memory思想）:
- 記憶蒸留: 類似記憶を抽象知識に自動変換
- 主動忘却: 低価値記憶を自動削除
- 強化学習: タスク結果に基づく記憶価値調整

使用方法（シンプルなAPI、内部は自動最適化）:
    >>> from agentflow.memory import MemoryManager
    >>> manager = MemoryManager()
    >>> await manager.start()
    >>> await manager.remember("重要な情報", topic="AI")
    >>> memories = await manager.recall(topic="AI")
    >>> # タスク成功時に強化（オプション）
    >>> await manager.reinforce(topic="AI", reward=1.0)
    >>> await manager.stop()
"""

from agentflow.memory.importance_adjuster import ImportanceAdjuster
from agentflow.memory.long_term_memory import LongTermMemory
from agentflow.memory.memory_distiller import MemoryDistiller
from agentflow.memory.memory_manager import MemoryManager
from agentflow.memory.sensory_memory import SensoryMemory
from agentflow.memory.short_term_memory import ShortTermMemory
from agentflow.memory.types import (
    CompressionConfig,
    MemoryEntry,
    MemorySemanticLevel,
    MemoryStability,
    MemoryType,
    TopicBuffer,
    UpdateQueue,
)

# NEW: Vector Store（LlamaIndex/LangChain 互換接口）
from agentflow.memory.vector_store import (
    # データモデル
    Document,
    Node,
    SearchResult,
    SearchType,
    # 埋め込み
    EmbeddingModel,
    SimpleEmbedding,
    # ベクトルストア
    VectorStore,
    InMemoryVectorStore,
    # ファクトリー
    create_vector_store,
    create_embedding_model,
)

# NEW: Enhanced Memory（記憶蒸留 + 主動忘却 + 強化学習）
from agentflow.memory.enhanced_memory import (
    DistillationStrategy,
    DistilledKnowledge,
    EnhancedMemoryManager,
    ForgettingStrategy,
    MemoryConfig,
    MemoryImportanceTracker,
    MemoryStats,
)

# NEW: Knowledge Store（Memvid長期知識記憶）
from agentflow.memory.knowledge import (
    # 主要API
    get_knowledge_manager,
    get_knowledge_store,
    reset_knowledge_manager,
    # マネージャー
    KnowledgeManager,
    # ストアインターフェース
    KnowledgeStore,
    # ストア実装
    MemvidKnowledgeStore,
    InMemoryKnowledgeStore,
    is_memvid_available,
    # 型定義
    KnowledgeEntry,
    KnowledgeSource,
    # 注: SearchResult, SearchTypeはvector_storeで既に定義
)

__all__ = [
    # Main Manager（ユーザー向け主要API）
    "MemoryManager",
    # Memory Layers（内部使用、通常は直接使わない）
    "SensoryMemory",
    "ShortTermMemory",
    "LongTermMemory",
    # 自動最適化エンジン（内部使用）
    "MemoryDistiller",
    "ImportanceAdjuster",
    # Types
    "MemoryEntry",
    "MemoryType",
    "MemorySemanticLevel",
    "MemoryStability",
    "TopicBuffer",
    "UpdateQueue",
    "CompressionConfig",
    # ==========================================================================
    # NEW: Vector Store（LlamaIndex/LangChain 互換）
    # ==========================================================================
    # データモデル
    "Document",
    "Node",
    "SearchResult",
    "SearchType",
    # 埋め込み
    "EmbeddingModel",
    "SimpleEmbedding",
    # ベクトルストア
    "VectorStore",
    "InMemoryVectorStore",
    # ファクトリー
    "create_vector_store",
    "create_embedding_model",
    # ==========================================================================
    # NEW: Enhanced Memory（記憶蒸留 + 主動忘却 + 強化学習）
    # ==========================================================================
    "EnhancedMemoryManager",
    "MemoryConfig",
    "DistillationStrategy",
    "ForgettingStrategy",
    "DistilledKnowledge",
    "MemoryStats",
    "MemoryImportanceTracker",
    # ==========================================================================
    # NEW: Knowledge Store（Memvid長期知識記憶）
    # ==========================================================================
    # 主要API
    "get_knowledge_manager",
    "get_knowledge_store",
    "reset_knowledge_manager",
    # マネージャー
    "KnowledgeManager",
    # ストアインターフェース
    "KnowledgeStore",
    # ストア実装
    "MemvidKnowledgeStore",
    "InMemoryKnowledgeStore",
    "is_memvid_available",
    # 型定義
    "KnowledgeEntry",
    "KnowledgeSource",
]

