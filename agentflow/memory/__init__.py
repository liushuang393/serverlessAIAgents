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
from agentflow.memory.importance_adjuster import ImportanceAdjuster

# NEW: Knowledge Store（Memvid長期知識記憶）
from agentflow.memory.knowledge import (
    InMemoryKnowledgeStore,
    # 型定義
    KnowledgeEntry,
    # マネージャー
    KnowledgeManager,
    KnowledgeSource,
    # 注: SearchResult, SearchTypeはvector_storeで既に定義
    # ストアインターフェース
    KnowledgeStore,
    # ストア実装
    MemvidKnowledgeStore,
    # 主要API
    get_knowledge_manager,
    get_knowledge_store,
    is_memvid_available,
    reset_knowledge_manager,
)
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
    # 埋め込み
    EmbeddingModel,
    InMemoryVectorStore,
    Node,
    SearchResult,
    SearchType,
    SimpleEmbedding,
    # ベクトルストア
    VectorStore,
    create_embedding_model,
    # ファクトリー
    create_vector_store,
)


__all__ = [
    "CompressionConfig",
    "DistillationStrategy",
    "DistilledKnowledge",
    # ==========================================================================
    # NEW: Vector Store（LlamaIndex/LangChain 互換）
    # ==========================================================================
    # データモデル
    "Document",
    # 埋め込み
    "EmbeddingModel",
    # ==========================================================================
    # NEW: Enhanced Memory（記憶蒸留 + 主動忘却 + 強化学習）
    # ==========================================================================
    "EnhancedMemoryManager",
    "ForgettingStrategy",
    "ImportanceAdjuster",
    "InMemoryKnowledgeStore",
    "InMemoryVectorStore",
    # 型定義
    "KnowledgeEntry",
    # マネージャー
    "KnowledgeManager",
    "KnowledgeSource",
    # ストアインターフェース
    "KnowledgeStore",
    "LongTermMemory",
    "MemoryConfig",
    # 自動最適化エンジン（内部使用）
    "MemoryDistiller",
    # Types
    "MemoryEntry",
    "MemoryImportanceTracker",
    # Main Manager（ユーザー向け主要API）
    "MemoryManager",
    "MemorySemanticLevel",
    "MemoryStability",
    "MemoryStats",
    "MemoryType",
    # ストア実装
    "MemvidKnowledgeStore",
    "Node",
    "SearchResult",
    "SearchType",
    # Memory Layers（内部使用、通常は直接使わない）
    "SensoryMemory",
    "ShortTermMemory",
    "SimpleEmbedding",
    "TopicBuffer",
    "UpdateQueue",
    # ベクトルストア
    "VectorStore",
    "create_embedding_model",
    # ファクトリー
    "create_vector_store",
    # ==========================================================================
    # NEW: Knowledge Store（Memvid長期知識記憶）
    # ==========================================================================
    # 主要API
    "get_knowledge_manager",
    "get_knowledge_store",
    "is_memvid_available",
    "reset_knowledge_manager",
]
