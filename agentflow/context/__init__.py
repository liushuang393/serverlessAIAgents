"""Context Engineering モジュール.

上下文エンジニアリングのための統合管理コンポーネント群。

設計思想（Context Engineering 原則）:
- 注意力予算: システムプロンプト≤500token、ツール5-7個
- 検索判断: RAGは必要な時のみ、Top1-2片段のみ注入
- 会話圧縮: 10ターンごとに摘要 + 重要Notesを永続化
- 結果過濾: 子Agent結果は最終出力のみ、中間状態は除外

コンポーネント:
- TokenBudgetManager: 各種上下文予算の統合管理
- ToolRelevanceSelector: クエリベースの動的ツール選択
- RetrievalGate: RAG検索必要性の判定
- KeyNotesStore: 重要情報の永続化
- TurnBasedCompressor: ターン数ベースの会話圧縮

使用例:
    >>> from agentflow.context import ContextEngineer
    >>> engineer = ContextEngineer()
    >>>
    >>> # 予算内でシステムプロンプトを構築
    >>> prompt = engineer.build_system_prompt(base_prompt, skills)
    >>>
    >>> # 関連ツールを選択
    >>> tools = await engineer.select_tools(query, all_tools)
    >>>
    >>> # 検索が必要か判定
    >>> if await engineer.should_retrieve(query):
    ...     results = await rag.search(query)
"""

from agentflow.context.budget_manager import (
    BudgetAllocation,
    BudgetConfig,
    TokenBudgetManager,
)
from agentflow.context.context_engineer import (
    ContextConfig,
    ContextEngineer,
)
from agentflow.context.key_notes import (
    KeyNote,
    KeyNotesStore,
    NoteImportance,
)
from agentflow.context.retrieval_gate import (
    RetrievalDecision,
    RetrievalGate,
    RetrievalReason,
)
from agentflow.context.tool_selector import (
    ToolRelevanceSelector,
    ToolScore,
)
from agentflow.context.turn_compressor import (
    CompressionResult,
    TurnBasedCompressor,
    TurnConfig,
)


__all__ = [
    "BudgetAllocation",
    "BudgetConfig",
    "CompressionResult",
    "ContextConfig",
    # 統合インターフェース
    "ContextEngineer",
    "KeyNote",
    # 重要Notes
    "KeyNotesStore",
    "NoteImportance",
    "RetrievalDecision",
    # 検索判断
    "RetrievalGate",
    "RetrievalReason",
    # 予算管理
    "TokenBudgetManager",
    # ツール選択
    "ToolRelevanceSelector",
    "ToolScore",
    # ターン圧縮
    "TurnBasedCompressor",
    "TurnConfig",
]
