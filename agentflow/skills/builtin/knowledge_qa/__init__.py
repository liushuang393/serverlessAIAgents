"""FAQ/知識庫Q&A Skills パッケージ.

このパッケージはRAGベースの知識庫Q&A機能を提供します。

含まれるスキル:
- DocIngester: ドキュメント取込
- Retriever: 関連ドキュメント検索
- AnswerGenerator: 回答生成
- GapAnalyzer: ルール/文書不足分析

使用例:
    >>> from agentflow.skills.builtin.knowledge_qa import (
    ...     DocIngester,
    ...     Retriever,
    ...     AnswerGenerator,
    ... )
"""

from agentflow.skills.builtin.knowledge_qa.answer_generator import (
    AnswerConfig,
    AnswerGenerator,
    Citation,
    GeneratedAnswer,
)
from agentflow.skills.builtin.knowledge_qa.doc_ingester import (
    DocIngester,
    IngestConfig,
    IngestedDoc,
    IngestResult,
)
from agentflow.skills.builtin.knowledge_qa.gap_analyzer import (
    GapAnalyzer,
    GapReport,
    SuggestedDoc,
    UnansweredQuestion,
)
from agentflow.skills.builtin.knowledge_qa.retriever import (
    RetrievalConfig,
    RetrievalResult,
    RetrievedChunk,
    Retriever,
)


__all__ = [
    "AnswerConfig",
    # 回答生成
    "AnswerGenerator",
    "Citation",
    # ドキュメント取込
    "DocIngester",
    # ギャップ分析
    "GapAnalyzer",
    "GapReport",
    "GeneratedAnswer",
    "IngestConfig",
    "IngestResult",
    "IngestedDoc",
    "RetrievalConfig",
    "RetrievalResult",
    "RetrievedChunk",
    # 検索
    "Retriever",
    "SuggestedDoc",
    "UnansweredQuestion",
]
