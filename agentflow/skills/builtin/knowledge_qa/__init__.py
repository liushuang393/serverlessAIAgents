# -*- coding: utf-8 -*-
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

from agentflow.skills.builtin.knowledge_qa.doc_ingester import (
    DocIngester,
    IngestConfig,
    IngestedDoc,
    IngestResult,
)
from agentflow.skills.builtin.knowledge_qa.retriever import (
    Retriever,
    RetrievalConfig,
    RetrievedChunk,
    RetrievalResult,
)
from agentflow.skills.builtin.knowledge_qa.answer_generator import (
    AnswerGenerator,
    GeneratedAnswer,
    AnswerConfig,
    Citation,
)
from agentflow.skills.builtin.knowledge_qa.gap_analyzer import (
    GapAnalyzer,
    GapReport,
    UnansweredQuestion,
    SuggestedDoc,
)

__all__ = [
    # ドキュメント取込
    "DocIngester",
    "IngestConfig",
    "IngestedDoc",
    "IngestResult",
    # 検索
    "Retriever",
    "RetrievalConfig",
    "RetrievedChunk",
    "RetrievalResult",
    # 回答生成
    "AnswerGenerator",
    "GeneratedAnswer",
    "AnswerConfig",
    "Citation",
    # ギャップ分析
    "GapAnalyzer",
    "GapReport",
    "UnansweredQuestion",
    "SuggestedDoc",
]
