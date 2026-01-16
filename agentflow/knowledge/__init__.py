# -*- coding: utf-8 -*-
"""AgentFlow 知識ベース統合モジュール.

RAG（検索増強生成）パターンの完全な実装を提供します：
- ドキュメントローダー（PDF、Markdown、CSV、JSON、HTML）
- ベクトルストア統合（Pinecone、Qdrant、ChromaDB）
- ベクトル検索Hook（useVectorSearch）
- RAGパイプライン

使用例:
    >>> from agentflow.knowledge import RAGPipeline, DocumentLoader
    >>> from agentflow.knowledge.hooks import use_vector_search
    >>>
    >>> # シンプルな使用
    >>> rag = RAGPipeline()
    >>> await rag.add_document("path/to/doc.pdf")
    >>> result = await rag.query("質問")
    >>>
    >>> # Hook スタイル
    >>> search = use_vector_search(collection="my-docs")
    >>> results = await search("類似検索クエリ", top_k=5)
"""

from agentflow.knowledge.document_loader import (
    DocumentLoader,
    CSVLoader,
    JSONLoader,
    MarkdownLoader,
    PDFLoader,
    TextLoader,
    HTMLLoader,
    DocumentChunk,
)
from agentflow.knowledge.rag_pipeline import (
    RAGPipeline,
    RAGConfig,
    RAGResult,
)
from agentflow.knowledge.hooks import (
    use_vector_search,
    use_rag,
    VectorSearchHook,
    RAGHook,
)

# 新規追加: 隔離KB、ドキュメント健康度
from agentflow.knowledge.isolated_kb import (
    IsolatedKBManager,
    KBConfig,
    KBDocument,
    KBType,
    KBVisibility,
    SearchResult,
    KBAccessLog,
)
from agentflow.knowledge.doc_health_checker import (
    DocHealthChecker,
    DocHealthConfig,
    Document,
    HealthReport,
    HealthIssue,
    HealthStatus,
    IssueType,
    IssueSeverity,
)

__all__ = [
    # Document Loaders
    "DocumentLoader",
    "CSVLoader",
    "JSONLoader",
    "MarkdownLoader",
    "PDFLoader",
    "TextLoader",
    "HTMLLoader",
    "DocumentChunk",
    # RAG Pipeline
    "RAGPipeline",
    "RAGConfig",
    "RAGResult",
    # Hooks
    "use_vector_search",
    "use_rag",
    "VectorSearchHook",
    "RAGHook",
    # 隔離KB
    "IsolatedKBManager",
    "KBConfig",
    "KBDocument",
    "KBType",
    "KBVisibility",
    "SearchResult",
    "KBAccessLog",
    # ドキュメント健康度
    "DocHealthChecker",
    "DocHealthConfig",
    "Document",
    "HealthReport",
    "HealthIssue",
    "HealthStatus",
    "IssueType",
    "IssueSeverity",
]

