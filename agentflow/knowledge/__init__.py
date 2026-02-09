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

from agentflow.knowledge.doc_health_checker import (
    DocHealthChecker,
    DocHealthConfig,
    Document,
    HealthIssue,
    HealthReport,
    HealthStatus,
    IssueSeverity,
    IssueType,
)
from agentflow.knowledge.document_loader import (
    CSVLoader,
    DocumentChunk,
    DocumentLoader,
    HTMLLoader,
    JSONLoader,
    MarkdownLoader,
    PDFLoader,
    TextLoader,
)
from agentflow.knowledge.hooks import (
    RAGHook,
    VectorSearchHook,
    use_rag,
    use_vector_search,
)

# 新規追加: 隔離KB、ドキュメント健康度
from agentflow.knowledge.isolated_kb import (
    IsolatedKBManager,
    KBAccessLog,
    KBConfig,
    KBDocument,
    KBType,
    KBVisibility,
    SearchResult,
)
from agentflow.knowledge.rag_pipeline import (
    RAGConfig,
    RAGPipeline,
    RAGResult,
)


__all__ = [
    "CSVLoader",
    # ドキュメント健康度
    "DocHealthChecker",
    "DocHealthConfig",
    "Document",
    "DocumentChunk",
    # Document Loaders
    "DocumentLoader",
    "HTMLLoader",
    "HealthIssue",
    "HealthReport",
    "HealthStatus",
    # 隔離KB
    "IsolatedKBManager",
    "IssueSeverity",
    "IssueType",
    "JSONLoader",
    "KBAccessLog",
    "KBConfig",
    "KBDocument",
    "KBType",
    "KBVisibility",
    "MarkdownLoader",
    "PDFLoader",
    "RAGConfig",
    "RAGHook",
    # RAG Pipeline
    "RAGPipeline",
    "RAGResult",
    "SearchResult",
    "TextLoader",
    "VectorSearchHook",
    "use_rag",
    # Hooks
    "use_vector_search",
]

