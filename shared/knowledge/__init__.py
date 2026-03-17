"""AgentFlow 知識ベース統合モジュール.

RAG（検索増強生成）パターンの完全な実装を提供します：
- ドキュメントローダー（PDF、Markdown、CSV、JSON、HTML）
- ベクトルストア統合（Pinecone、Qdrant、ChromaDB）
- ベクトル検索Hook（useVectorSearch）
- RAGパイプライン

使用例:
    >>> from shared.rag import RAGPipeline, DocumentLoader
    >>> from shared.knowledge.hooks import use_vector_search
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

# コレクション・ドキュメント管理
from shared.knowledge.collection_manager import CollectionManager
from shared.knowledge.doc_health_checker import (
    DocHealthChecker,
    DocHealthConfig,
    Document,
    HealthIssue,
    HealthReport,
    HealthStatus,
    IssueSeverity,
    IssueType,
)
from shared.knowledge.document_manager import DocumentManager
from shared.knowledge.document_loader import (
    CSVLoader,
    DocumentChunk,
    DocumentLoader,
    HTMLLoader,
    JSONLoader,
    MarkdownLoader,
    PDFLoader,
    TextLoader,
)
from shared.knowledge.hooks import (
    RAGHook,
    VectorSearchHook,
    use_rag,
    use_vector_search,
)

# 新規追加: 隔離KB、ドキュメント健康度
from shared.knowledge.isolated_kb import (
    IsolatedKBManager,
    KBAccessLog,
    KBConfig,
    KBDocument,
    KBType,
    KBVisibility,
    SearchResult,
)
from shared.knowledge.models import (
    CollectionConfigModel,
    DocumentRecordModel,
    DocumentStatus,
)
from shared.knowledge.rag_access_control import RAGAccessControl
from shared.knowledge.rag_pipeline import (
    RAGConfig,
    RAGPipeline,
    RAGResult,
)
from shared.knowledge.resource_manager import (
    ResourceDefinition as ResourceDefinitionDTO,
)
from shared.knowledge.resource_manager import (
    ResourceManager,
)
from shared.knowledge.scope_resolver import CollectionTarget, ScopeResolver


__all__ = [
    "CSVLoader",
    # コレクション・ドキュメント管理
    "CollectionConfigModel",
    "CollectionManager",
    "CollectionTarget",
    # ドキュメント健康度
    "DocHealthChecker",
    "DocHealthConfig",
    "Document",
    "DocumentChunk",
    # Document Loaders
    "DocumentLoader",
    "DocumentManager",
    "DocumentRecordModel",
    "DocumentStatus",
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
    "RAGAccessControl",
    "RAGConfig",
    "RAGHook",
    # RAG Pipeline
    "RAGPipeline",
    "RAGResult",
    "ResourceDefinitionDTO",
    "ResourceManager",
    "ScopeResolver",
    "SearchResult",
    "TextLoader",
    "VectorSearchHook",
    "use_rag",
    # Hooks
    "use_vector_search",
]
