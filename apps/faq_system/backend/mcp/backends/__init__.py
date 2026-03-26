"""検索バックエンド パッケージ.

プラガブルなバックエンド実装:
- VectorStoreBackend: ベクトルDB検索（既存RAGService統合）
- FileSystemBackend: ファイルシステム検索
- DatabaseBackend: Text2SQL連携データベース検索
- ExternalAPIBackend: REST API経由の外部検索
"""

from apps.faq_system.backend.mcp.backends.base import (
    BackendType,
    RetrievalBackend,
    RetrievalQuery,
    RetrievalResult,
    RetrievedDocument,
)


__all__ = [
    "BackendType",
    "RetrievalBackend",
    "RetrievalQuery",
    "RetrievalResult",
    "RetrievedDocument",
]
