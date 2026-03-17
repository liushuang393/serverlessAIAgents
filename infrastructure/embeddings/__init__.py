"""Layer 1 の embeddings 公開 API.

各種埋め込みプロバイダー（Ollama, OpenAI, SentenceTransformer, Mock）を提供。
"""

from infrastructure.embeddings.provider import (
    EmbeddingProvider,
    GatewayContractEmbeddingProvider,
    MockEmbeddingProvider,
    OllamaEmbeddingProvider,
    OpenAIEmbeddingProvider,
    SentenceTransformerProvider,
    get_embedding,
    reset_embedding,
)
from infrastructure.embeddings.registry import EmbeddingBackendRegistry, get_embedding_backend


__all__ = [
    "EmbeddingBackendRegistry",
    "EmbeddingProvider",
    "GatewayContractEmbeddingProvider",
    "MockEmbeddingProvider",
    "OllamaEmbeddingProvider",
    "OpenAIEmbeddingProvider",
    "SentenceTransformerProvider",
    "get_embedding",
    "get_embedding_backend",
    "reset_embedding",
]
