"""EmbeddingProvider - 松耦合埋め込みベクトル生成.

Agent/サービスは具体的な埋め込みモデルを知る必要がありません。
環境変数から自動検出してEmbeddingインスタンスを提供します。

使用例:
    >>> from agentflow import get_embedding
    >>> emb = get_embedding()  # 環境変数から自動検出
    >>> vector = await emb.embed_text("Hello world")
    >>> vectors = await emb.embed_batch(["text1", "text2"])

環境変数優先順位:
    1. OPENAI_API_KEY → OpenAI text-embedding-3-small
    2. VOYAGE_API_KEY → Voyage AI
    3. COHERE_API_KEY → Cohere
    4. なし → SentenceTransformer（ローカル）
"""

import logging
from typing import TYPE_CHECKING, Protocol, runtime_checkable


if TYPE_CHECKING:
    from agentflow.runtime import RuntimeContext

logger = logging.getLogger(__name__)

# グローバルシングルトン
_embedding_instance: "EmbeddingProvider | None" = None


@runtime_checkable
class EmbeddingProvider(Protocol):
    """埋め込みプロバイダーの統一インターフェース."""

    async def embed_text(self, text: str) -> list[float]:
        """テキストをベクトルに変換."""
        ...

    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """複数テキストを一括変換."""
        ...

    def get_dimension(self) -> int:
        """ベクトル次元数を取得."""
        ...

    def get_model_name(self) -> str:
        """モデル名を取得."""
        ...


class MockEmbeddingProvider:
    """Mock Embedding Provider（開発・テスト用）.

    ランダムな固定次元ベクトルを生成。
    """

    def __init__(self, dimension: int = 384) -> None:
        """初期化."""
        self._dimension = dimension

    async def embed_text(self, text: str) -> list[float]:
        """簡易埋め込み（文字コードベース）."""
        # テキストのハッシュから擬似ベクトル生成
        import hashlib
        h = hashlib.sha256(text.encode()).digest()
        # 384次元に拡張
        vector = []
        for i in range(self._dimension):
            idx = i % len(h)
            vector.append((h[idx] - 128) / 128.0)
        return vector

    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """バッチ埋め込み."""
        return [await self.embed_text(t) for t in texts]

    def get_dimension(self) -> int:
        """次元数."""
        return self._dimension

    def get_model_name(self) -> str:
        """モデル名."""
        return "mock"


class OpenAIEmbeddingProvider:
    """OpenAI Embedding Provider."""

    def __init__(self, api_key: str, model: str = "text-embedding-3-small") -> None:
        """初期化."""
        self._api_key = api_key
        self._model = model
        self._client = None
        # text-embedding-3-large: 3072, 3-small: 1536, ada-002: 1536
        self._dimension = 3072 if "3-large" in model else 1536

    def _ensure_client(self) -> None:
        """クライアント初期化."""
        if self._client is None:
            try:
                from openai import AsyncOpenAI
                self._client = AsyncOpenAI(api_key=self._api_key)
            except ImportError:
                msg = "openai package required: pip install openai"
                raise ImportError(msg)

    async def embed_text(self, text: str) -> list[float]:
        """OpenAI 埋め込み."""
        self._ensure_client()
        response = await self._client.embeddings.create(
            model=self._model,
            input=text,
        )
        return response.data[0].embedding

    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """バッチ埋め込み."""
        self._ensure_client()
        response = await self._client.embeddings.create(
            model=self._model,
            input=texts,
        )
        return [item.embedding for item in response.data]

    def get_dimension(self) -> int:
        """次元数."""
        return self._dimension

    def get_model_name(self) -> str:
        """モデル名."""
        return self._model


class SentenceTransformerProvider:
    """SentenceTransformer Embedding Provider（ローカル）."""

    def __init__(self, model_name: str = "all-MiniLM-L6-v2") -> None:
        """初期化."""
        self._model_name = model_name
        self._model = None
        self._dimension = 384  # MiniLM default

    def _ensure_model(self) -> None:
        """モデル初期化."""
        if self._model is None:
            try:
                from sentence_transformers import SentenceTransformer
                self._model = SentenceTransformer(self._model_name)
                self._dimension = self._model.get_sentence_embedding_dimension()
                logger.info(f"Loaded SentenceTransformer: {self._model_name}")
            except ImportError:
                msg = "sentence-transformers required: pip install sentence-transformers"
                raise ImportError(msg)

    async def embed_text(self, text: str) -> list[float]:
        """ローカル埋め込み."""
        self._ensure_model()
        embedding = self._model.encode(text)
        return embedding.tolist()

    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """バッチ埋め込み."""
        self._ensure_model()
        embeddings = self._model.encode(texts)
        return [e.tolist() for e in embeddings]

    def get_dimension(self) -> int:
        """次元数."""
        return self._dimension

    def get_model_name(self) -> str:
        """モデル名."""
        return self._model_name


def get_embedding(
    model: str | None = None,
    *,
    context: "RuntimeContext | None" = None,
    _new_instance: bool = False,
) -> EmbeddingProvider:
    """埋め込みプロバイダーを取得（松耦合）.

    環境変数から自動検出して最適な埋め込みモデルを返します。
    Agent/サービスは具体的な実装を知る必要がありません。

    Args:
        model: モデル名（省略時は自動選択）
        context: RuntimeContext（テナント/設定の分離用）
        _new_instance: 新しいインスタンスを強制作成（テスト用）

    Returns:
        EmbeddingProvider インスタンス

    環境変数優先順位:
        1. OPENAI_API_KEY → OpenAI text-embedding-3-small
        2. VOYAGE_API_KEY → Voyage AI（TODO）
        3. USE_LOCAL_EMBEDDING → SentenceTransformer
        4. なし → Mock
    """
    global _embedding_instance

    if _embedding_instance is not None and model is None and context is None and not _new_instance:
        return _embedding_instance

    from agentflow.runtime import get_env, resolve_settings

    settings = resolve_settings(context) if context is not None else None

    # OpenAI
    openai_key = (
        settings.openai_api_key if settings else get_env("OPENAI_API_KEY", context=context)
    )
    if openai_key:
        emb_model = model
        if emb_model is None:
            emb_model = get_env("EMBEDDING_MODEL", context=context)
        if emb_model is None and settings is not None:
            emb_model = settings.openai_embedding_model
        if emb_model is None:
            emb_model = "text-embedding-3-small"
        logger.info(f"Using OpenAI embedding: {emb_model}")
        provider = OpenAIEmbeddingProvider(openai_key, emb_model)
        if context is None and not _new_instance:
            _embedding_instance = provider
        return provider

    # Voyage AI (TODO)
    if get_env("VOYAGE_API_KEY", context=context):
        logger.warning("Voyage AI detected but not implemented")

    # SentenceTransformer（ローカル）
    if get_env("USE_LOCAL_EMBEDDING", context=context):
        local_model = (
            model
            or get_env("LOCAL_EMBEDDING_MODEL", context=context)
            or "all-MiniLM-L6-v2"
        )
        logger.info(f"Using local embedding: {local_model}")
        provider = SentenceTransformerProvider(local_model)
        if context is None and not _new_instance:
            _embedding_instance = provider
        return provider

    # フォールバック: Mock
    logger.info("No embedding config found. Using mock provider.")
    provider = MockEmbeddingProvider()
    if context is None and not _new_instance:
        _embedding_instance = provider
    return provider


def reset_embedding() -> None:
    """Embeddingインスタンスをリセット（テスト用）."""
    global _embedding_instance
    _embedding_instance = None
