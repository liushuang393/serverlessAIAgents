"""EmbeddingProvider - 松耦合埋め込みベクトル生成.

Agent/サービスは具体的な埋め込みモデルを知る必要がありません。
環境変数から自動検出してEmbeddingインスタンスを提供します。

使用例:
    >>> from agentflow import get_embedding
    >>> emb = get_embedding()  # 環境変数から自動検出
    >>> vector = await emb.embed_text("Hello world")
    >>> vectors = await emb.embed_batch(["text1", "text2"])

# =============================================================================
# 対応 Embedding プロバイダーと設定方法
# =============================================================================
#
# ── 1. Ollama（ローカル推奨） ──────────────────────────────────────────────
#   必要パッケージ : pip install httpx
#   Ollama インストール: https://ollama.com/download
#   モデル取得:
#     ollama pull nomic-embed-text:latest   # 768次元 ← FAQ デフォルト
#     ollama pull mxbai-embed-large:latest  # 1024次元
#     ollama pull all-minilm:latest         # 384次元（軽量）
#   .env 設定:
#     OLLAMA_EMBEDDING_MODEL=nomic-embed-text:latest
#     OLLAMA_BASE_URL=http://localhost:11434
#
# ── 2. OpenAI ────────────────────────────────────────────────────────────
#   必要パッケージ : pip install openai
#   .env 設定:
#     OPENAI_API_KEY=sk-...
#     OPENAI_EMBEDDING_MODEL=text-embedding-3-small  # 1536次元（省略可）
#     # 他の選択肢: text-embedding-3-large(3072次元), text-embedding-ada-002(1536次元)
#
# ── 3. SentenceTransformer（ローカル・オフライン） ────────────────────────
#   必要パッケージ : pip install sentence-transformers
#   .env 設定:
#     USE_LOCAL_EMBEDDING=1
#     LOCAL_EMBEDDING_MODEL=all-MiniLM-L6-v2  # 384次元（省略可）
#     # 他の選択肢: paraphrase-multilingual-MiniLM-L12-v2（多言語対応）
#
# ── 4. Mock（開発・テスト専用） ──────────────────────────────────────────
#   API キー不要。ランダム固定ベクトルを生成（RAG品質は無意味）。
#   上記のいずれも設定されていない場合に自動適用。
#
# 優先順位: Ollama > OpenAI > SentenceTransformer > Mock
# =============================================================================
"""

import logging
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable


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
        self._client: Any = None
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
        return [float(v) for v in response.data[0].embedding]

    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """バッチ埋め込み."""
        self._ensure_client()
        response = await self._client.embeddings.create(
            model=self._model,
            input=texts,
        )
        return [[float(v) for v in item.embedding] for item in response.data]

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
        self._model: Any = None
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
        return [float(v) for v in embedding.tolist()]

    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """バッチ埋め込み."""
        self._ensure_model()
        embeddings = self._model.encode(texts)
        return [[float(v) for v in e.tolist()] for e in embeddings]

    def get_dimension(self) -> int:
        """次元数."""
        return self._dimension

    def get_model_name(self) -> str:
        """モデル名."""
        return self._model_name


class OllamaEmbeddingProvider:
    """Ollama Embedding Provider（ローカル LLM サーバー）.

    ローカルで起動した Ollama サーバーの埋め込み API を使用。
    新仕様の /api/embed を優先し、未対応（404）の場合は旧 /api/embeddings へ自動フォールバックする。
    API キー不要・オフライン動作可能。

    対応モデルと次元数:
        - nomic-embed-text:latest  → 768次元  （FAQ デフォルト・推奨）
        - mxbai-embed-large:latest → 1024次元 （高精度）
        - all-minilm:latest        → 384次元  （軽量・高速）

    事前準備:
        1. Ollama インストール: https://ollama.com/download
        2. モデル取得: ollama pull nomic-embed-text:latest
        3. サーバー起動: ollama serve  （または systemd サービスで自動起動）

    .env 設定例:
        OLLAMA_EMBEDDING_MODEL=nomic-embed-text:latest
        OLLAMA_BASE_URL=http://localhost:11434
    """

    # 既知モデルの次元数テーブル（タグ除外したベース名で照合）
    _DIMENSION_MAP: dict[str, int] = {
        "nomic-embed-text": 768,
        "mxbai-embed-large": 1024,
        "all-minilm": 384,
    }

    def __init__(
        self,
        model: str = "nomic-embed-text:latest",
        base_url: str = "http://localhost:11434",
    ) -> None:
        """初期化.

        Args:
            model:    Ollama モデル名（タグ付き可）
            base_url: Ollama サーバー URL
        """
        self._model = model
        self._base_url = base_url.rstrip("/")
        # タグ（:latest 等）を除いた名前で次元数を解決
        base_name = model.split(":")[0]
        self._dimension = self._DIMENSION_MAP.get(base_name, 768)
        # None: 未判定, True: /api/embed 利用可, False: legacy /api/embeddings を使用
        self._supports_modern_endpoint: bool | None = None

    @staticmethod
    def _normalize_embeddings(data: Any) -> list[list[float]]:
        """Ollama 応答を list[list[float]] へ正規化する."""
        if not isinstance(data, dict):
            msg = "Invalid Ollama embedding response: expected JSON object"
            raise ValueError(msg)

        raw_batch = data.get("embeddings")
        if isinstance(raw_batch, list):
            normalized: list[list[float]] = []
            for item in raw_batch:
                if not isinstance(item, list):
                    msg = "Invalid Ollama embedding response: each embedding must be a list"
                    raise ValueError(msg)
                normalized.append([float(v) for v in item])
            return normalized

        raw_single = data.get("embedding")
        if isinstance(raw_single, list):
            return [[float(v) for v in raw_single]]

        msg = "Invalid Ollama embedding response: missing 'embeddings' or 'embedding'"
        raise ValueError(msg)

    @staticmethod
    def _extract_ollama_error(response: Any) -> str | None:
        """Ollama エラーレスポンス本文から message を抽出する."""
        try:
            payload = response.json()
        except ValueError:
            return None

        if not isinstance(payload, dict):
            return None

        error = payload.get("error")
        if isinstance(error, str) and error.strip():
            return error.strip()
        return None

    @classmethod
    def _is_model_not_found(cls, response: Any) -> bool:
        """404 がモデル未取得エラーか判定する."""
        detail = cls._extract_ollama_error(response)
        if detail is None:
            return False

        lowered = detail.lower()
        return "model" in lowered and "not found" in lowered

    async def _embed_batch_legacy(self, client: Any, texts: list[str]) -> list[list[float]]:
        """旧仕様 /api/embeddings をテキストごとに呼び出す."""
        embeddings: list[list[float]] = []
        for text in texts:
            response = await client.post(
                f"{self._base_url}/api/embeddings",
                json={"model": self._model, "prompt": text},
            )
            response.raise_for_status()
            data = response.json()
            normalized = self._normalize_embeddings(data)
            if len(normalized) != 1:
                msg = "Ollama /api/embeddings returned unexpected batch response"
                raise ValueError(msg)
            embeddings.append(normalized[0])
        return embeddings

    async def embed_text(self, text: str) -> list[float]:
        """1テキストを埋め込みベクトルに変換."""
        results = await self.embed_batch([text])
        return results[0]

    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """複数テキストを一括変換.

        Args:
            texts: 埋め込み対象テキストリスト

        Returns:
            各テキストの埋め込みベクトルリスト

        Raises:
            ImportError: httpx 未インストール時
            httpx.HTTPStatusError: Ollama サーバーエラー時
        """
        try:
            import httpx
        except ImportError:
            msg = "httpx required: pip install httpx"
            raise ImportError(msg)

        if not texts:
            return []

        async with httpx.AsyncClient(timeout=60.0) as client:
            if self._supports_modern_endpoint is not False:
                response = await client.post(
                    f"{self._base_url}/api/embed",
                    json={"model": self._model, "input": texts},
                )
                if response.status_code != 404:
                    response.raise_for_status()
                    data = response.json()
                    embeddings = self._normalize_embeddings(data)
                    if len(embeddings) != len(texts):
                        msg = (
                            f"Ollama /api/embed returned {len(embeddings)} embeddings "
                            f"for {len(texts)} inputs"
                        )
                        raise ValueError(msg)
                    self._supports_modern_endpoint = True
                    return embeddings

                if self._is_model_not_found(response):
                    detail = self._extract_ollama_error(response) or "model not found"
                    msg = (
                        f"Ollama embedding request failed: {detail}. "
                        f"Run `ollama pull {self._model}` or change OLLAMA_EMBEDDING_MODEL."
                    )
                    raise httpx.HTTPStatusError(msg, request=response.request, response=response)

                logger.warning(
                    "Ollama /api/embed returned 404. Falling back to legacy /api/embeddings."
                )

            embeddings = await self._embed_batch_legacy(client, texts)
            self._supports_modern_endpoint = False
            return embeddings

    def get_dimension(self) -> int:
        """ベクトル次元数."""
        return self._dimension

    def get_model_name(self) -> str:
        """モデル名."""
        return self._model


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
        model:        モデル名（省略時は環境変数から自動選択）
        context:      RuntimeContext（テナント/設定の分離用）
        _new_instance: 新しいインスタンスを強制作成（テスト用）

    Returns:
        EmbeddingProvider インスタンス

    環境変数優先順位:
        1. OLLAMA_EMBEDDING_MODEL → OllamaEmbeddingProvider（ローカル優先）
        2. OPENAI_API_KEY         → OpenAIEmbeddingProvider
        3. USE_LOCAL_EMBEDDING    → SentenceTransformerProvider（オフライン）
        4. なし（fallback）       → SentenceTransformerProvider all-MiniLM-L6-v2
    """
    global _embedding_instance

    if _embedding_instance is not None and model is None and context is None and not _new_instance:
        return _embedding_instance

    from agentflow.runtime import get_env, resolve_settings

    settings = resolve_settings(context) if context is not None else None

    provider: EmbeddingProvider

    # ── 1. Ollama（OLLAMA_EMBEDDING_MODEL が設定されていれば優先）──────────
    ollama_emb_model = model or get_env("OLLAMA_EMBEDDING_MODEL", context=context)
    if ollama_emb_model:
        base_url = (
            get_env("OLLAMA_BASE_URL", context=context)
            or (settings.ollama_base_url if settings else None)
            or "http://localhost:11434"
        )
        logger.info("Using Ollama embedding: %s @ %s", ollama_emb_model, base_url)
        provider = OllamaEmbeddingProvider(model=ollama_emb_model, base_url=base_url)
        if context is None and not _new_instance:
            _embedding_instance = provider
        return provider

    # ── 2. OpenAI（OPENAI_API_KEY が設定されていれば使用）────────────────
    openai_key = settings.openai_api_key if settings else get_env("OPENAI_API_KEY", context=context)
    if openai_key:
        emb_model = model or get_env("OPENAI_EMBEDDING_MODEL", context=context)
        if emb_model is None and settings is not None:
            emb_model = settings.openai_embedding_model
        emb_model = emb_model or "text-embedding-3-small"
        logger.info("Using OpenAI embedding: %s", emb_model)
        provider = OpenAIEmbeddingProvider(openai_key, emb_model)
        if context is None and not _new_instance:
            _embedding_instance = provider
        return provider

    # ── 3. SentenceTransformer（USE_LOCAL_EMBEDDING フラグ） ─────────────
    if get_env("USE_LOCAL_EMBEDDING", context=context):
        local_model = model or get_env("LOCAL_EMBEDDING_MODEL", context=context) or "all-MiniLM-L6-v2"
        logger.info("Using local SentenceTransformer embedding: %s", local_model)
        provider = SentenceTransformerProvider(local_model)
        if context is None and not _new_instance:
            _embedding_instance = provider
        return provider

    # ── 4. fallback: SentenceTransformer デフォルト ──────────────────────
    # API キー不要・外部接続不要。sentence-transformers が未インストールの場合は
    # ImportError が発生するので、その場合は pip install sentence-transformers を実行。
    logger.info("No embedding config found. Falling back to SentenceTransformer all-MiniLM-L6-v2.")
    provider = SentenceTransformerProvider("all-MiniLM-L6-v2")
    if context is None and not _new_instance:
        _embedding_instance = provider
    return provider


def reset_embedding() -> None:
    """Embeddingインスタンスをリセット（テスト用）."""
    global _embedding_instance
    _embedding_instance = None
