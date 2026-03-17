"""Gateway-backed embeddings implementation."""

import logging
import os

from infrastructure.llm.gateway import LiteLLMGateway
from infrastructure.storage.memory.embeddings.embedding_interface import EmbeddingEngine


class OpenAIEmbeddings(EmbeddingEngine):
    """OpenAI Embeddings実装.

    機能:
    - OpenAI APIを使用した高品質な埋め込み生成
    - text-embedding-3-small/large/ada-002 モデルをサポート
    - バッチ処理による効率的な埋め込み生成

    モデル:
    - text-embedding-3-small: 1536次元、高速、低コスト
    - text-embedding-3-large: 3072次元、高精度、高コスト
    - text-embedding-ada-002: 1536次元、旧モデル
    """

    def __init__(
        self,
        api_key: str,
        model: str = "text-embedding-3-small",
        dimension: int | None = None,
    ) -> None:
        """初期化.

        Args:
            api_key: OpenAI APIキー
            model: 使用するモデル名
            dimension: 埋め込みベクトルの次元数（Noneの場合はデフォルト）
        """
        self._api_key = api_key
        self._model = model
        self._logger = logging.getLogger(__name__)
        self._gateway = LiteLLMGateway()
        self._role = os.getenv("EMBEDDING_ROLE", "cheap")
        self._model_alias = os.getenv("OPENAI_EMBEDDING_MODEL_ALIAS")

        # モデルごとのデフォルト次元数
        self._default_dimensions = {
            "text-embedding-3-small": 1536,
            "text-embedding-3-large": 3072,
            "text-embedding-ada-002": 1536,
        }
        self._dimension: int = dimension if dimension is not None else self._default_dimensions.get(model, 1536)

    def _with_env_api_key(self) -> tuple[str | None, bool]:
        current = os.getenv("OPENAI_API_KEY")
        if current:
            return current, False
        if self._api_key:
            os.environ["OPENAI_API_KEY"] = self._api_key
            return None, True
        return None, False

    @staticmethod
    def _restore_env_api_key(previous: str | None, inserted: bool) -> None:
        if inserted:
            os.environ.pop("OPENAI_API_KEY", None)
            return
        if previous is not None:
            os.environ["OPENAI_API_KEY"] = previous

    async def embed_text(self, text: str) -> list[float]:
        """テキストをベクトル埋め込みに変換."""
        if not text or not text.strip():
            msg = "Text cannot be empty"
            raise ValueError(msg)

        try:
            previous, inserted = self._with_env_api_key()
            try:
                vectors = await self._gateway.embedding(
                    role=self._role,
                    input_texts=[text],
                    model_alias=self._model_alias,
                    model=self._model,
                )
            finally:
                self._restore_env_api_key(previous, inserted)
            if not vectors:
                msg = "Gateway returned no embeddings"
                raise RuntimeError(msg)
            embedding = vectors[0]
            self._logger.debug(f"Generated embedding for text (length: {len(text)})")
            return embedding

        except Exception as e:
            self._logger.exception(f"Failed to generate embedding: {e}")
            msg = f"Failed to generate embedding: {e}"
            raise RuntimeError(msg)

    async def embed_batch(self, texts: list[str]) -> list[list[float]]:
        """複数のテキストを一括変換."""
        if not texts:
            return []

        # 空のテキストをフィルタリング
        valid_texts = [t for t in texts if t and t.strip()]
        if not valid_texts:
            msg = "All texts are empty"
            raise ValueError(msg)

        try:
            previous, inserted = self._with_env_api_key()
            try:
                embeddings = await self._gateway.embedding(
                    role=self._role,
                    input_texts=valid_texts,
                    model_alias=self._model_alias,
                    model=self._model,
                )
            finally:
                self._restore_env_api_key(previous, inserted)
            self._logger.debug(f"Generated {len(embeddings)} embeddings")
            return embeddings

        except Exception as e:
            self._logger.exception(f"Failed to generate embeddings: {e}")
            msg = f"Failed to generate embeddings: {e}"
            raise RuntimeError(msg)

    def get_dimension(self) -> int:
        """埋め込みベクトルの次元数を取得."""
        return self._dimension

    def get_model_name(self) -> str:
        """モデル名を取得."""
        return self._model
