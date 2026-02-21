"""OpenAI Embeddings実装.

OpenAI APIを使用してテキストをベクトル埋め込みに変換します。
"""

import logging
from typing import Any

from agentflow.memory.embeddings.embedding_interface import EmbeddingEngine


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
        self._client: Any = None

        # モデルごとのデフォルト次元数
        self._default_dimensions = {
            "text-embedding-3-small": 1536,
            "text-embedding-3-large": 3072,
            "text-embedding-ada-002": 1536,
        }
        self._dimension: int = dimension if dimension is not None else self._default_dimensions.get(model, 1536)

        # OpenAIクライアントを初期化
        try:
            from openai import AsyncOpenAI

            self._client = AsyncOpenAI(api_key=api_key)
        except ImportError:
            msg = "openai package is required. Install with: pip install openai"
            raise ImportError(msg)

    async def embed_text(self, text: str) -> list[float]:
        """テキストをベクトル埋め込みに変換."""
        if not text or not text.strip():
            msg = "Text cannot be empty"
            raise ValueError(msg)

        try:
            response = await self._client.embeddings.create(
                model=self._model,
                input=text,
                dimensions=self._dimension,
            )

            embedding_raw = response.data[0].embedding
            embedding = [float(x) for x in embedding_raw]
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
            response = await self._client.embeddings.create(
                model=self._model,
                input=valid_texts,
                dimensions=self._dimension,
            )

            embeddings = [data.embedding for data in response.data]
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
