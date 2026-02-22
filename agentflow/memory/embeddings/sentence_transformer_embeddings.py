"""Sentence Transformers Embeddings実装.

Sentence Transformersライブラリを使用してテキストをベクトル埋め込みに変換します。
"""

import logging
from typing import Any, cast

from agentflow.memory.embeddings.embedding_interface import EmbeddingEngine


try:
    from sentence_transformers import SentenceTransformer
except ImportError:
    SentenceTransformer = None


class SentenceTransformerEmbeddings(EmbeddingEngine):
    """Sentence Transformers Embeddings実装.

    機能:
    - ローカルで動作する高品質な埋め込み生成
    - 多言語対応モデルをサポート
    - APIキー不要、オフライン動作可能

    推奨モデル:
    - all-MiniLM-L6-v2: 384次元、高速、英語
    - paraphrase-multilingual-MiniLM-L12-v2: 384次元、多言語
    - all-mpnet-base-v2: 768次元、高精度、英語
    """

    def __init__(
        self,
        model_name: str = "all-MiniLM-L6-v2",
        device: str | None = None,
    ) -> None:
        """初期化.

        Args:
            model_name: 使用するモデル名
            device: 使用するデバイス（'cpu', 'cuda', None=自動選択）
        """
        self._model_name = model_name
        self._device = device
        self._logger = logging.getLogger(__name__)
        self._model: Any = None
        self._dimension: int = 0

        # モデルを初期化
        if SentenceTransformer is None:
            msg = "sentence-transformers package is required. Install with: pip install sentence-transformers"
            raise ImportError(msg)
        self._model = SentenceTransformer(model_name, device=device)
        self._dimension = self._model.get_sentence_embedding_dimension()
        self._logger.info(f"Loaded Sentence Transformer model: {model_name} (dim={self._dimension})")

    async def embed_text(self, text: str) -> list[float]:
        """テキストをベクトル埋め込みに変換."""
        if not text or not text.strip():
            msg = "Text cannot be empty"
            raise ValueError(msg)

        try:
            # Sentence Transformersは同期APIなので、asyncio.to_threadで非同期化
            import asyncio

            embedding = await asyncio.to_thread(self._model.encode, text, convert_to_numpy=True)

            # numpy配列をリストに変換
            embedding_list = embedding.tolist()
            self._logger.debug(f"Generated embedding for text (length: {len(text)})")
            return cast("list[float]", embedding_list)

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
            # Sentence Transformersは同期APIなので、asyncio.to_threadで非同期化
            import asyncio

            embeddings = await asyncio.to_thread(
                self._model.encode, valid_texts, convert_to_numpy=True, show_progress_bar=False
            )

            # numpy配列をリストに変換
            embeddings_list = [emb.tolist() for emb in embeddings]
            self._logger.debug(f"Generated {len(embeddings_list)} embeddings")
            return embeddings_list

        except Exception as e:
            self._logger.exception(f"Failed to generate embeddings: {e}")
            msg = f"Failed to generate embeddings: {e}"
            raise RuntimeError(msg)

    def get_dimension(self) -> int:
        """埋め込みベクトルの次元数を取得."""
        return self._dimension

    def get_model_name(self) -> str:
        """モデル名を取得."""
        return self._model_name
