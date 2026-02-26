"""RAGBuilder - RAGContractConfig から RAGPipeline を構築するファクトリ.

contracts.rag 設定を読み込み、RAGPipeline インスタンスを生成する。
enabled=false や設定なしの場合は None を返す（Graceful Degradation）。

使用例:
    >>> from agentflow.bootstrap.rag_builder import build_rag_engine
    >>> engine = await build_rag_engine(rag_config_dict)
    >>> # engine is None if disabled, or RAGPipeline instance
"""

from __future__ import annotations

import logging
from typing import Any

from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)


class RagBootstrapConfig(BaseModel):
    """RAGビルダー用設定（contracts.rag から読み込み）.

    Attributes:
        enabled: RAG機能を有効化
        provider: VectorDB プロバイダ名
        collections: コレクション名リスト
        chunk_strategy: チャンク分割方式
        chunk_size: チャンクサイズ
        chunk_overlap: チャンク重複サイズ
        retrieval_method: 取得方式（hybrid / dense / sparse）
        embedding_model: 埋め込みモデル名
        rerank_model: リランクモデル名
        default_top_k: 既定TopK
        score_threshold: スコア閾値
    """

    enabled: bool = Field(default=False)
    provider: str | None = Field(default=None)
    collections: list[str] = Field(default_factory=list)
    chunk_strategy: str = Field(default="recursive")
    chunk_size: int = Field(default=800)
    chunk_overlap: int = Field(default=120)
    retrieval_method: str = Field(default="hybrid")
    embedding_model: str | None = Field(default=None)
    rerank_model: str | None = Field(default=None)
    default_top_k: int = Field(default=5)
    score_threshold: float | None = Field(default=None)


async def build_rag_engine(rag_config: dict[str, Any] | None) -> Any | None:
    """RAGContractConfig 辞書から RAGPipeline を構築.

    Args:
        rag_config: contracts.rag 設定辞書

    Returns:
        RAGPipeline インスタンス、または None（無効 / 設定なし）

    Graceful Degradation:
        - rag_config が None → None を返す
        - enabled=false → None を返す
        - RAGPipeline 初期化エラー → ログ警告して None を返す
    """
    # 設定なし → スキップ
    if rag_config is None:
        logger.debug("RAG設定なし: RAGEngine をスキップ")
        return None

    config = RagBootstrapConfig.model_validate(rag_config)

    # 明示的に無効 → スキップ
    if not config.enabled:
        logger.debug("RAG無効（enabled=false）: RAGEngine をスキップ")
        return None

    try:
        from agentflow.knowledge.rag_pipeline import RAGConfig, RAGPipeline

        collection_name = config.collections[0] if config.collections else "default"

        rag_cfg = RAGConfig(
            collection_name=collection_name,
            top_k=config.default_top_k,
            min_similarity=config.score_threshold or 0.3,
        )

        pipeline = RAGPipeline(config=rag_cfg)
        await pipeline.start()

        logger.info(
            "RAGPipeline 構築完了: collection=%s, top_k=%d",
            collection_name,
            config.default_top_k,
        )
        return pipeline

    except Exception as exc:
        logger.warning(
            "RAGPipeline 構築失敗（Graceful Degradation）: %s",
            exc,
            exc_info=True,
        )
        return None


__all__ = ["RagBootstrapConfig", "build_rag_engine"]
