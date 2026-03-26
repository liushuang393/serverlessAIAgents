"""重複排除ミドルウェア - Pipeline の検索結果から重複を除去.

複数バックエンドから同じドキュメントが返される場合に重複を排除。
コンテンツの類似度に基づく近似重複排除も可能。

使用例:
    >>> pipeline.add_middleware(create_dedup_middleware(similarity_threshold=0.9))
"""

from __future__ import annotations

import hashlib
import logging
from typing import Any

from apps.faq_system.backend.mcp.backends.base import (
    RetrievalResult,
    RetrievedDocument,
)


logger = logging.getLogger(__name__)


async def dedup_middleware(
    result: RetrievalResult,
    *,
    similarity_threshold: float = 0.95,
) -> RetrievalResult:
    """重複排除ミドルウェア本体.

    Args:
        result: パイプラインの検索結果
        similarity_threshold: コンテンツ類似度の閾値（0.0-1.0）

    Returns:
        重複排除済み RetrievalResult
    """
    if not result.has_results:
        return result

    seen_hashes: set[str] = set()
    unique_docs: list[RetrievedDocument] = []

    for doc in result.documents:
        # コンテンツハッシュで完全一致を排除
        content_hash = hashlib.sha256(doc.content.encode("utf-8")).hexdigest()
        if content_hash in seen_hashes:
            continue

        # 近似重複チェック（簡易版: 先頭 200 文字の一致率）
        is_near_dup = False
        for existing in unique_docs:
            sim = _simple_similarity(doc.content, existing.content)
            if sim >= similarity_threshold:
                is_near_dup = True
                break

        if not is_near_dup:
            seen_hashes.add(content_hash)
            unique_docs.append(doc)

    removed_count = len(result.documents) - len(unique_docs)
    metadata = {**result.metadata}
    metadata["dedup_removed"] = removed_count

    return RetrievalResult(
        documents=unique_docs,
        query=result.query,
        total_found=len(unique_docs),
        backend_type=result.backend_type,
        metadata=metadata,
    )


def _simple_similarity(text_a: str, text_b: str) -> float:
    """簡易テキスト類似度（Jaccard 係数ベース）.

    Args:
        text_a: テキスト A
        text_b: テキスト B

    Returns:
        類似度（0.0-1.0）
    """
    if not text_a or not text_b:
        return 0.0

    # 先頭 500 文字で比較（パフォーマンス対策）
    words_a = set(text_a[:500].lower().split())
    words_b = set(text_b[:500].lower().split())

    if not words_a or not words_b:
        return 0.0

    intersection = words_a & words_b
    union = words_a | words_b
    return len(intersection) / len(union)


def create_dedup_middleware(
    similarity_threshold: float = 0.95,
) -> Any:  # 理由: MiddlewareFn 型は Callable
    """重複排除ミドルウェアを生成するファクトリ.

    Args:
        similarity_threshold: コンテンツ類似度の閾値

    Returns:
        Pipeline に追加可能なミドルウェア関数
    """

    async def _middleware(result: RetrievalResult) -> RetrievalResult:
        return await dedup_middleware(result, similarity_threshold=similarity_threshold)

    return _middleware
