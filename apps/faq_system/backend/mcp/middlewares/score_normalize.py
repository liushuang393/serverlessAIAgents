"""スコア正規化ミドルウェア - 異なるバックエンドのスコアを統一スケールに変換.

バックエンドごとにスコアのスケールが異なるため、
0.0-1.0 の範囲に正規化して比較可能にする。

使用例:
    >>> pipeline.add_middleware(create_score_normalize_middleware(method="minmax"))
"""

from __future__ import annotations

import logging
from typing import Any

from apps.faq_system.backend.mcp.backends.base import (
    RetrievalResult,
    RetrievedDocument,
)


logger = logging.getLogger(__name__)


async def score_normalize_middleware(
    result: RetrievalResult,
    *,
    method: str = "minmax",
) -> RetrievalResult:
    """スコア正規化ミドルウェア本体.

    Args:
        result: パイプラインの検索結果
        method: 正規化方式（"minmax" / "rank"）

    Returns:
        スコア正規化済み RetrievalResult
    """
    if not result.has_results:
        return result

    if method == "rank":
        normalized = _rank_normalize(result.documents)
    else:
        normalized = _minmax_normalize(result.documents)

    metadata = {**result.metadata}
    metadata["score_normalization"] = method

    return RetrievalResult(
        documents=normalized,
        query=result.query,
        total_found=result.total_found,
        backend_type=result.backend_type,
        metadata=metadata,
    )


def _minmax_normalize(documents: list[RetrievedDocument]) -> list[RetrievedDocument]:
    """Min-Max 正規化（0.0-1.0 スケール）.

    Args:
        documents: ドキュメントリスト

    Returns:
        正規化済みドキュメントリスト
    """
    scores = [d.score for d in documents]
    min_score = min(scores)
    max_score = max(scores)
    score_range = max_score - min_score

    if score_range < 1e-9:
        # 全て同じスコアの場合
        return [
            RetrievedDocument(
                doc_id=d.doc_id,
                content=d.content,
                score=1.0,
                source=d.source,
                metadata={**d.metadata, "raw_score": d.score},
            )
            for d in documents
        ]

    return [
        RetrievedDocument(
            doc_id=d.doc_id,
            content=d.content,
            score=(d.score - min_score) / score_range,
            source=d.source,
            metadata={**d.metadata, "raw_score": d.score},
        )
        for d in documents
    ]


def _rank_normalize(documents: list[RetrievedDocument]) -> list[RetrievedDocument]:
    """ランクベース正規化（順位に基づく 0.0-1.0 スコア）.

    Args:
        documents: ドキュメントリスト（スコア降順想定）

    Returns:
        正規化済みドキュメントリスト
    """
    n = len(documents)
    return [
        RetrievedDocument(
            doc_id=d.doc_id,
            content=d.content,
            score=(n - i) / n,
            source=d.source,
            metadata={**d.metadata, "raw_score": d.score, "rank": i + 1},
        )
        for i, d in enumerate(documents)
    ]


def create_score_normalize_middleware(
    method: str = "minmax",
) -> Any:  # 理由: MiddlewareFn 型は Callable
    """スコア正規化ミドルウェアを生成するファクトリ.

    Args:
        method: 正規化方式（"minmax" / "rank"）

    Returns:
        Pipeline に追加可能なミドルウェア関数
    """

    async def _middleware(result: RetrievalResult) -> RetrievalResult:
        return await score_normalize_middleware(result, method=method)

    return _middleware
