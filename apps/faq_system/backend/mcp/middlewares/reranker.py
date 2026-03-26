"""リランカーミドルウェア - Pipeline に検索結果リランキングを追加.

既存の shared/rag/reranker.py のリランカーシステム
（BM25 / Cohere / CrossEncoder / LLM Listwise）を
Pipeline ミドルウェアとしてラップ。

使用例:
    >>> from apps.faq_system.backend.mcp.middlewares.reranker import (
    ...     create_reranker_middleware,
    ... )
    >>> pipeline.add_middleware(create_reranker_middleware("bm25"))
    >>> pipeline.add_middleware(create_reranker_middleware("cohere", top_k=3))
"""

from __future__ import annotations

import logging
from typing import Any

from apps.faq_system.backend.mcp.backends.base import (
    RetrievalResult,
    RetrievedDocument,
)


logger = logging.getLogger(__name__)


async def rerank_middleware(
    result: RetrievalResult,
    *,
    reranker_type: str = "bm25",
    top_k: int = 5,
    reranker_options: dict[str, Any] | None = None,
) -> RetrievalResult:
    """リランカーミドルウェア本体.

    Pipeline の RetrievalResult を受け取り、
    既存の Reranker でリランキングした結果を返す。

    Args:
        result: パイプラインの検索結果
        reranker_type: リランカー種別（bm25 / cohere / cross_encoder / llm_listwise）
        top_k: リランク後の上位K件
        reranker_options: リランカー固有オプション

    Returns:
        リランク済み RetrievalResult
    """
    if not result.has_results:
        return result

    try:
        from shared.rag.reranker import get_reranker

        reranker = get_reranker(reranker_type, options=reranker_options)

        # RetrievedDocument → DocumentInput（dict 形式）に変換
        doc_inputs: list[str | dict[str, Any]] = [
            {
                "id": doc.doc_id,
                "content": doc.content,
                "source": doc.source,
                **doc.metadata,
            }
            for doc in result.documents
        ]

        # リランク実行
        ranked = await reranker.rerank(
            query=result.query,
            documents=doc_inputs,
            top_k=top_k,
        )

        # RankedDocument → RetrievedDocument に変換
        reranked_docs: list[RetrievedDocument] = []
        for ranked_doc in ranked:
            # original_index で元ドキュメントを参照
            idx = ranked_doc.original_index
            if 0 <= idx < len(result.documents):
                original = result.documents[idx]
                reranked_docs.append(
                    RetrievedDocument(
                        doc_id=original.doc_id,
                        content=original.content,
                        score=ranked_doc.score,
                        source=original.source,
                        metadata={
                            **original.metadata,
                            "original_score": original.score,
                            "reranker": reranker_type,
                        },
                    )
                )

        # メタデータにリランカー情報を追加
        metadata = {**result.metadata}
        metadata["reranker_type"] = reranker_type
        metadata["reranker_top_k"] = top_k
        metadata["pre_rerank_count"] = len(result.documents)
        metadata["post_rerank_count"] = len(reranked_docs)

        return RetrievalResult(
            documents=reranked_docs,
            query=result.query,
            total_found=len(reranked_docs),
            backend_type=result.backend_type,
            metadata=metadata,
        )

    except Exception:
        logger.exception("リランカーミドルウェアエラー（元の結果をそのまま返却）")
        return result


def create_reranker_middleware(
    reranker_type: str = "bm25",
    top_k: int = 5,
    reranker_options: dict[str, Any] | None = None,
) -> Any:  # 理由: MiddlewareFn 型は Callable で、ここでは部分適用関数を返す
    """リランカーミドルウェアを生成するファクトリ.

    Pipeline.add_middleware() に渡す非同期関数を返す。

    Args:
        reranker_type: リランカー種別
        top_k: リランク後の上位K件
        reranker_options: リランカー固有オプション

    Returns:
        Pipeline に追加可能なミドルウェア関数

    使用例:
        >>> pipeline.add_middleware(create_reranker_middleware("bm25", top_k=3))
        >>> pipeline.add_middleware(create_reranker_middleware("cohere", top_k=5))
    """

    async def _middleware(result: RetrievalResult) -> RetrievalResult:
        return await rerank_middleware(
            result,
            reranker_type=reranker_type,
            top_k=top_k,
            reranker_options=reranker_options,
        )

    return _middleware
