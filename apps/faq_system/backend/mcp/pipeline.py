"""RetrievalPipeline - Sequential 処理パイプライン.

検索クエリを受け取り、複数の Backend / ミドルウェアを順次処理する。
ステップの追加・削除が容易な Chain パターン。

使用例:
    >>> pipeline = RetrievalPipeline()
    >>> pipeline.add_backend(vector_backend, priority=1)
    >>> pipeline.add_backend(file_backend, priority=2)
    >>> pipeline.add_middleware(dedup_middleware)
    >>> result = await pipeline.execute(query)
"""

from __future__ import annotations

import logging
from collections.abc import Callable, Coroutine
from dataclasses import dataclass, field
from typing import Any

from apps.faq_system.backend.mcp.backends.base import (
    RetrievalBackend,
    RetrievalQuery,
    RetrievalResult,
    RetrievedDocument,
)


logger = logging.getLogger(__name__)

# ミドルウェア型: RetrievalResult を受け取り、加工した RetrievalResult を返す
MiddlewareFn = Callable[[RetrievalResult], Coroutine[Any, Any, RetrievalResult]]


@dataclass
class PipelineStep:
    """パイプラインステップ.

    Attributes:
        backend: 検索バックエンド
        priority: 優先度（小さいほど先に実行）
        enabled: 有効フラグ
    """

    backend: RetrievalBackend
    priority: int = 0
    enabled: bool = True


class RetrievalPipeline:
    """Sequential 検索パイプライン.

    複数バックエンドを順次/並列で実行し、結果を統合する。
    ミドルウェアで後処理（重複排除、スコア正規化等）を追加可能。
    """

    def __init__(self, name: str = "default") -> None:
        """初期化.

        Args:
            name: パイプライン名
        """
        self._name = name
        self._steps: list[PipelineStep] = []
        self._middlewares: list[MiddlewareFn] = []
        self._logger = logging.getLogger(f"{__name__}.{name}")

    def add_backend(
        self,
        backend: RetrievalBackend,
        priority: int = 0,
        enabled: bool = True,
    ) -> RetrievalPipeline:
        """バックエンドを追加.

        Args:
            backend: 検索バックエンド
            priority: 優先度（小さいほど先に実行）
            enabled: 有効フラグ

        Returns:
            self（チェーン呼び出し用）
        """
        self._steps.append(PipelineStep(backend=backend, priority=priority, enabled=enabled))
        self._steps.sort(key=lambda s: s.priority)
        return self

    def add_middleware(self, middleware: MiddlewareFn) -> RetrievalPipeline:
        """ミドルウェアを追加（実行順序は追加順）.

        Args:
            middleware: 結果を加工する非同期関数

        Returns:
            self（チェーン呼び出し用）
        """
        self._middlewares.append(middleware)
        return self

    async def execute(
        self,
        query: RetrievalQuery,
        *,
        merge: bool = True,
    ) -> RetrievalResult:
        """パイプラインを実行.

        Args:
            query: 統一検索クエリ
            merge: 複数バックエンドの結果をマージするか

        Returns:
            統一検索結果
        """
        active_steps = [s for s in self._steps if s.enabled]
        if not active_steps:
            self._logger.warning("有効なバックエンドなし")
            return RetrievalResult(query=query.query)

        all_docs: list[RetrievedDocument] = []
        metadata: dict[str, Any] = {"pipeline": self._name, "backends_used": []}

        for step in active_steps:
            try:
                result = await step.backend.retrieve(query)
                all_docs.extend(result.documents)
                metadata["backends_used"].append(step.backend.name)
                self._logger.debug(
                    "バックエンド %s: %d 件取得",
                    step.backend.name,
                    len(result.documents),
                )
            except Exception:
                self._logger.exception("バックエンド %s でエラー", step.backend.name)

        # スコア降順ソート → top_k 件
        all_docs.sort(key=lambda d: -d.score)
        all_docs = all_docs[: query.top_k]

        merged = RetrievalResult(
            documents=all_docs,
            query=query.query,
            total_found=len(all_docs),
            metadata=metadata,
        )

        # ミドルウェア適用
        for mw in self._middlewares:
            try:
                merged = await mw(merged)
            except Exception:
                self._logger.exception("ミドルウェアでエラー")

        return merged

    async def initialize_all(self) -> None:
        """全バックエンドを初期化."""
        for step in self._steps:
            await step.backend.initialize()

    async def cleanup_all(self) -> None:
        """全バックエンドをクリーンアップ."""
        for step in self._steps:
            await step.backend.cleanup()

