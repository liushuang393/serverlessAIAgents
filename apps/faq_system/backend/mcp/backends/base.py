"""検索バックエンド 抽象基底.

全ての検索バックエンドが実装すべき統一インターフェースを定義。
Agent は直接バックエンドを呼ばず、MCP Tool → Pipeline → Backend の順で呼ばれる。

使用例:
    >>> class MyBackend(RetrievalBackend):
    ...     async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
    ...         ...
    ...     async def health_check(self) -> bool:
    ...         return True
"""

from __future__ import annotations

import logging
from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from enum import StrEnum
from typing import Any


logger = logging.getLogger(__name__)


# =============================================================================
# 統一型定義
# =============================================================================


class BackendType(StrEnum):
    """バックエンド種別."""

    VECTOR_STORE = "vector_store"
    FILE_SYSTEM = "file_system"
    DATABASE = "database"
    EXTERNAL_API = "external_api"


@dataclass(frozen=True)
class RetrievalQuery:
    """統一検索クエリ.

    全バックエンドが受け取る共通クエリ型。
    バックエンド固有のパラメータは options に格納。
    """

    query: str
    top_k: int = 5
    filters: dict[str, Any] = field(default_factory=dict)
    options: dict[str, Any] = field(default_factory=dict)


@dataclass
class RetrievedDocument:
    """検索結果ドキュメント.

    全バックエンドが返す共通ドキュメント型。
    """

    doc_id: str
    content: str
    score: float = 0.0
    source: str = ""
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class RetrievalResult:
    """統一検索結果.

    全バックエンドが返す共通結果型。
    """

    documents: list[RetrievedDocument] = field(default_factory=list)
    query: str = ""
    total_found: int = 0
    backend_type: BackendType = BackendType.VECTOR_STORE
    metadata: dict[str, Any] = field(default_factory=dict)

    @property
    def has_results(self) -> bool:
        """結果があるかどうか."""
        return len(self.documents) > 0

    @property
    def best_score(self) -> float:
        """最高スコア."""
        if not self.documents:
            return 0.0
        return max(d.score for d in self.documents)


# =============================================================================
# 抽象基底クラス
# =============================================================================


class RetrievalBackend(ABC):
    """検索バックエンド 抽象基底.

    全ての検索バックエンドはこのクラスを継承し、
    retrieve() と health_check() を実装する。
    """

    def __init__(self, backend_type: BackendType, name: str = "") -> None:
        """初期化.

        Args:
            backend_type: バックエンド種別
            name: バックエンド名（ログ識別用）
        """
        self.backend_type = backend_type
        self.name = name or backend_type.value
        self._logger = logging.getLogger(f"{__name__}.{self.name}")

    @abstractmethod
    async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
        """検索実行.

        Args:
            query: 統一検索クエリ

        Returns:
            統一検索結果
        """

    @abstractmethod
    async def health_check(self) -> bool:
        """ヘルスチェック.

        Returns:
            利用可能な場合 True
        """

    async def initialize(self) -> None:
        """初期化（オプション）. サブクラスでオーバーライド可能."""
        self._logger.info("バックエンド初期化: %s", self.name)

    async def cleanup(self) -> None:
        """クリーンアップ（オプション）. サブクラスでオーバーライド可能."""
        self._logger.info("バックエンドクリーンアップ: %s", self.name)

    def __repr__(self) -> str:
        """文字列表現."""
        return f"{self.__class__.__name__}(type={self.backend_type.value}, name={self.name})"
