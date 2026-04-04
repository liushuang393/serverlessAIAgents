"""コンテキストプロバイダ — プラガブルなコンテキスト供給プロトコル.

外部コンテキストソースを ContextEngineer に注入するための
Protocol 定義とデータ型を提供する。

使用例:
    >>> class MyProvider:
    ...     @property
    ...     def name(self) -> str:
    ...         return "my_source"
    ...
    ...     async def provide(
    ...         self, query: str, budget: int,
    ...     ) -> list[ContextChunk]:
    ...         return [ContextChunk(
    ...             content="関連情報",
    ...             source="my_db",
    ...             token_count=10,
    ...             priority=5,
    ...         )]
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Protocol, runtime_checkable


@dataclass
class ContextChunk:
    """プロバイダが返すコンテキストの断片.

    Attributes:
        content: コンテキスト本文
        source: 情報の出所（例: "vector_db", "knowledge_graph"）
        token_count: 推定トークン数
        priority: 優先度（大きいほど高優先、デフォルト 0）
        metadata: 任意の付加情報
    """

    content: str
    source: str
    token_count: int
    priority: int = 0
    metadata: dict[str, Any] = field(default_factory=dict)


@runtime_checkable
class ContextProvider(Protocol):
    """プラガブルなコンテキスト供給プロトコル.

    ContextEngineer に追加コンテキストソースを注入するための
    構造的サブタイピング（Protocol）インターフェース。

    実装クラスは name プロパティと provide メソッドを持てばよく、
    明示的な継承は不要。
    """

    @property
    def name(self) -> str:
        """プロバイダの識別名."""
        ...

    async def provide(self, query: str, budget: int) -> list[ContextChunk]:
        """クエリに基づきコンテキストチャンクを返す.

        Args:
            query: ユーザークエリ
            budget: 残りトークン予算

        Returns:
            優先度順のコンテキストチャンクリスト
        """
        ...
