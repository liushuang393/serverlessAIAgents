"""Web search provider 抽象."""

from __future__ import annotations

from typing import Protocol

from pydantic import Field

from contracts.base import ContractModel


class SearchItem(ContractModel):
    """検索候補."""

    url: str = Field(..., description="候補 URL")
    title: str | None = Field(default=None, description="タイトル")
    snippet: str | None = Field(default=None, description="スニペット")
    score: float = Field(default=0.0, ge=0.0, le=1.0, description="初期スコア")
    metadata: dict[str, str] = Field(default_factory=dict, description="追加メタ情報")


class SearchProvider(Protocol):
    """検索 provider 契約."""

    name: str

    async def search(self, query: str, *, top_k: int = 5) -> list[SearchItem]:
        """クエリから候補 URL 群を返す."""
