"""知識ストア型定義.

KnowledgeEntry, KnowledgeSource, SearchResult, SearchType を定義。
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import StrEnum
from typing import Any


class KnowledgeSource(StrEnum):
    """知識エントリのソース種別."""

    DOCUMENT = "document"
    FAQ = "faq"
    CONVERSATION = "conversation"
    WEB = "web"
    CODE = "code"
    MANUAL = "manual"


class SearchType(StrEnum):
    """検索タイプ."""

    SEMANTIC = "semantic"
    LEXICAL = "lexical"
    HYBRID = "hybrid"


@dataclass
class KnowledgeEntry:
    """知識エントリ.

    知識ストアに保存される1件のデータ。
    """

    id: str
    title: str
    body: str
    tags: list[str] = field(default_factory=list)
    source: KnowledgeSource = KnowledgeSource.DOCUMENT
    source_id: str = ""
    timestamp: datetime = field(default_factory=lambda: datetime.now(UTC))
    updated_at: datetime | None = None
    importance: float = 0.5
    metadata: dict[str, Any] = field(default_factory=dict)
    access_count: int = 0

    def record_access(self) -> None:
        """アクセス回数を記録."""
        self.access_count += 1

    def to_text(self) -> str:
        """検索用テキストに変換."""
        parts = [self.title, self.body]
        if self.tags:
            parts.append(" ".join(self.tags))
        return " ".join(parts)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "id": self.id,
            "title": self.title,
            "body": self.body,
            "tags": self.tags,
            "source": self.source.value,
            "source_id": self.source_id,
            "timestamp": self.timestamp.isoformat() if self.timestamp else None,
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "importance": self.importance,
            "metadata": self.metadata,
            "access_count": self.access_count,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> KnowledgeEntry:
        """辞書からKnowledgeEntryを生成."""
        source_val = data.get("source", "document")
        try:
            source = KnowledgeSource(source_val)
        except ValueError:
            source = KnowledgeSource.DOCUMENT

        timestamp = data.get("timestamp")
        if isinstance(timestamp, str):
            timestamp = datetime.fromisoformat(timestamp)
        elif timestamp is None:
            timestamp = datetime.now(UTC)

        updated_at = data.get("updated_at")
        if isinstance(updated_at, str):
            updated_at = datetime.fromisoformat(updated_at)

        return cls(
            id=data.get("id", ""),
            title=data.get("title", ""),
            body=data.get("body", ""),
            tags=data.get("tags", []),
            source=source,
            source_id=data.get("source_id", ""),
            timestamp=timestamp,
            updated_at=updated_at,
            importance=data.get("importance", 0.5),
            metadata=data.get("metadata", {}),
            access_count=data.get("access_count", 0),
        )


@dataclass
class SearchResult:
    """検索結果."""

    entry: KnowledgeEntry
    score: float = 0.0
    search_type: SearchType = SearchType.HYBRID
    snippet: str = ""


__all__ = [
    "KnowledgeEntry",
    "KnowledgeSource",
    "SearchResult",
    "SearchType",
]
