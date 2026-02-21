"""知識ストア - 型定義.

長期知識記憶システムのデータ型定義。
Memvid技術に基づく高性能RAG記憶層をサポート。

設計原則:
- 明確な構造化データ
- 検索最適化フィールド
- メタデータ拡張性
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any


class KnowledgeSource(str, Enum):
    """知識の出所タイプ.

    - FAQ: FAQ/問答データ
    - DOCUMENT: ドキュメント/マニュアル
    - USER_PREFERENCE: ユーザー好み/設定
    - CONVERSATION: 会話履歴から抽出
    - EXTERNAL: 外部ソース
    """

    FAQ = "faq"
    DOCUMENT = "document"
    USER_PREFERENCE = "user_preference"
    CONVERSATION = "conversation"
    EXTERNAL = "external"


class SearchType(str, Enum):
    """検索タイプ.

    - SEMANTIC: 意味検索（ベクトル類似度）
    - LEXICAL: 全文検索（BM25）
    - HYBRID: ハイブリッド検索（意味+全文）
    """

    SEMANTIC = "semantic"
    LEXICAL = "lexical"
    HYBRID = "hybrid"


@dataclass
class KnowledgeEntry:
    """知識エントリ.

    長期記憶に保存される知識単位の構造。

    Attributes:
        id: 一意識別子
        title: タイトル/識別名
        body: 本体内容
        tags: 検索タグ（キーワード）
        source: 出所タイプ
        source_id: 出所の識別子（例：FAQ ID）
        timestamp: 作成日時
        updated_at: 更新日時
        importance: 重要度スコア（0.0-1.0）
        access_count: アクセス回数
        metadata: 追加メタデータ
    """

    id: str
    title: str
    body: str
    tags: list[str] = field(default_factory=list)
    source: KnowledgeSource = KnowledgeSource.DOCUMENT
    source_id: str | None = None
    timestamp: datetime = field(default_factory=lambda: datetime.now(UTC))
    updated_at: datetime | None = None
    importance: float = 0.5
    access_count: int = 0
    metadata: dict[str, Any] = field(default_factory=dict)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換.

        Returns:
            辞書形式の知識エントリ
        """
        return {
            "id": self.id,
            "title": self.title,
            "body": self.body,
            "tags": self.tags,
            "source": self.source.value,
            "source_id": self.source_id,
            "timestamp": self.timestamp.isoformat(),
            "updated_at": self.updated_at.isoformat() if self.updated_at else None,
            "importance": self.importance,
            "access_count": self.access_count,
            "metadata": self.metadata,
        }

    @classmethod
    def from_dict(cls, data: dict[str, Any]) -> KnowledgeEntry:
        """辞書から生成.

        Args:
            data: 辞書データ

        Returns:
            KnowledgeEntryインスタンス
        """
        return cls(
            id=data["id"],
            title=data["title"],
            body=data["body"],
            tags=data.get("tags", []),
            source=KnowledgeSource(data.get("source", "document")),
            source_id=data.get("source_id"),
            timestamp=datetime.fromisoformat(data["timestamp"])
            if isinstance(data.get("timestamp"), str)
            else data.get("timestamp", datetime.now(UTC)),
            updated_at=datetime.fromisoformat(data["updated_at"]) if data.get("updated_at") else None,
            importance=data.get("importance", 0.5),
            access_count=data.get("access_count", 0),
            metadata=data.get("metadata", {}),
        )

    def record_access(self) -> None:
        """アクセスを記録."""
        self.access_count += 1

    def to_text(self) -> str:
        """検索用テキストに変換.

        Returns:
            タイトル + 本文 + タグの結合テキスト
        """
        parts = [self.title, self.body]
        if self.tags:
            parts.append(" ".join(self.tags))
        return "\n".join(parts)


@dataclass
class SearchResult:
    """検索結果.

    Attributes:
        entry: 知識エントリ
        score: 類似度/関連度スコア
        search_type: 使用された検索タイプ
    """

    entry: KnowledgeEntry
    score: float
    search_type: SearchType = SearchType.HYBRID
