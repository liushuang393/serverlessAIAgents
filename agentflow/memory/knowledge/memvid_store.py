"""Memvid知識ストア実装.

Memvid（Rust製高性能RAG）を使用した長期知識記憶の実装。
単一の.mv2ファイルでセマンティック検索・全文検索・長期記憶を実現。

特徴:
- DBサーバー不要・オフライン動作可能
- BM25全文検索 + ベクトル意味検索（ハイブリッド）
- 高速・低メモリ消費

参照: https://github.com/memvid/memvid
Python SDK: https://pypi.org/project/memvid-sdk/
"""

from __future__ import annotations

import logging
import uuid
from datetime import UTC, datetime
from pathlib import Path
from typing import Any

from agentflow.memory.knowledge.types import (
    KnowledgeEntry,
    SearchResult,
    SearchType,
)


logger = logging.getLogger(__name__)

# Memvid SDK のインポート試行
_MEMVID_AVAILABLE = False
_memvid_create: Any = None
_memvid_use: Any = None

try:
    from memvid_sdk import create as memvid_create  # type: ignore[import-not-found]
    from memvid_sdk import use as memvid_use  # type: ignore[import-not-found]

    _memvid_create = memvid_create
    _memvid_use = memvid_use
    _MEMVID_AVAILABLE = True
except ImportError:
    logger.debug("memvid-sdk not installed. MemvidKnowledgeStore will be unavailable.")


def is_memvid_available() -> bool:
    """Memvidライブラリが利用可能かチェック.

    Returns:
        memvid-sdkがインストールされている場合True
    """
    return _MEMVID_AVAILABLE


class MemvidKnowledgeStore:
    """Memvid知識ストア.

    .mv2ファイルを使用した高性能長期知識記憶。

    使用例:
        >>> store = MemvidKnowledgeStore("memory/knowledge.mv2")
        >>> await store.connect()
        >>> await store.store(entry)
        >>> results = await store.search("質問", top_k=5)
        >>> await store.disconnect()
    """

    def __init__(
        self,
        file_path: str | Path = "memory/knowledge.mv2",
        auto_save: bool = True,
    ) -> None:
        """初期化.

        Args:
            file_path: .mv2ファイルのパス
            auto_save: 自動保存を有効化（disconnect時にsealを呼ぶ）
        """
        if not _MEMVID_AVAILABLE:
            msg = (
                "memvid-sdk is not installed. "
                "Install with: pip install memvid-sdk"
            )
            raise ImportError(msg)

        self._file_path = Path(file_path)
        self._auto_save = auto_save
        self._mv: Any = None  # memvid_sdk のインスタンス
        self._entries: dict[str, KnowledgeEntry] = {}
        self._is_connected = False

    async def connect(self) -> None:
        """ストアに接続.

        ファイルが存在する場合は読み込み、存在しない場合は新規作成。
        """
        self._file_path.parent.mkdir(parents=True, exist_ok=True)

        if self._file_path.exists():
            # 既存ファイルを開く（autoモード）
            self._mv = _memvid_use("basic", str(self._file_path), mode="open")
            logger.info(f"Memvid store opened: {self._file_path}")
        else:
            # 新規ファイルを作成
            self._mv = _memvid_create(str(self._file_path))
            logger.info(f"Memvid store created: {self._file_path}")

        self._is_connected = True

    async def disconnect(self) -> None:
        """ストアから切断."""
        if self._mv and self._auto_save:
            self._mv.seal()

        self._mv = None
        self._entries.clear()
        self._is_connected = False
        logger.info("Memvid store disconnected")

    async def store(self, entry: KnowledgeEntry) -> str:
        """知識エントリを保存.

        Args:
            entry: 保存する知識エントリ

        Returns:
            保存されたエントリのID
        """
        self._ensure_connected()

        if not entry.id:
            entry.id = str(uuid.uuid4())

        # memvid-sdk の put() API を使用
        # 参照: https://pypi.org/project/memvid-sdk/
        self._mv.put(
            title=entry.title,
            label=entry.source.value,
            text=entry.body,
            metadata={
                "id": entry.id,
                "tags": entry.tags,
                "source_id": entry.source_id,
                "timestamp": entry.timestamp.isoformat(),
                "importance": entry.importance,
                "access_count": entry.access_count,
                **entry.metadata,
            },
        )
        self._entries[entry.id] = entry

        logger.debug(f"Stored entry: {entry.id}")
        return entry.id

    async def store_batch(self, entries: list[KnowledgeEntry]) -> list[str]:
        """知識エントリを一括保存.

        Args:
            entries: 保存するエントリのリスト

        Returns:
            保存されたエントリIDのリスト
        """
        self._ensure_connected()

        # put_many() で一括保存（パフォーマンス向上）
        documents = []
        ids = []
        for entry in entries:
            if not entry.id:
                entry.id = str(uuid.uuid4())

            documents.append({
                "title": entry.title,
                "label": entry.source.value,
                "text": entry.body,
                "metadata": {
                    "id": entry.id,
                    "tags": entry.tags,
                    "source_id": entry.source_id,
                    "timestamp": entry.timestamp.isoformat(),
                    "importance": entry.importance,
                    "access_count": entry.access_count,
                    **entry.metadata,
                },
            })
            self._entries[entry.id] = entry
            ids.append(entry.id)

        if documents:
            self._mv.put_many(documents)

        return ids

    async def search(
        self,
        query: str,
        top_k: int = 5,
        search_type: SearchType | None = None,
        tags: list[str] | None = None,
        min_score: float = 0.0,
    ) -> list[SearchResult]:
        """知識を検索.

        Args:
            query: 検索クエリ
            top_k: 取得する結果数
            search_type: 検索タイプ（SEMANTIC/LEXICAL/HYBRID）
            tags: タグフィルター
            min_score: 最低スコア

        Returns:
            検索結果のリスト
        """
        self._ensure_connected()

        if not self._mv:
            return []

        effective_type = search_type or SearchType.HYBRID

        # memvid-sdk の find() API を使用
        # mode: "lex"（BM25）, "sem"（意味検索）, "auto"（ハイブリッド）
        mode_map = {
            SearchType.SEMANTIC: "sem",
            SearchType.LEXICAL: "lex",
            SearchType.HYBRID: "auto",
        }
        mode = mode_map.get(effective_type, "auto")

        raw_response = self._mv.find(query, k=top_k * 2, mode=mode)
        hits = raw_response.get("hits", [])

        results: list[SearchResult] = []
        for hit in hits:
            score = hit.get("score", 0.0)
            if score < min_score:
                continue

            # メタデータからエントリを復元
            metadata = hit.get("metadata", {})
            entry_id = metadata.get("id")

            if entry_id and entry_id in self._entries:
                entry = self._entries[entry_id]
            else:
                # キャッシュにない場合はhitから復元
                entry = KnowledgeEntry(
                    id=entry_id or str(uuid.uuid4()),
                    title=hit.get("title", ""),
                    body=hit.get("snippet", hit.get("text", "")),
                    tags=metadata.get("tags", []),
                    importance=metadata.get("importance", 0.5),
                )

            # タグフィルター
            if tags and not any(tag in entry.tags for tag in tags):
                continue

            entry.record_access()
            results.append(SearchResult(
                entry=entry,
                score=score,
                search_type=effective_type,
            ))

            if len(results) >= top_k:
                break

        return results

    async def get(self, entry_id: str) -> KnowledgeEntry | None:
        """IDで知識エントリを取得.

        Args:
            entry_id: エントリID

        Returns:
            エントリ（存在しない場合None）
        """
        self._ensure_connected()
        entry = self._entries.get(entry_id)
        if entry:
            entry.record_access()
        return entry

    async def update(self, entry: KnowledgeEntry) -> bool:
        """知識エントリを更新.

        注意: Memvidは追記型のため、更新は新規追加として扱われる。

        Args:
            entry: 更新するエントリ

        Returns:
            更新成功時True
        """
        self._ensure_connected()

        if entry.id not in self._entries:
            msg = f"Entry not found: {entry.id}"
            raise ValueError(msg)

        entry.updated_at = datetime.now(UTC)
        self._entries[entry.id] = entry

        # Memvidは追記型なので、更新は新規putとして扱う
        self._mv.put(
            title=entry.title,
            label=entry.source.value,
            text=entry.body,
            metadata={
                "id": entry.id,
                "tags": entry.tags,
                "source_id": entry.source_id,
                "timestamp": entry.timestamp.isoformat(),
                "updated_at": entry.updated_at.isoformat(),
                "importance": entry.importance,
                "access_count": entry.access_count,
                **entry.metadata,
            },
        )
        logger.debug(f"Updated entry: {entry.id}")
        return True

    async def delete(self, entry_id: str) -> bool:
        """知識エントリを削除.

        注意: Memvidは追記型のため、論理削除となる。

        Args:
            entry_id: 削除するエントリID

        Returns:
            削除成功時True
        """
        self._ensure_connected()

        if entry_id in self._entries:
            del self._entries[entry_id]
            logger.debug(f"Deleted entry (logical): {entry_id}")
            return True
        return False

    async def clear(self) -> int:
        """全知識エントリをクリア.

        注意: メモリ内キャッシュをクリアし、新規.mv2ファイルを作成する。

        Returns:
            クリアされたエントリ数
        """
        self._ensure_connected()
        count = len(self._entries)
        self._entries.clear()

        # 既存ファイルを閉じて新規作成
        if self._mv:
            self._mv.seal()
        self._mv = _memvid_create(str(self._file_path))

        logger.info(f"Cleared {count} entries")
        return count

    async def count(self) -> int:
        """知識エントリ数を取得.

        Returns:
            エントリ数
        """
        return len(self._entries)

    def get_provider_name(self) -> str:
        """プロバイダー名を取得.

        Returns:
            "memvid"
        """
        return "memvid"

    def _ensure_connected(self) -> None:
        """接続状態を確認.

        Raises:
            RuntimeError: 未接続の場合
        """
        if not self._is_connected:
            msg = "Store is not connected. Call connect() first."
            raise RuntimeError(msg)

