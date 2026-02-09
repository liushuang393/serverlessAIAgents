"""インメモリ知識ストア実装.

memvid-sdkが利用できない場合の軽量回退実装。
開発・テスト環境向け。

特徴:
- 外部依存なし
- シンプルなBM25全文検索
- メモリ内ストレージ（永続化なし）
"""

from __future__ import annotations

import json
import logging
import math
import re
import uuid
from collections import Counter
from datetime import UTC, datetime
from pathlib import Path

from agentflow.memory.knowledge.types import (
    KnowledgeEntry,
    SearchResult,
    SearchType,
)


logger = logging.getLogger(__name__)


class InMemoryKnowledgeStore:
    """インメモリ知識ストア.

    軽量な回退実装。BM25アルゴリズムによる全文検索をサポート。

    使用例:
        >>> store = InMemoryKnowledgeStore()
        >>> await store.connect()
        >>> await store.store(entry)
        >>> results = await store.search("質問", top_k=5)
        >>> await store.disconnect()
    """

    # BM25パラメータ
    _BM25_K1 = 1.5
    _BM25_B = 0.75

    def __init__(
        self,
        persist_path: str | Path | None = None,
        auto_save: bool = True,
    ) -> None:
        """初期化.

        Args:
            persist_path: 永続化ファイルパス（オプション）
            auto_save: 自動保存を有効化
        """
        self._persist_path = Path(persist_path) if persist_path else None
        self._auto_save = auto_save
        self._entries: dict[str, KnowledgeEntry] = {}
        self._is_connected = False
        self._dirty = False

        # BM25インデックス
        self._doc_freq: Counter[str] = Counter()
        self._doc_lengths: dict[str, int] = {}
        self._avg_doc_length = 0.0

    async def connect(self) -> None:
        """ストアに接続."""
        if self._persist_path and self._persist_path.exists():
            await self._load()
        self._is_connected = True
        logger.info("InMemory knowledge store connected")

    async def _load(self) -> None:
        """永続化ファイルから読み込み."""
        if not self._persist_path:
            return

        try:
            with self._persist_path.open("r", encoding="utf-8") as f:
                data = json.load(f)

            for entry_data in data.get("entries", []):
                entry = KnowledgeEntry.from_dict(entry_data)
                self._entries[entry.id] = entry
                self._index_entry(entry)

            logger.info(f"Loaded {len(self._entries)} entries from {self._persist_path}")
        except Exception as e:
            logger.warning(f"Failed to load: {e}")

    async def disconnect(self) -> None:
        """ストアから切断."""
        if self._dirty and self._auto_save and self._persist_path:
            await self._save()

        self._is_connected = False
        logger.info("InMemory knowledge store disconnected")

    async def _save(self) -> None:
        """永続化ファイルに保存."""
        if not self._persist_path:
            return

        self._persist_path.parent.mkdir(parents=True, exist_ok=True)
        data = {
            "entries": [e.to_dict() for e in self._entries.values()],
            "saved_at": datetime.now(UTC).isoformat(),
        }

        with self._persist_path.open("w", encoding="utf-8") as f:
            json.dump(data, f, ensure_ascii=False, indent=2)

        self._dirty = False
        logger.debug(f"Saved {len(self._entries)} entries to {self._persist_path}")

    def _tokenize(self, text: str) -> list[str]:
        """テキストをトークン化."""
        # シンプルな正規表現トークナイザー
        text = text.lower()
        return re.findall(r"\w+", text)

    def _index_entry(self, entry: KnowledgeEntry) -> None:
        """エントリをインデックスに追加."""
        text = entry.to_text()
        tokens = self._tokenize(text)

        # 文書長を記録
        self._doc_lengths[entry.id] = len(tokens)

        # 文書頻度を更新
        unique_tokens = set(tokens)
        for token in unique_tokens:
            self._doc_freq[token] += 1

        # 平均文書長を更新
        if self._doc_lengths:
            self._avg_doc_length = sum(self._doc_lengths.values()) / len(self._doc_lengths)

    def _remove_from_index(self, entry: KnowledgeEntry) -> None:
        """エントリをインデックスから削除."""
        text = entry.to_text()
        tokens = self._tokenize(text)

        # 文書長を削除
        if entry.id in self._doc_lengths:
            del self._doc_lengths[entry.id]

        # 文書頻度を更新
        unique_tokens = set(tokens)
        for token in unique_tokens:
            if self._doc_freq[token] > 0:
                self._doc_freq[token] -= 1

    def _bm25_score(self, query_tokens: list[str], entry: KnowledgeEntry) -> float:
        """BM25スコアを計算."""
        text = entry.to_text()
        doc_tokens = self._tokenize(text)
        doc_length = len(doc_tokens)
        term_freq = Counter(doc_tokens)

        n_docs = len(self._entries)
        if n_docs == 0:
            return 0.0

        score = 0.0
        for term in query_tokens:
            if term not in term_freq:
                continue

            tf = term_freq[term]
            df = self._doc_freq.get(term, 0)

            # IDF計算
            idf = math.log((n_docs - df + 0.5) / (df + 0.5) + 1)

            # TF正規化
            numerator = tf * (self._BM25_K1 + 1)
            denominator = tf + self._BM25_K1 * (
                1 - self._BM25_B + self._BM25_B * doc_length / max(self._avg_doc_length, 1)
            )

            score += idf * numerator / denominator

        return score

    async def store(self, entry: KnowledgeEntry) -> str:
        """知識エントリを保存."""
        self._ensure_connected()

        if not entry.id:
            entry.id = str(uuid.uuid4())

        self._entries[entry.id] = entry
        self._index_entry(entry)
        self._dirty = True

        logger.debug(f"Stored entry: {entry.id}")
        return entry.id

    async def store_batch(self, entries: list[KnowledgeEntry]) -> list[str]:
        """知識エントリを一括保存."""
        ids = []
        for entry in entries:
            entry_id = await self.store(entry)
            ids.append(entry_id)
        return ids

    async def search(
        self,
        query: str,
        top_k: int = 5,
        search_type: SearchType | None = None,
        tags: list[str] | None = None,
        min_score: float = 0.0,
    ) -> list[SearchResult]:
        """知識を検索."""
        self._ensure_connected()

        if not self._entries:
            return []

        # BM25検索（インメモリは全文検索のみ）
        query_tokens = self._tokenize(query)

        scored_entries: list[tuple[KnowledgeEntry, float]] = []
        for entry in self._entries.values():
            # タグフィルター
            if tags and not any(tag in entry.tags for tag in tags):
                continue

            score = self._bm25_score(query_tokens, entry)
            if score >= min_score:
                scored_entries.append((entry, score))

        # スコア降順でソート
        scored_entries.sort(key=lambda x: x[1], reverse=True)

        results: list[SearchResult] = []
        for entry, score in scored_entries[:top_k]:
            entry.record_access()
            results.append(SearchResult(
                entry=entry,
                score=score,
                search_type=SearchType.LEXICAL,
            ))

        return results

    async def get(self, entry_id: str) -> KnowledgeEntry | None:
        """IDで知識エントリを取得."""
        self._ensure_connected()
        entry = self._entries.get(entry_id)
        if entry:
            entry.record_access()
        return entry

    async def update(self, entry: KnowledgeEntry) -> bool:
        """知識エントリを更新."""
        self._ensure_connected()

        if entry.id not in self._entries:
            msg = f"Entry not found: {entry.id}"
            raise ValueError(msg)

        old_entry = self._entries[entry.id]
        self._remove_from_index(old_entry)

        entry.updated_at = datetime.now(UTC)
        self._entries[entry.id] = entry
        self._index_entry(entry)
        self._dirty = True

        logger.debug(f"Updated entry: {entry.id}")
        return True

    async def delete(self, entry_id: str) -> bool:
        """知識エントリを削除."""
        self._ensure_connected()

        if entry_id in self._entries:
            entry = self._entries[entry_id]
            self._remove_from_index(entry)
            del self._entries[entry_id]
            self._dirty = True
            logger.debug(f"Deleted entry: {entry_id}")
            return True
        return False

    async def clear(self) -> int:
        """全知識エントリをクリア."""
        self._ensure_connected()
        count = len(self._entries)
        self._entries.clear()
        self._doc_freq.clear()
        self._doc_lengths.clear()
        self._avg_doc_length = 0.0
        self._dirty = True
        logger.info(f"Cleared {count} entries")
        return count

    async def count(self) -> int:
        """知識エントリ数を取得."""
        return len(self._entries)

    def get_provider_name(self) -> str:
        """プロバイダー名を取得."""
        return "memory"

    def _ensure_connected(self) -> None:
        """接続状態を確認."""
        if not self._is_connected:
            msg = "Store is not connected. Call connect() first."
            raise RuntimeError(msg)

