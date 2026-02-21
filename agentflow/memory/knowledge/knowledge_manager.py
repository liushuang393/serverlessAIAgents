"""知識管理器 - 統一入口.

長期知識記憶の統一管理インターフェース。
Spring Boot風の自動設定：環境に応じて最適なストアを自動選択。

設計原則:
- 松耦合: 呼び出し側はストア実装を意識しない
- 自動検出: memvid-sdk利用可能時は自動でMemvid使用
- 拡張性: カスタムストアの注入が可能
"""

from __future__ import annotations

import logging
from pathlib import Path
from typing import TYPE_CHECKING, Any

from agentflow.memory.knowledge.memory_store import InMemoryKnowledgeStore
from agentflow.memory.knowledge.memvid_store import (
    MemvidKnowledgeStore,
    is_memvid_available,
)
from agentflow.memory.knowledge.types import (
    KnowledgeEntry,
    KnowledgeSource,
    SearchResult,
    SearchType,
)


if TYPE_CHECKING:
    from agentflow.memory.knowledge.knowledge_store import KnowledgeStore


logger = logging.getLogger(__name__)


class KnowledgeManager:
    """知識管理器.

    長期知識記憶の統一入口。AI判断による保存・検索・更新・削除を支援。

    使用例（推奨）:
        >>> manager = KnowledgeManager()
        >>> await manager.start()
        >>> # 知識を追加
        >>> await manager.add("FAQ: よくある質問", tags=["faq"])
        >>> # 知識を検索
        >>> results = await manager.query("質問内容")
        >>> # 関連知識をプロンプトに組み込む
        >>> context = manager.format_context(results)
        >>> await manager.stop()

    使用例（カスタムストア）:
        >>> custom_store = MyCustomKnowledgeStore(...)
        >>> manager = KnowledgeManager(store=custom_store)
    """

    def __init__(
        self,
        store: KnowledgeStore | None = None,
        storage_path: str | Path = "memory/knowledge",
        use_memvid: bool = True,
        auto_persist: bool = True,
    ) -> None:
        """初期化.

        Args:
            store: カスタム知識ストア（Noneの場合は自動選択）
            storage_path: ストレージパス
            use_memvid: Memvidを使用（利用可能な場合）
            auto_persist: 自動永続化を有効化
        """
        self._storage_path = Path(storage_path)
        self._use_memvid = use_memvid
        self._auto_persist = auto_persist
        self._store: KnowledgeStore | None = store
        self._is_started = False

    async def start(self) -> None:
        """管理器を開始.

        ストアを初期化し、接続を確立。
        """
        if self._store is None:
            self._store = self._create_store()

        await self._store.connect()
        self._is_started = True
        logger.info(f"KnowledgeManager started with {self._store.get_provider_name()} store")

    def _create_store(self) -> KnowledgeStore:
        """ストアを自動選択して作成."""
        if self._use_memvid and is_memvid_available():
            logger.info("Using Memvid knowledge store (high-performance)")
            return MemvidKnowledgeStore(
                file_path=self._storage_path / "knowledge.mv2",
                auto_save=self._auto_persist,
            )

        # 回退: インメモリストア
        logger.info("Using InMemory knowledge store (fallback)")
        persist_path = self._storage_path / "knowledge.json" if self._auto_persist else None
        return InMemoryKnowledgeStore(
            persist_path=persist_path,
            auto_save=self._auto_persist,
        )

    async def stop(self) -> None:
        """管理器を停止.

        ストアを切断し、リソースを解放。
        """
        if self._store:
            await self._store.disconnect()
        self._is_started = False
        logger.info("KnowledgeManager stopped")

    async def add(
        self,
        content: str,
        title: str | None = None,
        tags: list[str] | None = None,
        source: KnowledgeSource = KnowledgeSource.DOCUMENT,
        source_id: str | None = None,
        importance: float = 0.5,
        metadata: dict[str, Any] | None = None,
    ) -> str:
        """知識を追加.

        AIが「保存すべき」と判断した情報を追加。

        Args:
            content: 知識の本文
            title: タイトル（省略時は本文から自動生成）
            tags: 検索タグ
            source: 出所タイプ
            source_id: 出所ID
            importance: 重要度（0.0-1.0）
            metadata: 追加メタデータ

        Returns:
            追加されたエントリのID
        """
        self._ensure_started()

        # タイトル自動生成
        effective_title = title or content[:50] + ("..." if len(content) > 50 else "")

        import uuid

        entry = KnowledgeEntry(
            id=str(uuid.uuid4()),
            title=effective_title,
            body=content,
            tags=tags or [],
            source=source,
            source_id=source_id,
            importance=importance,
            metadata=metadata or {},
        )

        return await self._store.store(entry)  # type: ignore[union-attr]

    async def add_batch(
        self,
        entries: list[dict[str, Any]],
    ) -> list[str]:
        """知識を一括追加.

        Args:
            entries: 知識エントリのリスト（辞書形式）

        Returns:
            追加されたエントリのIDリスト
        """
        self._ensure_started()

        knowledge_entries = []
        import uuid

        for entry_data in entries:
            entry = KnowledgeEntry(
                id=str(uuid.uuid4()),
                title=entry_data.get("title", entry_data.get("content", "")[:50]),
                body=entry_data.get("content", entry_data.get("body", "")),
                tags=entry_data.get("tags", []),
                source=KnowledgeSource(entry_data.get("source", "document")),
                source_id=entry_data.get("source_id"),
                importance=entry_data.get("importance", 0.5),
                metadata=entry_data.get("metadata", {}),
            )
            knowledge_entries.append(entry)

        return await self._store.store_batch(knowledge_entries)  # type: ignore[union-attr]

    async def query(
        self,
        query: str,
        top_k: int = 5,
        search_type: SearchType | None = None,
        tags: list[str] | None = None,
        min_score: float = 0.0,
    ) -> list[SearchResult]:
        """知識を検索.

        AIの応答生成前に関連知識を召喚。

        Args:
            query: 検索クエリ
            top_k: 最大取得件数
            search_type: 検索タイプ
            tags: タグフィルター
            min_score: 最小スコア閾値

        Returns:
            検索結果のリスト
        """
        self._ensure_started()
        return await self._store.search(  # type: ignore[union-attr]
            query=query,
            top_k=top_k,
            search_type=search_type,
            tags=tags,
            min_score=min_score,
        )

    async def get(self, entry_id: str) -> KnowledgeEntry | None:
        """IDで知識を取得.

        Args:
            entry_id: エントリID

        Returns:
            知識エントリ（存在しない場合はNone）
        """
        self._ensure_started()
        return await self._store.get(entry_id)  # type: ignore[union-attr]

    async def update(
        self,
        entry_id: str,
        content: str | None = None,
        title: str | None = None,
        tags: list[str] | None = None,
        importance: float | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> bool:
        """知識を更新.

        AIが「更新すべき」と判断した情報を更新。

        Args:
            entry_id: エントリID
            content: 新しい本文
            title: 新しいタイトル
            tags: 新しいタグ
            importance: 新しい重要度
            metadata: 新しいメタデータ

        Returns:
            更新成功の場合True
        """
        self._ensure_started()

        entry = await self._store.get(entry_id)  # type: ignore[union-attr]
        if not entry:
            return False

        if content is not None:
            entry.body = content
        if title is not None:
            entry.title = title
        if tags is not None:
            entry.tags = tags
        if importance is not None:
            entry.importance = importance
        if metadata is not None:
            entry.metadata.update(metadata)

        return await self._store.update(entry)  # type: ignore[union-attr]

    async def delete(self, entry_id: str) -> bool:
        """知識を削除.

        AIが「削除すべき」と判断した情報を削除。

        Args:
            entry_id: エントリID

        Returns:
            削除成功の場合True
        """
        self._ensure_started()
        return await self._store.delete(entry_id)  # type: ignore[union-attr]

    def format_context(
        self,
        results: list[SearchResult],
        max_length: int = 4000,
    ) -> str:
        """検索結果をプロンプト用コンテキストに整形.

        Args:
            results: 検索結果
            max_length: 最大文字数

        Returns:
            整形されたコンテキスト文字列
        """
        if not results:
            return ""

        lines = ["## 関連知識:\n"]
        total_length = len(lines[0])

        for i, result in enumerate(results, 1):
            entry = result.entry
            item = f"{i}. **{entry.title}**\n   {entry.body}\n"

            if total_length + len(item) > max_length:
                break

            lines.append(item)
            total_length += len(item)

        return "\n".join(lines)

    async def count(self) -> int:
        """知識エントリ数を取得."""
        self._ensure_started()
        return await self._store.count()  # type: ignore[union-attr]

    async def clear(self) -> int:
        """全知識をクリア."""
        self._ensure_started()
        return await self._store.clear()  # type: ignore[union-attr]

    def get_store(self) -> KnowledgeStore:
        """内部ストアを取得（上級者向け）."""
        self._ensure_started()
        return self._store  # type: ignore[return-value]

    def _ensure_started(self) -> None:
        """開始状態を確認."""
        if not self._is_started:
            msg = "Manager is not started. Call start() first."
            raise RuntimeError(msg)
