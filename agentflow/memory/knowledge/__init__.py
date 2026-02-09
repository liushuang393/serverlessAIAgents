"""AgentFlow 長期知識記憶システム.

Memvid技術に基づく高性能長期記憶実装。
AI がセッションをまたいで記憶を保持し、FAQ/ナレッジを活用した応答が可能。

特徴:
- 単一ファイル(.mv2)で意味検索・全文検索・長期記憶を実現
- DBサーバー不要・オフライン動作可能
- BM25全文検索 + ベクトル意味検索（ハイブリッド）

使用例（推奨 - 松耦合API）:
    >>> from agentflow.memory.knowledge import get_knowledge_manager
    >>>
    >>> manager = get_knowledge_manager()
    >>> await manager.start()
    >>>
    >>> # 知識を追加
    >>> await manager.add("FAQ: よくある質問の回答", tags=["faq"])
    >>>
    >>> # 知識を検索
    >>> results = await manager.query("質問内容")
    >>>
    >>> # 関連知識をプロンプトに組み込む
    >>> context = manager.format_context(results)
    >>>
    >>> await manager.stop()

使用例（カスタムストア）:
    >>> from agentflow.memory.knowledge import KnowledgeManager, InMemoryKnowledgeStore
    >>>
    >>> custom_store = InMemoryKnowledgeStore()
    >>> manager = KnowledgeManager(store=custom_store)

環境変数:
    - KNOWLEDGE_BACKEND: ストアバックエンド (auto/memvid/memory)
    - KNOWLEDGE_STORAGE_PATH: ストレージパス
    - KNOWLEDGE_AUTO_PERSIST: 自動永続化 (true/false)
"""

import logging
from pathlib import Path
from typing import Any

from agentflow.memory.knowledge.knowledge_manager import KnowledgeManager
from agentflow.memory.knowledge.knowledge_store import KnowledgeStore
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


logger = logging.getLogger(__name__)

# グローバルシングルトン
_default_manager: KnowledgeManager | None = None


def get_knowledge_manager(
    storage_path: str | Path | None = None,
    use_memvid: bool | None = None,
    auto_persist: bool | None = None,
    _new_instance: bool = False,
) -> KnowledgeManager:
    """知識管理器を取得（松耦合API）.

    環境変数から自動設定し、最適なストアを選択。

    Args:
        storage_path: ストレージパス（Noneの場合は設定から）
        use_memvid: Memvidを使用（Noneの場合は自動判定）
        auto_persist: 自動永続化
        _new_instance: 新しいインスタンスを生成（テスト用）

    Returns:
        KnowledgeManager インスタンス

    使用例:
        >>> manager = get_knowledge_manager()
        >>> await manager.start()
        >>> await manager.add("知識内容", tags=["tag1"])
        >>> results = await manager.query("検索クエリ")
    """
    global _default_manager

    # カスタムパラメータがある場合は新しいインスタンス
    if _new_instance or storage_path is not None or use_memvid is not None or auto_persist is not None:
        return _create_manager(storage_path, use_memvid, auto_persist)

    # シングルトン
    if _default_manager is None:
        _default_manager = _create_manager(None, None, None)

    return _default_manager


def _create_manager(
    storage_path: str | Path | None,
    use_memvid: bool | None,
    auto_persist: bool | None,
) -> KnowledgeManager:
    """設定に基づいてManagerを生成."""
    from agentflow.config import get_settings

    settings = get_settings()
    config = settings.get_knowledge_config()

    effective_path = storage_path or config["storage_path"]
    effective_persist = auto_persist if auto_persist is not None else config["auto_persist"]

    # バックエンド選択
    backend = config["backend"]
    if use_memvid is not None:
        effective_memvid = use_memvid
    elif backend == "memvid":
        effective_memvid = True
    elif backend == "memory":
        effective_memvid = False
    else:  # auto
        effective_memvid = is_memvid_available()

    return KnowledgeManager(
        storage_path=effective_path,
        use_memvid=effective_memvid,
        auto_persist=effective_persist,
    )


def reset_knowledge_manager() -> None:
    """知識管理器シングルトンをリセット（テスト用）."""
    global _default_manager
    _default_manager = None


def get_knowledge_store(
    backend: str = "auto",
    storage_path: str | Path = "memory/knowledge",
    auto_save: bool = True,
) -> KnowledgeStore:
    """知識ストアを直接取得（上級者向け）.

    Args:
        backend: バックエンド (auto/memvid/memory)
        storage_path: ストレージパス
        auto_save: 自動保存

    Returns:
        KnowledgeStore インスタンス
    """
    path = Path(storage_path)

    if backend == "memvid" or (backend == "auto" and is_memvid_available()):
        return MemvidKnowledgeStore(
            file_path=path / "knowledge.mv2",
            auto_save=auto_save,
        )

    return InMemoryKnowledgeStore(
        persist_path=path / "knowledge.json" if auto_save else None,
        auto_save=auto_save,
    )


__all__ = [
    "InMemoryKnowledgeStore",
    # 型定義
    "KnowledgeEntry",
    # マネージャー
    "KnowledgeManager",
    "KnowledgeSource",
    # ストアインターフェース
    "KnowledgeStore",
    # ストア実装
    "MemvidKnowledgeStore",
    "SearchResult",
    "SearchType",
    # 主要API（推奨）
    "get_knowledge_manager",
    "get_knowledge_store",
    "is_memvid_available",
    "reset_knowledge_manager",
]

