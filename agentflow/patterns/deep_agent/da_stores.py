# -*- coding: utf-8 -*-
"""DeepAgent ストレージ抽象.

三層ストレージ設計:
- L1: RuntimeStore - 実行時の一時データ（メモリ/Redis）
- L2: App Session Store - App固有の業務データ（Protocol定義）
- L3: EvolutionStore - フレームワーク級の進化データ（永続化）

Virtual Filesystem機能:
- Agent間でartifact（ファイル）を共有
- 実際のI/Oなしで仮想ファイル操作
- DeepAgents互換のファイルシステム抽象
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from datetime import datetime
from typing import Any

from agentflow.patterns.deep_agent.da_models import EvolutionRecord


class RuntimeStore(ABC):
    """L1: 実行時ストア（一時データ）.

    TodoList、進捗、Agent通信など実行中のデータを管理。
    デフォルトはメモリ、長時間タスクはRedisでcheckpoint可能。
    """

    # =========================================================================
    # コンテキスト管理
    # =========================================================================

    @abstractmethod
    async def save_context(self, key: str, data: dict[str, Any]) -> None:
        """コンテキストを保存."""
        pass

    @abstractmethod
    async def load_context(self, key: str) -> dict[str, Any] | None:
        """コンテキストを読み込み."""
        pass

    # =========================================================================
    # チェックポイント管理
    # =========================================================================

    @abstractmethod
    async def save_checkpoint(self, checkpoint_id: str, state: dict[str, Any]) -> None:
        """チェックポイントを保存（長時間タスク用）."""
        pass

    @abstractmethod
    async def load_checkpoint(self, checkpoint_id: str) -> dict[str, Any] | None:
        """チェックポイントを復元."""
        pass

    @abstractmethod
    async def list_checkpoints(self) -> list[str]:
        """チェックポイント一覧を取得."""
        pass

    # =========================================================================
    # Virtual Filesystem（Artifact管理）
    # =========================================================================

    @abstractmethod
    async def write_artifact(
        self,
        path: str,
        content: bytes | str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """仮想ファイルを書き込み.

        Args:
            path: 仮想パス（例: "/reports/analysis.md"）
            content: ファイル内容（bytes or str）
            metadata: メタデータ（content_type, created_by等）
        """
        pass

    @abstractmethod
    async def read_artifact(self, path: str) -> bytes | None:
        """仮想ファイルを読み込み."""
        pass

    @abstractmethod
    async def list_artifacts(self, prefix: str = "") -> list[dict[str, Any]]:
        """仮想ファイル一覧を取得."""
        pass

    @abstractmethod
    async def delete_artifact(self, path: str) -> bool:
        """仮想ファイルを削除."""
        pass

    @abstractmethod
    async def artifact_exists(self, path: str) -> bool:
        """仮想ファイルの存在確認."""
        pass

    # =========================================================================
    # クリア
    # =========================================================================

    @abstractmethod
    async def clear(self) -> None:
        """全データをクリア."""
        pass


class EvolutionStore(ABC):
    """L3: 進化ストア（フレームワーク級永続データ）.

    成功パターン、反馈記録、進化したSkillsを永続化。
    全Appで共有され、フレームワークを継続的に強化する。
    """

    @abstractmethod
    async def save_pattern(self, pattern_key: str, pattern_data: dict[str, Any]) -> None:
        """成功パターンを保存."""
        pass

    @abstractmethod
    async def load_pattern(self, pattern_key: str) -> dict[str, Any] | None:
        """成功パターンを読み込み."""
        pass

    @abstractmethod
    async def save_feedback(self, record: EvolutionRecord) -> None:
        """反馈記録を保存."""
        pass

    @abstractmethod
    async def list_patterns(self, limit: int = 100) -> list[dict[str, Any]]:
        """パターン一覧を取得."""
        pass

    @abstractmethod
    async def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        pass


class MemoryRuntimeStore(RuntimeStore):
    """メモリベースの実行時ストア（デフォルト実装）.

    開発・テスト環境向け。本番環境ではRedis実装を推奨。
    """

    def __init__(self) -> None:
        """初期化."""
        self._contexts: dict[str, dict[str, Any]] = {}
        self._checkpoints: dict[str, dict[str, Any]] = {}
        self._artifacts: dict[str, bytes] = {}
        self._artifact_metadata: dict[str, dict[str, Any]] = {}

    async def save_context(self, key: str, data: dict[str, Any]) -> None:
        """コンテキストを保存."""
        self._contexts[key] = data

    async def load_context(self, key: str) -> dict[str, Any] | None:
        """コンテキストを読み込み."""
        return self._contexts.get(key)

    async def save_checkpoint(self, checkpoint_id: str, state: dict[str, Any]) -> None:
        """チェックポイントを保存."""
        self._checkpoints[checkpoint_id] = {
            **state,
            "_saved_at": datetime.now().isoformat(),
        }

    async def load_checkpoint(self, checkpoint_id: str) -> dict[str, Any] | None:
        """チェックポイントを復元."""
        return self._checkpoints.get(checkpoint_id)

    async def list_checkpoints(self) -> list[str]:
        """チェックポイント一覧を取得."""
        return list(self._checkpoints.keys())

    async def write_artifact(
        self,
        path: str,
        content: bytes | str,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """仮想ファイルを書き込み."""
        normalized_path = self._normalize_path(path)
        content_bytes = content.encode("utf-8") if isinstance(content, str) else content
        self._artifacts[normalized_path] = content_bytes
        self._artifact_metadata[normalized_path] = {
            "size": len(content_bytes),
            "created_at": datetime.now().isoformat(),
            "content_type": (metadata or {}).get("content_type", "application/octet-stream"),
            **(metadata or {}),
        }

    async def read_artifact(self, path: str) -> bytes | None:
        """仮想ファイルを読み込み."""
        return self._artifacts.get(self._normalize_path(path))

    async def list_artifacts(self, prefix: str = "") -> list[dict[str, Any]]:
        """仮想ファイル一覧を取得."""
        normalized_prefix = self._normalize_path(prefix) if prefix else ""
        results = [
            {"path": path, **meta}
            for path, meta in self._artifact_metadata.items()
            if path.startswith(normalized_prefix)
        ]
        return sorted(results, key=lambda x: x["path"])

    async def delete_artifact(self, path: str) -> bool:
        """仮想ファイルを削除."""
        normalized_path = self._normalize_path(path)
        if normalized_path in self._artifacts:
            del self._artifacts[normalized_path]
            del self._artifact_metadata[normalized_path]
            return True
        return False

    async def artifact_exists(self, path: str) -> bool:
        """仮想ファイルの存在確認."""
        return self._normalize_path(path) in self._artifacts

    def _normalize_path(self, path: str) -> str:
        """パスを正規化（先頭スラッシュ統一）."""
        if not path.startswith("/"):
            path = "/" + path
        while "//" in path:
            path = path.replace("//", "/")
        return path

    async def clear(self) -> None:
        """全データをクリア."""
        self._contexts.clear()
        self._checkpoints.clear()
        self._artifacts.clear()
        self._artifact_metadata.clear()


class MemoryEvolutionStore(EvolutionStore):
    """メモリベースの進化ストア（開発/テスト用）.

    Note:
        本番環境ではPostgreSQLなどの永続ストアを使用すべき。
    """

    def __init__(self) -> None:
        """初期化."""
        self._patterns: dict[str, dict[str, Any]] = {}
        self._feedbacks: list[EvolutionRecord] = []

    async def save_pattern(self, pattern_key: str, pattern_data: dict[str, Any]) -> None:
        """成功パターンを保存."""
        self._patterns[pattern_key] = {
            **pattern_data,
            "updated_at": datetime.now().isoformat(),
        }

    async def load_pattern(self, pattern_key: str) -> dict[str, Any] | None:
        """成功パターンを読み込み."""
        return self._patterns.get(pattern_key)

    async def save_feedback(self, record: EvolutionRecord) -> None:
        """反馈記録を保存."""
        self._feedbacks.append(record)

    async def list_patterns(self, limit: int = 100) -> list[dict[str, Any]]:
        """パターン一覧を取得."""
        return list(self._patterns.values())[:limit]

    async def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "total_patterns": len(self._patterns),
            "total_feedbacks": len(self._feedbacks),
            "success_feedbacks": sum(1 for f in self._feedbacks if f.event_type == "success"),
        }


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    "RuntimeStore",
    "EvolutionStore",
    "MemoryRuntimeStore",
    "MemoryEvolutionStore",
]

