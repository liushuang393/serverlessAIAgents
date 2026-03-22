"""Layer 4 Cleanup - リソースクリーンアップサービス.

フロー実行後の一時リソース・セッション・キャッシュの後処理を行う。
"""

from __future__ import annotations

import logging
from collections.abc import Callable, Coroutine
from dataclasses import dataclass, field
from typing import Any


_logger = logging.getLogger(__name__)

# クリーンアップハンドラの型
CleanupHandler = Callable[[], Coroutine[Any, Any, None]]


@dataclass
class CleanupTask:
    """クリーンアップタスク.

    Attributes:
        name: タスク名
        handler: 非同期ハンドラ
        priority: 優先度（小さいほど先に実行）
    """

    name: str
    handler: CleanupHandler
    priority: int = 100


@dataclass
class CleanupResult:
    """クリーンアップ結果.

    Attributes:
        success: 全タスク成功かどうか
        completed: 完了タスク名リスト
        failed: 失敗タスク名→エラーメッセージ
    """

    success: bool = True
    completed: list[str] = field(default_factory=list)
    failed: dict[str, str] = field(default_factory=dict)


class CleanupManager:
    """クリーンアップマネージャ."""

    def __init__(self) -> None:
        """初期化."""
        self._tasks: list[CleanupTask] = []

    def register(self, name: str, handler: CleanupHandler, priority: int = 100) -> None:
        """クリーンアップタスクを登録.

        Args:
            name: タスク名
            handler: 非同期ハンドラ
            priority: 優先度
        """
        self._tasks.append(CleanupTask(name=name, handler=handler, priority=priority))

    async def run_all(self) -> CleanupResult:
        """全クリーンアップタスクを実行.

        Returns:
            クリーンアップ結果
        """
        result = CleanupResult()
        sorted_tasks = sorted(self._tasks, key=lambda t: t.priority)

        for task in sorted_tasks:
            try:
                await task.handler()
                result.completed.append(task.name)
            except Exception as exc:
                _logger.warning("クリーンアップ失敗: %s - %s", task.name, exc)
                result.failed[task.name] = str(exc)
                result.success = False

        return result

    def clear(self) -> None:
        """登録済みタスクをクリア."""
        self._tasks.clear()


__all__ = [
    "CleanupHandler",
    "CleanupManager",
    "CleanupResult",
    "CleanupTask",
]
