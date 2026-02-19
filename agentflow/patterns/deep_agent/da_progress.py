"""DeepAgent 進捗管理システム.

リアルタイム進捗追跡と可視化を提供:
- タスク進捗の追跡
- 並行実行グループの管理
- 進捗イベントの発行
- 統計情報の集計

DeepAgentsの「透明性」原則を実装:
- ユーザーは常に現在の状態を把握可能
- 各ステップの結果が可視化される
- エラー発生時の詳細情報を提供
"""

from __future__ import annotations

import asyncio
import logging
from datetime import datetime
from typing import TYPE_CHECKING, Any

from agentflow.patterns.deep_agent.da_models import (
    ParallelGroup,
    TaskStatus,
    TodoItem,
)


if TYPE_CHECKING:
    from collections.abc import Callable


_logger = logging.getLogger(__name__)


# =============================================================================
# 進捗イベント
# =============================================================================


class ProgressEvent:
    """進捗イベント.

    進捗変更時に発行されるイベント。
    コールバックやWebSocket通知に使用。
    """

    def __init__(
        self,
        event_type: str,
        task_id: str | None = None,
        data: dict[str, Any] | None = None,
    ) -> None:
        """初期化.

        Args:
            event_type: イベント種別（task_started, task_completed, etc.）
            task_id: 関連タスクID
            data: 追加データ
        """
        self.event_type = event_type
        self.task_id = task_id
        self.data = data or {}
        self.timestamp = datetime.now()

    def to_dict(self) -> dict[str, Any]:
        """辞書形式に変換."""
        return {
            "event_type": self.event_type,
            "task_id": self.task_id,
            "data": self.data,
            "timestamp": self.timestamp.isoformat(),
        }


# =============================================================================
# 進捗マネージャー
# =============================================================================


class ProgressManager:
    """進捗管理マネージャー.

    TodoListの進捗を追跡し、イベントを発行する。

    Example:
        >>> pm = ProgressManager()
        >>> pm.add_listener(my_callback)
        >>> pm.register_todos(todo_list)
        >>> pm.mark_started("todo-123")
    """

    def __init__(self) -> None:
        """初期化."""
        self._todos: dict[str, TodoItem] = {}
        self._parallel_groups: dict[str, ParallelGroup] = {}
        self._listeners: list[Callable[[ProgressEvent], None]] = []
        self._async_listeners: list[Callable[[ProgressEvent], Any]] = []
        self._started_at: datetime | None = None
        self._completed_at: datetime | None = None
        self._background_tasks: set[asyncio.Task[None]] = set()

    # =========================================================================
    # TodoList管理
    # =========================================================================

    def register_todos(self, todos: list[TodoItem]) -> None:
        """TodoListを登録.

        Args:
            todos: 登録するTodoItem
        """
        for todo in todos:
            self._todos[todo.id] = todo
        self._started_at = datetime.now()
        self._emit(ProgressEvent("todos_registered", data={"count": len(todos)}))

    def get_todo(self, todo_id: str) -> TodoItem | None:
        """TodoItemを取得."""
        return self._todos.get(todo_id)

    def get_all_todos(self) -> list[TodoItem]:
        """全TodoItemを取得."""
        return list(self._todos.values())

    # =========================================================================
    # 状態更新
    # =========================================================================

    def mark_started(self, todo_id: str) -> None:
        """タスク開始をマーク."""
        if todo := self._todos.get(todo_id):
            todo.status = TaskStatus.IN_PROGRESS
            self._emit(ProgressEvent("task_started", todo_id))

    def mark_completed(self, todo_id: str, result: dict[str, Any] | None = None) -> None:
        """タスク完了をマーク."""
        if todo := self._todos.get(todo_id):
            todo.mark_completed()
            if result:
                todo.result = result
            self._emit(ProgressEvent("task_completed", todo_id, {"result": result}))
            self._check_all_completed()

    def mark_failed(self, todo_id: str, error: str) -> None:
        """タスク失敗をマーク."""
        if todo := self._todos.get(todo_id):
            todo.mark_failed(error)
            self._emit(ProgressEvent("task_failed", todo_id, {"error": error}))

    def mark_blocked(self, todo_id: str, reason: str) -> None:
        """タスクブロックをマーク."""
        if todo := self._todos.get(todo_id):
            todo.status = TaskStatus.BLOCKED
            self._emit(ProgressEvent("task_blocked", todo_id, {"reason": reason}))

    def _check_all_completed(self) -> None:
        """全タスク完了チェック."""
        all_done = all(
            t.status in (TaskStatus.COMPLETED, TaskStatus.FAILED) for t in self._todos.values()
        )
        if all_done and not self._completed_at:
            self._completed_at = datetime.now()
            self._emit(ProgressEvent("all_completed", data=self.get_stats()))

    # =========================================================================
    # 並行グループ管理
    # =========================================================================

    def create_parallel_group(self, todo_ids: list[str]) -> ParallelGroup:
        """並行実行グループを作成.

        Args:
            todo_ids: グループに含めるTodoID

        Returns:
            作成されたParallelGroup
        """
        group = ParallelGroup(todo_ids=todo_ids)
        self._parallel_groups[group.group_id] = group
        self._emit(
            ProgressEvent(
                "parallel_group_created",
                data={"group_id": group.group_id, "todo_ids": todo_ids},
            )
        )
        return group

    def mark_group_started(self, group_id: str) -> None:
        """グループ開始をマーク."""
        if group := self._parallel_groups.get(group_id):
            group.status = TaskStatus.IN_PROGRESS
            for todo_id in group.todo_ids:
                self.mark_started(todo_id)

    def mark_group_completed(self, group_id: str) -> None:
        """グループ完了をマーク."""
        if group := self._parallel_groups.get(group_id):
            group.status = TaskStatus.COMPLETED
            self._emit(
                ProgressEvent(
                    "parallel_group_completed",
                    data={"group_id": group_id},
                )
            )

    # =========================================================================
    # イベントリスナー
    # =========================================================================

    def add_listener(self, callback: Callable[[ProgressEvent], None]) -> None:
        """同期リスナーを追加."""
        self._listeners.append(callback)

    def add_async_listener(self, callback: Callable[[ProgressEvent], Any]) -> None:
        """非同期リスナーを追加."""
        self._async_listeners.append(callback)

    def remove_listener(self, callback: Callable[[ProgressEvent], None]) -> None:
        """リスナーを削除."""
        if callback in self._listeners:
            self._listeners.remove(callback)

    def _emit(self, event: ProgressEvent) -> None:
        """イベントを発行."""
        _logger.debug("進捗イベント: %s", event.event_type)
        for listener in self._listeners:
            try:
                listener(event)
            except Exception as e:
                _logger.warning("リスナーエラー: %s", e)

        # 非同期リスナーはタスクとして実行（GC防止のためsetで保持）
        for async_listener in self._async_listeners:
            task = asyncio.create_task(self._call_async_listener(async_listener, event))
            self._background_tasks.add(task)
            task.add_done_callback(self._background_tasks.discard)

    async def _call_async_listener(
        self,
        listener: Callable[[ProgressEvent], Any],
        event: ProgressEvent,
    ) -> None:
        """非同期リスナーを呼び出し."""
        try:
            await listener(event)
        except Exception as e:
            _logger.warning("非同期リスナーエラー: %s", e)

    # =========================================================================
    # 統計情報
    # =========================================================================

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        total = len(self._todos)
        completed = sum(1 for t in self._todos.values() if t.status == TaskStatus.COMPLETED)
        failed = sum(1 for t in self._todos.values() if t.status == TaskStatus.FAILED)
        in_progress = sum(1 for t in self._todos.values() if t.status == TaskStatus.IN_PROGRESS)
        pending = sum(1 for t in self._todos.values() if t.status == TaskStatus.PENDING)
        blocked = sum(1 for t in self._todos.values() if t.status == TaskStatus.BLOCKED)

        elapsed = None
        if self._started_at:
            end_time = self._completed_at or datetime.now()
            elapsed = (end_time - self._started_at).total_seconds()

        return {
            "total": total,
            "completed": completed,
            "failed": failed,
            "in_progress": in_progress,
            "pending": pending,
            "blocked": blocked,
            "progress_percent": (completed / total * 100) if total > 0 else 0,
            "success_rate": (completed / (completed + failed) * 100)
            if (completed + failed) > 0
            else 0,
            "elapsed_seconds": elapsed,
            "started_at": self._started_at.isoformat() if self._started_at else None,
            "completed_at": self._completed_at.isoformat() if self._completed_at else None,
        }

    def get_ready_todos(self) -> list[TodoItem]:
        """実行準備完了のTodoを取得."""
        completed_ids = {t.id for t in self._todos.values() if t.status == TaskStatus.COMPLETED}
        return [t for t in self._todos.values() if t.is_ready(completed_ids)]

    def get_next_parallel_group(self) -> list[TodoItem] | None:
        """次の並行実行可能グループを取得.

        依存関係のないタスクをグループ化して返す。
        """
        ready = self.get_ready_todos()
        if not ready:
            return None

        # 優先度でソート
        ready.sort(key=lambda t: -t.priority)
        return ready

    def reset(self) -> None:
        """状態をリセット."""
        self._todos.clear()
        self._parallel_groups.clear()
        self._started_at = None
        self._completed_at = None


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    "ProgressEvent",
    "ProgressManager",
]
