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
    AgentMessage,
    MessageType,
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

    def __init__(self, runtime_store: Any = None) -> None:
        """初期化.

        Args:
            runtime_store: メッセージ永続化に使用するRuntimeStore（省略可）
        """
        self._todos: dict[str, TodoItem] = {}
        self._parallel_groups: dict[str, ParallelGroup] = {}
        self._listeners: list[Callable[[ProgressEvent], None]] = []
        self._async_listeners: list[Callable[[ProgressEvent], Any]] = []
        self._started_at: datetime | None = None
        self._completed_at: datetime | None = None
        self._background_tasks: set[asyncio.Task[None]] = set()
        self._messages: list[AgentMessage] = []
        self._context: dict[str, Any] = {}
        self._runtime_store = runtime_store

    # =========================================================================
    # TodoList管理
    # =========================================================================

    def add_todo(self, todo: TodoItem) -> str:
        """単一TodoItemを追加.

        Args:
            todo: 追加するTodoItem

        Returns:
            追加されたTodoのID
        """
        self._todos[todo.id] = todo
        if self._started_at is None:
            self._started_at = datetime.now()
        self._emit(ProgressEvent("todo_added", todo.id, {"task": todo.task}))
        return todo.id

    def update_todo(self, todo_id: str, **kwargs: Any) -> bool:
        """TodoItemを更新.

        Args:
            todo_id: 更新対象のTodoID
            **kwargs: 更新するフィールドと値

        Returns:
            更新成功ならTrue、見つからなければFalse
        """
        todo = self._todos.get(todo_id)
        if todo is None:
            return False
        for key, value in kwargs.items():
            if hasattr(todo, key):
                setattr(todo, key, value)
        self._emit(ProgressEvent("todo_updated", todo_id, kwargs))
        return True

    def get_next_todo(self) -> TodoItem | None:
        """優先度の高い実行可能なTodoを取得.

        Returns:
            優先度が最も高い準備完了TodoItem、なければNone
        """
        ready = self.get_ready_todos()
        if not ready:
            return None
        return max(ready, key=lambda t: t.priority)

    def validate_dependencies(self) -> list[str]:
        """依存関係の検証（循環依存チェック）.

        Returns:
            エラーメッセージのリスト（エラーなければ空）
        """
        errors: list[str] = []
        # DFS で循環依存検出
        visited: set[str] = set()
        in_stack: set[str] = set()

        def dfs(node_id: str) -> bool:
            """深さ優先探索で循環を検出."""
            visited.add(node_id)
            in_stack.add(node_id)
            todo = self._todos.get(node_id)
            if todo:
                for dep in todo.dependencies or []:
                    if dep not in visited:
                        if dfs(dep):
                            return True
                    elif dep in in_stack:
                        errors.append(f"循環依存が検出されました: {node_id} -> {dep}")
                        return True
            in_stack.discard(node_id)
            return False

        for todo_id in self._todos:
            if todo_id not in visited:
                dfs(todo_id)

        return errors

    def get_execution_order(self) -> list[str]:
        """トポロジカルソートで実行順序を返す.

        Returns:
            実行順序のTodo IDリスト（循環依存がある場合は空リスト）
        """
        if self.validate_dependencies():
            return []  # 循環依存あり

        in_degree: dict[str, int] = dict.fromkeys(self._todos, 0)
        for todo in self._todos.values():
            for dep in todo.dependencies or []:
                if dep in in_degree:
                    in_degree[todo.id] = in_degree.get(todo.id, 0) + 1

        # Kahn's algorithm
        queue = [tid for tid, deg in in_degree.items() if deg == 0]
        order: list[str] = []

        while queue:
            queue.sort()  # deterministic order
            node = queue.pop(0)
            order.append(node)
            for todo in self._todos.values():
                if node in (todo.dependencies or []):
                    in_degree[todo.id] -= 1
                    if in_degree[todo.id] == 0:
                        queue.append(todo.id)

        return order if len(order) == len(self._todos) else []

    def get_parallel_groups(self) -> list[ParallelGroup]:
        """DAGに基づく並行実行グループを全て返す.

        Returns:
            並行実行グループのリスト（実行順）
        """
        if self.validate_dependencies():
            return []  # 循環依存あり

        remaining = set(self._todos.keys())
        completed: set[str] = set()
        groups: list[ParallelGroup] = []

        while remaining:
            # この時点で実行可能なtodo（依存が全て完了）
            ready_ids = {
                todo_id
                for todo_id in remaining
                if all(dep in completed for dep in (self._todos[todo_id].dependencies or []))
            }
            if not ready_ids:
                break  # 循環依存などで進めない
            group = ParallelGroup(todo_ids=sorted(ready_ids))
            groups.append(group)
            completed.update(ready_ids)
            remaining -= ready_ids

        return groups

    def get_progress(self) -> dict[str, Any]:
        """進捗状況を取得.

        Returns:
            total/completed/failed/progress キーを含む辞書
        """
        total = len(self._todos)
        completed = sum(1 for t in self._todos.values() if t.status == TaskStatus.COMPLETED)
        failed = sum(1 for t in self._todos.values() if t.status == TaskStatus.FAILED)
        return {
            "total": total,
            "completed": completed,
            "failed": failed,
            "progress": completed / total if total > 0 else 0.0,
        }

    def send_message(
        self,
        from_agent: str,
        to_agent: str,
        content: Any = None,
        msg_type: MessageType = MessageType.NOTIFY,
        **kwargs: Any,
    ) -> AgentMessage:
        """エージェント間メッセージを同期送信.

        Args:
            from_agent: 送信元エージェントID
            to_agent: 送信先エージェントID
            content: メッセージデータ
            msg_type: メッセージ種別

        Returns:
            送信したAgentMessage
        """
        message = AgentMessage(
            from_agent=from_agent,
            to_agent=to_agent,
            content=content,
            msg_type=msg_type,
        )
        self._messages.append(message)
        return message

    async def send_message_async(
        self,
        from_agent: str,
        to_agent: str,
        content: Any = None,
        msg_type: MessageType = MessageType.NOTIFY,
        persist: bool = True,
    ) -> AgentMessage:
        """エージェント間メッセージを非同期送信.

        Args:
            from_agent: 送信元エージェントID
            to_agent: 送信先エージェントID
            content: メッセージデータ
            msg_type: メッセージ種別
            persist: RuntimeStoreに永続化するか

        Returns:
            送信したAgentMessage
        """
        message = AgentMessage(
            from_agent=from_agent,
            to_agent=to_agent,
            content=content,
            msg_type=msg_type,
        )
        self._messages.append(message)

        if persist and self._runtime_store is not None:
            try:
                await self._runtime_store.save_context(
                    f"msg:{message.id}",
                    {
                        "from_agent": message.from_agent,
                        "to_agent": message.to_agent,
                        "content": message.content,
                        "msg_type": message.msg_type.value
                        if hasattr(message.msg_type, "value")
                        else str(message.msg_type),
                    },
                )
            except Exception as e:
                _logger.warning("メッセージ永続化失敗: %s", e)

        return message

    def get_messages(self, agent: str | None = None) -> list[AgentMessage]:
        """メッセージを取得.

        Args:
            agent: フィルタするエージェントID（Noneの場合は全て）

        Returns:
            メッセージのリスト
        """
        if agent is None:
            return list(self._messages)
        return [m for m in self._messages if agent in (m.from_agent, m.to_agent)]

    @property
    def context(self) -> dict[str, Any]:
        """共有コンテキストを返す."""
        return self._context

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
        all_done = all(t.status in (TaskStatus.COMPLETED, TaskStatus.FAILED) for t in self._todos.values())
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
