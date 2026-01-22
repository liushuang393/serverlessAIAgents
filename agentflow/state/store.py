# -*- coding: utf-8 -*-
"""グローバル状態ストア.

Redux風の状態管理パターンを実装。
全コンポーネント間で状態を共有。

設計原則:
- 単一ソース: 状態は一箇所で管理
- 不変性: 状態は直接変更しない
- 予測可能: アクションで状態変更
- サブスクリプション: 変更を購読可能

使用例:
    >>> from agentflow.state.store import GlobalStateStore
    >>> from agentflow.state.actions import update_progress
    >>>
    >>> store = GlobalStateStore()
    >>>
    >>> # 変更を購読
    >>> unsubscribe = store.subscribe(lambda state: print(state))
    >>>
    >>> # アクションをディスパッチ
    >>> store.dispatch(update_progress(0.5, "処理中..."))
    >>>
    >>> # 状態を取得
    >>> progress = store.get_state("execution.progress")
"""

from __future__ import annotations

import asyncio
import copy
import logging
import threading
import uuid
from collections.abc import Callable
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any

from agentflow.state.actions import Action, ActionType
from agentflow.state.selectors import select


@dataclass
class StateSnapshot:
    """状態スナップショット.

    特定時点の状態を保存。

    Attributes:
        id: スナップショットID
        state: 状態のコピー
        timestamp: タイムスタンプ
        label: ラベル
    """

    id: str = field(default_factory=lambda: f"snap-{uuid.uuid4().hex[:8]}")
    state: dict[str, Any] = field(default_factory=dict)
    timestamp: datetime = field(default_factory=datetime.now)
    label: str = ""


@dataclass
class StateSubscription:
    """状態購読.

    Attributes:
        id: 購読ID
        callback: コールバック関数
        selector: セレクター（特定パスのみ監視）
    """

    id: str = field(default_factory=lambda: f"sub-{uuid.uuid4().hex[:8]}")
    callback: Callable[[dict[str, Any]], Any] = field(default=lambda _: None)
    selector: str | None = None


class GlobalStateStore:
    """グローバル状態ストア.

    Redux風の状態管理を実装。
    スレッドセーフで、非同期操作にも対応。

    主な機能:
    - 状態の集中管理
    - アクションによる状態変更
    - サブスクリプションによる変更通知
    - スナップショットと復元
    - 履歴管理

    Example:
        >>> store = GlobalStateStore()
        >>>
        >>> # 購読
        >>> def on_change(state):
        ...     print(f"Progress: {state['execution']['progress']}")
        >>> store.subscribe(on_change, selector="execution.progress")
        >>>
        >>> # アクションをディスパッチ
        >>> store.dispatch(update_progress(0.5))
    """

    def __init__(
        self,
        initial_state: dict[str, Any] | None = None,
        max_history: int = 100,
        enable_snapshots: bool = True,
    ) -> None:
        """初期化.

        Args:
            initial_state: 初期状態
            max_history: 最大履歴数
            enable_snapshots: スナップショットを有効化
        """
        self._state: dict[str, Any] = initial_state or self._create_initial_state()
        self._subscriptions: dict[str, StateSubscription] = {}
        self._action_history: list[Action] = []
        self._snapshots: dict[str, StateSnapshot] = {}
        self._max_history = max_history
        self._enable_snapshots = enable_snapshots
        self._lock = threading.RLock()
        self._logger = logging.getLogger(__name__)

    def _create_initial_state(self) -> dict[str, Any]:
        """初期状態を作成.

        L2標準状態フィールド（goal/facts/decisions）を含む。
        """
        return {
            "execution": {
                "id": None,
                "status": "idle",
                "phase": None,
                "progress": 0.0,
                "current_step": None,
                "error": None,
                "started_at": None,
                "completed_at": None,
            },
            "context": {},
            "plan": None,
            "results": {},
            "history": [],
            "hitl": {
                "pending_approvals": [],
            },
            "checkpoints": [],
            # =================================================================
            # L2標準状態フィールド（Goal/Facts/Decisions）
            # =================================================================
            "goal": {
                "objective": None,
                "constraints": [],
                "success_criteria": [],
                "priority": 1,
                "deadline": None,
                "context": {},
            },
            "facts": [],  # 収集した事実リスト
            "decisions": [],  # 判断履歴リスト
            "metadata": {
                "created_at": datetime.now().isoformat(),
                "version": 1,
            },
        }

    def get_state(self, path: str | None = None, default: Any = None) -> Any:
        """状態を取得.

        Args:
            path: ドット区切りのパス（Noneの場合は全状態）
            default: デフォルト値

        Returns:
            状態（のコピー）
        """
        with self._lock:
            if path is None:
                return copy.deepcopy(self._state)
            return select(self._state, path, default)

    def dispatch(self, action: Action) -> None:
        """アクションをディスパッチ.

        Args:
            action: アクション
        """
        with self._lock:
            self._logger.debug(f"ディスパッチ: {action.type.value}")

            # 状態を更新
            self._reduce(action)

            # 履歴に追加
            self._action_history.append(action)
            if len(self._action_history) > self._max_history:
                self._action_history.pop(0)

            # 購読者に通知
            self._notify_subscribers(action)

    def _reduce(self, action: Action) -> None:
        """リデューサー - アクションに基づいて状態を更新.

        Args:
            action: アクション
        """
        payload = action.payload

        if action.type == ActionType.SET_EXECUTION_STATUS:
            self._state["execution"]["status"] = payload.get("status")
            if execution_id := payload.get("execution_id"):
                self._state["execution"]["id"] = execution_id

        elif action.type == ActionType.UPDATE_PROGRESS:
            self._state["execution"]["progress"] = payload.get("progress", 0.0)
            if message := payload.get("message"):
                self._state["execution"]["progress_message"] = message

        elif action.type == ActionType.SET_CURRENT_STEP:
            self._state["execution"]["current_step"] = payload.get("step_id")

        elif action.type == ActionType.COMPLETE_STEP:
            step_id = payload.get("step_id")
            output = payload.get("output")
            if step_id:
                self._state["results"][step_id] = {
                    "output": output,
                    "completed_at": datetime.now().isoformat(),
                }
            self._state["execution"]["current_step"] = None

        elif action.type == ActionType.FAIL_STEP:
            step_id = payload.get("step_id")
            error = payload.get("error")
            if step_id:
                self._state["results"][step_id] = {
                    "error": error,
                    "failed_at": datetime.now().isoformat(),
                }
            self._state["execution"]["error"] = error

        elif action.type == ActionType.SET_CONTEXT:
            self._state["context"] = payload.get("context", {})

        elif action.type == ActionType.UPDATE_CONTEXT:
            key = payload.get("key")
            value = payload.get("value")
            if key:
                self._state["context"][key] = value

        elif action.type == ActionType.MERGE_CONTEXT:
            context = payload.get("context", {})
            self._state["context"].update(context)

        elif action.type == ActionType.ADD_RESULT:
            step_id = payload.get("step_id")
            result = payload.get("result")
            if step_id:
                self._state["results"][step_id] = result

        elif action.type == ActionType.SET_FINAL_OUTPUT:
            self._state["results"]["_final_output"] = payload.get("output")

        elif action.type == ActionType.SET_ERROR:
            self._state["execution"]["error"] = payload.get("error")

        elif action.type == ActionType.SET_PLAN:
            self._state["plan"] = payload.get("plan")

        elif action.type == ActionType.UPDATE_PLAN:
            if self._state["plan"]:
                self._state["plan"].update(payload.get("updates", {}))

        elif action.type == ActionType.REQUEST_APPROVAL:
            request = {
                "request_id": payload.get("request_id"),
                "action": payload.get("action"),
                "context": payload.get("context"),
                "created_at": datetime.now().isoformat(),
            }
            self._state["hitl"]["pending_approvals"].append(request)

        elif action.type == ActionType.RECEIVE_APPROVAL:
            request_id = payload.get("request_id")
            self._state["hitl"]["pending_approvals"] = [
                r for r in self._state["hitl"]["pending_approvals"]
                if r.get("request_id") != request_id
            ]

        elif action.type == ActionType.PUSH_HISTORY:
            entry = {
                "action": action.type.value,
                "payload": payload,
                "timestamp": action.timestamp.isoformat(),
            }
            self._state["history"].append(entry)
            if len(self._state["history"]) > self._max_history:
                self._state["history"].pop(0)

        elif action.type == ActionType.CLEAR_HISTORY:
            self._state["history"] = []

        elif action.type == ActionType.CREATE_CHECKPOINT:
            checkpoint_id = payload.get("checkpoint_id")
            if checkpoint_id and self._enable_snapshots:
                self._create_snapshot(checkpoint_id, payload.get("label", ""))
                self._state["checkpoints"].append(checkpoint_id)

        elif action.type == ActionType.RESTORE_CHECKPOINT:
            checkpoint_id = payload.get("checkpoint_id")
            if checkpoint_id:
                self._restore_snapshot(checkpoint_id)

        # ======================================================================
        # L2標準状態フィールド（Goal/Facts/Decisions）
        # ======================================================================
        elif action.type == ActionType.SET_GOAL:
            self._state["goal"] = {
                "objective": payload.get("objective"),
                "constraints": payload.get("constraints", []),
                "success_criteria": payload.get("success_criteria", []),
                "priority": payload.get("priority", 1),
                "deadline": payload.get("deadline"),
                "context": payload.get("context", {}),
            }

        elif action.type == ActionType.UPDATE_GOAL:
            updates = payload.get("updates", {})
            if self._state.get("goal"):
                self._state["goal"].update(updates)

        elif action.type == ActionType.ADD_CONSTRAINT:
            constraint = {
                "type": payload.get("type"),
                "description": payload.get("description"),
                "value": payload.get("value"),
                "is_hard": payload.get("is_hard", True),
                "added_at": datetime.now().isoformat(),
            }
            if self._state.get("goal"):
                self._state["goal"]["constraints"].append(constraint)

        elif action.type == ActionType.ADD_FACT:
            fact = {
                "id": f"fact-{datetime.now().strftime('%Y%m%d%H%M%S%f')}",
                "source": payload.get("source"),
                "source_name": payload.get("source_name"),
                "data": payload.get("data"),
                "confidence": payload.get("confidence", 1.0),
                "metadata": payload.get("metadata", {}),
                "timestamp": datetime.now().isoformat(),
            }
            self._state["facts"].append(fact)

        elif action.type == ActionType.ADD_DECISION:
            decision = {
                "id": f"dec-{datetime.now().strftime('%Y%m%d%H%M%S%f')}",
                "step": payload.get("step"),
                "decision_type": payload.get("decision_type"),
                "choice": payload.get("choice"),
                "reason": payload.get("reason"),
                "alternatives": payload.get("alternatives", []),
                "evidence_facts": payload.get("evidence_facts", []),
                "confidence": payload.get("confidence", 1.0),
                "timestamp": datetime.now().isoformat(),
            }
            self._state["decisions"].append(decision)

        elif action.type == ActionType.CLEAR_FACTS:
            self._state["facts"] = []

        # メタデータを更新
        self._state["metadata"]["version"] += 1
        self._state["metadata"]["last_action"] = action.type.value
        self._state["metadata"]["last_updated"] = datetime.now().isoformat()

    def _notify_subscribers(self, action: Action) -> None:
        """購読者に通知.

        Args:
            action: ディスパッチされたアクション
        """
        for subscription in self._subscriptions.values():
            try:
                if subscription.selector:
                    # 特定パスのみ監視
                    value = select(self._state, subscription.selector)
                    subscription.callback({subscription.selector: value})
                else:
                    # 全状態
                    subscription.callback(copy.deepcopy(self._state))
            except Exception as e:
                self._logger.error(f"購読者への通知でエラー: {e}")

    def subscribe(
        self,
        callback: Callable[[dict[str, Any]], Any],
        selector: str | None = None,
    ) -> Callable[[], None]:
        """状態変更を購読.

        Args:
            callback: コールバック関数
            selector: 監視するパス（Noneの場合は全状態）

        Returns:
            購読解除関数
        """
        subscription = StateSubscription(
            callback=callback,
            selector=selector,
        )

        with self._lock:
            self._subscriptions[subscription.id] = subscription

        def unsubscribe() -> None:
            with self._lock:
                self._subscriptions.pop(subscription.id, None)

        return unsubscribe

    def create_snapshot(self, label: str = "") -> str:
        """現在の状態のスナップショットを作成.

        Args:
            label: ラベル

        Returns:
            スナップショットID
        """
        with self._lock:
            snapshot = StateSnapshot(
                state=copy.deepcopy(self._state),
                label=label,
            )
            self._snapshots[snapshot.id] = snapshot
            return snapshot.id

    def _create_snapshot(self, snapshot_id: str, label: str) -> None:
        """内部スナップショット作成."""
        snapshot = StateSnapshot(
            id=snapshot_id,
            state=copy.deepcopy(self._state),
            label=label,
        )
        self._snapshots[snapshot_id] = snapshot

    def restore_snapshot(self, snapshot_id: str) -> bool:
        """スナップショットから状態を復元.

        Args:
            snapshot_id: スナップショットID

        Returns:
            成功したかどうか
        """
        with self._lock:
            return self._restore_snapshot(snapshot_id)

    def _restore_snapshot(self, snapshot_id: str) -> bool:
        """内部スナップショット復元."""
        snapshot = self._snapshots.get(snapshot_id)
        if not snapshot:
            return False

        self._state = copy.deepcopy(snapshot.state)
        self._logger.info(f"スナップショットを復元: {snapshot_id}")
        return True

    def get_snapshots(self) -> list[StateSnapshot]:
        """全スナップショットを取得.

        Returns:
            スナップショットリスト
        """
        with self._lock:
            return list(self._snapshots.values())

    def get_action_history(self, limit: int = 50) -> list[Action]:
        """アクション履歴を取得.

        Args:
            limit: 最大取得数

        Returns:
            アクションリスト
        """
        with self._lock:
            return self._action_history[-limit:]

    def reset(self) -> None:
        """状態をリセット."""
        with self._lock:
            self._state = self._create_initial_state()
            self._action_history.clear()
            self._snapshots.clear()
            self._logger.info("状態をリセットしました")

    def get_diff(self, snapshot_id: str) -> dict[str, Any]:
        """現在の状態とスナップショットの差分を取得.

        Args:
            snapshot_id: スナップショットID

        Returns:
            差分情報
        """
        with self._lock:
            snapshot = self._snapshots.get(snapshot_id)
            if not snapshot:
                return {"error": "スナップショットが見つかりません"}

            return self._compute_diff(snapshot.state, self._state)

    def _compute_diff(
        self,
        old_state: dict[str, Any],
        new_state: dict[str, Any],
        path: str = "",
    ) -> dict[str, Any]:
        """状態の差分を計算.

        Args:
            old_state: 古い状態
            new_state: 新しい状態
            path: 現在のパス

        Returns:
            差分
        """
        diff: dict[str, Any] = {"added": {}, "removed": {}, "changed": {}}

        all_keys = set(old_state.keys()) | set(new_state.keys())

        for key in all_keys:
            current_path = f"{path}.{key}" if path else key
            old_value = old_state.get(key)
            new_value = new_state.get(key)

            if key not in old_state:
                diff["added"][current_path] = new_value
            elif key not in new_state:
                diff["removed"][current_path] = old_value
            elif old_value != new_value:
                if isinstance(old_value, dict) and isinstance(new_value, dict):
                    nested_diff = self._compute_diff(old_value, new_value, current_path)
                    diff["added"].update(nested_diff["added"])
                    diff["removed"].update(nested_diff["removed"])
                    diff["changed"].update(nested_diff["changed"])
                else:
                    diff["changed"][current_path] = {
                        "old": old_value,
                        "new": new_value,
                    }

        return diff

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        with self._lock:
            return {
                "state_version": self._state["metadata"]["version"],
                "subscription_count": len(self._subscriptions),
                "action_history_count": len(self._action_history),
                "snapshot_count": len(self._snapshots),
                "checkpoint_count": len(self._state.get("checkpoints", [])),
            }


# エクスポート
__all__ = [
    "GlobalStateStore",
    "StateSnapshot",
    "StateSubscription",
]
