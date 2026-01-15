# -*- coding: utf-8 -*-
"""GlobalStateStore 単体テスト."""

import pytest

from agentflow.state.store import GlobalStateStore, StateSnapshot
from agentflow.state.actions import (
    Action,
    ActionType,
    create_action,
    update_progress,
    set_execution_status,
    set_context,
    update_context,
    add_result,
    set_error,
)
from agentflow.state.selectors import (
    select,
    StateSelector,
    select_execution_status,
    select_progress,
    select_context,
    select_results,
)


class TestActions:
    """Action テストクラス."""

    def test_create_action(self) -> None:
        """アクションを作成できること."""
        action = create_action(
            ActionType.UPDATE_PROGRESS,
            {"progress": 0.5, "message": "処理中"},
        )

        assert action.type == ActionType.UPDATE_PROGRESS
        assert action.payload["progress"] == 0.5

    def test_update_progress_action(self) -> None:
        """進捗更新アクションを作成できること."""
        action = update_progress(0.75, "分析中...")

        assert action.type == ActionType.UPDATE_PROGRESS
        assert action.payload["progress"] == 0.75
        assert action.payload["message"] == "分析中..."

    def test_set_execution_status_action(self) -> None:
        """実行状態設定アクションを作成できること."""
        action = set_execution_status("running", "exec-001")

        assert action.type == ActionType.SET_EXECUTION_STATUS
        assert action.payload["status"] == "running"
        assert action.payload["execution_id"] == "exec-001"

    def test_set_context_action(self) -> None:
        """コンテキスト設定アクションを作成できること."""
        action = set_context({"key": "value"})

        assert action.type == ActionType.SET_CONTEXT
        assert action.payload["context"]["key"] == "value"

    def test_action_to_dict(self) -> None:
        """アクションを辞書に変換できること."""
        action = create_action(ActionType.SET_ERROR, {"error": "テストエラー"})
        data = action.to_dict()

        assert data["type"] == "SET_ERROR"
        assert "timestamp" in data


class TestSelectors:
    """Selector テストクラス."""

    def test_select_simple_path(self) -> None:
        """単純なパスを選択できること."""
        state = {"execution": {"status": "running"}}
        value = select(state, "execution.status")

        assert value == "running"

    def test_select_nested_path(self) -> None:
        """ネストしたパスを選択できること."""
        state = {
            "context": {
                "user": {
                    "name": "テスト",
                },
            },
        }
        value = select(state, "context.user.name")

        assert value == "テスト"

    def test_select_with_default(self) -> None:
        """存在しないパスでデフォルト値を返すこと."""
        state = {"a": 1}
        value = select(state, "b.c.d", default="デフォルト")

        assert value == "デフォルト"

    def test_state_selector_class(self) -> None:
        """StateSelectorクラスが動作すること."""
        selector = StateSelector("execution.progress", default=0.0)
        state = {"execution": {"progress": 0.5}}

        value = selector(state)
        assert value == 0.5

    def test_select_execution_status(self) -> None:
        """実行状態セレクターが動作すること."""
        state = {"execution": {"status": "completed"}}
        status = select_execution_status(state)

        assert status == "completed"

    def test_select_progress(self) -> None:
        """進捗セレクターが動作すること."""
        state = {"execution": {"progress": 0.75}}
        progress = select_progress(state)

        assert progress == 0.75

    def test_select_context(self) -> None:
        """コンテキストセレクターが動作すること."""
        state = {"context": {"key": "value"}}
        context = select_context(state)

        assert context == {"key": "value"}

    def test_select_results(self) -> None:
        """結果セレクターが動作すること."""
        state = {"results": {"step1": {"output": "data"}}}
        results = select_results(state)

        assert results["step1"]["output"] == "data"


class TestGlobalStateStore:
    """GlobalStateStore テストクラス."""

    @pytest.fixture
    def store(self) -> GlobalStateStore:
        """ストアを作成."""
        return GlobalStateStore()

    def test_initial_state(self, store: GlobalStateStore) -> None:
        """初期状態が正しいこと."""
        state = store.get_state()

        assert "execution" in state
        assert "context" in state
        assert "results" in state
        assert state["execution"]["status"] == "idle"

    def test_get_state_with_path(self, store: GlobalStateStore) -> None:
        """パス指定で状態を取得できること."""
        status = store.get_state("execution.status")
        assert status == "idle"

    def test_dispatch_update_progress(self, store: GlobalStateStore) -> None:
        """進捗更新をディスパッチできること."""
        store.dispatch(update_progress(0.5, "処理中..."))

        progress = store.get_state("execution.progress")
        assert progress == 0.5

    def test_dispatch_set_context(self, store: GlobalStateStore) -> None:
        """コンテキスト設定をディスパッチできること."""
        store.dispatch(set_context({"user_input": "テスト"}))

        context = store.get_state("context")
        assert context["user_input"] == "テスト"

    def test_dispatch_update_context(self, store: GlobalStateStore) -> None:
        """コンテキスト更新をディスパッチできること."""
        store.dispatch(set_context({"a": 1}))
        store.dispatch(update_context("b", 2))

        context = store.get_state("context")
        assert context["a"] == 1
        assert context["b"] == 2

    def test_dispatch_add_result(self, store: GlobalStateStore) -> None:
        """結果追加をディスパッチできること."""
        store.dispatch(add_result("step-001", {"output": "データ"}))

        results = store.get_state("results")
        assert results["step-001"]["output"] == "データ"

    def test_dispatch_set_error(self, store: GlobalStateStore) -> None:
        """エラー設定をディスパッチできること."""
        store.dispatch(set_error("テストエラー"))

        error = store.get_state("execution.error")
        assert error == "テストエラー"

    def test_subscribe(self, store: GlobalStateStore) -> None:
        """状態変更を購読できること."""
        changes: list[dict] = []

        def callback(state: dict) -> None:
            changes.append(state)

        unsubscribe = store.subscribe(callback)
        store.dispatch(update_progress(0.25))
        store.dispatch(update_progress(0.5))

        assert len(changes) == 2
        unsubscribe()

    def test_subscribe_with_selector(self, store: GlobalStateStore) -> None:
        """セレクター付きで購読できること."""
        progress_values: list[float] = []

        def callback(state: dict) -> None:
            progress_values.append(state.get("execution.progress", 0))

        store.subscribe(callback, selector="execution.progress")
        store.dispatch(update_progress(0.5))

        assert len(progress_values) == 1

    def test_create_snapshot(self, store: GlobalStateStore) -> None:
        """スナップショットを作成できること."""
        store.dispatch(update_progress(0.5))
        snapshot_id = store.create_snapshot("テスト")

        assert snapshot_id is not None

        snapshots = store.get_snapshots()
        assert len(snapshots) >= 1

    def test_restore_snapshot(self, store: GlobalStateStore) -> None:
        """スナップショットから復元できること."""
        store.dispatch(update_progress(0.3))
        snapshot_id = store.create_snapshot("保存点")

        store.dispatch(update_progress(0.8))
        assert store.get_state("execution.progress") == 0.8

        success = store.restore_snapshot(snapshot_id)
        assert success is True
        assert store.get_state("execution.progress") == 0.3

    def test_get_action_history(self, store: GlobalStateStore) -> None:
        """アクション履歴を取得できること."""
        store.dispatch(update_progress(0.1))
        store.dispatch(update_progress(0.2))

        history = store.get_action_history(limit=10)
        assert len(history) >= 2

    def test_reset(self, store: GlobalStateStore) -> None:
        """状態をリセットできること."""
        store.dispatch(update_progress(0.9))
        store.dispatch(set_context({"key": "value"}))

        store.reset()

        assert store.get_state("execution.progress") == 0.0
        assert store.get_state("context") == {}

    def test_get_stats(self, store: GlobalStateStore) -> None:
        """統計情報を取得できること."""
        store.dispatch(update_progress(0.5))

        stats = store.get_stats()
        assert "state_version" in stats
        assert "subscription_count" in stats
        assert stats["state_version"] >= 1

    def test_version_increment(self, store: GlobalStateStore) -> None:
        """アクションごとにバージョンが増加すること."""
        initial_version = store.get_state("metadata.version")

        store.dispatch(update_progress(0.1))
        v1 = store.get_state("metadata.version")

        store.dispatch(update_progress(0.2))
        v2 = store.get_state("metadata.version")

        assert v1 > initial_version
        assert v2 > v1
