import pytest
from agentflow.state.store import GlobalStateStore
from agentflow.state.actions import ActionType, create_action


def test_task_created_action():
    store = GlobalStateStore()

    store.dispatch(
        create_action(
            ActionType.TASK_CREATED,
            {"task_id": "task_exec_20240101_abc12345", "name": "Test Task"},
        )
    )

    tasks = store.get_state("tasks")
    assert tasks is not None
    assert "task_exec_20240101_abc12345" in tasks
    assert tasks["task_exec_20240101_abc12345"]["name"] == "Test Task"
    assert tasks["task_exec_20240101_abc12345"]["state"] == "created"


def test_task_planned_action():
    store = GlobalStateStore()

    store.dispatch(
        create_action(
            ActionType.TASK_CREATED,
            {"task_id": "task_exec_20240101_abc12345", "name": "Test Task"},
        )
    )
    store.dispatch(
        create_action(
            ActionType.TASK_PLANNED,
            {"task_id": "task_exec_20240101_abc12345"},
        )
    )

    tasks = store.get_state("tasks")
    assert tasks["task_exec_20240101_abc12345"]["state"] == "planned"


def test_task_started_action():
    store = GlobalStateStore()

    store.dispatch(
        create_action(
            ActionType.TASK_CREATED,
            {"task_id": "task_exec_20240101_abc12345", "name": "Test Task"},
        )
    )
    store.dispatch(
        create_action(
            ActionType.TASK_STARTED,
            {"task_id": "task_exec_20240101_abc12345"},
        )
    )

    tasks = store.get_state("tasks")
    assert tasks["task_exec_20240101_abc12345"]["state"] == "running"
    assert tasks["task_exec_20240101_abc12345"]["started_at"] is not None


def test_task_completed_action():
    store = GlobalStateStore()

    store.dispatch(
        create_action(
            ActionType.TASK_CREATED,
            {"task_id": "task_exec_20240101_abc12345", "name": "Test Task"},
        )
    )
    store.dispatch(
        create_action(
            ActionType.TASK_COMPLETED,
            {
                "task_id": "task_exec_20240101_abc12345",
                "outputs": {"result": "success"},
            },
        )
    )

    tasks = store.get_state("tasks")
    assert tasks["task_exec_20240101_abc12345"]["state"] == "done"
    assert tasks["task_exec_20240101_abc12345"]["completed_at"] is not None
    assert tasks["task_exec_20240101_abc12345"]["outputs"]["result"] == "success"


def test_task_failed_action():
    store = GlobalStateStore()

    store.dispatch(
        create_action(
            ActionType.TASK_CREATED,
            {"task_id": "task_exec_20240101_abc12345", "name": "Test Task"},
        )
    )
    store.dispatch(
        create_action(
            ActionType.TASK_FAILED,
            {"task_id": "task_exec_20240101_abc12345", "error": "Something went wrong"},
        )
    )

    tasks = store.get_state("tasks")
    assert tasks["task_exec_20240101_abc12345"]["state"] == "failed"
    assert tasks["task_exec_20240101_abc12345"]["error"] == "Something went wrong"


def test_task_cancelled_action():
    store = GlobalStateStore()

    store.dispatch(
        create_action(
            ActionType.TASK_CREATED,
            {"task_id": "task_exec_20240101_abc12345", "name": "Test Task"},
        )
    )
    store.dispatch(
        create_action(
            ActionType.TASK_CANCELLED,
            {"task_id": "task_exec_20240101_abc12345"},
        )
    )

    tasks = store.get_state("tasks")
    assert tasks["task_exec_20240101_abc12345"]["state"] == "cancelled"


def test_task_paused_action():
    store = GlobalStateStore()

    store.dispatch(
        create_action(
            ActionType.TASK_CREATED,
            {"task_id": "task_exec_20240101_abc12345", "name": "Test Task"},
        )
    )
    store.dispatch(
        create_action(
            ActionType.TASK_PAUSED,
            {"task_id": "task_exec_20240101_abc12345"},
        )
    )

    tasks = store.get_state("tasks")
    assert tasks["task_exec_20240101_abc12345"]["state"] == "paused"
