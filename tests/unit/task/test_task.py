import pytest
from agentflow.task.task import Task
from agentflow.task.task_id import TaskID
from agentflow.task.task_state import TaskState


def test_task_creation():
    task = Task(id=TaskID.generate(), name="test task")
    assert task.state == TaskState.CREATED
    assert task.name == "test task"


def test_task_valid_transition():
    task = Task(id=TaskID.generate(), name="test")
    task.transition_to(TaskState.PLANNED)
    assert task.state == TaskState.PLANNED
    assert len(task.state_history) == 1


def test_task_invalid_transition():
    task = Task(id=TaskID.generate(), name="test")
    with pytest.raises(ValueError, match="Invalid transition"):
        task.transition_to(TaskState.DONE)


def test_task_started_at_timestamp():
    task = Task(id=TaskID.generate(), name="test")
    assert task.started_at is None

    task.transition_to(TaskState.PLANNED)
    task.transition_to(TaskState.RUNNING)
    assert task.started_at is not None


def test_task_completed_at_timestamp():
    task = Task(id=TaskID.generate(), name="test")
    assert task.completed_at is None

    task.transition_to(TaskState.PLANNED)
    task.transition_to(TaskState.RUNNING)
    task.transition_to(TaskState.VERIFIED)
    task.transition_to(TaskState.DONE)
    assert task.completed_at is not None


def test_task_with_parent():
    parent_id = TaskID.generate()
    task = Task(id=TaskID.generate(), name="child", parent_id=parent_id)
    assert task.parent_id == parent_id


def test_task_inputs_outputs():
    task = Task(
        id=TaskID.generate(),
        name="test",
        inputs={"query": "hello"},
        outputs={"result": "world"},
    )
    assert task.inputs["query"] == "hello"
    assert task.outputs["result"] == "world"


def test_task_full_lifecycle():
    """Test a complete task lifecycle."""
    task = Task(id=TaskID.generate(), name="full test")

    # created -> planned -> running -> verified -> done
    task.transition_to(TaskState.PLANNED)
    task.transition_to(TaskState.RUNNING)
    task.transition_to(TaskState.VERIFIED)
    task.transition_to(TaskState.DONE)

    assert task.state == TaskState.DONE
    assert len(task.state_history) == 4
    assert task.state_history[0][0] == TaskState.CREATED
    assert task.started_at is not None
    assert task.completed_at is not None
