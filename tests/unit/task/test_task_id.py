import pytest
from agentflow.task.task_id import TaskID


def test_task_id_generation():
    task_id = TaskID.generate("exec")
    assert str(task_id).startswith("task_exec_")
    assert len(str(task_id).split("_")) == 4


def test_task_id_parse():
    task_id = TaskID.generate("sub")
    parsed = TaskID.parse(str(task_id))
    assert parsed.type == "sub"


def test_task_id_uniqueness():
    id1 = TaskID.generate("exec")
    id2 = TaskID.generate("exec")
    assert str(id1) != str(id2)


def test_task_id_frozen():
    task_id = TaskID.generate("exec")
    with pytest.raises(AttributeError):
        task_id.type = "new_type"
