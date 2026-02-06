import pytest
from agentflow.task.task_graph import TaskGraph
from agentflow.task.task import Task
from agentflow.task.task_id import TaskID
from agentflow.task.task_state import TaskState


def test_task_graph_creation():
    graph = TaskGraph(id="test-graph", root_goal="Complete the mission")
    assert graph.id == "test-graph"
    assert graph.root_goal == "Complete the mission"
    assert len(graph.tasks) == 0


def test_task_graph_add_task():
    graph = TaskGraph(id="test", root_goal="Test goal")
    t1 = Task(id=TaskID.generate(), name="Task 1", state=TaskState.PLANNED)

    task_id = graph.add_task(t1)
    assert task_id == t1.id
    assert t1.id in graph.tasks


def test_task_graph_add_with_dependencies():
    graph = TaskGraph(id="test", root_goal="Test goal")
    t1 = Task(id=TaskID.generate(), name="Task 1", state=TaskState.PLANNED)
    t2 = Task(id=TaskID.generate(), name="Task 2", state=TaskState.PLANNED)

    graph.add_task(t1)
    graph.add_task(t2, blocked_by=[t1.id])

    assert t1.id in t2.blocked_by
    assert t2.id in graph.tasks[t1.id].blocks


def test_get_ready_tasks():
    graph = TaskGraph(id="test", root_goal="Test")
    t1 = Task(id=TaskID.generate(), name="T1", state=TaskState.DONE)
    t2 = Task(id=TaskID.generate(), name="T2", state=TaskState.PLANNED)

    graph.add_task(t1)
    graph.add_task(t2, blocked_by=[t1.id])

    ready = graph.get_ready_tasks()
    assert len(ready) == 1
    assert ready[0].id == t2.id


def test_get_ready_tasks_blocked():
    """Task should not be ready if dependency is not DONE."""
    graph = TaskGraph(id="test", root_goal="Test")
    t1 = Task(id=TaskID.generate(), name="T1", state=TaskState.RUNNING)
    t2 = Task(id=TaskID.generate(), name="T2", state=TaskState.PLANNED)

    graph.add_task(t1)
    graph.add_task(t2, blocked_by=[t1.id])

    ready = graph.get_ready_tasks()
    assert len(ready) == 0


def test_insert_after():
    """Test inserting a task after an existing one (for re-planning)."""
    graph = TaskGraph(id="test", root_goal="Test")
    t1 = Task(id=TaskID.generate(), name="T1", state=TaskState.DONE)
    t2 = Task(id=TaskID.generate(), name="T2", state=TaskState.PLANNED)
    t3 = Task(id=TaskID.generate(), name="T3", state=TaskState.PLANNED)

    graph.add_task(t1)
    graph.add_task(t3, blocked_by=[t1.id])

    # Insert t2 between t1 and t3
    graph.insert_after(t1.id, t2)

    # t2 should now be blocked by t1
    assert t1.id in t2.blocked_by
    # t3 should now be blocked by t2 (not t1)
    assert t2.id in t3.blocked_by
    assert t1.id not in t3.blocked_by
    # t1 should block t2
    assert t2.id in graph.tasks[t1.id].blocks


def test_version_increments():
    """TaskGraph version should increment on modifications."""
    graph = TaskGraph(id="test", root_goal="Test")
    initial_version = graph.version

    t1 = Task(id=TaskID.generate(), name="T1", state=TaskState.PLANNED)
    graph.add_task(t1)

    assert graph.version > initial_version


def test_multiple_dependencies():
    """Task can depend on multiple other tasks."""
    graph = TaskGraph(id="test", root_goal="Test")
    t1 = Task(id=TaskID.generate(), name="T1", state=TaskState.DONE)
    t2 = Task(id=TaskID.generate(), name="T2", state=TaskState.DONE)
    t3 = Task(id=TaskID.generate(), name="T3", state=TaskState.PLANNED)

    graph.add_task(t1)
    graph.add_task(t2)
    graph.add_task(t3, blocked_by=[t1.id, t2.id])

    assert len(t3.blocked_by) == 2
    ready = graph.get_ready_tasks()
    assert len(ready) == 1
    assert ready[0].id == t3.id
