from agentflow.task.task_state import TaskState, can_transition, is_terminal


def test_valid_transition():
    assert can_transition(TaskState.CREATED, TaskState.PLANNED)
    assert can_transition(TaskState.RUNNING, TaskState.VERIFIED)


def test_invalid_transition():
    assert not can_transition(TaskState.DONE, TaskState.RUNNING)
    assert not can_transition(TaskState.CREATED, TaskState.DONE)


def test_terminal_states():
    assert is_terminal(TaskState.DONE)
    assert is_terminal(TaskState.FAILED)
    assert is_terminal(TaskState.CANCELLED)
    assert not is_terminal(TaskState.RUNNING)


def test_all_states_have_transitions():
    """Every state should be defined in TASK_TRANSITIONS."""
    from agentflow.task.task_state import TASK_TRANSITIONS

    for state in TaskState:
        assert state in TASK_TRANSITIONS


def test_paused_can_resume():
    assert can_transition(TaskState.PAUSED, TaskState.RUNNING)
    assert can_transition(TaskState.PAUSED, TaskState.CANCELLED)


def test_waiting_input_transitions():
    assert can_transition(TaskState.WAITING_INPUT, TaskState.RUNNING)
    assert can_transition(TaskState.WAITING_INPUT, TaskState.CANCELLED)


def test_replanning_transitions():
    assert can_transition(TaskState.REPLANNING, TaskState.PLANNED)
    assert can_transition(TaskState.REPLANNING, TaskState.FAILED)
