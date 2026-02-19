from agentflow.reasoner.decisions import ActionDecision, ActionType


def test_action_decision_creation():
    decision = ActionDecision(
        next_action=ActionType.TOOL_CALL,
        action_params={"tool": "search", "query": "test"},
        reason="Need to search for information",
    )
    assert decision.next_action == ActionType.TOOL_CALL
    assert decision.confidence == 0.8  # default


def test_action_decision_default_values():
    decision = ActionDecision(
        next_action=ActionType.MESSAGE,
        reason="Responding to user",
    )
    assert decision.decision_id.startswith("dec-")
    assert decision.timestamp > 0
    assert decision.action_params == {}
    assert decision.chain_of_thought == []
    assert decision.required_evidence == []


def test_wait_for_evidence():
    decision = ActionDecision(
        next_action=ActionType.WAIT,
        reason="Insufficient evidence",
        required_evidence=["user confirmation"],
        confidence=0.0,
    )
    assert decision.next_action == ActionType.WAIT
    assert len(decision.required_evidence) == 1


def test_action_types():
    # Verify all action types are defined
    assert ActionType.TOOL_CALL
    assert ActionType.MESSAGE
    assert ActionType.DELEGATE
    assert ActionType.WAIT
    assert ActionType.ESCALATE
    assert ActionType.CLARIFY
    assert ActionType.COMPLETE
    assert ActionType.ABORT


def test_action_decision_with_chain_of_thought():
    decision = ActionDecision(
        next_action=ActionType.TOOL_CALL,
        action_params={"tool": "file_read", "path": "/tmp/data.txt"},
        reason="Need to read file contents",
        chain_of_thought=[
            "User asked about file contents",
            "File path was provided",
            "Need to read the file to answer",
        ],
        confidence=0.95,
    )
    assert len(decision.chain_of_thought) == 3
    assert decision.confidence == 0.95


def test_delegate_action():
    decision = ActionDecision(
        next_action=ActionType.DELEGATE,
        action_params={"agent": "research_agent", "task": "Find documentation"},
        reason="Task requires specialized agent",
    )
    assert decision.next_action == ActionType.DELEGATE
    assert decision.action_params["agent"] == "research_agent"


def test_complete_action():
    decision = ActionDecision(
        next_action=ActionType.COMPLETE,
        action_params={"output": "Task completed successfully"},
        reason="All objectives achieved",
        confidence=1.0,
    )
    assert decision.next_action == ActionType.COMPLETE
