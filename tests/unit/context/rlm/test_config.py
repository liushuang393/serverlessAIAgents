"""Unit tests for RLM config module."""

from agentflow.context.rlm.config import (
    ActionType,
    RLMConfig,
    StopReason,
    SubCallBudget,
)


class TestRLMConfig:
    """Tests for RLMConfig."""

    def test_default_values(self) -> None:
        """Test default configuration values."""
        config = RLMConfig()

        assert config.activation_threshold == 15_000
        assert config.max_iterations == 20
        assert config.max_subcall_budget == 50_000
        assert config.max_subcalls == 30
        assert config.convergence_threshold == 0.90
        assert config.chunk_size == 2000
        assert config.prefer_deterministic is True
        assert config.workspace_capacity == 50
        assert config.workspace_token_budget == 10_000
        assert config.emit_events is True
        assert config.debug_mode is False

    def test_custom_values(self) -> None:
        """Test custom configuration values."""
        config = RLMConfig(
            activation_threshold=20_000,
            max_iterations=30,
            convergence_threshold=0.95,
        )

        assert config.activation_threshold == 20_000
        assert config.max_iterations == 30
        assert config.convergence_threshold == 0.95

    def test_validate_valid_config(self) -> None:
        """Test validation of valid config."""
        config = RLMConfig()
        errors = config.validate()
        assert errors == []
        assert config.is_valid() is True

    def test_validate_invalid_activation_threshold(self) -> None:
        """Test validation with invalid activation threshold."""
        config = RLMConfig(activation_threshold=500)
        errors = config.validate()
        assert any("activation_threshold" in e for e in errors)
        assert config.is_valid() is False

    def test_validate_invalid_max_iterations(self) -> None:
        """Test validation with invalid max iterations."""
        config = RLMConfig(max_iterations=0)
        errors = config.validate()
        assert any("max_iterations" in e for e in errors)

    def test_validate_too_many_iterations(self) -> None:
        """Test validation with too many iterations."""
        config = RLMConfig(max_iterations=150)
        errors = config.validate()
        assert any("max_iterations" in e and "100" in e for e in errors)

    def test_validate_invalid_convergence_threshold(self) -> None:
        """Test validation with invalid convergence threshold."""
        config = RLMConfig(convergence_threshold=1.5)
        errors = config.validate()
        assert any("convergence_threshold" in e for e in errors)

    def test_validate_invalid_subcall_budget(self) -> None:
        """Test validation with invalid subcall budget."""
        config = RLMConfig(max_subcall_budget=500)
        errors = config.validate()
        assert any("max_subcall_budget" in e for e in errors)


class TestSubCallBudget:
    """Tests for SubCallBudget."""

    def test_default_values(self) -> None:
        """Test default budget values."""
        budget = SubCallBudget()

        assert budget.max_tokens == 50_000
        assert budget.max_calls == 30
        assert budget.tokens_used == 0
        assert budget.calls_made == 0

    def test_remaining_tokens(self) -> None:
        """Test remaining tokens calculation."""
        budget = SubCallBudget(max_tokens=10000, tokens_used=3000)
        assert budget.remaining_tokens == 7000

    def test_remaining_calls(self) -> None:
        """Test remaining calls calculation."""
        budget = SubCallBudget(max_calls=10, calls_made=4)
        assert budget.remaining_calls == 6

    def test_is_exhausted_tokens(self) -> None:
        """Test budget exhaustion by tokens."""
        budget = SubCallBudget(max_tokens=1000, tokens_used=1000)
        assert budget.is_exhausted is True

    def test_is_exhausted_calls(self) -> None:
        """Test budget exhaustion by calls."""
        budget = SubCallBudget(max_calls=5, calls_made=5)
        assert budget.is_exhausted is True

    def test_is_not_exhausted(self) -> None:
        """Test budget not exhausted."""
        budget = SubCallBudget(max_tokens=1000, max_calls=10)
        assert budget.is_exhausted is False

    def test_can_afford_true(self) -> None:
        """Test can_afford when budget available."""
        budget = SubCallBudget(max_tokens=10000, max_calls=10)
        assert budget.can_afford(5000) is True

    def test_can_afford_false_tokens(self) -> None:
        """Test can_afford when not enough tokens."""
        budget = SubCallBudget(max_tokens=1000, tokens_used=800)
        assert budget.can_afford(500) is False

    def test_can_afford_false_calls(self) -> None:
        """Test can_afford when no calls remaining."""
        budget = SubCallBudget(max_calls=5, calls_made=5)
        assert budget.can_afford(100) is False

    def test_consume(self) -> None:
        """Test consuming budget."""
        budget = SubCallBudget()
        budget.consume(1000)

        assert budget.tokens_used == 1000
        assert budget.calls_made == 1

    def test_consume_multiple(self) -> None:
        """Test consuming budget multiple times."""
        budget = SubCallBudget()
        budget.consume(500)
        budget.consume(300)
        budget.consume(200)

        assert budget.tokens_used == 1000
        assert budget.calls_made == 3

    def test_to_dict(self) -> None:
        """Test converting budget to dict."""
        budget = SubCallBudget(max_tokens=10000, max_calls=20, tokens_used=3000, calls_made=5)
        d = budget.to_dict()

        assert d["max_tokens"] == 10000
        assert d["max_calls"] == 20
        assert d["tokens_used"] == 3000
        assert d["calls_made"] == 5
        assert d["remaining_tokens"] == 7000
        assert d["remaining_calls"] == 15

    def test_from_config(self) -> None:
        """Test creating budget from config."""
        config = RLMConfig(max_subcall_budget=30000, max_subcalls=15)
        budget = SubCallBudget.from_config(config)

        assert budget.max_tokens == 30000
        assert budget.max_calls == 15
        assert budget.tokens_used == 0
        assert budget.calls_made == 0


class TestStopReason:
    """Tests for StopReason enum."""

    def test_values(self) -> None:
        """Test stop reason values."""
        assert StopReason.ANSWER_FOUND.value == "answer_found"
        assert StopReason.BUDGET_EXHAUSTED.value == "budget_exhausted"
        assert StopReason.MAX_ITERATIONS.value == "max_iterations"
        assert StopReason.CONVERGENCE.value == "convergence"
        assert StopReason.ERROR.value == "error"
        assert StopReason.USER_ABORT.value == "user_abort"


class TestActionType:
    """Tests for ActionType enum."""

    def test_low_cost_actions(self) -> None:
        """Test low-cost action types."""
        assert ActionType.PEEK.value == "peek"
        assert ActionType.REGEX_FIND.value == "regex_find"
        assert ActionType.KEYWORD_FIND.value == "keyword_find"
        assert ActionType.GET_STRUCTURE.value == "get_structure"

    def test_semantic_actions(self) -> None:
        """Test semantic action types."""
        assert ActionType.SEMANTIC_SEARCH.value == "semantic_search"
        assert ActionType.SUMMARIZE.value == "summarize"
        assert ActionType.EXTRACT.value == "extract"

    def test_terminal_actions(self) -> None:
        """Test terminal action types."""
        assert ActionType.SYNTHESIZE.value == "synthesize"
        assert ActionType.GIVE_UP.value == "give_up"
