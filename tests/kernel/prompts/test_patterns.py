"""プロンプトパターンのユニットテスト."""

from __future__ import annotations

import pytest

from kernel.prompts.models import (
    LAYER_CONVERSATION_STATE,
    LAYER_CORE_SYSTEM,
    LAYER_MEMORY_PROFILE,
    LAYER_RUNTIME_CONTEXT,
    LAYER_TASK_SYSTEM,
    LAYER_TOOL_ENVIRONMENT,
    PromptPattern,
)
from kernel.prompts.patterns import (
    MULTI_STEP_CONFIG,
    MULTI_TURN_CONFIG,
    PATTERN_REGISTRY,
    SINGLE_TASK_CONFIG,
    TOOL_AUGMENTED_CONFIG,
    get_pattern_config,
    select_pattern,
)


class TestPatternRegistry:
    """パターンレジストリのテスト."""

    def test_all_patterns_registered(self) -> None:
        """全4パターンが登録されていること."""
        assert len(PATTERN_REGISTRY) == 4
        for p in PromptPattern:
            assert p in PATTERN_REGISTRY

    def test_budget_ratios_sum_to_one(self) -> None:
        """各パターンの予算配分比率が合計1.0になること."""
        for pattern, config in PATTERN_REGISTRY.items():
            total = sum(config.budget_ratios.values())
            assert abs(total - 1.0) < 0.01, f"{pattern}: 予算比率合計 = {total}"

    def test_required_layers_in_budget(self) -> None:
        """必須レイヤーが予算配分に含まれていること."""
        for pattern, config in PATTERN_REGISTRY.items():
            for layer in config.required_layers:
                assert layer in config.budget_ratios, f"{pattern}: {layer} が予算配分に未定義"


class TestSingleTaskConfig:
    """SINGLE_TASK パターンのテスト."""

    def test_required_layers(self) -> None:
        assert LAYER_CORE_SYSTEM in SINGLE_TASK_CONFIG.required_layers
        assert LAYER_TASK_SYSTEM in SINGLE_TASK_CONFIG.required_layers
        assert LAYER_RUNTIME_CONTEXT in SINGLE_TASK_CONFIG.required_layers

    def test_no_optional_layers(self) -> None:
        assert SINGLE_TASK_CONFIG.optional_layers == ()


class TestMultiStepConfig:
    """MULTI_STEP パターンのテスト."""

    def test_optional_layers(self) -> None:
        assert LAYER_CONVERSATION_STATE in MULTI_STEP_CONFIG.optional_layers
        assert LAYER_MEMORY_PROFILE in MULTI_STEP_CONFIG.optional_layers


class TestMultiTurnConfig:
    """MULTI_TURN パターンのテスト."""

    def test_conversation_state_required(self) -> None:
        """会話状態が必須であること."""
        assert LAYER_CONVERSATION_STATE in MULTI_TURN_CONFIG.required_layers

    def test_task_system_optional(self) -> None:
        """タスク定義はオプションであること（会話継続では不要な場合がある）."""
        assert LAYER_TASK_SYSTEM in MULTI_TURN_CONFIG.optional_layers


class TestToolAugmentedConfig:
    """TOOL_AUGMENTED パターンのテスト."""

    def test_tool_env_required(self) -> None:
        assert LAYER_TOOL_ENVIRONMENT in TOOL_AUGMENTED_CONFIG.required_layers

    def test_memory_optional(self) -> None:
        assert LAYER_MEMORY_PROFILE in TOOL_AUGMENTED_CONFIG.optional_layers


class TestGetPatternConfig:
    """get_pattern_config() のテスト."""

    def test_valid_pattern(self) -> None:
        config = get_pattern_config(PromptPattern.SINGLE_TASK)
        assert config.pattern == PromptPattern.SINGLE_TASK

    def test_invalid_pattern(self) -> None:
        with pytest.raises(KeyError, match="未登録パターン"):
            get_pattern_config("nonexistent")  # type: ignore[arg-type]


class TestSelectPattern:
    """select_pattern() のテスト."""

    def test_default_single_task(self) -> None:
        """デフォルトは SINGLE_TASK."""
        assert select_pattern() == PromptPattern.SINGLE_TASK

    def test_tools_priority(self) -> None:
        """ツールがあれば TOOL_AUGMENTED（最優先）."""
        result = select_pattern(has_tools=True, turn_count=5, has_plan=True)
        assert result == PromptPattern.TOOL_AUGMENTED

    def test_plan_priority(self) -> None:
        """計画があれば MULTI_STEP（ツールなし時）."""
        result = select_pattern(has_plan=True, turn_count=5)
        assert result == PromptPattern.MULTI_STEP

    def test_multi_turn(self) -> None:
        """ターン2以上で MULTI_TURN."""
        result = select_pattern(turn_count=2)
        assert result == PromptPattern.MULTI_TURN

    def test_turn_count_one_is_single(self) -> None:
        """ターン1は SINGLE_TASK（マルチターンではない）."""
        result = select_pattern(turn_count=1)
        assert result == PromptPattern.SINGLE_TASK

    def test_tools_only(self) -> None:
        result = select_pattern(has_tools=True)
        assert result == PromptPattern.TOOL_AUGMENTED

    def test_plan_only(self) -> None:
        result = select_pattern(has_plan=True)
        assert result == PromptPattern.MULTI_STEP
