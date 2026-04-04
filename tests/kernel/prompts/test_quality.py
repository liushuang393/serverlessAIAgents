"""プロンプト品質チェックのユニットテスト."""

from __future__ import annotations

import pytest

from kernel.prompts.models import (
    LAYER_CORE_SYSTEM,
    LAYER_RUNTIME_CONTEXT,
    LAYER_TASK_SYSTEM,
    AssembledPrompt,
    PromptPattern,
)
from kernel.prompts.patterns import SINGLE_TASK_CONFIG, PatternConfig
from kernel.prompts.quality import PromptQualityChecker, QualityReport


@pytest.fixture
def checker() -> PromptQualityChecker:
    return PromptQualityChecker()


class TestQualityReport:
    """QualityReport のテスト."""

    def test_overall_score_perfect(self) -> None:
        report = QualityReport(
            redundancy_score=0.0,
            completeness_score=1.0,
            minimality_score=1.0,
        )
        assert report.overall_score == 1.0
        assert report.passed is True

    def test_overall_score_poor(self) -> None:
        report = QualityReport(
            redundancy_score=1.0,
            completeness_score=0.0,
            minimality_score=0.0,
        )
        assert report.overall_score == 0.0
        assert report.passed is False

    def test_passed_threshold(self) -> None:
        # 総合スコア0.6以上でパス
        report = QualityReport(
            redundancy_score=0.2,
            completeness_score=0.7,
            minimality_score=0.7,
        )
        assert report.passed is True


class TestCheckRedundancy:
    """冗長性チェックのテスト."""

    def test_no_redundancy(self, checker: PromptQualityChecker) -> None:
        prompt = AssembledPrompt(
            system_prompt="行1: ユニークな内容\n行2: 別のユニークな内容",
            pattern=PromptPattern.SINGLE_TASK,
            token_count=100,
            token_budget=4000,
        )
        score = checker.check_redundancy(prompt)
        assert score < 0.3

    def test_high_redundancy(self, checker: PromptQualityChecker) -> None:
        # 同じ行を繰り返す
        lines = ["これは重複するテストの行です。"] * 10
        prompt = AssembledPrompt(
            system_prompt="\n".join(lines),
            pattern=PromptPattern.SINGLE_TASK,
            token_count=200,
            token_budget=4000,
        )
        score = checker.check_redundancy(prompt)
        assert score > 0.5

    def test_empty_prompt(self, checker: PromptQualityChecker) -> None:
        prompt = AssembledPrompt(
            system_prompt="",
            pattern=PromptPattern.SINGLE_TASK,
            token_count=0,
            token_budget=4000,
        )
        score = checker.check_redundancy(prompt)
        assert score == 0.0


class TestCheckCompleteness:
    """完全性チェックのテスト."""

    def test_all_required_present(self, checker: PromptQualityChecker) -> None:
        prompt = AssembledPrompt(
            system_prompt="テスト",
            pattern=PromptPattern.SINGLE_TASK,
            layers_used=[LAYER_CORE_SYSTEM, LAYER_TASK_SYSTEM, LAYER_RUNTIME_CONTEXT],
            token_count=100,
            token_budget=4000,
        )
        score = checker.check_completeness(prompt, SINGLE_TASK_CONFIG)
        assert score == 1.0

    def test_missing_required(self, checker: PromptQualityChecker) -> None:
        prompt = AssembledPrompt(
            system_prompt="テスト",
            pattern=PromptPattern.SINGLE_TASK,
            layers_used=[LAYER_CORE_SYSTEM],  # task_system, runtime_context 欠落
            token_count=100,
            token_budget=4000,
        )
        score = checker.check_completeness(prompt, SINGLE_TASK_CONFIG)
        # 3必須のうち1つのみ → 1/3
        assert abs(score - 1 / 3) < 0.01

    def test_no_required_layers(self, checker: PromptQualityChecker) -> None:
        """必須レイヤーがないパターン."""
        config = PatternConfig(
            pattern=PromptPattern.SINGLE_TASK,
            required_layers=(),
        )
        prompt = AssembledPrompt(
            system_prompt="テスト",
            pattern=PromptPattern.SINGLE_TASK,
            token_count=100,
            token_budget=4000,
        )
        score = checker.check_completeness(prompt, config)
        assert score == 1.0


class TestCheckMinimality:
    """最小性チェックのテスト."""

    def test_optimal_usage(self, checker: PromptQualityChecker) -> None:
        """最適ゾーン（30%-80%）."""
        prompt = AssembledPrompt(
            system_prompt="テスト",
            pattern=PromptPattern.SINGLE_TASK,
            token_count=2000,  # 50% of 4000
            token_budget=4000,
        )
        score = checker.check_minimality(prompt)
        assert score == 1.0

    def test_under_utilized(self, checker: PromptQualityChecker) -> None:
        """活用不足（30%未満）."""
        prompt = AssembledPrompt(
            system_prompt="テスト",
            pattern=PromptPattern.SINGLE_TASK,
            token_count=100,  # 2.5% of 4000
            token_budget=4000,
        )
        score = checker.check_minimality(prompt)
        assert score < 1.0

    def test_over_budget(self, checker: PromptQualityChecker) -> None:
        """予算超過に近い（80%超）."""
        prompt = AssembledPrompt(
            system_prompt="テスト",
            pattern=PromptPattern.SINGLE_TASK,
            token_count=3800,  # 95% of 4000
            token_budget=4000,
        )
        score = checker.check_minimality(prompt)
        assert score < 1.0

    def test_zero_budget(self, checker: PromptQualityChecker) -> None:
        prompt = AssembledPrompt(
            system_prompt="テスト",
            pattern=PromptPattern.SINGLE_TASK,
            token_count=100,
            token_budget=0,
        )
        score = checker.check_minimality(prompt)
        assert score == 1.0


class TestFullCheck:
    """full_check() のテスト."""

    def test_good_prompt(self, checker: PromptQualityChecker) -> None:
        prompt = AssembledPrompt(
            system_prompt="あなたはテスト用エージェントです。\nタスク: API設計\n入力: 検索要求",
            pattern=PromptPattern.SINGLE_TASK,
            layers_used=[LAYER_CORE_SYSTEM, LAYER_TASK_SYSTEM, LAYER_RUNTIME_CONTEXT],
            token_count=2000,
            token_budget=4000,
        )
        report = checker.full_check(prompt, SINGLE_TASK_CONFIG)
        assert report.completeness_score == 1.0
        assert report.passed is True

    def test_issues_detected(self, checker: PromptQualityChecker) -> None:
        """問題が検出される場合."""
        prompt = AssembledPrompt(
            system_prompt="重複テスト\n" * 20,
            pattern=PromptPattern.SINGLE_TASK,
            layers_used=[LAYER_CORE_SYSTEM],  # 必須レイヤー欠落
            layers_dropped=["memory_profile"],
            token_count=100,
            token_budget=4000,
        )
        report = checker.full_check(prompt, SINGLE_TASK_CONFIG)
        assert report.completeness_score < 1.0
        assert len(report.issues) > 0

    def test_dropped_layers_warning(self, checker: PromptQualityChecker) -> None:
        """ドロップレイヤーの警告."""
        prompt = AssembledPrompt(
            system_prompt="テスト",
            pattern=PromptPattern.SINGLE_TASK,
            layers_used=[LAYER_CORE_SYSTEM, LAYER_TASK_SYSTEM, LAYER_RUNTIME_CONTEXT],
            layers_dropped=["memory_profile", "tool_environment"],
            token_count=2000,
            token_budget=4000,
        )
        report = checker.full_check(prompt, SINGLE_TASK_CONFIG)
        assert any("ドロップ" in issue for issue in report.issues)
