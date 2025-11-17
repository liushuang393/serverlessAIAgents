"""StepValidator のテスト."""

import pytest

from agentflow.patterns.planner import Step
from agentflow.patterns.validator import StepValidator, ValidationResult


@pytest.mark.asyncio
class TestValidationResult:
    """ValidationResult クラスのテスト."""

    def test_validation_result_success(self) -> None:
        """成功した検証結果をテスト."""
        result = ValidationResult(
            is_valid=True,
            confidence=0.95,
            error=None,
            suggestions=[],
        )

        assert result.is_valid is True
        assert result.confidence == 0.95
        assert result.error is None
        assert len(result.suggestions) == 0

    def test_validation_result_failure(self) -> None:
        """失敗した検証結果をテスト."""
        result = ValidationResult(
            is_valid=False,
            confidence=0.8,
            error="Result is empty",
            suggestions=["Retry the step", "Check parameters"],
        )

        assert result.is_valid is False
        assert result.confidence == 0.8
        assert result.error == "Result is empty"
        assert len(result.suggestions) == 2


@pytest.mark.asyncio
class TestStepValidator:
    """StepValidator クラスのテスト."""

    @pytest.fixture
    def validator(self) -> StepValidator:
        """テスト用の StepValidator を作成."""
        return StepValidator()

    @pytest.fixture
    def sample_step(self) -> Step:
        """テスト用のステップを作成."""
        return Step(
            step_id="E1",
            description="Search for information",
            tool="search",
            parameters={"query": "AI agents"},
            dependencies=[],
        )

    async def test_validate_step_success(
        self,
        validator: StepValidator,
        sample_step: Step,
    ) -> None:
        """成功したステップの検証をテスト."""
        execution_result = {
            "success": True,
            "result": "AI agents are software programs",
        }

        result = await validator.validate_step(sample_step, execution_result)

        assert result.is_valid is True
        assert result.error is None

    async def test_validate_step_failure(
        self,
        validator: StepValidator,
        sample_step: Step,
    ) -> None:
        """失敗したステップの検証をテスト."""
        execution_result = {
            "success": False,
            "error": "Tool not found",
        }

        result = await validator.validate_step(sample_step, execution_result)

        assert result.is_valid is False
        assert result.error == "Tool not found"
        assert len(result.suggestions) > 0

    async def test_validate_step_empty_result(
        self,
        validator: StepValidator,
        sample_step: Step,
    ) -> None:
        """空の結果の検証をテスト."""
        execution_result = {
            "success": True,
            "result": "",
        }

        result = await validator.validate_step(sample_step, execution_result)

        assert result.is_valid is False
        assert "empty" in result.error.lower()

    async def test_validate_step_none_result(
        self,
        validator: StepValidator,
        sample_step: Step,
    ) -> None:
        """None 結果の検証をテスト."""
        execution_result = {
            "success": True,
            "result": None,
        }

        result = await validator.validate_step(sample_step, execution_result)

        assert result.is_valid is False
        assert result.error is not None

    async def test_validate_step_empty_list_result(
        self,
        validator: StepValidator,
        sample_step: Step,
    ) -> None:
        """空のリスト結果の検証をテスト."""
        execution_result = {
            "success": True,
            "result": [],
        }

        result = await validator.validate_step(sample_step, execution_result)

        assert result.is_valid is False

    async def test_validate_step_empty_dict_result(
        self,
        validator: StepValidator,
        sample_step: Step,
    ) -> None:
        """空の辞書結果の検証をテスト."""
        execution_result = {
            "success": True,
            "result": {},
        }

        result = await validator.validate_step(sample_step, execution_result)

        assert result.is_valid is False

