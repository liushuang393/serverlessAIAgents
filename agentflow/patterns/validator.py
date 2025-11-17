"""Step Validator - ステップ実行結果の検証.

業界最佳実践に基づいた検証:
- 成功/失敗検出
- 品質評価
- 再計画トリガー

参考:
- Anthropic: Building Effective Agents
- LangChain: Output validation
"""

from __future__ import annotations

import logging
from typing import Any

from pydantic import BaseModel, Field


class ValidationResult(BaseModel):
    """検証結果.

    Example:
        >>> result = ValidationResult(
        ...     is_valid=True,
        ...     confidence=0.95,
        ...     error=None,
        ...     suggestions=[]
        ... )
    """

    is_valid: bool = Field(..., description="検証が成功したか")
    confidence: float = Field(
        default=1.0,
        description="信頼度（0.0-1.0）",
        ge=0.0,
        le=1.0,
    )
    error: str | None = Field(
        default=None,
        description="エラーメッセージ（失敗時）",
    )
    suggestions: list[str] = Field(
        default_factory=list,
        description="改善提案",
    )


class StepValidator:
    """ステップ検証器.

    ステップの実行結果を検証し、品質を評価します。

    Example:
        >>> validator = StepValidator()
        >>> result = await validator.validate_step(step, execution_result)
        >>> if not result.is_valid:
        ...     print(f"Validation failed: {result.error}")
    """

    def __init__(
        self,
        llm: Any | None = None,
        logger: logging.Logger | None = None,
    ) -> None:
        """ステップ検証器を初期化.

        Args:
            llm: LLM インスタンス（品質評価用、オプション）
            logger: ロガーインスタンス（オプション）
        """
        self._llm = llm
        self._logger = logger or logging.getLogger(__name__)

    async def validate_step(
        self,
        step: Any,
        execution_result: dict[str, Any],
    ) -> ValidationResult:
        """ステップ実行結果を検証.

        Args:
            step: 実行されたステップ
            execution_result: 実行結果

        Returns:
            検証結果

        Example:
            >>> result = await validator.validate_step(step, {"success": True, "result": "data"})
            >>> print(result.is_valid)  # True
        """
        self._logger.debug(f"Validating step: {step.step_id}")

        # 基本的な成功/失敗チェック
        if not execution_result.get("success", False):
            error = execution_result.get("error", "Unknown error")
            return ValidationResult(
                is_valid=False,
                confidence=1.0,
                error=error,
                suggestions=["Retry the step", "Check tool parameters"],
            )

        # 結果が空でないかチェック
        result_data = execution_result.get("result")
        if result_data is None or (isinstance(result_data, (list, dict, str)) and not result_data):
            return ValidationResult(
                is_valid=False,
                confidence=0.8,
                error="Result is empty",
                suggestions=["Verify tool output", "Check if tool executed correctly"],
            )

        # LLM による品質評価（オプション）
        if self._llm:
            quality_result = await self._evaluate_quality(step, execution_result)
            return quality_result

        # デフォルト: 成功
        return ValidationResult(
            is_valid=True,
            confidence=0.9,
            error=None,
            suggestions=[],
        )

    async def _evaluate_quality(
        self,
        step: Any,
        execution_result: dict[str, Any],
    ) -> ValidationResult:
        """LLM を使用して結果の品質を評価.

        Args:
            step: 実行されたステップ
            execution_result: 実行結果

        Returns:
            検証結果
        """
        prompt = f"""Evaluate the quality of this step execution result.

Step: {step.description}
Tool: {step.tool}
Result: {execution_result.get('result')}

Is the result satisfactory for the step's purpose? Answer with:
- VALID: if the result is good
- INVALID: if the result is problematic
- Provide a brief explanation

Format:
Status: [VALID/INVALID]
Confidence: [0.0-1.0]
Explanation: [brief explanation]
"""

        try:
            response = await self._llm.generate(prompt)
            return self._parse_quality_response(response)
        except Exception as e:
            self._logger.warning(f"Quality evaluation failed: {e}")
            # フォールバック: 成功とみなす
            return ValidationResult(
                is_valid=True,
                confidence=0.7,
                error=None,
                suggestions=[],
            )

    def _parse_quality_response(self, response: str) -> ValidationResult:
        """LLM レスポンスをパース.

        Args:
            response: LLM レスポンス

        Returns:
            検証結果
        """
        lines = response.strip().split("\n")
        status = "VALID"
        confidence = 0.8
        explanation = ""

        for line in lines:
            if line.startswith("Status:"):
                status = line.split(":", 1)[1].strip()
            elif line.startswith("Confidence:"):
                try:
                    confidence = float(line.split(":", 1)[1].strip())
                except ValueError:
                    confidence = 0.8
            elif line.startswith("Explanation:"):
                explanation = line.split(":", 1)[1].strip()

        is_valid = status.upper() == "VALID"

        return ValidationResult(
            is_valid=is_valid,
            confidence=confidence,
            error=None if is_valid else explanation,
            suggestions=[] if is_valid else ["Review step execution", "Consider alternative approach"],
        )

