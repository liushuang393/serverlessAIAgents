"""Replanner - 失敗時の再計画生成.

業界最佳実践に基づいた再計画:
- 失敗分析
- 代替アプローチ生成
- 部分的な再計画

参考:
- Anthropic: Error recovery patterns
- LangChain: Adaptive planning
"""

from __future__ import annotations

import logging
from typing import Any

from agentflow.patterns.planner import DynamicPlanner, Plan, Step
from agentflow.patterns.validator import ValidationResult


class Replanner:
    """再計画生成器.

    ステップ失敗時に代替計画を生成します。

    Example:
        >>> replanner = Replanner(planner=my_planner)
        >>> new_plan = await replanner.replan(
        ...     original_plan=plan,
        ...     failed_step=step,
        ...     validation_result=result,
        ...     execution_results=results
        ... )
    """

    def __init__(
        self,
        planner: DynamicPlanner,
        max_replans: int = 3,
        logger: logging.Logger | None = None,
    ) -> None:
        """再計画生成器を初期化.

        Args:
            planner: 動的プランナー
            max_replans: 最大再計画回数（デフォルト: 3）
            logger: ロガーインスタンス（オプション）
        """
        self._planner = planner
        self._max_replans = max_replans
        self._logger = logger or logging.getLogger(__name__)

    async def replan(
        self,
        original_plan: Plan,
        failed_step: Step,
        validation_result: ValidationResult,
        execution_results: dict[str, Any],
    ) -> Plan:
        """失敗したステップの代替計画を生成.

        Args:
            original_plan: 元の計画
            failed_step: 失敗したステップ
            validation_result: 検証結果
            execution_results: これまでの実行結果

        Returns:
            新しい計画

        Example:
            >>> new_plan = await replanner.replan(
            ...     original_plan=plan,
            ...     failed_step=step,
            ...     validation_result=result,
            ...     execution_results={"E1": {...}}
            ... )
        """
        self._logger.info(f"Replanning after step {failed_step.step_id} failure")

        # 失敗コンテキストを構築
        context = self._build_replan_context(
            original_plan,
            failed_step,
            validation_result,
            execution_results,
        )

        # 新しいタスク説明を生成
        new_task = self._generate_replan_task(
            original_plan,
            failed_step,
            validation_result,
        )

        # 新しい計画を生成
        new_plan = await self._planner.create_plan(new_task, context)

        self._logger.info(f"Replanning completed with {len(new_plan.steps)} steps")
        return new_plan

    def _build_replan_context(
        self,
        original_plan: Plan,
        failed_step: Step,
        validation_result: ValidationResult,
        execution_results: dict[str, Any],
    ) -> dict[str, Any]:
        """再計画用のコンテキストを構築.

        Args:
            original_plan: 元の計画
            failed_step: 失敗したステップ
            validation_result: 検証結果
            execution_results: これまでの実行結果

        Returns:
            コンテキスト辞書
        """
        # 成功したステップを収集
        successful_steps = []
        for step in original_plan.steps:
            if step.step_id in execution_results:
                result = execution_results[step.step_id]
                if result.get("success", False):
                    successful_steps.append(
                        {
                            "step_id": step.step_id,
                            "description": step.description,
                            "result": result.get("result"),
                        }
                    )

        return {
            "original_task": original_plan.task,
            "failed_step_id": failed_step.step_id,
            "failed_step_description": failed_step.description,
            "error": validation_result.error,
            "suggestions": validation_result.suggestions,
            "successful_steps": successful_steps,
            "remaining_steps": [
                s.description
                for s in original_plan.steps
                if s.step_id > failed_step.step_id
            ],
        }

    def _generate_replan_task(
        self,
        original_plan: Plan,
        failed_step: Step,
        validation_result: ValidationResult,
    ) -> str:
        """再計画用のタスク説明を生成.

        Args:
            original_plan: 元の計画
            failed_step: 失敗したステップ
            validation_result: 検証結果

        Returns:
            新しいタスク説明
        """
        error_msg = validation_result.error or "Unknown error"
        suggestions = ", ".join(validation_result.suggestions) if validation_result.suggestions else "None"

        task = f"""Original task: {original_plan.task}

Failed step: {failed_step.description}
Tool used: {failed_step.tool}
Error: {error_msg}
Suggestions: {suggestions}

Create an alternative plan to complete the original task, avoiding the failed approach.
Consider using different tools or breaking down the task differently.
"""
        return task

