"""Plan-and-Execute Agent - 完全な Plan-then-Execute パターン実装.

業界最佳実践に基づいた統合エージェント:
- 動的計画生成
- ステップバイステップ実行
- 結果検証
- 失敗時の再計画

参考:
- LangChain Plan-and-Execute Agent
- Anthropic Building Effective Agents
"""

from __future__ import annotations

import logging
from typing import Any

from agentflow.patterns.executor import ExecutionError, PlanExecutor
from agentflow.patterns.planner import DynamicPlanner, Plan
from agentflow.patterns.replanner import Replanner
from agentflow.patterns.validator import StepValidator


class PlanAndExecuteAgent:
    """Plan-and-Execute エージェント.

    タスクを受け取り、計画を生成し、実行し、検証し、必要に応じて再計画します。

    Example:
        >>> agent = PlanAndExecuteAgent(
        ...     planner=my_planner,
        ...     executor=my_executor,
        ...     validator=my_validator,
        ...     replanner=my_replanner
        ... )
        >>> result = await agent.execute("Find the capital of France and its population")
        >>> print(result["final_answer"])
    """

    def __init__(
        self,
        planner: DynamicPlanner,
        executor: PlanExecutor,
        validator: StepValidator,
        replanner: Replanner,
        max_iterations: int = 5,
        logger: logging.Logger | None = None,
    ) -> None:
        """Plan-and-Execute エージェントを初期化.

        Args:
            planner: 動的プランナー
            executor: 計画実行器
            validator: ステップ検証器
            replanner: 再計画生成器
            max_iterations: 最大イテレーション数（デフォルト: 5）
            logger: ロガーインスタンス（オプション）
        """
        self._planner = planner
        self._executor = executor
        self._validator = validator
        self._replanner = replanner
        self._max_iterations = max_iterations
        self._logger = logger or logging.getLogger(__name__)

    async def execute(
        self,
        task: str,
        context: dict[str, Any] | None = None,
        user_id: str = "system",
    ) -> dict[str, Any]:
        """タスクを実行.

        Args:
            task: タスク説明
            context: 追加コンテキスト（オプション）
            user_id: ユーザー ID（審計ログ用）

        Returns:
            実行結果（final_answer, steps, iterations など）

        Example:
            >>> result = await agent.execute(
            ...     "What is the weather in Tokyo?",
            ...     context={"language": "en"}
            ... )
        """
        self._logger.info(f"Starting Plan-and-Execute for task: {task}")

        iteration = 0
        current_plan: Plan | None = None
        all_results: dict[str, Any] = {}

        while iteration < self._max_iterations:
            iteration += 1
            self._logger.info(f"Iteration {iteration}/{self._max_iterations}")

            try:
                # 1. 計画を生成（初回または再計画）
                if current_plan is None:
                    current_plan = await self._planner.create_plan(task, context)
                    self._logger.info(f"Plan created with {len(current_plan.steps)} steps")

                # 2. 計画を実行
                try:
                    step_results = await self._executor.execute_plan(current_plan, user_id)
                    all_results.update(step_results)
                except ExecutionError as e:
                    self._logger.warning(f"Execution error: {e}")
                    # 失敗したステップを特定
                    failed_step_id = self._find_failed_step(step_results)
                    if failed_step_id:
                        failed_step = next(
                            s for s in current_plan.steps if s.step_id == failed_step_id
                        )
                        # 検証結果を取得
                        validation_result = await self._validator.validate_step(
                            failed_step,
                            step_results[failed_step_id],
                        )
                        # 再計画
                        current_plan = await self._replanner.replan(
                            current_plan,
                            failed_step,
                            validation_result,
                            all_results,
                        )
                        continue
                    raise

                # 3. 各ステップを検証
                all_valid = True
                for step in current_plan.steps:
                    if step.step_id in step_results:
                        validation_result = await self._validator.validate_step(
                            step,
                            step_results[step.step_id],
                        )
                        if not validation_result.is_valid:
                            self._logger.warning(
                                f"Step {step.step_id} validation failed: {validation_result.error}"
                            )
                            all_valid = False
                            # 再計画
                            current_plan = await self._replanner.replan(
                                current_plan,
                                step,
                                validation_result,
                                all_results,
                            )
                            break

                # 4. すべて成功したら終了
                if all_valid:
                    self._logger.info("All steps completed successfully")
                    final_answer = self._extract_final_answer(current_plan, all_results)
                    return {
                        "success": True,
                        "final_answer": final_answer,
                        "steps": all_results,
                        "iterations": iteration,
                        "plan": current_plan.model_dump(),
                    }

            except Exception as e:
                self._logger.error(f"Iteration {iteration} failed: {e}")
                if iteration >= self._max_iterations:
                    raise

        # 最大イテレーション到達
        self._logger.warning(f"Max iterations ({self._max_iterations}) reached")
        return {
            "success": False,
            "error": "Max iterations reached",
            "steps": all_results,
            "iterations": iteration,
        }

    def _find_failed_step(self, step_results: dict[str, Any]) -> str | None:
        """失敗したステップ ID を見つける.

        Args:
            step_results: ステップ実行結果

        Returns:
            失敗したステップ ID（見つからない場合は None）
        """
        for step_id, result in step_results.items():
            if not result.get("success", False):
                return step_id
        return None

    def _extract_final_answer(
        self,
        plan: Plan,
        results: dict[str, Any],
    ) -> Any:
        """最終回答を抽出.

        Args:
            plan: 実行された計画
            results: すべてのステップ結果

        Returns:
            最終回答
        """
        # 最後のステップの結果を返す
        if plan.steps:
            last_step_id = plan.steps[-1].step_id
            if last_step_id in results:
                return results[last_step_id].get("result")
        return results

