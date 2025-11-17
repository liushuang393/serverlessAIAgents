"""Plan Executor - 計画を実行し、変数を置換.

業界最佳実践に基づいた実行:
- ステップバイステップ実行
- 変数置換（#E1, #E2）
- 結果収集
- エラーハンドリング

参考:
- LangChain Plan-and-Execute
- ReWOO: Variable substitution
"""

from __future__ import annotations

import logging
import re
from typing import Any

from agentflow.patterns.planner import Plan, Step


class ExecutionError(Exception):
    """実行エラー."""


class PlanExecutor:
    """計画実行器.

    計画のステップを順次実行し、変数参照を解決します。

    Example:
        >>> executor = PlanExecutor(tool_caller=my_tool_caller)
        >>> result = await executor.execute_plan(plan)
        >>> print(result["E1"])  # 最初のステップの結果
    """

    def __init__(
        self,
        tool_caller: Any,
        logger: logging.Logger | None = None,
    ) -> None:
        """計画実行器を初期化.

        Args:
            tool_caller: ツール呼び出しインターフェース（call_tool メソッドを持つ）
            logger: ロガーインスタンス（オプション）
        """
        self._tool_caller = tool_caller
        self._logger = logger or logging.getLogger(__name__)

    async def execute_plan(
        self,
        plan: Plan,
        user_id: str = "system",
    ) -> dict[str, Any]:
        """計画を実行.

        Args:
            plan: 実行する計画
            user_id: ユーザー ID（審計ログ用）

        Returns:
            ステップ ID をキーとする結果の辞書

        Raises:
            ExecutionError: ステップ実行に失敗した場合

        Example:
            >>> results = await executor.execute_plan(plan)
            >>> print(results["E1"])  # {"success": True, "result": ...}
        """
        self._logger.info(f"Executing plan with {len(plan.steps)} steps")

        results: dict[str, Any] = {}

        for step in plan.steps:
            self._logger.debug(f"Executing step: {step.step_id}")

            try:
                # 変数を置換
                resolved_params = self._resolve_variables(step.parameters, results)

                # ステップを実行
                result = await self._execute_step(step, resolved_params, user_id)

                # 結果を保存
                results[step.step_id] = result

                self._logger.info(f"Step {step.step_id} completed successfully")

            except Exception as e:
                error_msg = f"Step {step.step_id} failed: {e}"
                self._logger.error(error_msg)
                results[step.step_id] = {
                    "success": False,
                    "error": str(e),
                }
                # エラーを伝播
                raise ExecutionError(error_msg) from e

        self._logger.info("Plan execution completed")
        return results

    def _resolve_variables(
        self,
        parameters: dict[str, Any],
        results: dict[str, Any],
    ) -> dict[str, Any]:
        """パラメータ内の変数参照を解決.

        Args:
            parameters: 元のパラメータ
            results: これまでのステップ結果

        Returns:
            変数が解決されたパラメータ

        Example:
            >>> params = {"query": "Stats for #E1"}
            >>> results = {"E1": {"result": "Tom Brady"}}
            >>> resolved = executor._resolve_variables(params, results)
            >>> print(resolved)  # {"query": "Stats for Tom Brady"}
        """
        resolved: dict[str, Any] = {}

        for key, value in parameters.items():
            if isinstance(value, str):
                # #E1, #E2 などの参照を置換
                resolved_value = self._substitute_references(value, results)
                resolved[key] = resolved_value
            else:
                resolved[key] = value

        return resolved

    def _substitute_references(self, text: str, results: dict[str, Any]) -> str:
        """テキスト内の変数参照を置換.

        Args:
            text: 元のテキスト
            results: ステップ結果

        Returns:
            変数が置換されたテキスト
        """

        def replace_ref(match: re.Match[str]) -> str:
            step_id = match.group(1)
            if step_id in results:
                result = results[step_id]
                # 結果から実際の値を抽出
                if isinstance(result, dict) and "result" in result:
                    return str(result["result"])
                return str(result)
            return match.group(0)  # 見つからない場合はそのまま

        return re.sub(r"#(E\d+)", replace_ref, text)

    async def _execute_step(
        self,
        step: Step,
        parameters: dict[str, Any],
        user_id: str,
    ) -> dict[str, Any]:
        """単一ステップを実行.

        Args:
            step: 実行するステップ
            parameters: 解決済みパラメータ
            user_id: ユーザー ID

        Returns:
            ステップ実行結果
        """
        # ツールを呼び出す
        tool_uri = f"mcp://default/{step.tool}"
        result = await self._tool_caller.call_tool(
            tool_uri=tool_uri,
            arguments=parameters,
            user_id=user_id,
        )

        return result

