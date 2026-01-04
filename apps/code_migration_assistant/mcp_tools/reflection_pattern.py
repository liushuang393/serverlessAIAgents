# -*- coding: utf-8 -*-
"""ReflectionPattern MCP Tool.

このモジュールは反射模式编排を提供するMCP工具を実装します。

主な機能:
    - Generate → Evaluate → Improve ループ編排
    - 最大反復回数制御
    - 受け入れ閾値判定
    - 履歴記録
"""

from typing import Any

from agentflow import MCPTool, MCPToolRequest, MCPToolResponse


class ReflectionPattern(MCPTool):
    """ReflectionPattern MCP Tool.

    Generate → Evaluate → Improve ループを編排します。

    Input:
        - generator_tool: 生成工具名（必須）
        - evaluator_tool: 評価工具名（必須）
        - improver_tool: 改善工具名（必須、通常はgenerator_toolと同じ）
        - initial_input: 初期入力データ（必須）
        - max_iterations: 最大反復回数（デフォルト: 3）
        - acceptance_threshold: 受け入れ閾値（デフォルト: 85.0）

    Output:
        - final_output: 最終出力
        - final_score: 最終スコア
        - iterations: 実行した反復回数
        - history: 各反復の履歴
        - is_acceptable: 受け入れ可能フラグ

    Note:
        このMCP工具は他のMCP工具を呼び出すため、MCPClientが必要です。
        実際の実装では、MCPClientを注入する必要があります。
    """

    def __init__(self, mcp_client: Any | None = None) -> None:
        """ReflectionPatternを初期化.

        Args:
            mcp_client: MCP Client（他の工具を呼び出すため）
        """
        super().__init__(tool_name="reflection_pattern", version="1.0.0")
        self.mcp_client = mcp_client

    async def handle_request(self, request: MCPToolRequest) -> MCPToolResponse:
        """反射模式を実行.

        Args:
            request: MCP工具リクエスト

        Returns:
            MCP工具レスポンス
        """
        # 入力パラメータを取得
        generator_tool = request.input.get("generator_tool")
        evaluator_tool = request.input.get("evaluator_tool")
        improver_tool = request.input.get("improver_tool")
        initial_input = request.input.get("initial_input")
        max_iterations = request.input.get("max_iterations", 3)
        acceptance_threshold = request.input.get("acceptance_threshold", 85.0)

        # 必須パラメータチェック
        if not generator_tool:
            return MCPToolResponse(
                success=False,
                errors=["generator_tool is required"],
            )

        if not evaluator_tool:
            return MCPToolResponse(
                success=False,
                errors=["evaluator_tool is required"],
            )

        if not improver_tool:
            return MCPToolResponse(
                success=False,
                errors=["improver_tool is required"],
            )

        if not initial_input:
            return MCPToolResponse(
                success=False,
                errors=["initial_input is required"],
            )

        if not self.mcp_client:
            return MCPToolResponse(
                success=False,
                errors=["MCP Client is not configured"],
            )

        # 反射模式実行
        try:
            final_output, final_score, iterations, history, is_acceptable = await self._run_reflection_loop(
                generator_tool=generator_tool,
                evaluator_tool=evaluator_tool,
                improver_tool=improver_tool,
                initial_input=initial_input,
                max_iterations=max_iterations,
                acceptance_threshold=acceptance_threshold,
            )

            return MCPToolResponse(
                success=True,
                output={
                    "final_output": final_output,
                    "final_score": final_score,
                    "iterations": iterations,
                    "history": history,
                    "is_acceptable": is_acceptable,
                },
            )

        except Exception as e:
            return MCPToolResponse(
                success=False,
                errors=[f"Reflection loop failed: {str(e)}"],
            )

    async def _run_reflection_loop(
        self,
        generator_tool: str,
        evaluator_tool: str,
        improver_tool: str,
        initial_input: dict[str, Any],
        max_iterations: int,
        acceptance_threshold: float,
    ) -> tuple[dict[str, Any], float, int, list[dict[str, Any]], bool]:
        """反射ループを実行（内部実装）.

        Args:
            generator_tool: 生成工具名
            evaluator_tool: 評価工具名
            improver_tool: 改善工具名
            initial_input: 初期入力データ
            max_iterations: 最大反復回数
            acceptance_threshold: 受け入れ閾値

        Returns:
            (最終出力, 最終スコア, 反復回数, 履歴, 受け入れ可能フラグ)
        """
        history: list[dict[str, Any]] = []
        current_input = initial_input
        current_output: dict[str, Any] | None = None
        current_score = 0.0
        is_acceptable = False

        for iteration in range(1, max_iterations + 1):
            # Step 1: Generate
            generate_response = await self._call_tool(generator_tool, current_input)

            if not generate_response.success:
                raise RuntimeError(f"Generation failed: {generate_response.errors}")

            current_output = generate_response.output

            # Step 2: Evaluate
            evaluate_input = {
                "java_code": current_output.get("java_code"),
                "ast": initial_input.get("ast"),
                "metadata": initial_input.get("metadata"),
                "mappings": current_output.get("mappings", {}),
            }

            evaluate_response = await self._call_tool(evaluator_tool, evaluate_input)

            if not evaluate_response.success:
                raise RuntimeError(f"Evaluation failed: {evaluate_response.errors}")

            evaluation_result = evaluate_response.output
            current_score = evaluation_result.get("score", 0.0)
            is_acceptable = evaluation_result.get("is_acceptable", False)

            # 履歴記録
            history.append(
                {
                    "iteration": iteration,
                    "output": current_output,
                    "score": current_score,
                    "is_acceptable": is_acceptable,
                    "feedback": evaluation_result.get("feedback", []),
                    "suggestions": evaluation_result.get("suggestions", []),
                }
            )

            # 受け入れ可能なら終了
            if is_acceptable:
                break

            # 最大反復回数に達したら終了
            if iteration >= max_iterations:
                break

            # Step 3: Improve
            improve_input = {
                **initial_input,
                "previous_output": current_output,
                "feedback": evaluation_result.get("feedback", []),
                "suggestions": evaluation_result.get("suggestions", []),
                "score": current_score,
            }

            current_input = improve_input

        return current_output or {}, current_score, len(history), history, is_acceptable

    async def _call_tool(self, tool_name: str, input_data: dict[str, Any]) -> MCPToolResponse:
        """MCP工具を呼び出す.

        Args:
            tool_name: 工具名
            input_data: 入力データ

        Returns:
            MCP工具レスポンス
        """
        # MCPClientを使用して工具を呼び出す
        # 注意: 実際の実装では、MCPClientのインターフェースに合わせて調整が必要
        request = MCPToolRequest(
            tool=tool_name,
            version="1.0.0",
            input=input_data,
        )

        # 工具を直接呼び出す（簡易実装）
        # 実際の実装では、MCPClientを使用してリモート工具を呼び出す
        if self.mcp_client:
            return await self.mcp_client.call_tool(request)
        else:
            raise RuntimeError("MCP Client is not configured")

