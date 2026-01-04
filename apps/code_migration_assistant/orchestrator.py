# -*- coding: utf-8 -*-
"""Code Migration Orchestrator.

このモジュールはCOBOL→Java移行を編排するオーケストレーターを提供します。

主な機能:
    - MCP工具の編排
    - 移行ワークフローの実行
    - エラーハンドリング

Flow互換インターフェース:
    >>> orchestrator = CodeMigrationOrchestrator(mcp_client)
    >>> result = await orchestrator.run({"cobol_code": "..."})
"""

from collections.abc import AsyncIterator
from typing import Any

from agentflow import MCPToolClient as MCPClient


class CodeMigrationOrchestrator:
    """Code Migration Orchestrator.

    COBOL→Java移行を編排します。

    ワークフロー:
        1. COBOLParser: COBOLコードを解析
        2. MemorySystem (recall): 過去の移行パターンを想起
        3. ReflectionPattern: JavaGenerator + CodeValidator でコード生成・検証
        4. MemorySystem (remember): 移行結果を記憶
        5. 結果を返す

    使用例:
        ```python
        orchestrator = CodeMigrationOrchestrator(mcp_client)
        result = await orchestrator.migrate(cobol_code="...")
        ```
    """

    def __init__(self, mcp_client: MCPClient) -> None:
        """CodeMigrationOrchestratorを初期化.

        Args:
            mcp_client: MCP Client
        """
        self.mcp = mcp_client

    async def migrate(
        self,
        cobol_code: str,
        file_name: str = "unknown.cob",
        encoding: str = "utf-8",
        max_iterations: int = 3,
        acceptance_threshold: float = 85.0,
    ) -> dict[str, Any]:
        """COBOL→Java移行を実行.

        Args:
            cobol_code: COBOLソースコード
            file_name: ファイル名
            encoding: エンコーディング
            max_iterations: 最大反復回数
            acceptance_threshold: 受け入れ閾値

        Returns:
            移行結果
        """
        # Step 1: COBOLParser - COBOLコードを解析
        parse_response = await self.mcp.call_tool_by_name(
            tool_name="cobol_parser",
            input_data={
                "cobol_code": cobol_code,
                "file_name": file_name,
                "encoding": encoding,
            },
        )

        if not parse_response.success:
            return {
                "success": False,
                "errors": parse_response.errors,
                "stage": "parsing",
            }

        ast = parse_response.output.get("ast")
        metadata = parse_response.output.get("metadata")

        # Step 2: MemorySystem (recall) - 過去の移行パターンを想起
        recall_response = await self.mcp.call_tool_by_name(
            tool_name="memory_system",
            input_data={
                "operation": "recall",
                "data": {
                    "query": f"COBOL migration pattern for {ast.get('program_id', 'unknown')}",
                    "memory_type": "pattern",
                    "top_k": 5,
                },
            },
        )

        patterns = []
        if recall_response.success:
            patterns = recall_response.output.get("memories", [])

        # Step 3: ReflectionPattern - JavaGenerator + CodeValidator でコード生成・検証
        reflection_response = await self.mcp.call_tool_by_name(
            tool_name="reflection_pattern",
            input_data={
                "generator_tool": "java_generator",
                "evaluator_tool": "code_validator",
                "improver_tool": "java_generator",
                "initial_input": {
                    "ast": ast,
                    "metadata": metadata,
                    "patterns": patterns,
                },
                "max_iterations": max_iterations,
                "acceptance_threshold": acceptance_threshold,
            },
        )

        if not reflection_response.success:
            return {
                "success": False,
                "errors": reflection_response.errors,
                "stage": "generation",
            }

        final_output = reflection_response.output.get("final_output")
        final_score = reflection_response.output.get("final_score")
        iterations = reflection_response.output.get("iterations")
        is_acceptable = reflection_response.output.get("is_acceptable")

        # Step 4: MemorySystem (remember) - 移行結果を記憶
        if is_acceptable:
            await self.mcp.call_tool_by_name(
                tool_name="memory_system",
                input_data={
                    "operation": "remember",
                    "data": {
                        "content": final_output.get("java_code"),
                        "topic": f"migration_{ast.get('program_id', 'unknown')}",
                        "memory_type": "history",
                        "importance_score": final_score / 100.0,
                        "metadata": {
                            "cobol_program_id": ast.get("program_id"),
                            "java_class_name": final_output.get("class_name"),
                            "score": final_score,
                            "iterations": iterations,
                        },
                    },
                },
            )

        # Step 5: 結果を返す
        return {
            "success": True,
            "java_code": final_output.get("java_code"),
            "class_name": final_output.get("class_name"),
            "package_name": final_output.get("package_name"),
            "score": final_score,
            "iterations": iterations,
            "is_acceptable": is_acceptable,
            "ast": ast,
            "metadata": metadata,
        }

    # ========================================
    # Flow 互換インターフェース
    # ========================================

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Flow互換のrunメソッド.

        Args:
            inputs: 入力データ
                - cobol_code: COBOLソースコード（必須）
                - file_name: ファイル名（オプション）
                - encoding: エンコーディング（オプション）

        Returns:
            移行結果
        """
        cobol_code = inputs.get("cobol_code", "")
        if not cobol_code:
            return {"success": False, "errors": ["cobol_code is required"]}

        return await self.migrate(
            cobol_code=cobol_code,
            file_name=inputs.get("file_name", "unknown.cob"),
            encoding=inputs.get("encoding", "utf-8"),
            max_iterations=inputs.get("max_iterations", 3),
            acceptance_threshold=inputs.get("acceptance_threshold", 85.0),
        )

    async def run_stream(
        self, inputs: dict[str, Any]
    ) -> AsyncIterator[dict[str, Any]]:
        """Flow互換のストリーム実行.

        各ステップでイベントを発火。

        Yields:
            イベント dict
        """
        cobol_code = inputs.get("cobol_code", "")
        if not cobol_code:
            yield {"type": "error", "error": "cobol_code is required"}
            return

        file_name = inputs.get("file_name", "unknown.cob")

        # Step 1: Parsing
        yield {"type": "node_start", "node": "COBOLParser", "data": {}}
        parse_response = await self.mcp.call_tool_by_name(
            tool_name="cobol_parser",
            input_data={"cobol_code": cobol_code, "file_name": file_name},
        )
        yield {
            "type": "node_complete",
            "node": "COBOLParser",
            "data": {"success": parse_response.success},
        }

        if not parse_response.success:
            yield {"type": "error", "error": parse_response.errors}
            return

        ast = parse_response.output.get("ast")

        # Step 2: Memory recall
        yield {"type": "node_start", "node": "MemoryRecall", "data": {}}
        recall_response = await self.mcp.call_tool_by_name(
            tool_name="memory_system",
            input_data={
                "operation": "recall",
                "data": {"query": f"COBOL migration for {ast.get('program_id')}"},
            },
        )
        yield {
            "type": "node_complete",
            "node": "MemoryRecall",
            "data": {"patterns_found": len(recall_response.output.get("memories", []))},
        }

        # Step 3: Reflection
        yield {"type": "node_start", "node": "ReflectionPattern", "data": {}}
        reflection_response = await self.mcp.call_tool_by_name(
            tool_name="reflection_pattern",
            input_data={
                "generator_tool": "java_generator",
                "evaluator_tool": "code_validator",
                "improver_tool": "java_generator",
                "initial_input": {"ast": ast},
            },
        )
        yield {
            "type": "node_complete",
            "node": "ReflectionPattern",
            "data": {"success": reflection_response.success},
        }

        # Final result
        if reflection_response.success:
            yield {
                "type": "result",
                "data": {
                    "success": True,
                    "java_code": reflection_response.output.get("final_output", {}).get(
                        "java_code"
                    ),
                },
            }
        else:
            yield {"type": "error", "error": reflection_response.errors}

    @property
    def name(self) -> str:
        """Flow名."""
        return "code-migration-assistant"
