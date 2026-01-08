# -*- coding: utf-8 -*-
"""Code Migration Engine - agentflow PipelineEngine 統合.

agentflow の Engine パターンを使用してコード移行を実行。
Transform → Check → (Fix) → Check のループを PipelineEngine で実装。

使用例:
    >>> from apps.code_migration_assistant.engine import CodeMigrationEngine
    >>>
    >>> engine = CodeMigrationEngine(migration_type="cobol-to-java")
    >>> result = await engine.run({"source_code": cobol_code})
    >>>
    >>> # ストリーム実行（SSE 対応）
    >>> async for event in engine.run_stream({"source_code": cobol_code}):
    ...     print(event)
"""

from typing import Any

from agentflow.engines.base import BaseEngine, EngineConfig

from apps.code_migration_assistant.adapters import get_adapter_factory
from apps.code_migration_assistant.agents.transform_agent import TransformAgent
from apps.code_migration_assistant.agents.checker_agent import CheckerAgent
from apps.code_migration_assistant.agents.fixer_agent import FixerAgent


class CodeMigrationEngine(BaseEngine):
    """コード移行 Engine.

    agentflow の Engine パターンを使用した移行エンジン。
    Transform → Check → Fix → Check のループを実装。

    Attributes:
        migration_type: 移行タイプ（例: "cobol-to-java"）
        max_fix_iterations: 最大修復反復回数
    """

    def __init__(
        self,
        migration_type: str = "cobol-to-java",
        max_fix_iterations: int = 3,
        config: EngineConfig | None = None,
    ) -> None:
        """CodeMigrationEngine を初期化.

        Args:
            migration_type: 移行タイプ名
            max_fix_iterations: 最大修復反復回数
            config: Engine 設定
        """
        super().__init__(config=config)
        self._migration_type = migration_type
        self._max_fix_iterations = max_fix_iterations
        self._factory = get_adapter_factory()

        # Agent は _initialize で初期化
        self._transform_agent: TransformAgent | None = None
        self._checker_agent: CheckerAgent | None = None
        self._fixer_agent: FixerAgent | None = None

    async def _initialize(self) -> None:
        """内部 Agent を初期化."""
        self._transform_agent = TransformAgent(migration_type=self._migration_type)
        self._checker_agent = CheckerAgent(migration_type=self._migration_type)
        self._fixer_agent = FixerAgent(migration_type=self._migration_type)

    async def _execute(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """コア実行ロジック（BaseEngine.run から呼び出される）.

        Args:
            inputs:
                - source_code: ソースコード（必須）
                - expected_outputs: 期待される出力（オプション）

        Returns:
            移行結果
        """
        # Agent が初期化されていることを確認
        if self._transform_agent is None:
            await self._initialize()

        source_code = inputs.get("source_code") or inputs.get("cobol_code", "")
        if not source_code:
            return {"success": False, "error": "source_code is required"}

        expected_outputs = inputs.get("expected_outputs", {})

        # Stage 1: Transform
        self._logger.info("Stage 1: Transform")
        transform_result = self._transform_agent.process({"source_code": source_code})

        if not transform_result.get("success"):
            return {
                "success": False,
                "stage": "transform",
                "error": transform_result.get("error"),
            }

        # LLM がコードを生成する想定（実際には AgentClient 経由）
        target_code = transform_result.get("target_code", "")

        # Stage 2: Check-Fix Loop
        iteration = 0
        verdict = "FAIL"
        check_result: dict[str, Any] = {}

        while iteration < self._max_fix_iterations:
            iteration += 1
            self._logger.info(f"Stage 2: Check (iteration {iteration})")

            # Check
            check_result = self._checker_agent.process({
                "target_code": target_code,
                "expected_outputs": expected_outputs,
            })

            verdict = check_result.get("verdict", "FAIL")

            if verdict == "PASS":
                self._logger.info("Verification PASSED")
                break

            if iteration >= self._max_fix_iterations:
                self._logger.warning("Max iterations reached")
                break

            # Fix
            self._logger.info(f"Stage 3: Fix (iteration {iteration})")
            fix_result = self._fixer_agent.process({
                "target_code": target_code,
                "diff_report": check_result.get("summary_report", ""),
                "source_code": source_code,
            })

            # LLM が修正したコードを取得
            target_code = fix_result.get("fixed_code", target_code)

        return {
            "success": verdict == "PASS",
            "migration_type": self._migration_type,
            "source_language": self._transform_agent._source_adapter.language_name,
            "target_language": self._transform_agent._target_adapter.language_name,
            "target_code": target_code,
            "verdict": verdict,
            "iterations": iteration,
            "check_result": check_result,
        }

    async def run_stream(self, inputs: dict[str, Any]):
        """ストリーム実行（AG-UI イベント発火）.

        Args:
            inputs: 入力データ

        Yields:
            AG-UI イベント
        """
        # Agent が初期化されていることを確認
        if self._transform_agent is None:
            await self._initialize()

        source_code = inputs.get("source_code") or inputs.get("cobol_code", "")
        if not source_code:
            yield {"type": "error", "error": "source_code is required"}
            return

        expected_outputs = inputs.get("expected_outputs", {})

        # Stage 1: Transform
        yield {"type": "stage_start", "stage": "transform"}

        transform_result = self._transform_agent.process({"source_code": source_code})

        if not transform_result.get("success"):
            yield {"type": "error", "error": transform_result.get("error", "Transform failed")}
            return

        yield {"type": "stage_complete", "stage": "transform", "data": transform_result}

        target_code = transform_result.get("target_code", "")

        # Stage 2: Check-Fix Loop
        iteration = 0
        verdict = "FAIL"
        check_result: dict[str, Any] = {}

        while iteration < self._max_fix_iterations:
            iteration += 1

            # Check
            yield {"type": "stage_start", "stage": f"check_{iteration}"}

            check_result = self._checker_agent.process({
                "target_code": target_code,
                "expected_outputs": expected_outputs,
            })

            verdict = check_result.get("verdict", "FAIL")
            yield {"type": "stage_complete", "stage": f"check_{iteration}", "data": check_result}

            if verdict == "PASS":
                break

            if iteration >= self._max_fix_iterations:
                break

            # Fix
            yield {"type": "stage_start", "stage": f"fix_{iteration}"}

            fix_result = self._fixer_agent.process({
                "target_code": target_code,
                "diff_report": check_result.get("summary_report", ""),
                "source_code": source_code,
            })

            target_code = fix_result.get("fixed_code", target_code)
            yield {"type": "stage_complete", "stage": f"fix_{iteration}", "data": fix_result}

        # 最終結果
        yield {
            "type": "complete",
            "data": {
                "success": verdict == "PASS",
                "migration_type": self._migration_type,
                "target_code": target_code,
                "verdict": verdict,
                "iterations": iteration,
            },
        }

