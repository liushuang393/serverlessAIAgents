# -*- coding: utf-8 -*-
"""Code Migration Orchestrator.

ソース言語→ターゲット言語移行のオーケストレーター。
agentflow の Engine パターンを使用。

v4.0: 工程固定パイプライン統合
    - 分析 → 設計 → 変換 → テスト生成 → 差分検証 → 品質裁定 → 限定修正
    - 成果物は artifacts/ 配下の JSON として保持
    - AG-UI イベント対応

Flow互換インターフェース:
    >>> orchestrator = CodeMigrationOrchestrator()
    >>> result = await orchestrator.run({"cobol_code": "..."})
    >>>
    >>> # ストリーム実行
    >>> async for event in orchestrator.run_stream({"cobol_code": "..."}):
    ...     print(event)
"""

from collections.abc import AsyncIterator
from typing import Any


class CodeMigrationOrchestrator:
    """Code Migration Orchestrator.

    agentflow の Engine パターンを使用したオーケストレーター。

    ワークフロー:
        1. LegacyAnalysisAgent: 事実抽出
        2. MigrationDesignAgent: 等価移行設計
        3. CodeTransformationAgent: 設計拘束下の変換
        4. TestSynthesisAgent: Golden Master 生成
        5. DifferentialVerificationAgent: 差分分類
        6. QualityGateAgent: 責任工程裁定
        7. LimitedFixerAgent: 限定修正

    使用例:
        >>> orchestrator = CodeMigrationOrchestrator()
        >>> result = await orchestrator.run({"cobol_code": "..."})
    """

    def __init__(self, migration_type: str = "cobol-to-java") -> None:
        """CodeMigrationOrchestratorを初期化.

        Args:
            migration_type: 移行タイプ名
        """
        self._migration_type = migration_type
        # Engine を遅延初期化
        self._engine = None

    def _get_engine(self):
        """CodeMigrationEngine を取得（遅延初期化）."""
        if self._engine is None:
            from apps.code_migration_assistant.engine import CodeMigrationEngine

            self._engine = CodeMigrationEngine(migration_type=self._migration_type)
        return self._engine

    async def migrate(
        self,
        source_code: str,
        expected_outputs: dict[str, Any] | None = None,
        run_tests: bool = False,
    ) -> dict[str, Any]:
        """コード移行を実行.

        Args:
            source_code: ソースコード
            expected_outputs: 期待される出力（検証用）
            run_tests: テストを実行するか

        Returns:
            移行結果
        """
        engine = self._get_engine()

        # Engine で移行を実行
        result = await engine.run(
            {
                "source_code": source_code,
                "expected_outputs": expected_outputs or {},
                "fast_mode": not run_tests,
            }
        )

        return result

    # ========================================
    # Flow 互換インターフェース
    # ========================================

    async def run(self, inputs: dict[str, Any]) -> dict[str, Any]:
        """Flow互換のrunメソッド.

        Args:
            inputs: 入力データ
                - source_code または cobol_code: ソースコード（必須）
                - expected_outputs: 期待出力（オプション）
                - run_tests: テスト実行（オプション）

        Returns:
            移行結果
        """
        source_code = inputs.get("source_code") or inputs.get("cobol_code", "")
        if not source_code:
            return {"success": False, "errors": ["source_code is required"]}

        # migrate メソッドを呼び出し
        return await self.migrate(
            source_code=source_code,
            expected_outputs=inputs.get("expected_outputs"),
            run_tests=inputs.get("run_tests", False),
        )

    async def run_stream(self, inputs: dict[str, Any]) -> AsyncIterator[dict[str, Any]]:
        """Flow互換のストリーム実行.

        Engine の run_stream を使用。

        Yields:
            AG-UI イベント
        """
        source_code = inputs.get("source_code") or inputs.get("cobol_code", "")
        if not source_code:
            yield {"type": "error", "error": "source_code is required"}
            return

        engine = self._get_engine()

        # Engine のストリーム実行を委譲
        async for event in engine.run_stream(
            {
                "source_code": source_code,
                "expected_outputs": inputs.get("expected_outputs", {}),
            }
        ):
            yield event

    @property
    def name(self) -> str:
        """Flow名."""
        return f"code-migration-{self._migration_type}"
