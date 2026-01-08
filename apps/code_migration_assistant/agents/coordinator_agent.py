# -*- coding: utf-8 -*-
"""Coordinator Agent - 編排.

フロー編排 Agent、他の Agent を協調して移行タスクを完了させる。
直接コード処理は行わず、決策と調度のみを担当。

Factory パターンにより、設定ベースで言語ペアを切り替え可能。
"""

from typing import Any

from agentflow import agent

from apps.code_migration_assistant.adapters import get_adapter_factory
from apps.code_migration_assistant.agents.transform_agent import TransformAgent
from apps.code_migration_assistant.agents.testgen_agent import TestGenAgent
from apps.code_migration_assistant.agents.checker_agent import CheckerAgent
from apps.code_migration_assistant.agents.fixer_agent import FixerAgent


@agent
class CoordinatorAgent:
    """編排 Agent.

    職責：
    1. 協調 TransformAgent, TestGenAgent, CheckerAgent, FixerAgent
    2. 次のアクションを決定
    3. リトライロジックを管理

    Attributes:
        migration_type: 移行タイプ（例: "cobol-to-java"）
    """

    system_prompt = """あなたは移行プロジェクトのコーディネーターです。

## 役割
各専門 Agent を適切に呼び出し、移行タスクを完了させます。

## 利用可能な Agent
1. **TransformAgent**: ソース → ターゲット 変換
2. **TestGenAgent**: テストコード生成
3. **CheckerAgent**: 等価性検証
4. **FixerAgent**: コード修復

## ワークフロー
1. TransformAgent でターゲットコードを生成
2. CheckerAgent で検証
3. FAIL の場合 → FixerAgent で修復 → 再検証
4. PASS まで繰り返し（最大 N 回）

## 判断基準
- 検証が PASS → 完了
- 検証が FAIL で修復可能 → FixerAgent 呼び出し
- 検証が FAIL で修復不可 → エラー報告

## 重要
- 各 Agent の結果を適切に次の Agent に渡す
- 無限ループを避ける（最大リトライ回数を守る）
- 進捗をユーザーに報告する
"""

    MAX_RETRIES = 3

    def __init__(self, migration_type: str = "cobol-to-java") -> None:
        """初期化.

        Args:
            migration_type: 移行タイプ名
        """
        self._migration_type = migration_type
        self._factory = get_adapter_factory()

        # 各 Agent を同じ移行タイプで初期化
        self._transform_agent = TransformAgent(migration_type=migration_type)
        self._testgen_agent = TestGenAgent()
        self._checker_agent = CheckerAgent(migration_type=migration_type)
        self._fixer_agent = FixerAgent(migration_type=migration_type)

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """移行を編排.

        Args:
            input_data:
                - source_code: ソースコード（または cobol_code）
                - expected_outputs: 期待される出力（オプション）

        Returns:
            - target_code: 最終的なターゲットコード
            - verdict: 最終判定
            - iterations: 実行した反復回数
        """
        # source_code または cobol_code を取得（後方互換性）
        source_code = input_data.get("source_code") or input_data.get("cobol_code", "")
        expected_outputs = input_data.get("expected_outputs", {})

        if not source_code:
            return {"success": False, "error": "source_code is required"}

        # Step 1: TransformAgent で変換
        transform_result = self._transform_agent.process({
            "source_code": source_code,
        })

        if not transform_result.get("success"):
            return {
                "success": False,
                "error": transform_result.get("error"),
                "stage": "transform",
            }

        # この時点で LLM がターゲットコードを生成している想定
        target_code = transform_result.get("target_code", "")

        # Step 2: 検証ループ
        iteration = 0
        verdict = "FAIL"
        check_result: dict[str, Any] = {}

        while iteration < self.MAX_RETRIES:
            iteration += 1

            # CheckerAgent で検証
            check_result = self._checker_agent.process({
                "target_code": target_code,
                "expected_outputs": expected_outputs,
            })

            verdict = check_result.get("verdict", "FAIL")

            if verdict == "PASS":
                break

            if verdict == "FAIL" and iteration >= self.MAX_RETRIES:
                break

            # FixerAgent で修復
            fix_result = self._fixer_agent.process({
                "target_code": target_code,
                "diff_report": check_result.get("report", ""),
                "source_code": source_code,
                "comparison": check_result.get("comparison"),
            })

            # LLM が修正したコードを取得
            target_code = fix_result.get("fixed_code", target_code)

        return {
            "success": verdict == "PASS",
            "migration_type": self._migration_type,
            "target_code": target_code,
            "verdict": verdict,
            "iterations": iteration,
            "final_check": check_result,
        }

