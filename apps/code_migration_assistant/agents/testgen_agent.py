# -*- coding: utf-8 -*-
"""TestGen Agent - テスト生成.

LLM 全権負責テストコード生成。
確定性ツールはメソッドとして提供。
"""

from typing import Any

from agentflow import agent

from apps.code_migration_assistant.adapters import JavaAdapter
from apps.code_migration_assistant.tools import JUnitRunner


@agent
class TestGenAgent:
    """テスト生成 Agent.

    職責：
    1. COBOL/Java コードからテストケースを生成
    2. 境界条件・エッジケースを考慮
    3. Mock コードの生成（外部依存の置換）
    4. JUnit テストの実行と結果解析
    """

    system_prompt = """あなたはテストコード生成の専門家です。

## 役割
COBOL と Java の両方のコードを理解し、等価性を検証するテストを生成します。

## 入力
- COBOL ソースコード（オリジナル）
- Java ソースコード（変換後）
- 変数情報

## 出力
- JUnit テストクラス
- テストケース（境界条件含む）
- Mock コード（必要な場合）

## テスト戦略
1. **正常系**: 典型的な入力での動作確認
2. **境界値**: 最小/最大/ゼロ/空文字列
3. **異常系**: 不正な入力での例外処理
4. **等価性**: COBOL と Java の出力一致確認

## 重要
- あなたがテスト設計の全責任を持ちます
- 網羅的なテストケースを生成してください
- テストは実行可能な JUnit 5 形式で出力
"""

    def __init__(self, junit_jar_path: str | None = None) -> None:
        """初期化.

        Args:
            junit_jar_path: JUnit Console JAR のパス（オプション）
        """
        self._java_adapter = JavaAdapter()
        self._junit_runner = JUnitRunner(junit_jar_path)

    # =========================================================================
    # 確定性ツール
    # =========================================================================

    def run_junit(
        self, test_code: str, target_code: str, timeout: int = 60
    ) -> dict[str, Any]:
        """JUnit テストを実行（確定的処理）.

        Args:
            test_code: JUnit テストコード
            target_code: テスト対象の Java コード
            timeout: タイムアウト秒数

        Returns:
            テスト結果
        """
        result = self._junit_runner.run(test_code, target_code, timeout)

        return {
            "success": result.success,
            "total": result.total,
            "passed": result.passed,
            "failed": result.failed,
            "errors": result.errors,
            "skipped": result.skipped,
            "tests": [
                {
                    "name": t.name,
                    "status": t.status,
                    "duration_ms": t.duration_ms,
                    "message": t.message,
                }
                for t in result.tests
            ],
            "stdout": result.stdout,
            "stderr": result.stderr,
            "error": result.error,
        }

    def compile_test(self, test_code: str, target_code: str) -> dict[str, Any]:
        """テストコードをコンパイル（確定的処理）.

        Args:
            test_code: JUnit テストコード
            target_code: テスト対象の Java コード

        Returns:
            コンパイル結果
        """
        # 先にターゲットコードをコンパイル
        target_ok, target_errors = self._java_adapter.compile(target_code)
        if not target_ok:
            return {
                "success": False,
                "stage": "target",
                "errors": target_errors,
            }

        # テストコードをコンパイル（JUnit アノテーションがあるとエラーになる可能性）
        # 簡易チェックのみ
        test_ok, test_errors = self._java_adapter.compile(test_code)

        return {
            "success": test_ok,
            "stage": "test" if target_ok else "target",
            "errors": test_errors if not test_ok else [],
        }

    def get_test_template(self, class_name: str) -> str:
        """JUnit テストテンプレートを取得.

        Args:
            class_name: テスト対象クラス名

        Returns:
            JUnit テストのテンプレート
        """
        return f'''package com.migration.generated;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.CsvSource;
import static org.junit.jupiter.api.Assertions.*;

class {class_name}Test {{

    private {class_name} instance;

    @BeforeEach
    void setUp() {{
        instance = new {class_name}();
    }}

    @Test
    @DisplayName("正常系: 基本動作確認")
    void testBasicOperation() {{
        // TODO: LLM がテストケースを生成
    }}

    @Test
    @DisplayName("境界値: 最小値テスト")
    void testMinValue() {{
        // TODO: LLM がテストケースを生成
    }}

    @Test
    @DisplayName("境界値: 最大値テスト")
    void testMaxValue() {{
        // TODO: LLM がテストケースを生成
    }}

    @ParameterizedTest
    @DisplayName("パラメータ化テスト")
    @CsvSource({{
        // "input1, input2, expected"
    }})
    void testParameterized(String input1, String input2, String expected) {{
        // TODO: LLM がテストケースを生成
    }}
}}
'''

    def analyze_code_for_tests(
        self, java_code: str, cobol_code: str = ""
    ) -> dict[str, Any]:
        """コードを分析してテストすべき項目を抽出.

        Args:
            java_code: Java ソースコード
            cobol_code: COBOL ソースコード（オプション）

        Returns:
            テストすべき項目リスト
        """
        import re

        test_points: list[dict[str, Any]] = []

        # パブリックメソッドを抽出
        method_pattern = r"public\s+(\w+)\s+(\w+)\s*\(([^)]*)\)"
        for match in re.finditer(method_pattern, java_code):
            return_type = match.group(1)
            method_name = match.group(2)
            params = match.group(3)

            # main は除外
            if method_name == "main":
                continue

            test_points.append({
                "type": "method",
                "name": method_name,
                "return_type": return_type,
                "parameters": params,
                "suggested_tests": [
                    f"test_{method_name}_normal",
                    f"test_{method_name}_boundary",
                    f"test_{method_name}_null_input",
                ],
            })

        # フィールドを抽出（getter/setter テスト用）
        field_pattern = r"private\s+(\w+)\s+(\w+)\s*;"
        for match in re.finditer(field_pattern, java_code):
            field_type = match.group(1)
            field_name = match.group(2)

            test_points.append({
                "type": "field",
                "name": field_name,
                "field_type": field_type,
                "suggested_tests": [
                    f"test_{field_name}_initialization",
                ],
            })

        return {
            "test_points": test_points,
            "total_methods": sum(1 for t in test_points if t["type"] == "method"),
            "total_fields": sum(1 for t in test_points if t["type"] == "field"),
        }

    async def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """テストコードを生成・実行.

        Args:
            input_data:
                - cobol_code: COBOL ソースコード
                - java_code: Java ソースコード
                - class_name: クラス名
                - variables: 変数情報
                - test_code: 既存のテストコード（オプション）
                - run_tests: テストを実行するか（デフォルト: False）

        Returns:
            - test_code: 生成された JUnit テストコード
            - analysis: コード分析結果
            - test_result: テスト実行結果（run_tests=True の場合）
        """
        java_code = input_data.get("java_code", "")
        cobol_code = input_data.get("cobol_code", "")
        class_name = input_data.get("class_name", "Unknown")
        test_code = input_data.get("test_code", "")
        run_tests = input_data.get("run_tests", False)

        if not java_code:
            return {"success": False, "error": "java_code is required"}

        # コード分析
        analysis = self.analyze_code_for_tests(java_code, cobol_code)

        # テストテンプレートを取得
        template = self.get_test_template(class_name)

        result: dict[str, Any] = {
            "success": True,
            "template": template,
            "class_name": class_name,
            "analysis": analysis,
            "message": "Template and analysis ready. LLM should generate tests.",
        }

        # テストコードがあり、実行フラグがある場合
        if test_code and run_tests:
            test_result = self.run_junit(test_code, java_code)
            result["test_result"] = test_result
            result["tests_executed"] = True

        return result
