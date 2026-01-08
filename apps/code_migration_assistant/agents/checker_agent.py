# -*- coding: utf-8 -*-
"""Checker Agent - 検証.

核心 Agent：変換結果の正確性を検証。
確定的なテスト実行・比較を行い、
LLM で差異の分析と判定を行う。

Factory パターンにより、設定ベースで言語ペアを切り替え可能。
"""

import re
from dataclasses import dataclass, field
from decimal import Decimal, InvalidOperation
from enum import Enum
from typing import Any

from agentflow import agent

from apps.code_migration_assistant.adapters import (
    SourceLanguageAdapter,
    TargetLanguageAdapter,
    get_adapter_factory,
)


class Verdict(str, Enum):
    """検証結果の判定."""

    PASS = "PASS"
    PASS_WITH_WARNING = "PASS_WITH_WARNING"
    FAIL = "FAIL"
    RETRY = "RETRY"


class DifferenceType(str, Enum):
    """差異の種類."""

    VALUE = "value"  # 値の不一致
    PRECISION = "precision"  # 精度差異（許容可能）
    WHITESPACE = "whitespace"  # 空白差異（許容可能）
    FORMAT = "format"  # フォーマット差異
    MISSING = "missing"  # 欠損
    TYPE = "type"  # 型の不一致


@dataclass
class TestCase:
    """テストケース."""

    name: str
    inputs: dict[str, Any] = field(default_factory=dict)
    expected_outputs: dict[str, Any] = field(default_factory=dict)
    description: str = ""


@dataclass
class Difference:
    """差異情報."""

    field: str
    expected: Any
    actual: Any
    diff_type: DifferenceType
    severity: str = "error"  # error, warning, info


@dataclass
class ComparisonResult:
    """比較結果."""

    is_equal: bool
    differences: list[Difference] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)
    match_rate: float = 1.0


@agent
class CheckerAgent:
    """検証 Agent.

    職責：
    1. ソースコードとターゲットコードを実行
    2. 出力を比較（数値精度、文字列、構造化データ対応）
    3. 差異を分析し、PASS/FAIL を判定
    4. テストケースの管理と実行

    Attributes:
        migration_type: 移行タイプ（例: "cobol-to-java"）
    """

    # system_prompt は __init__ で動的に設定

    # 数値比較の許容精度
    NUMERIC_TOLERANCE = Decimal("0.0001")

    # デフォルトプロンプト
    _DEFAULT_PROMPT = """あなたは移行検証の専門家です。

## 役割
ソースコードとターゲットコードの実行結果を比較し、等価性を判定します。

## 入力
- 比較結果（compare_outputs ツールの結果）
- 差異リスト

## 判定基準
1. **PASS**: 全ての出力が一致
2. **PASS_WITH_WARNING**: 軽微な差異（空白、フォーマット）
3. **FAIL**: 値の不一致がある
4. **RETRY**: 修復可能な差異

## 出力
- 判定結果（PASS/FAIL/RETRY）
- 差異の原因分析
- 修復が必要な場合、修復方針
"""

    def __init__(
        self,
        migration_type: str = "cobol-to-java",
        source_adapter: SourceLanguageAdapter | None = None,
        target_adapter: TargetLanguageAdapter | None = None,
    ) -> None:
        """初期化.

        Args:
            migration_type: 移行タイプ名
            source_adapter: ソースアダプター（DI用）
            target_adapter: ターゲットアダプター（DI用）
        """
        self._migration_type = migration_type
        self._factory = get_adapter_factory()

        # DI またはファクトリーからアダプターを取得
        if source_adapter is not None:
            self._source_adapter = source_adapter
        else:
            self._source_adapter = self._factory.get_source_adapter(migration_type)

        if target_adapter is not None:
            self._target_adapter = target_adapter
        else:
            self._target_adapter = self._factory.get_target_adapter(migration_type)

        self._test_cases: list[TestCase] = []

        # プロンプトを設定ファイルから読み込み
        prompt = self._factory.get_prompt(migration_type, "checker")
        if prompt:
            self.system_prompt = prompt
        else:
            self.system_prompt = self._DEFAULT_PROMPT

    # =========================================================================
    # テストケース管理
    # =========================================================================

    def add_test_case(
        self,
        name: str,
        inputs: dict[str, Any],
        expected_outputs: dict[str, Any],
        description: str = "",
    ) -> dict[str, Any]:
        """テストケースを追加.

        Args:
            name: テストケース名
            inputs: 入力データ
            expected_outputs: 期待される出力
            description: 説明

        Returns:
            追加結果
        """
        tc = TestCase(
            name=name,
            inputs=inputs,
            expected_outputs=expected_outputs,
            description=description,
        )
        self._test_cases.append(tc)
        return {
            "success": True,
            "test_case": name,
            "total_cases": len(self._test_cases),
        }

    def list_test_cases(self) -> list[dict[str, Any]]:
        """登録済みテストケース一覧.

        Returns:
            テストケースリスト
        """
        return [
            {
                "name": tc.name,
                "description": tc.description,
                "input_keys": list(tc.inputs.keys()),
                "output_keys": list(tc.expected_outputs.keys()),
            }
            for tc in self._test_cases
        ]

    # =========================================================================
    # 実行ツール
    # =========================================================================

    def execute_target(
        self, target_code: str, inputs: dict[str, Any] | None = None
    ) -> dict[str, Any]:
        """ターゲットコードを実行（確定的処理）.

        Args:
            target_code: ターゲット言語ソースコード
            inputs: 入力パラメータ

        Returns:
            実行結果
        """
        result = self._target_adapter.execute(target_code, inputs or {})
        return {
            "success": result.success,
            "language": self._target_adapter.language_name,
            "stdout": result.stdout,
            "stderr": result.stderr,
            "return_code": result.return_code,
            "error": result.error,
        }

    # 後方互換性のためのエイリアス
    def execute_java(
        self, java_code: str, inputs: dict[str, Any] | None = None
    ) -> dict[str, Any]:
        """Java コードを実行（execute_target のエイリアス）."""
        return self.execute_target(java_code, inputs)

    def parse_java_output(self, stdout: str) -> dict[str, Any]:
        """Java 出力をパース.

        キーバリュー形式の出力をパースする。
        例: "RESULT=100" → {"RESULT": "100"}

        Args:
            stdout: Java の標準出力

        Returns:
            パース結果
        """
        result: dict[str, Any] = {}
        lines = stdout.strip().split("\n")

        for line in lines:
            line = line.strip()
            if not line:
                continue

            # KEY=VALUE 形式
            if "=" in line:
                key, _, value = line.partition("=")
                result[key.strip()] = self._parse_value(value.strip())
            # JSON 形式の検出
            elif line.startswith("{") and line.endswith("}"):
                try:
                    import json
                    result.update(json.loads(line))
                except json.JSONDecodeError:
                    result["raw_line"] = line
            else:
                # 生の出力行
                if "output_lines" not in result:
                    result["output_lines"] = []
                result["output_lines"].append(line)

        return result

    def _parse_value(self, value: str) -> Any:
        """値をパース."""
        # 数値判定
        try:
            if "." in value:
                return Decimal(value)
            return int(value)
        except (ValueError, InvalidOperation):
            pass

        # ブール判定
        if value.upper() in ("TRUE", "FALSE"):
            return value.upper() == "TRUE"

        return value

    # =========================================================================
    # 比較ツール
    # =========================================================================

    def compare_outputs(
        self,
        expected: dict[str, Any],
        actual: dict[str, Any],
        strict_numeric: bool = True,
    ) -> dict[str, Any]:
        """出力を比較（確定的処理）.

        Args:
            expected: 期待される出力（COBOL 側）
            actual: 実際の出力（Java 側）
            strict_numeric: 数値を厳格に比較するか

        Returns:
            比較結果
        """
        differences: list[dict[str, Any]] = []
        warnings: list[str] = []

        all_keys = set(expected.keys()) | set(actual.keys())

        for key in sorted(all_keys):
            exp_val = expected.get(key)
            act_val = actual.get(key)

            # 完全一致
            if exp_val == act_val:
                continue

            # 差異分類
            diff = self._classify_difference(key, exp_val, act_val, strict_numeric)

            if diff.severity == "warning":
                warnings.append(f"{key}: {diff.diff_type.value}")
            elif diff.severity == "info":
                pass  # 無視
            else:
                differences.append({
                    "field": diff.field,
                    "expected": diff.expected,
                    "actual": diff.actual,
                    "type": diff.diff_type.value,
                    "severity": diff.severity,
                })

        match_rate = 1.0 - len(differences) / max(len(all_keys), 1)

        return {
            "is_equal": len(differences) == 0,
            "differences": differences,
            "warnings": warnings,
            "match_rate": match_rate,
            "total_fields": len(all_keys),
            "matched_fields": len(all_keys) - len(differences),
        }

    def _classify_difference(
        self, field: str, expected: Any, actual: Any, strict_numeric: bool
    ) -> Difference:
        """差異の種類を分類."""
        # 欠損チェック
        if expected is None:
            return Difference(
                field=field,
                expected=expected,
                actual=actual,
                diff_type=DifferenceType.MISSING,
                severity="error",
            )
        if actual is None:
            return Difference(
                field=field,
                expected=expected,
                actual=actual,
                diff_type=DifferenceType.MISSING,
                severity="error",
            )

        # 文字列比較
        if isinstance(expected, str) and isinstance(actual, str):
            return self._compare_strings(field, expected, actual)

        # 数値比較
        if self._is_numeric(expected) and self._is_numeric(actual):
            return self._compare_numbers(field, expected, actual, strict_numeric)

        # 型不一致
        if type(expected) != type(actual):
            # 型変換を試みる
            try:
                if str(expected) == str(actual):
                    return Difference(
                        field=field,
                        expected=expected,
                        actual=actual,
                        diff_type=DifferenceType.TYPE,
                        severity="warning",
                    )
            except (ValueError, TypeError):
                pass

        return Difference(
            field=field,
            expected=expected,
            actual=actual,
            diff_type=DifferenceType.VALUE,
            severity="error",
        )

    def _compare_strings(self, field: str, expected: str, actual: str) -> Difference:
        """文字列比較."""
        # 空白差異
        if expected.strip() == actual.strip():
            return Difference(
                field=field,
                expected=expected,
                actual=actual,
                diff_type=DifferenceType.WHITESPACE,
                severity="warning",
            )

        # 大文字小文字差異
        if expected.upper() == actual.upper():
            return Difference(
                field=field,
                expected=expected,
                actual=actual,
                diff_type=DifferenceType.FORMAT,
                severity="warning",
            )

        return Difference(
            field=field,
            expected=expected,
            actual=actual,
            diff_type=DifferenceType.VALUE,
            severity="error",
        )

    def _compare_numbers(
        self, field: str, expected: Any, actual: Any, strict: bool
    ) -> Difference:
        """数値比較."""
        try:
            exp_dec = Decimal(str(expected))
            act_dec = Decimal(str(actual))

            diff = abs(exp_dec - act_dec)

            if diff == 0:
                return Difference(
                    field=field,
                    expected=expected,
                    actual=actual,
                    diff_type=DifferenceType.PRECISION,
                    severity="info",
                )

            if not strict and diff <= self.NUMERIC_TOLERANCE:
                return Difference(
                    field=field,
                    expected=expected,
                    actual=actual,
                    diff_type=DifferenceType.PRECISION,
                    severity="warning",
                )

        except (InvalidOperation, ValueError):
            pass

        return Difference(
            field=field,
            expected=expected,
            actual=actual,
            diff_type=DifferenceType.VALUE,
            severity="error",
        )

    def _is_numeric(self, value: Any) -> bool:
        """数値かどうか判定."""
        if isinstance(value, (int, float, Decimal)):
            return True
        if isinstance(value, str):
            try:
                Decimal(value)
                return True
            except InvalidOperation:
                return False
        return False

    # =========================================================================
    # レポート生成
    # =========================================================================

    def generate_diff_report(
        self, comparison: dict[str, Any]
    ) -> str:
        """差分レポートを生成（確定的処理）.

        Args:
            comparison: compare_outputs の結果

        Returns:
            人間可読な差分レポート
        """
        lines = ["=" * 50, "差分レポート", "=" * 50, ""]

        if comparison.get("is_equal"):
            lines.append("✅ 全ての出力が一致しました。")
        else:
            lines.append(f"❌ {len(comparison.get('differences', []))} 件の差異があります。")
            lines.append("")

            for diff in comparison.get("differences", []):
                lines.append(f"フィールド: {diff['field']}")
                lines.append(f"  期待値: {diff['expected']}")
                lines.append(f"  実際値: {diff['actual']}")
                lines.append(f"  種類: {diff['type']}")
                lines.append("")

        if comparison.get("warnings"):
            lines.append("⚠️ 警告:")
            for warn in comparison["warnings"]:
                lines.append(f"  - {warn}")

        lines.append("")
        lines.append(f"一致率: {comparison.get('match_rate', 0) * 100:.1f}%")

        return "\n".join(lines)

    # =========================================================================
    # メイン処理
    # =========================================================================

    def process(self, input_data: dict[str, Any]) -> dict[str, Any]:
        """検証を実行.

        Args:
            input_data:
                - target_code: ターゲットコード（または java_code）
                - expected_outputs: 期待される出力
                - test_inputs: テスト入力
                - test_cases: テストケースリスト（オプション）
                - strict_numeric: 数値を厳格に比較するか

        Returns:
            - verdict: PASS/FAIL/RETRY
            - comparison: 比較結果
            - report: 差分レポート
            - test_results: テスト結果リスト
        """
        # target_code または java_code を取得（後方互換性）
        target_code = input_data.get("target_code") or input_data.get("java_code", "")

        if not target_code:
            return {"success": False, "error": "target_code is required"}

        # テストケースを取得
        test_cases = input_data.get("test_cases", [])
        if not test_cases:
            # 単一テストケース
            test_cases = [{
                "name": "default",
                "inputs": input_data.get("test_inputs", {}),
                "expected_outputs": input_data.get("expected_outputs", {}),
            }]

        # 全テストケースを実行
        results = []
        all_passed = True
        any_retryable = False

        for tc in test_cases:
            result = self._run_single_test(
                target_code=target_code,
                test_case=tc,
                strict_numeric=input_data.get("strict_numeric", True),
            )
            results.append(result)

            if result["verdict"] != Verdict.PASS.value:
                all_passed = False
            if result["verdict"] == Verdict.RETRY.value:
                any_retryable = True

        # 総合判定
        if all_passed:
            verdict = Verdict.PASS.value
        elif any_retryable:
            verdict = Verdict.RETRY.value
        else:
            verdict = Verdict.FAIL.value

        # 総合レポート
        summary_report = self._generate_summary_report(results)

        return {
            "success": True,
            "migration_type": self._migration_type,
            "target_language": self._target_adapter.language_name,
            "verdict": verdict,
            "test_results": results,
            "summary_report": summary_report,
            "total_tests": len(results),
            "passed_tests": sum(1 for r in results if r["verdict"] == Verdict.PASS.value),
        }

    def _run_single_test(
        self,
        target_code: str,
        test_case: dict[str, Any],
        strict_numeric: bool,
    ) -> dict[str, Any]:
        """単一テストケースを実行."""
        tc_name = test_case.get("name", "unnamed")
        inputs = test_case.get("inputs", {})
        expected = test_case.get("expected_outputs", {})

        # ターゲットコード実行
        exec_result = self.execute_target(target_code, inputs)

        if not exec_result["success"]:
            return {
                "name": tc_name,
                "verdict": Verdict.FAIL.value,
                "error": exec_result.get("error"),
                "stderr": exec_result.get("stderr"),
            }

        # 出力パース
        actual = self.parse_java_output(exec_result.get("stdout", ""))

        # 比較
        comparison = self.compare_outputs(expected, actual, strict_numeric)

        # レポート生成
        report = self.generate_diff_report(comparison)

        # 判定
        if comparison["is_equal"]:
            verdict = Verdict.PASS.value
        elif len(comparison.get("warnings", [])) > 0 and len(comparison.get("differences", [])) == 0:
            verdict = Verdict.PASS_WITH_WARNING.value
        elif comparison.get("match_rate", 0) > 0.8:
            verdict = Verdict.RETRY.value
        else:
            verdict = Verdict.FAIL.value

        return {
            "name": tc_name,
            "verdict": verdict,
            "comparison": comparison,
            "report": report,
            "exec_result": exec_result,
            "actual_output": actual,
        }

    def _generate_summary_report(self, results: list[dict[str, Any]]) -> str:
        """総合レポートを生成."""
        lines = [
            "=" * 60,
            "テスト実行サマリー",
            "=" * 60,
            "",
        ]

        passed = sum(1 for r in results if r["verdict"] == Verdict.PASS.value)
        warned = sum(1 for r in results if r["verdict"] == Verdict.PASS_WITH_WARNING.value)
        failed = sum(1 for r in results if r["verdict"] == Verdict.FAIL.value)
        retryable = sum(1 for r in results if r["verdict"] == Verdict.RETRY.value)

        lines.append(f"総テスト数: {len(results)}")
        lines.append(f"  ✅ PASS: {passed}")
        lines.append(f"  ⚠️ PASS_WITH_WARNING: {warned}")
        lines.append(f"  🔄 RETRY: {retryable}")
        lines.append(f"  ❌ FAIL: {failed}")
        lines.append("")

        # 失敗テストの詳細
        failed_tests = [r for r in results if r["verdict"] in (Verdict.FAIL.value, Verdict.RETRY.value)]
        if failed_tests:
            lines.append("-" * 40)
            lines.append("失敗/要修正テストの詳細:")
            lines.append("")

            for r in failed_tests:
                lines.append(f"【{r['name']}】 - {r['verdict']}")
                if "error" in r:
                    lines.append(f"  エラー: {r['error']}")
                if "comparison" in r:
                    for diff in r["comparison"].get("differences", [])[:3]:  # 最大3件
                        lines.append(f"  - {diff['field']}: {diff['expected']} ≠ {diff['actual']}")
                lines.append("")

        return "\n".join(lines)
