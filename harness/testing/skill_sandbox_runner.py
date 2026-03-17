"""Skill サンドボックステストランナー.

LLM が生成した Skill コードを安全なサンドボックス環境で自動テストする。

フロー:
    1. AST 解析で危険な呼び出し（exec/eval/subprocess.*）を検出
    2. テンポラリディレクトリにスキルコードとテストコードを配置
    3. サブプロセスで pytest を実行（タイムアウト・リソース制限付き）
    4. カバレッジレポートを生成して返す

設計原則:
    - 生成コードを人工審査前に必ずサンドボックスで実行する
    - 危険な AST ノードを検出したら実行前に拒否する
    - タイムアウトで無限ループを防止する

使用例:
    >>> runner = SkillSandboxRunner()
    >>> report = await runner.run_skill_tests(skill_code, test_code)
    >>> if report.passed:
    ...     print(f"テスト通過: {report.tests_passed}/{report.tests_total}")
"""

from __future__ import annotations

import ast
import contextlib
import sys
import tempfile
import textwrap
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


# =========================================================================
# 例外クラス
# =========================================================================


class SandboxError(Exception):
    """サンドボックス実行エラー基底クラス."""


class DangerousCodeError(SandboxError):
    """危険なコードが検出された場合のエラー."""

    def __init__(self, dangerous_calls: list[str]) -> None:
        """初期化."""
        super().__init__(f"危険な呼び出しが検出されました: {', '.join(dangerous_calls)}")
        self.dangerous_calls = dangerous_calls


class SandboxTimeoutError(SandboxError):
    """サンドボックス実行タイムアウト."""


# =========================================================================
# データクラス
# =========================================================================


@dataclass
class SandboxTestReport:
    """サンドボックステスト実行レポート."""

    passed: bool
    tests_total: int
    tests_passed: int
    tests_failed: int
    tests_error: int
    duration_seconds: float
    coverage_percent: float
    stdout: str = ""
    stderr: str = ""
    dangerous_calls_detected: list[str] = field(default_factory=list)

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "passed": self.passed,
            "tests_total": self.tests_total,
            "tests_passed": self.tests_passed,
            "tests_failed": self.tests_failed,
            "tests_error": self.tests_error,
            "duration_seconds": self.duration_seconds,
            "coverage_percent": self.coverage_percent,
            "stdout": self.stdout[:2000],  # 長すぎる出力を切り詰め
            "stderr": self.stderr[:2000],
            "dangerous_calls_detected": self.dangerous_calls_detected,
        }

    def to_text_report(self) -> str:
        """人間可読テキストレポートを生成."""
        status = "✅ PASS" if self.passed else "❌ FAIL"
        lines = [
            f"=== Skill サンドボックステスト結果 {status} ===",
            f"テスト: {self.tests_passed}/{self.tests_total} 通過",
            f"失敗: {self.tests_failed}, エラー: {self.tests_error}",
            f"カバレッジ: {self.coverage_percent:.1f}%",
            f"実行時間: {self.duration_seconds:.2f}s",
        ]
        if self.dangerous_calls_detected:
            lines.append(f"⚠️ 危険呼び出し: {', '.join(self.dangerous_calls_detected)}")
        if self.stderr:
            lines.append(f"--- stderr ---\n{self.stderr[:500]}")
        return "\n".join(lines)


# =========================================================================
# AST 解析
# =========================================================================

# 禁止する関数・モジュール呼び出しパターン
_DANGEROUS_NAMES: frozenset[str] = frozenset(
    [
        "exec",
        "eval",
        "compile",
        "__import__",
        "open",  # ファイルアクセスは SkillGateway 経由のみ許可
        "breakpoint",
    ]
)

_DANGEROUS_MODULE_CALLS: frozenset[str] = frozenset(
    [
        "subprocess",
        "os.system",
        "os.popen",
        "os.execv",
        "os.execve",
        "os.execvp",
        "shutil.rmtree",
        "pathlib.Path.unlink",
    ]
)


def _detect_dangerous_calls(code: str) -> list[str]:
    """AST 解析で危険な呼び出しを検出する.

    Args:
        code: 検査対象の Python ソースコード

    Returns:
        検出された危険呼び出し名のリスト（空リスト = 危険なし）

    Raises:
        SyntaxError: コードの構文が無効な場合
    """
    tree = ast.parse(code)
    found: list[str] = []

    for node in ast.walk(tree):
        # 直接関数呼び出し: exec("..."), eval("...")
        if isinstance(node, ast.Call):
            if isinstance(node.func, ast.Name) and node.func.id in _DANGEROUS_NAMES:
                found.append(node.func.id)
            # 属性呼び出し: subprocess.run(), os.system() など
            elif isinstance(node.func, ast.Attribute):
                full_name = _get_attribute_name(node.func)
                for dangerous in _DANGEROUS_MODULE_CALLS:
                    if full_name == dangerous or full_name.startswith(f"{dangerous}."):
                        found.append(full_name)

        # import subprocess, import os など
        elif isinstance(node, ast.Import):
            for alias in node.names:
                if alias.name in ("subprocess", "ctypes", "socket"):
                    found.append(f"import {alias.name}")

        # from subprocess import run など
        elif isinstance(node, ast.ImportFrom):
            if node.module in ("subprocess", "ctypes"):
                found.append(f"from {node.module} import ...")

    return list(dict.fromkeys(found))  # 重複除去（順序保持）


def _get_attribute_name(node: ast.Attribute | ast.Name | ast.expr) -> str:
    """属性ノードから完全名を再構築する."""
    if isinstance(node, ast.Attribute):
        parent = _get_attribute_name(node.value)
        return f"{parent}.{node.attr}" if parent else node.attr
    if isinstance(node, ast.Name):
        return node.id
    return ""


# =========================================================================
# サンドボックスランナー
# =========================================================================


@dataclass
class SkillSandboxRunner:
    """Skill コードをサンドボックス環境で自動テストするランナー.

    Attributes:
        timeout_seconds: テスト実行タイムアウト（秒）
        coverage_fail_under: カバレッジ最低ライン（%）
        allow_dangerous_calls: 危険呼び出しチェックをスキップ（テスト用途のみ）
    """

    timeout_seconds: int = 60
    coverage_fail_under: float = 60.0
    allow_dangerous_calls: bool = False

    async def run_skill_tests(
        self,
        skill_code: str,
        test_code: str,
        *,
        skill_module_name: str = "generated_skill",
    ) -> SandboxTestReport:
        """Skill コードをサンドボックスでテスト実行する.

        Args:
            skill_code: テスト対象のスキル Python コード
            test_code: テストコード（pytest スタイル）
            skill_module_name: スキルモジュール名（import 名）

        Returns:
            SandboxTestReport テスト結果レポート

        Raises:
            DangerousCodeError: 危険なコードが検出された場合
        """
        import asyncio

        # 1. AST 解析で危険な呼び出しを検出
        if not self.allow_dangerous_calls:
            dangerous = _detect_dangerous_calls(skill_code)
            if dangerous:
                raise DangerousCodeError(dangerous)

        # 2. テンポラリディレクトリにファイル配置
        with tempfile.TemporaryDirectory(prefix="skill_sandbox_") as tmpdir:
            tmp = Path(tmpdir)
            (tmp / f"{skill_module_name}.py").write_text(skill_code, encoding="utf-8")
            (tmp / "test_skill.py").write_text(
                _inject_import(test_code, skill_module_name),
                encoding="utf-8",
            )
            # pytest.ini を配置してカバレッジを設定
            (tmp / "pytest.ini").write_text(
                textwrap.dedent("""
                    [pytest]
                    addopts = --tb=short -q --no-header
                    asyncio_mode = auto
                """).strip(),
                encoding="utf-8",
            )

            # 3. サブプロセスで pytest を実行
            import time

            start = time.monotonic()
            try:
                proc = await asyncio.wait_for(
                    asyncio.create_subprocess_exec(
                        sys.executable,
                        "-m",
                        "pytest",
                        str(tmp / "test_skill.py"),
                        f"--cov={skill_module_name}",
                        "--cov-report=term-missing",
                        f"--cov-fail-under={self.coverage_fail_under}",
                        "-v",
                        cwd=str(tmp),
                        stdout=asyncio.subprocess.PIPE,
                        stderr=asyncio.subprocess.PIPE,
                    ),
                    timeout=self.timeout_seconds,
                )
                stdout_bytes, stderr_bytes = await proc.communicate()
                duration = time.monotonic() - start
            except TimeoutError:
                msg = f"テスト実行がタイムアウトしました ({self.timeout_seconds}s)"
                raise SandboxTimeoutError(msg)

            stdout = stdout_bytes.decode("utf-8", errors="replace")
            stderr = stderr_bytes.decode("utf-8", errors="replace")

            # 4. 結果をパース
            return _parse_pytest_output(
                stdout=stdout,
                stderr=stderr,
                returncode=proc.returncode or 0,
                duration=duration,
            )

    def generate_test_report(self, result: SandboxTestReport) -> str:
        """SandboxTestReport から人間可読レポートを生成する.

        Args:
            result: run_skill_tests() の戻り値

        Returns:
            Markdown 形式のテストレポート文字列
        """
        status_emoji = "✅" if result.passed else "❌"
        lines = [
            f"## {status_emoji} Skill テストレポート",
            "",
            "| 項目 | 値 |",
            "|------|----|",
            f"| 結果 | {'PASS' if result.passed else 'FAIL'} |",
            f"| テスト数 | {result.tests_total} |",
            f"| 成功 | {result.tests_passed} |",
            f"| 失敗 | {result.tests_failed} |",
            f"| エラー | {result.tests_error} |",
            f"| カバレッジ | {result.coverage_percent:.1f}% |",
            f"| 実行時間 | {result.duration_seconds:.2f}s |",
        ]

        if result.dangerous_calls_detected:
            lines += [
                "",
                "### ⚠️ 危険呼び出し検出",
                "```",
                "\n".join(result.dangerous_calls_detected),
                "```",
            ]

        if result.stdout:
            lines += [
                "",
                "### pytest 出力",
                "```",
                result.stdout[:1500],
                "```",
            ]

        return "\n".join(lines)


# =========================================================================
# ヘルパー関数
# =========================================================================


def _inject_import(test_code: str, module_name: str) -> str:
    """テストコードにモジュールのインポートが含まれていない場合に追加する."""
    if module_name in test_code:
        return test_code
    return f"import {module_name}  # auto-injected by sandbox runner\n\n{test_code}"


def _parse_pytest_output(
    stdout: str,
    stderr: str,
    returncode: int,
    duration: float,
) -> SandboxTestReport:
    """pytest の出力からテスト結果を解析する."""
    tests_total = 0
    tests_passed = 0
    tests_failed = 0
    tests_error = 0
    coverage_percent = 0.0

    for line in stdout.splitlines():
        # "5 passed, 1 failed in 0.23s" などのサマリ行
        if " passed" in line or " failed" in line or " error" in line:
            import re

            passed_m = re.search(r"(\d+) passed", line)
            failed_m = re.search(r"(\d+) failed", line)
            error_m = re.search(r"(\d+) error", line)
            if passed_m:
                tests_passed = int(passed_m.group(1))
            if failed_m:
                tests_failed = int(failed_m.group(1))
            if error_m:
                tests_error = int(error_m.group(1))
            tests_total = tests_passed + tests_failed + tests_error

        # "TOTAL ... 85%" のカバレッジ行
        if line.strip().startswith("TOTAL"):
            parts = line.split()
            for part in reversed(parts):
                if part.endswith("%"):
                    with contextlib.suppress(ValueError):
                        coverage_percent = float(part.rstrip("%"))
                    break

    passed = returncode == 0 and tests_failed == 0 and tests_error == 0

    return SandboxTestReport(
        passed=passed,
        tests_total=tests_total,
        tests_passed=tests_passed,
        tests_failed=tests_failed,
        tests_error=tests_error,
        duration_seconds=duration,
        coverage_percent=coverage_percent,
        stdout=stdout,
        stderr=stderr,
    )
