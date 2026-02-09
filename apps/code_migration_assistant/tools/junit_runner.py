"""JUnit Runner - JUnit テスト実行ツール.

JUnit 5 テストをコンパイル・実行し、結果を解析する。
"""

import re
import subprocess
import tempfile
import xml.etree.ElementTree as ET
from dataclasses import dataclass, field
from pathlib import Path
from typing import Any


@dataclass
class TestResult:
    """テスト結果."""

    name: str
    status: str  # passed, failed, error, skipped
    duration_ms: float = 0.0
    message: str = ""
    stack_trace: str = ""


@dataclass
class JUnitResult:
    """JUnit 実行結果."""

    success: bool
    total: int = 0
    passed: int = 0
    failed: int = 0
    errors: int = 0
    skipped: int = 0
    tests: list[TestResult] = field(default_factory=list)
    stdout: str = ""
    stderr: str = ""
    error: str = ""


class JUnitRunner:
    """JUnit テストランナー.

    JUnit 5 (Jupiter) をスタンドアロンで実行する。
    """

    # JUnit Platform Console Standalone JAR のパス
    # 環境変数または設定で上書き可能
    JUNIT_CONSOLE_JAR = "junit-platform-console-standalone.jar"

    def __init__(self, junit_jar_path: str | None = None) -> None:
        """初期化.

        Args:
            junit_jar_path: JUnit Console JAR のパス（オプション）
        """
        self._junit_jar = junit_jar_path or self.JUNIT_CONSOLE_JAR

    def run(
        self,
        test_code: str,
        target_code: str,
        timeout: int = 60,
    ) -> JUnitResult:
        """JUnit テストを実行.

        Args:
            test_code: JUnit テストコード
            target_code: テスト対象の Java コード
            timeout: タイムアウト秒数

        Returns:
            テスト実行結果
        """
        # クラス名を抽出
        test_class = self._extract_class_name(test_code)
        target_class = self._extract_class_name(target_code)

        if not test_class or not target_class:
            return JUnitResult(
                success=False,
                error="Cannot extract class names from code",
            )

        try:
            with tempfile.TemporaryDirectory() as tmpdir:
                tmp_path = Path(tmpdir)

                # ソースファイルを書き出し
                target_file = tmp_path / f"{target_class}.java"
                test_file = tmp_path / f"{test_class}.java"

                target_file.write_text(target_code, encoding="utf-8")
                test_file.write_text(test_code, encoding="utf-8")

                # コンパイル
                compile_result = self._compile(tmp_path, [target_file, test_file])
                if not compile_result["success"]:
                    return JUnitResult(
                        success=False,
                        error=f"Compilation failed: {compile_result['stderr']}",
                        stderr=compile_result["stderr"],
                    )

                # JUnit 実行
                return self._execute_junit(tmp_path, test_class, timeout)

        except FileNotFoundError as e:
            return JUnitResult(success=False, error=f"Command not found: {e}")
        except subprocess.TimeoutExpired:
            return JUnitResult(success=False, error="Test execution timeout")
        except Exception as e:
            return JUnitResult(success=False, error=str(e))

    def _extract_class_name(self, code: str) -> str | None:
        """クラス名を抽出."""
        match = re.search(r"class\s+(\w+)", code)
        return match.group(1) if match else None

    def _compile(
        self, work_dir: Path, source_files: list[Path]
    ) -> dict[str, Any]:
        """Java ソースをコンパイル."""
        # JUnit JAR をクラスパスに追加
        classpath = str(work_dir)
        if Path(self._junit_jar).exists():
            classpath = f"{self._junit_jar}:{work_dir}"

        cmd = [
            "javac",
            "-cp", classpath,
            "-d", str(work_dir),
        ] + [str(f) for f in source_files]

        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=30,
        )

        return {
            "success": result.returncode == 0,
            "stdout": result.stdout,
            "stderr": result.stderr,
        }

    def _execute_junit(
        self, work_dir: Path, test_class: str, timeout: int
    ) -> JUnitResult:
        """JUnit テストを実行."""
        # JUnit Platform Console を使用
        if Path(self._junit_jar).exists():
            return self._run_with_console(work_dir, test_class, timeout)
        # フォールバック: 直接 Java で実行（基本的なテスト用）
        return self._run_simple(work_dir, test_class, timeout)

    def _run_with_console(
        self, work_dir: Path, test_class: str, timeout: int
    ) -> JUnitResult:
        """JUnit Console で実行."""
        reports_dir = work_dir / "test-reports"
        reports_dir.mkdir(exist_ok=True)

        cmd = [
            "java",
            "-jar", self._junit_jar,
            "--class-path", str(work_dir),
            "--select-class", test_class,
            "--reports-dir", str(reports_dir),
        ]

        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=work_dir,
        )

        # XML レポートをパース
        return self._parse_junit_xml(reports_dir, result)

    def _run_simple(
        self, work_dir: Path, test_class: str, timeout: int
    ) -> JUnitResult:
        """簡易実行（JUnit JAR なし）.

        テストクラスの main メソッドを直接実行。
        実際の JUnit 機能は使用できない。
        """
        cmd = ["java", "-cp", str(work_dir), test_class]

        result = subprocess.run(
            cmd,
            capture_output=True,
            text=True,
            timeout=timeout,
            cwd=work_dir,
        )

        # 簡易的な結果解析
        success = result.returncode == 0
        return JUnitResult(
            success=success,
            total=1,
            passed=1 if success else 0,
            failed=0 if success else 1,
            stdout=result.stdout,
            stderr=result.stderr,
            tests=[
                TestResult(
                    name=test_class,
                    status="passed" if success else "failed",
                    message=result.stderr if not success else "",
                )
            ],
        )

    def _parse_junit_xml(
        self, reports_dir: Path, run_result: subprocess.CompletedProcess
    ) -> JUnitResult:
        """JUnit XML レポートをパース."""
        tests: list[TestResult] = []
        total = passed = failed = errors = skipped = 0

        # TEST-*.xml を探す
        xml_files = list(reports_dir.glob("TEST-*.xml"))

        if not xml_files:
            # XML がない場合は stdout から解析を試みる
            return self._parse_from_stdout(run_result)

        for xml_file in xml_files:
            try:
                tree = ET.parse(xml_file)
                root = tree.getroot()

                # testsuite 属性
                total += int(root.get("tests", 0))
                failed += int(root.get("failures", 0))
                errors += int(root.get("errors", 0))
                skipped += int(root.get("skipped", 0))

                # testcase 要素
                for tc in root.findall(".//testcase"):
                    name = tc.get("name", "unknown")
                    classname = tc.get("classname", "")
                    duration = float(tc.get("time", 0)) * 1000

                    # 結果判定
                    failure = tc.find("failure")
                    error = tc.find("error")
                    skip = tc.find("skipped")

                    if failure is not None:
                        status = "failed"
                        message = failure.get("message", "")
                        stack = failure.text or ""
                    elif error is not None:
                        status = "error"
                        message = error.get("message", "")
                        stack = error.text or ""
                    elif skip is not None:
                        status = "skipped"
                        message = skip.get("message", "")
                        stack = ""
                    else:
                        status = "passed"
                        message = ""
                        stack = ""

                    tests.append(TestResult(
                        name=f"{classname}.{name}" if classname else name,
                        status=status,
                        duration_ms=duration,
                        message=message,
                        stack_trace=stack,
                    ))

            except ET.ParseError as e:
                return JUnitResult(
                    success=False,
                    error=f"Failed to parse JUnit XML: {e}",
                    stderr=run_result.stderr,
                )

        passed = total - failed - errors - skipped

        return JUnitResult(
            success=failed == 0 and errors == 0,
            total=total,
            passed=passed,
            failed=failed,
            errors=errors,
            skipped=skipped,
            tests=tests,
            stdout=run_result.stdout,
            stderr=run_result.stderr,
        )

    def _parse_from_stdout(
        self, run_result: subprocess.CompletedProcess
    ) -> JUnitResult:
        """stdout からテスト結果を解析."""
        stdout = run_result.stdout

        # JUnit Console の出力形式を解析
        # 例: [         3 tests successful      ]
        #     [         1 tests failed          ]

        total = passed = failed = 0

        # 成功テスト数
        match = re.search(r"\[\s*(\d+)\s+tests?\s+successful\s*\]", stdout)
        if match:
            passed = int(match.group(1))

        # 失敗テスト数
        match = re.search(r"\[\s*(\d+)\s+tests?\s+failed\s*\]", stdout)
        if match:
            failed = int(match.group(1))

        total = passed + failed

        return JUnitResult(
            success=failed == 0 and run_result.returncode == 0,
            total=total,
            passed=passed,
            failed=failed,
            stdout=stdout,
            stderr=run_result.stderr,
        )

