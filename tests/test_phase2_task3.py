"""Phase 2 Task 3 カバレッジ測定用テストスクリプト."""

import subprocess
import sys

import pytest


if sys.platform != "win32":
    pytest.skip("Windows専用テストスクリプト（powershell使用）", allow_module_level=True)


# MCP のインポート問題を避けるため、一時的に conftest.py をリネーム
subprocess.run(
    [
        "powershell",
        "-Command",
        "Rename-Item",
        "-Path",
        "tests\\conftest.py",
        "-NewName",
        "conftest.py.bak",
        "-Force",
    ],
    check=False,
)

try:
    # テストを実行
    result = subprocess.run(
        [
            sys.executable,
            "-m",
            "pytest",
            "tests/unit/test_hooks.py",
            "-v",
            "--cov=agentflow.core.hooks",
            "--cov-report=term-missing",
            "--tb=short",
        ],
        capture_output=False,
        text=True,
        check=False,
    )

    sys.exit(result.returncode)
finally:
    # conftest.py を復元
    subprocess.run(
        [
            "powershell",
            "-Command",
            "Rename-Item",
            "-Path",
            "tests\\conftest.py.bak",
            "-NewName",
            "conftest.py",
            "-Force",
        ],
        check=False,
    )
