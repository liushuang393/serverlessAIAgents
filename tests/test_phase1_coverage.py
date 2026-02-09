"""Phase 1 カバレッジ測定用テストスクリプト."""

import subprocess
import sys


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
            "tests/unit/test_validator.py",
            "tests/unit/test_manager.py",
            "tests/unit/test_loader.py",
            "-v",
            "--cov=agentflow.core.manager",
            "--cov=agentflow.core.loader",
            "--cov=agentflow.core.validator",
            "--cov-report=term-missing",
            "--cov-report=json:phase1_coverage.json",
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
