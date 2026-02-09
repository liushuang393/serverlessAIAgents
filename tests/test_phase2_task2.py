"""Phase 2 Task 2 カバレッジ測定用テストスクリプト."""

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
            "tests/unit/test_schemas.py",
            "-v",
            "--cov=agentflow.core.schemas",
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
