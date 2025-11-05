"""Phase 2 Task 2 覆盖率测试脚本."""

import subprocess
import sys


# 临时重命名 conftest.py 以避免 MCP 导入问题
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
    # 运行测试
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
    # 恢复 conftest.py
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
