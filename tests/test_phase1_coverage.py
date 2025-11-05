"""Phase 1 覆盖率测试脚本."""

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
