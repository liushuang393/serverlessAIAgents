"""テスト用インメモリサンドボックスプロバイダ.

本番では利用せず、ユニットテスト用にコードを同プロセスで実行する。
"""

from __future__ import annotations

import io
import sys
import time
from agentflow.sandbox.base import ExecutionResult, SandboxConfig, SandboxProvider


class MockSandbox(SandboxProvider):
    """テスト用モックサンドボックス。コードを exec で同プロセス実行し結果を返す。"""

    def __init__(self, config: SandboxConfig | None = None) -> None:
        super().__init__(config)

    async def execute(
        self,
        code: str,
        *,
        timeout: float | None = None,
        packages: list[str] | None = None,
        env: dict[str, str] | None = None,
        files: dict[str, bytes] | None = None,
    ) -> ExecutionResult:
        """コードを exec で実行し、stdout/stderr をキャプチャして返す。"""
        _ = packages, env, files
        timeout_sec = timeout or self._config.timeout
        start = time.perf_counter()
        stdout_capture = io.StringIO()
        stderr_capture = io.StringIO()
        old_stdout, old_stderr = sys.stdout, sys.stderr
        try:
            sys.stdout = stdout_capture
            sys.stderr = stderr_capture
            exec(code, {"__builtins__": __builtins__, "print": print})
            exit_code = 0
            error = None
        except SyntaxError as e:
            exit_code = 1
            error = f"SyntaxError: {e}"
        except Exception as e:
            exit_code = 1
            error = f"{type(e).__name__}: {e}"
        finally:
            sys.stdout = old_stdout
            sys.stderr = old_stderr
        duration_ms = (time.perf_counter() - start) * 1000
        return ExecutionResult(
            stdout=stdout_capture.getvalue().rstrip("\n"),
            stderr=stderr_capture.getvalue().rstrip("\n"),
            exit_code=exit_code,
            duration_ms=duration_ms,
            error=error,
        )
