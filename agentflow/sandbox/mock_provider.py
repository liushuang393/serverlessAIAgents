"""MockSandbox - テスト用インプロセス Python 実行サンドボックス.

実際のコンテナや外部サービスを使わず、Python の exec() で
コードをインプロセス実行するテスト・開発用プロバイダ。

Example:
    >>> from agentflow.sandbox import MockSandbox
    >>> sandbox = MockSandbox()
    >>> result = await sandbox.execute("print('hello')")
    >>> print(result.stdout)  # hello
"""

from __future__ import annotations

import io
import time
import traceback
from contextlib import redirect_stdout
from typing import Any

from agentflow.sandbox.base import (
    ExecutionResult,
    SandboxConfig,
    SandboxProvider,
)


class MockSandbox(SandboxProvider):
    """テスト用インプロセス Python 実行サンドボックス.

    exec() を使って Python コードを実行し、stdout を捕捉する。
    テスト・開発環境での使用を想定。本番環境では docker/microsandbox を使用すること。

    Attributes:
        _config: サンドボックス設定
    """

    def __init__(self, config: SandboxConfig | None = None) -> None:
        """初期化."""
        super().__init__(config)

    async def execute(self, code: str, **kwargs: Any) -> ExecutionResult:
        """Python コードをインプロセスで実行する.

        Args:
            code: 実行する Python コード
            **kwargs: 追加オプション（無視される）

        Returns:
            ExecutionResult: 実行結果
        """
        start = time.monotonic()
        stdout_buf = io.StringIO()

        try:
            # コンパイル時の構文エラーを検出
            compiled = compile(code, "<sandbox>", "exec")
        except SyntaxError as e:
            duration_ms = (time.monotonic() - start) * 1000
            return ExecutionResult(
                stdout="",
                stderr="",
                exit_code=1,
                duration_ms=duration_ms,
                error=f"SyntaxError: {e}",
            )

        try:
            with redirect_stdout(stdout_buf):
                exec(compiled, {})
            stdout = stdout_buf.getvalue().rstrip("\n")
            duration_ms = (time.monotonic() - start) * 1000
            return ExecutionResult(
                stdout=stdout,
                stderr="",
                exit_code=0,
                duration_ms=duration_ms,
            )
        except Exception:
            tb = traceback.format_exc()
            duration_ms = (time.monotonic() - start) * 1000
            return ExecutionResult(
                stdout=stdout_buf.getvalue().rstrip("\n"),
                stderr="",
                exit_code=1,
                duration_ms=duration_ms,
                error=tb.rstrip(),
            )
