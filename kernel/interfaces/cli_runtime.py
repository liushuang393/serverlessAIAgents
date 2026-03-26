"""kernel/interfaces/cli_runtime.py — CLI 実行の抽象."""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, AsyncIterator, Protocol, runtime_checkable


@dataclass(frozen=True)
class CLIResult:
    """CLI 実行結果."""

    exit_code: int
    stdout: str = ""
    stderr: str = ""
    trace_id: str = ""
    artifacts: list[dict[str, Any]] = field(default_factory=list)


@runtime_checkable
class CLIRuntime(Protocol):
    """CLI コマンド実行の抽象インターフェース."""

    async def execute(
        self,
        command: str,
        args: list[str],
        *,
        dry_run: bool = False,
        json_output: bool = False,
        **kwargs: Any,
    ) -> CLIResult:
        """コマンドを実行して結果を返す."""
        ...

    async def execute_stream(
        self,
        command: str,
        args: list[str],
        **kwargs: Any,
    ) -> AsyncIterator[str]:
        """コマンドをストリーミング実行."""
        ...
