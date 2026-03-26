"""CLI Runtime Protocol のテスト."""


def test_cli_runtime_protocol_exists() -> None:
    """CLIRuntime Protocol がインポートできること."""
    from kernel.interfaces.cli_runtime import CLIRuntime, CLIResult
    assert hasattr(CLIRuntime, "execute")
    assert hasattr(CLIRuntime, "execute_stream")


def test_cli_result_dataclass() -> None:
    """CLIResult が正しくインスタンス化できること."""
    from kernel.interfaces.cli_runtime import CLIResult
    result = CLIResult(exit_code=0, stdout="ok", trace_id="abc-123")
    assert result.exit_code == 0
    assert result.stdout == "ok"
    assert result.trace_id == "abc-123"
    assert result.stderr == ""
    assert result.artifacts == []
