"""Sandbox モジュールのユニットテスト."""

import pytest

from agentflow.sandbox import (
    ExecutionResult,
    MockSandbox,
    SandboxConfig,
    get_sandbox,
    reset_sandbox,
)


class TestExecutionResult:
    """ExecutionResult のテスト."""

    def test_success_when_exit_code_zero(self) -> None:
        """終了コード 0 で成功."""
        result = ExecutionResult(stdout="output", exit_code=0)
        assert result.success is True

    def test_failure_when_exit_code_nonzero(self) -> None:
        """終了コード非 0 で失敗."""
        result = ExecutionResult(exit_code=1)
        assert result.success is False

    def test_failure_when_error_present(self) -> None:
        """エラーメッセージありで失敗."""
        result = ExecutionResult(exit_code=0, error="Something went wrong")
        assert result.success is False

    def test_output_combines_stdout_stderr(self) -> None:
        """stdout と stderr が結合される."""
        result = ExecutionResult(stdout="out", stderr="err")
        assert "out" in result.output
        assert "err" in result.output

    def test_to_dict(self) -> None:
        """辞書変換."""
        result = ExecutionResult(stdout="test", exit_code=0)
        d = result.to_dict()
        assert d["stdout"] == "test"
        assert d["exit_code"] == 0
        assert d["success"] is True


class TestSandboxConfig:
    """SandboxConfig のテスト."""

    def test_default_values(self) -> None:
        """デフォルト値."""
        config = SandboxConfig()
        assert config.timeout == 60.0
        assert config.memory_mb == 1024
        assert config.cpus == 1
        assert config.image == "python"

    def test_custom_values(self) -> None:
        """カスタム値."""
        config = SandboxConfig(timeout=120.0, memory_mb=2048)
        assert config.timeout == 120.0
        assert config.memory_mb == 2048


class TestMockSandbox:
    """MockSandbox のテスト."""

    @pytest.fixture
    def sandbox(self) -> MockSandbox:
        """サンドボックスインスタンス."""
        return MockSandbox()

    @pytest.mark.asyncio
    async def test_simple_print(self, sandbox: MockSandbox) -> None:
        """シンプルな print 文."""
        result = await sandbox.execute("print('hello')")
        assert result.success is True
        assert result.stdout == "hello"

    @pytest.mark.asyncio
    async def test_arithmetic(self, sandbox: MockSandbox) -> None:
        """計算結果の出力."""
        result = await sandbox.execute("print(1 + 2)")
        assert result.success is True
        assert result.stdout == "3"

    @pytest.mark.asyncio
    async def test_multiline_code(self, sandbox: MockSandbox) -> None:
        """複数行のコード."""
        code = """
x = 10
y = 20
print(x + y)
"""
        result = await sandbox.execute(code)
        assert result.success is True
        assert result.stdout == "30"

    @pytest.mark.asyncio
    async def test_syntax_error(self, sandbox: MockSandbox) -> None:
        """構文エラー."""
        result = await sandbox.execute("print(")
        assert result.success is False
        assert result.exit_code == 1
        assert "SyntaxError" in result.error

    @pytest.mark.asyncio
    async def test_runtime_error(self, sandbox: MockSandbox) -> None:
        """実行時エラー."""
        result = await sandbox.execute("raise ValueError('test error')")
        assert result.success is False
        assert "ValueError" in result.error

    @pytest.mark.asyncio
    async def test_duration_recorded(self, sandbox: MockSandbox) -> None:
        """実行時間が記録される."""
        result = await sandbox.execute("print('fast')")
        assert result.duration_ms > 0


class TestGetSandbox:
    """get_sandbox 関数のテスト."""

    def setup_method(self) -> None:
        """各テスト前にリセット."""
        reset_sandbox()

    def teardown_method(self) -> None:
        """各テスト後にリセット."""
        reset_sandbox()

    def test_default_is_mock(self) -> None:
        """デフォルトは MockSandbox."""
        sandbox = get_sandbox()
        assert isinstance(sandbox, MockSandbox)

    def test_explicit_mock(self) -> None:
        """明示的に mock を指定."""
        sandbox = get_sandbox(provider="mock")
        assert isinstance(sandbox, MockSandbox)

    def test_caching(self) -> None:
        """同じインスタンスがキャッシュされる."""
        sandbox1 = get_sandbox()
        sandbox2 = get_sandbox()
        assert sandbox1 is sandbox2

    def test_unknown_provider_raises(self) -> None:
        """不明なプロバイダでエラー."""
        with pytest.raises(ValueError, match="Unknown sandbox provider"):
            get_sandbox(provider="unknown")

    def test_custom_config(self) -> None:
        """カスタム設定."""
        config = SandboxConfig(timeout=120.0)
        sandbox = get_sandbox(provider="mock", config=config)
        assert sandbox.config.timeout == 120.0
