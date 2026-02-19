"""サンドボックスライフサイクル管理のテスト.

ManagedSandbox、SandboxManager、Workspaceのテスト。
"""

import asyncio
from unittest.mock import AsyncMock, MagicMock, patch

import pytest

from agentflow.sandbox.base import ExecutionResult, SandboxConfig, SandboxState
from agentflow.sandbox.lifecycle import EventType, ManagedSandbox, SandboxEvent
from agentflow.sandbox.manager import SandboxManager
from agentflow.sandbox.workspace import Workspace, WorkspaceState


class TestSandboxState:
    """SandboxState のテスト."""

    def test_state_values(self) -> None:
        """状態値が正しいことを確認."""
        assert SandboxState.CREATED.value == "created"
        assert SandboxState.STARTED.value == "started"
        assert SandboxState.STOPPED.value == "stopped"
        assert SandboxState.ARCHIVED.value == "archived"
        assert SandboxState.DELETED.value == "deleted"

    def test_valid_transitions(self) -> None:
        """有効な状態遷移を確認."""
        # CREATED → STARTED
        assert SandboxState.can_transition(SandboxState.CREATED, SandboxState.STARTED)
        # STARTED → STOPPED
        assert SandboxState.can_transition(SandboxState.STARTED, SandboxState.STOPPED)
        # STOPPED → STARTED (再起動)
        assert SandboxState.can_transition(SandboxState.STOPPED, SandboxState.STARTED)
        # STOPPED → ARCHIVED
        assert SandboxState.can_transition(SandboxState.STOPPED, SandboxState.ARCHIVED)
        # ARCHIVED → DELETED
        assert SandboxState.can_transition(SandboxState.ARCHIVED, SandboxState.DELETED)

    def test_invalid_transitions(self) -> None:
        """無効な状態遷移を確認."""
        # CREATED → STOPPED (直接停止不可)
        assert not SandboxState.can_transition(SandboxState.CREATED, SandboxState.STOPPED)
        # STARTED → ARCHIVED (停止せずにアーカイブ不可)
        assert not SandboxState.can_transition(SandboxState.STARTED, SandboxState.ARCHIVED)
        # DELETED → STARTED (削除後は復活不可)
        assert not SandboxState.can_transition(SandboxState.DELETED, SandboxState.STARTED)


class TestManagedSandbox:
    """ManagedSandbox のテスト."""

    @pytest.fixture
    def mock_provider(self) -> MagicMock:
        """モックプロバイダを作成."""
        provider = MagicMock()
        provider.execute = AsyncMock(
            return_value=ExecutionResult(
                exit_code=0,
                stdout="Hello",
                stderr="",
                duration_ms=100.0,
            )
        )
        provider.close = AsyncMock()
        return provider

    @pytest.fixture
    def sandbox(self, mock_provider: MagicMock) -> ManagedSandbox:
        """テスト用サンドボックスを作成."""
        return ManagedSandbox(mock_provider, SandboxConfig(), "test-sandbox")

    def test_initial_state(self, sandbox: ManagedSandbox) -> None:
        """初期状態を確認."""
        assert sandbox.state == SandboxState.CREATED
        assert sandbox.sandbox_id == "test-sandbox"
        assert not sandbox.is_running

    @pytest.mark.asyncio
    async def test_start(self, sandbox: ManagedSandbox) -> None:
        """起動テスト."""
        await sandbox.start()
        assert sandbox.state == SandboxState.STARTED
        assert sandbox.is_running

    @pytest.mark.asyncio
    async def test_stop(self, sandbox: ManagedSandbox) -> None:
        """停止テスト."""
        await sandbox.start()
        await sandbox.stop()
        assert sandbox.state == SandboxState.STOPPED
        assert not sandbox.is_running

    @pytest.mark.asyncio
    async def test_execute(self, sandbox: ManagedSandbox, mock_provider: MagicMock) -> None:
        """実行テスト."""
        await sandbox.start()
        result = await sandbox.execute("print('Hello')")

        assert result.success
        assert result.stdout == "Hello"
        mock_provider.execute.assert_called_once()

    @pytest.mark.asyncio
    async def test_execute_not_running(self, sandbox: ManagedSandbox) -> None:
        """実行中でない場合のエラーテスト."""
        with pytest.raises(RuntimeError, match="実行中ではありません"):
            await sandbox.execute("print('Hello')")

    @pytest.mark.asyncio
    async def test_invalid_transition(self, sandbox: ManagedSandbox) -> None:
        """無効な状態遷移のエラーテスト."""
        with pytest.raises(ValueError, match="無効な状態遷移"):
            await sandbox.stop()  # CREATED → STOPPED は無効

    @pytest.mark.asyncio
    async def test_event_callback(self, sandbox: ManagedSandbox) -> None:
        """イベントコールバックテスト."""
        events: list[SandboxEvent] = []

        def callback(event: SandboxEvent) -> None:
            events.append(event)

        sandbox.on_event(callback)
        await sandbox.start()

        assert len(events) == 1
        assert events[0].event_type == EventType.STATE_CHANGED
        assert events[0].data["new_state"] == "started"

    @pytest.mark.asyncio
    async def test_context_manager(self, mock_provider: MagicMock) -> None:
        """コンテキストマネージャーテスト."""
        sandbox = ManagedSandbox(mock_provider, SandboxConfig(), "ctx-test")

        async with sandbox:
            assert sandbox.is_running

        assert sandbox.state == SandboxState.DELETED

    def test_stats(self, sandbox: ManagedSandbox) -> None:
        """統計情報テスト."""
        stats = sandbox.stats
        assert stats["sandbox_id"] == "test-sandbox"
        assert stats["state"] == "created"
        assert stats["execution_count"] == 0

    @pytest.mark.asyncio
    async def test_uptime(self, sandbox: ManagedSandbox) -> None:
        """稼働時間テスト."""
        assert sandbox.uptime_seconds == 0.0

        await sandbox.start()
        await asyncio.sleep(0.1)

        assert sandbox.uptime_seconds > 0.0


class TestSandboxManager:
    """SandboxManager のテスト."""

    @pytest.fixture
    def manager(self) -> SandboxManager:
        """テスト用マネージャーを作成."""
        # シングルトンをリセット
        SandboxManager._instance = None
        return SandboxManager()

    @pytest.mark.asyncio
    async def test_create_and_get(self, manager: SandboxManager) -> None:
        """作成と取得テスト."""
        with patch("agentflow.sandbox.lifecycle.ManagedSandbox.create") as mock_create:
            mock_sandbox = MagicMock()
            mock_sandbox.sandbox_id = "test-id"
            mock_create.return_value = mock_sandbox

            sandbox = await manager.create(provider="docker")

            assert sandbox.sandbox_id == "test-id"
            assert manager.get("test-id") == mock_sandbox

    def test_list_empty(self, manager: SandboxManager) -> None:
        """空の一覧テスト."""
        assert manager.list() == []

    @pytest.mark.asyncio
    async def test_delete(self, manager: SandboxManager) -> None:
        """削除テスト."""
        with patch("agentflow.sandbox.lifecycle.ManagedSandbox.create") as mock_create:
            mock_sandbox = MagicMock()
            mock_sandbox.sandbox_id = "delete-test"
            mock_sandbox.delete = AsyncMock()
            mock_create.return_value = mock_sandbox

            await manager.create(provider="docker")
            result = await manager.delete("delete-test")

            assert result is True
            assert manager.get("delete-test") is None

    @pytest.mark.asyncio
    async def test_delete_not_found(self, manager: SandboxManager) -> None:
        """存在しないサンドボックスの削除テスト."""
        result = await manager.delete("not-found")
        assert result is False

    def test_get_stats(self, manager: SandboxManager) -> None:
        """統計情報テスト."""
        stats = manager.get_stats()
        assert stats["total_sandboxes"] == 0
        assert stats["total_executions"] == 0

    def test_singleton(self) -> None:
        """シングルトンパターンテスト."""
        SandboxManager._instance = None
        m1 = SandboxManager()
        m2 = SandboxManager()
        assert m1 is m2


class TestWorkspace:
    """Workspace のテスト."""

    @pytest.fixture
    def mock_sandbox(self) -> MagicMock:
        """モックサンドボックスを作成."""
        sandbox = MagicMock(spec=ManagedSandbox)
        sandbox.is_running = True
        sandbox.state = SandboxState.STARTED
        sandbox.stats = {"execution_count": 0, "total_execution_ms": 0}
        sandbox.start = AsyncMock()
        sandbox.stop = AsyncMock()
        sandbox.delete = AsyncMock()
        sandbox.execute = AsyncMock(
            return_value=ExecutionResult(
                exit_code=0,
                stdout="2",
                stderr="",
                duration_ms=50.0,
            )
        )
        return sandbox

    @pytest.fixture
    def workspace(self, mock_sandbox: MagicMock) -> Workspace:
        """テスト用ワークスペースを作成."""
        return Workspace(mock_sandbox, "test-workspace")

    @pytest.mark.asyncio
    async def test_write_and_read_file(self, workspace: Workspace) -> None:
        """ファイル書き込み・読み込みテスト."""
        await workspace.write_file("test.py", b"print('Hello')")
        content = await workspace.read_file("test.py")

        assert content == b"print('Hello')"

    @pytest.mark.asyncio
    async def test_write_file_string(self, workspace: Workspace) -> None:
        """文字列でのファイル書き込みテスト."""
        await workspace.write_file("test.py", "print('Hello')")
        content = await workspace.read_file("test.py")

        assert content == b"print('Hello')"

    @pytest.mark.asyncio
    async def test_delete_file(self, workspace: Workspace) -> None:
        """ファイル削除テスト."""
        await workspace.write_file("test.py", b"content")
        result = await workspace.delete_file("test.py")

        assert result is True
        assert await workspace.read_file("test.py") is None

    @pytest.mark.asyncio
    async def test_list_files(self, workspace: Workspace) -> None:
        """ファイル一覧テスト."""
        await workspace.write_file("a.py", b"a")
        await workspace.write_file("b.py", b"bb")

        files = await workspace.list_files()

        assert len(files) == 2
        paths = [f.path for f in files]
        assert "a.py" in paths
        assert "b.py" in paths

    @pytest.mark.asyncio
    async def test_execute(self, workspace: Workspace, mock_sandbox: MagicMock) -> None:
        """コード実行テスト."""
        result = await workspace.execute("print(1 + 1)")

        assert result.success
        assert result.stdout == "2"
        mock_sandbox.execute.assert_called_once()

    @pytest.mark.asyncio
    async def test_run_file(self, workspace: Workspace) -> None:
        """ファイル実行テスト."""
        await workspace.write_file("main.py", b"print(1 + 1)")
        result = await workspace.run_file("main.py")

        assert result.success

    @pytest.mark.asyncio
    async def test_run_file_not_found(self, workspace: Workspace) -> None:
        """存在しないファイルの実行テスト."""
        result = await workspace.run_file("not_found.py")

        assert not result.success
        assert "見つかりません" in result.error

    @pytest.mark.asyncio
    async def test_save_and_restore_state(self, workspace: Workspace) -> None:
        """状態保存・復元テスト."""
        await workspace.write_file("test.py", b"content")
        workspace.set_metadata("key", "value")

        state = await workspace.save_state()

        # 新しいワークスペースで復元
        new_workspace = Workspace(MagicMock(), "new-workspace")
        await new_workspace.restore_state(state)

        content = await new_workspace.read_file("test.py")
        assert content == b"content"
        assert new_workspace.get_metadata("key") == "value"

    def test_workspace_state_json(self) -> None:
        """WorkspaceState JSON変換テスト."""
        state = WorkspaceState(
            workspace_id="ws-123",
            name="test",
            files={"test.py": b"print('Hello')"},
            metadata={"key": "value"},
        )

        json_str = state.to_json()
        restored = WorkspaceState.from_json(json_str)

        assert restored.workspace_id == "ws-123"
        assert restored.name == "test"
        assert restored.files["test.py"] == b"print('Hello')"
        assert restored.metadata["key"] == "value"

    def test_get_stats(self, workspace: Workspace) -> None:
        """統計情報テスト."""
        stats = workspace.get_stats()
        assert stats["workspace_id"] == workspace.workspace_id
        assert stats["name"] == "test-workspace"
        assert stats["file_count"] == 0
