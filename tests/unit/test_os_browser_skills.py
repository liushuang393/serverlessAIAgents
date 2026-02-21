"""OS/Browser スキルの単体テスト.

セキュリティ隔離設計に基づくOS/Browserスキルのテスト。
- ファイルシステムスキル
- コマンドスキル
- ゲートウェイ
- モード切替機構
"""

from pathlib import Path
from unittest.mock import AsyncMock

import pytest

from agentflow.skills.gateway import (
    GatewayConfig,
    HumanConfirmationRequired,
    RiskLevel,
    SkillCategory,
    SkillDefinition,
    SkillGateway,
    SkillNotFoundError,
    SkillPermissionError,
)
from agentflow.skills.mode_switcher import (
    ModeSwitchDenied,
    ModeSwitcher,
)
from agentflow.skills.os.command import CommandSkill
from agentflow.skills.os.config import ExecutionMode, OSSkillConfig
from agentflow.skills.os.filesystem import FileSystemSkill


# ========== OSSkillConfig テスト ==========
class TestOSSkillConfig:
    """OSSkillConfig のテスト."""

    def test_default_config(self) -> None:
        """デフォルト設定のテスト."""
        config = OSSkillConfig()
        assert config.execution_mode == ExecutionMode.ISOLATED
        assert config.allow_write is False
        assert config.allow_delete is False
        assert len(config.command_whitelist) > 0

    def test_command_whitelist_check(self) -> None:
        """コマンドホワイトリストチェックのテスト."""
        config = OSSkillConfig(command_whitelist=["ls", "cat", "grep"])
        assert config.is_command_allowed("ls") is True
        assert config.is_command_allowed("ls -la") is True
        assert config.is_command_allowed("rm") is False

    def test_command_blacklist_priority(self) -> None:
        """ブラックリストがホワイトリストより優先されることをテスト."""
        config = OSSkillConfig(
            command_whitelist=["rm", "ls"],
            command_blacklist=["rm"],
        )
        assert config.is_command_allowed("rm") is False
        assert config.is_command_allowed("ls") is True

    def test_domain_whitelist_check(self) -> None:
        """ドメインホワイトリストチェックのテスト."""
        config = OSSkillConfig(domain_whitelist=["example.com", "api.test.com"])
        assert config.is_domain_allowed("example.com") is True
        assert config.is_domain_allowed("sub.example.com") is True
        assert config.is_domain_allowed("malicious.com") is False

    def test_path_in_workspace(self, tmp_path: Path) -> None:
        """ワークスペース内パスチェックのテスト."""
        config = OSSkillConfig(workspace_path=tmp_path)
        assert config.is_path_in_workspace(tmp_path / "file.txt") is True
        assert config.is_path_in_workspace(Path("/etc/passwd")) is False


# ========== FileSystemSkill テスト ==========
class TestFileSystemSkill:
    """FileSystemSkill のテスト."""

    @pytest.fixture
    def workspace(self, tmp_path: Path) -> Path:
        """テスト用ワークスペースを作成."""
        return tmp_path

    @pytest.fixture
    def config(self, workspace: Path) -> OSSkillConfig:
        """テスト用設定を作成."""
        return OSSkillConfig(
            workspace_path=workspace,
            execution_mode=ExecutionMode.ISOLATED,
        )

    @pytest.fixture
    def fs_skill(self, config: OSSkillConfig) -> FileSystemSkill:
        """FileSystemSkill インスタンスを作成."""
        return FileSystemSkill(config)

    @pytest.mark.asyncio
    async def test_read_file(self, fs_skill: FileSystemSkill, workspace: Path) -> None:
        """ファイル読み込みのテスト."""
        test_file = workspace / "test.txt"
        test_file.write_text("Hello, World!", encoding="utf-8")

        content = await fs_skill.read_file("test.txt")
        assert content == "Hello, World!"

    @pytest.mark.asyncio
    async def test_read_file_outside_workspace(self, fs_skill: FileSystemSkill) -> None:
        """ワークスペース外ファイル読み込みが拒否されることをテスト."""
        from agentflow.skills.os.base import PathSecurityError

        with pytest.raises(PathSecurityError):
            await fs_skill.read_file("/etc/passwd")

    @pytest.mark.asyncio
    async def test_list_dir(self, fs_skill: FileSystemSkill, workspace: Path) -> None:
        """ディレクトリ一覧のテスト."""
        (workspace / "file1.txt").touch()
        (workspace / "file2.txt").touch()
        (workspace / "subdir").mkdir()

        files = await fs_skill.list_dir(".")
        names = [f.name for f in files]
        assert "file1.txt" in names
        assert "file2.txt" in names
        assert "subdir" in names

    @pytest.mark.asyncio
    async def test_exists(self, fs_skill: FileSystemSkill, workspace: Path) -> None:
        """ファイル存在確認のテスト."""
        (workspace / "exists.txt").touch()

        assert await fs_skill.exists("exists.txt") is True
        assert await fs_skill.exists("not_exists.txt") is False

    @pytest.mark.asyncio
    async def test_write_file_denied_in_isolated(self, fs_skill: FileSystemSkill) -> None:
        """isolated モードで書き込みが拒否されることをテスト."""
        from agentflow.skills.os.base import ExecutionModeError

        with pytest.raises(ExecutionModeError):
            await fs_skill.write_file("output.txt", "content")

    @pytest.mark.asyncio
    async def test_write_file_allowed_with_permission(self, workspace: Path) -> None:
        """書き込み許可時にファイル書き込みが成功することをテスト."""
        config = OSSkillConfig(
            workspace_path=workspace,
            execution_mode=ExecutionMode.REAL_MACHINE,
            allow_write=True,
        )
        fs_skill = FileSystemSkill(config)

        result = await fs_skill.write_file("output.txt", "test content")
        assert result.success is True
        assert (workspace / "output.txt").read_text() == "test content"


# ========== CommandSkill テスト ==========
class TestCommandSkill:
    """CommandSkill のテスト."""

    @pytest.fixture
    def workspace(self, tmp_path: Path) -> Path:
        """テスト用ワークスペースを作成."""
        return tmp_path

    @pytest.fixture
    def config(self, workspace: Path) -> OSSkillConfig:
        """テスト用設定を作成."""
        return OSSkillConfig(
            workspace_path=workspace,
            command_whitelist=["echo", "ls", "cat"],
        )

    @pytest.fixture
    def cmd_skill(self, config: OSSkillConfig) -> CommandSkill:
        """CommandSkill インスタンスを作成."""
        return CommandSkill(config)

    @pytest.mark.asyncio
    async def test_run_whitelisted_command(self, cmd_skill: CommandSkill, workspace: Path) -> None:
        """ホワイトリストコマンドの実行テスト."""
        result = await cmd_skill.run_command(
            os_type="linux",
            command="echo",
            args=["hello"],
            cwd=str(workspace),
        )
        assert result.success is True
        assert "hello" in result.stdout

    @pytest.mark.asyncio
    async def test_run_blacklisted_command_denied(self, cmd_skill: CommandSkill, workspace: Path) -> None:
        """ブラックリストコマンドが拒否されることをテスト."""
        from agentflow.skills.os.base import CommandSecurityError

        with pytest.raises(CommandSecurityError):
            await cmd_skill.run_command(
                os_type="linux",
                command="rm",
                args=["-rf", "/"],
                cwd=str(workspace),
            )

    @pytest.mark.asyncio
    async def test_dry_run_mode(self, cmd_skill: CommandSkill, workspace: Path) -> None:
        """dry_run モードのテスト."""
        result = await cmd_skill.run_command(
            os_type="linux",
            command="echo",
            args=["test"],
            cwd=str(workspace),
            dry_run=True,
        )
        assert result.success is True
        assert result.dry_run is True
        # dry_run では検証メッセージが返される
        assert "dry_run" in result.stdout


# ========== SkillGateway テスト ==========
class TestSkillGateway:
    """SkillGateway のテスト."""

    @pytest.fixture
    def gateway(self) -> SkillGateway:
        """ゲートウェイインスタンスを作成."""
        config = GatewayConfig(
            execution_mode="isolated",
            require_confirmation_for_high_risk=True,
        )
        return SkillGateway(config)

    @pytest.fixture
    def mock_handler(self) -> AsyncMock:
        """モックハンドラを作成."""
        return AsyncMock(return_value="success")

    def test_register_skill(self, gateway: SkillGateway, mock_handler: AsyncMock) -> None:
        """スキル登録のテスト."""
        skill = SkillDefinition(
            name="test_skill",
            description="Test skill",
            category=SkillCategory.OS_READ,
            risk_level=RiskLevel.LOW,
            handler=mock_handler,
        )
        gateway.register_skill(skill)

        skills = gateway.list_skills()
        assert len(skills) == 1
        assert skills[0].name == "test_skill"

    @pytest.mark.asyncio
    async def test_call_registered_skill(self, gateway: SkillGateway, mock_handler: AsyncMock) -> None:
        """登録済みスキルの呼び出しテスト."""
        skill = SkillDefinition(
            name="test_skill",
            description="Test skill",
            category=SkillCategory.OS_READ,
            risk_level=RiskLevel.LOW,
            handler=mock_handler,
        )
        gateway.register_skill(skill)

        result = await gateway.call("test_skill", {"param": "value"})
        assert result.success is True
        assert result.result == "success"
        mock_handler.assert_called_once_with(param="value")

    @pytest.mark.asyncio
    async def test_call_unregistered_skill_raises(self, gateway: SkillGateway) -> None:
        """未登録スキル呼び出しでエラーが発生することをテスト."""
        with pytest.raises(SkillNotFoundError):
            await gateway.call("nonexistent", {})

    @pytest.mark.asyncio
    async def test_isolated_mode_blocks_real_machine_skill(
        self, gateway: SkillGateway, mock_handler: AsyncMock
    ) -> None:
        """isolated モードで real_machine 専用スキルがブロックされることをテスト."""
        skill = SkillDefinition(
            name="dangerous_skill",
            description="Dangerous",
            category=SkillCategory.OS_EXECUTE,
            risk_level=RiskLevel.HIGH,
            handler=mock_handler,
            allowed_in_isolated=False,
            allowed_in_real_machine=True,
        )
        gateway.register_skill(skill)

        with pytest.raises(SkillPermissionError):
            await gateway.call("dangerous_skill", {})

    @pytest.mark.asyncio
    async def test_high_risk_requires_confirmation(self, gateway: SkillGateway, mock_handler: AsyncMock) -> None:
        """高リスクスキルが人工確認を要求することをテスト."""
        skill = SkillDefinition(
            name="high_risk_skill",
            description="High risk",
            category=SkillCategory.OS_WRITE,
            risk_level=RiskLevel.HIGH,
            handler=mock_handler,
            allowed_in_isolated=True,
        )
        gateway.register_skill(skill)

        with pytest.raises(HumanConfirmationRequired):
            await gateway.call("high_risk_skill", {})

    @pytest.mark.asyncio
    async def test_dry_run_skips_execution(self, gateway: SkillGateway, mock_handler: AsyncMock) -> None:
        """dry_run モードで実行がスキップされることをテスト."""
        skill = SkillDefinition(
            name="test_skill",
            description="Test",
            category=SkillCategory.OS_READ,
            risk_level=RiskLevel.LOW,
            handler=mock_handler,
        )
        gateway.register_skill(skill)

        result = await gateway.call("test_skill", {"param": "value"}, dry_run=True)
        assert result.success is True
        assert result.dry_run is True
        mock_handler.assert_not_called()


# ========== ModeSwitcher テスト ==========
class TestModeSwitcher:
    """ModeSwitcher のテスト."""

    @pytest.fixture
    def gateway(self) -> SkillGateway:
        """ゲートウェイインスタンスを作成."""
        config = GatewayConfig(execution_mode="isolated")
        return SkillGateway(config)

    @pytest.fixture
    def switcher(self, gateway: SkillGateway) -> ModeSwitcher:
        """モード切替機構を作成."""
        return ModeSwitcher(gateway)

    def test_initial_mode_is_isolated(self, switcher: ModeSwitcher) -> None:
        """初期モードが isolated であることをテスト."""
        assert switcher.current_mode == ExecutionMode.ISOLATED

    @pytest.mark.asyncio
    async def test_switch_to_isolated_immediate(self, switcher: ModeSwitcher) -> None:
        """isolated への切替が即時可能であることをテスト."""
        transition = await switcher.switch_to_isolated(reason="テスト")
        assert transition.success is True
        assert switcher.current_mode == ExecutionMode.ISOLATED

    @pytest.mark.asyncio
    async def test_switch_to_real_machine_requires_confirmation(self, switcher: ModeSwitcher) -> None:
        """real_machine への切替が確認を要求することをテスト."""
        with pytest.raises(ModeSwitchDenied):
            await switcher.switch_to_real_machine(reason="テスト")

    @pytest.mark.asyncio
    async def test_switch_to_real_machine_with_handler(self, gateway: SkillGateway) -> None:
        """確認ハンドラ付きで real_machine への切替が成功することをテスト."""
        confirm_handler = AsyncMock(return_value=True)
        switcher = ModeSwitcher(gateway, confirmation_handler=confirm_handler)

        transition = await switcher.switch_to_real_machine(reason="デプロイ作業")
        assert transition.success is True
        assert switcher.current_mode == ExecutionMode.REAL_MACHINE
        confirm_handler.assert_called_once()

    @pytest.mark.asyncio
    async def test_switch_denied_when_user_rejects(self, gateway: SkillGateway) -> None:
        """ユーザーが拒否した場合に切替が失敗することをテスト."""
        confirm_handler = AsyncMock(return_value=False)
        switcher = ModeSwitcher(gateway, confirmation_handler=confirm_handler)

        with pytest.raises(ModeSwitchDenied):
            await switcher.switch_to_real_machine(reason="テスト")

        assert switcher.current_mode == ExecutionMode.ISOLATED

    @pytest.mark.asyncio
    async def test_history_tracking(self, gateway: SkillGateway) -> None:
        """切替履歴が記録されることをテスト."""
        confirm_handler = AsyncMock(return_value=True)
        switcher = ModeSwitcher(gateway, confirmation_handler=confirm_handler)

        await switcher.switch_to_real_machine(reason="作業開始")
        await switcher.switch_to_isolated(reason="作業完了")

        assert len(switcher.history) == 2
        assert switcher.history[0].to_mode == ExecutionMode.REAL_MACHINE
        assert switcher.history[1].to_mode == ExecutionMode.ISOLATED
