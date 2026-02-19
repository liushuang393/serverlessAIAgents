"""スキルファクトリ.

OS/Browser スキルをゲートウェイに登録するヘルパー。

Example:
    >>> from agentflow.skills.factory import create_skill_gateway
    >>> gateway = create_skill_gateway(
    ...     workspace_path=Path("./workspace"),
    ...     domain_whitelist=["example.com"],
    ... )
    >>> result = await gateway.call("read_file", {"path": "data.txt"})
"""

from __future__ import annotations

from pathlib import Path

from agentflow.skills.browser.browser_skill import BrowserSkill
from agentflow.skills.browser.config import BrowserSkillConfig
from agentflow.skills.gateway import (
    GatewayConfig,
    RiskLevel,
    SkillCategory,
    SkillDefinition,
    SkillGateway,
)
from agentflow.skills.os.command import CommandSkill
from agentflow.skills.os.config import ExecutionMode, OSSkillConfig
from agentflow.skills.os.filesystem import FileSystemSkill
from agentflow.skills.os.network import NetworkSkill
from agentflow.skills.os.process import ProcessSkill
from agentflow.skills.os.system_info import SystemInfoSkill


def create_skill_gateway(
    *,
    workspace_path: Path | None = None,
    execution_mode: str = "isolated",
    command_whitelist: list[str] | None = None,
    domain_whitelist: list[str] | None = None,
    allow_write: bool = False,
    allow_delete: bool = False,
    require_confirmation: bool = True,
) -> SkillGateway:
    """OS/Browser スキル付きゲートウェイを作成.

    Args:
        workspace_path: ワークスペースパス
        execution_mode: 実行モード (isolated/real_machine)
        command_whitelist: 許可コマンドリスト
        domain_whitelist: 許可ドメインリスト
        allow_write: 書き込み許可
        allow_delete: 削除許可
        require_confirmation: 高リスク操作時の確認要求

    Returns:
        設定済みゲートウェイ
    """
    workspace = workspace_path or Path.cwd()

    # OS スキル設定
    os_config = OSSkillConfig(
        workspace_path=workspace,
        execution_mode=ExecutionMode(execution_mode),
        command_whitelist=command_whitelist or [],
        domain_whitelist=domain_whitelist or [],
        allow_write=allow_write,
        allow_delete=allow_delete,
        require_human_confirmation=require_confirmation,
    )

    # Browser スキル設定
    browser_config = BrowserSkillConfig(
        domain_whitelist=domain_whitelist or [],
        require_human_confirmation=require_confirmation,
    )

    # スキルインスタンス
    fs = FileSystemSkill(os_config)
    cmd = CommandSkill(os_config)
    proc = ProcessSkill(os_config)
    net = NetworkSkill(os_config)
    sys_info = SystemInfoSkill(os_config)
    browser = BrowserSkill(browser_config)

    # ゲートウェイ設定
    gateway_config = GatewayConfig(
        execution_mode=execution_mode,
        workspace_path=workspace,
        require_confirmation_for_high_risk=require_confirmation,
    )

    gateway = SkillGateway(gateway_config)

    # ========== OS スキル登録 ==========
    # ファイルシステム（読み取り）
    gateway.register_skill(
        SkillDefinition(
            name="read_file",
            description="ファイルを読み込む",
            category=SkillCategory.OS_READ,
            risk_level=RiskLevel.LOW,
            handler=fs.read_file,
            allowed_in_isolated=True,
        )
    )

    gateway.register_skill(
        SkillDefinition(
            name="list_dir",
            description="ディレクトリ内容を一覧取得",
            category=SkillCategory.OS_READ,
            risk_level=RiskLevel.LOW,
            handler=fs.list_dir,
            allowed_in_isolated=True,
        )
    )

    gateway.register_skill(
        SkillDefinition(
            name="exists",
            description="パスが存在するか確認",
            category=SkillCategory.OS_READ,
            risk_level=RiskLevel.LOW,
            handler=fs.exists,
            allowed_in_isolated=True,
        )
    )

    gateway.register_skill(
        SkillDefinition(
            name="get_file_info",
            description="ファイル/ディレクトリ情報を取得",
            category=SkillCategory.OS_READ,
            risk_level=RiskLevel.LOW,
            handler=fs.get_file_info,
            allowed_in_isolated=True,
        )
    )

    # ファイルシステム（書き込み）
    gateway.register_skill(
        SkillDefinition(
            name="write_file",
            description="ファイルに書き込む",
            category=SkillCategory.OS_WRITE,
            risk_level=RiskLevel.HIGH,
            handler=fs.write_file,
            requires_confirmation=True,
            allowed_in_isolated=False,
            allowed_in_real_machine=True,
        )
    )

    # コマンド実行
    gateway.register_skill(
        SkillDefinition(
            name="run_command",
            description="コマンドを実行（ホワイトリスト制限）",
            category=SkillCategory.OS_EXECUTE,
            risk_level=RiskLevel.MEDIUM,
            handler=cmd.run_command,
            allowed_in_isolated=True,
        )
    )

    # プロセス制御
    gateway.register_skill(
        SkillDefinition(
            name="start_process",
            description="プロセスを起動",
            category=SkillCategory.OS_EXECUTE,
            risk_level=RiskLevel.HIGH,
            handler=proc.start_process,
            requires_confirmation=True,
            allowed_in_isolated=False,
        )
    )

    gateway.register_skill(
        SkillDefinition(
            name="stop_process",
            description="プロセスを停止",
            category=SkillCategory.OS_EXECUTE,
            risk_level=RiskLevel.MEDIUM,
            handler=proc.stop_process,
            allowed_in_isolated=False,
        )
    )

    # ネットワーク
    gateway.register_skill(
        SkillDefinition(
            name="http_request",
            description="HTTPリクエストを送信（ドメイン制限）",
            category=SkillCategory.NETWORK,
            risk_level=RiskLevel.MEDIUM,
            handler=net.http_request,
            allowed_in_isolated=True,
        )
    )

    # システム情報
    gateway.register_skill(
        SkillDefinition(
            name="get_os_info",
            description="OS情報を取得",
            category=SkillCategory.OS_READ,
            risk_level=RiskLevel.LOW,
            handler=sys_info.get_os_info,
        )
    )

    gateway.register_skill(
        SkillDefinition(
            name="get_resource_usage",
            description="リソース使用状況を取得",
            category=SkillCategory.OS_READ,
            risk_level=RiskLevel.LOW,
            handler=sys_info.get_resource_usage,
        )
    )

    # ========== Browser スキル登録 ==========
    gateway.register_skill(
        SkillDefinition(
            name="browser_navigate",
            description="URLに移動",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.MEDIUM,
            handler=browser.navigate,
            allowed_in_isolated=True,
        )
    )

    gateway.register_skill(
        SkillDefinition(
            name="browser_click",
            description="要素をクリック",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.MEDIUM,
            handler=browser.click,
            allowed_in_isolated=True,
        )
    )

    gateway.register_skill(
        SkillDefinition(
            name="browser_type",
            description="テキストを入力",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.MEDIUM,
            handler=browser.type_text,
            allowed_in_isolated=True,
        )
    )

    gateway.register_skill(
        SkillDefinition(
            name="browser_get_text",
            description="要素のテキストを取得",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.LOW,
            handler=browser.get_text,
            allowed_in_isolated=True,
        )
    )

    gateway.register_skill(
        SkillDefinition(
            name="browser_screenshot",
            description="スクリーンショットを取得",
            category=SkillCategory.BROWSER,
            risk_level=RiskLevel.LOW,
            handler=browser.take_screenshot,
            allowed_in_isolated=True,
        )
    )

    return gateway
