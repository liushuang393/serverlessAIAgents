"""OS制御スキルパッケージ.

安全なOS操作API群を提供。セキュリティ隔離設計に基づき、
Agentは宣言済みAPIのみ呼び出し可能。

設計原則:
- Agent は直接 OS コマンドを実行できない
- 全てのセキュリティはコード制御
- workspace 外へのアクセス禁止
- コマンドはホワイトリスト制限

使用例:
    >>> from infrastructure.sandbox.os import OSSkillConfig, FileSystemSkill
    >>> config = OSSkillConfig(workspace_path=Path("./workspace"))
    >>> fs = FileSystemSkill(config)
    >>> content = await fs.read_file("data.txt")
"""

from infrastructure.sandbox.os.base import OSSkillBase
from infrastructure.sandbox.os.command import CommandSkill
from infrastructure.sandbox.os.config import (
    ExecutionMode,
    OSSkillConfig,
)
from infrastructure.sandbox.os.filesystem import FileSystemSkill
from infrastructure.sandbox.os.network import NetworkSkill
from infrastructure.sandbox.os.process import ProcessSkill
from infrastructure.sandbox.os.system_info import SystemInfoSkill


__all__ = [
    "CommandSkill",
    # 設定
    "ExecutionMode",
    # スキル
    "FileSystemSkill",
    "NetworkSkill",
    # 基底クラス
    "OSSkillBase",
    "OSSkillConfig",
    "ProcessSkill",
    "SystemInfoSkill",
]
