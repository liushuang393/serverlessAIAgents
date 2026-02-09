"""ツール発見サービス.

すべてのソースからツールを発見し、レジストリに登録するサービスモジュール。

設計原則:
- 高度な抽象化: ソースに依存しない統一発見インターフェース
- 高凝集: ツール発見機能のみに責任を持つ
- 拡張性: 新しいソースの追加が容易

サポートするソース:
- Builtin: @tool デコレータで定義された関数
- Skills: SkillEngine でロードされたスキル
- MCP: MCPサーバーが提供するツール
- Dynamic: ランタイムで動的生成されたツール

使用例:
    >>> # 発見サービスを作成
    >>> service = ToolDiscoveryService(tool_registry)
    >>>
    >>> # Skillsを発見
    >>> await service.discover_skills(skills_list)
    >>>
    >>> # MCPツールを発見
    >>> await service.discover_mcp_tools("filesystem", mcp_tools)
    >>>
    >>> # 全ソースを発見
    >>> await service.discover_all()
"""
from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from agentflow.core.tool_definition import ToolDefinition, ToolSource


if TYPE_CHECKING:
    from agentflow.core.tool_registry import ToolRegistry


class ToolDiscoveryService:
    """ツール発見サービス.

    すべてのソースからツールを発見しレジストリに登録。

    主な機能:
    - ビルトインツール発見（@tool デコレータ）
    - Skills発見
    - MCPサーバーツール発見
    - 全発見パイプライン
    - リフレッシュ（再発見）

    Attributes:
        _registry: ツールを登録するToolRegistry
        _logger: ロガー
    """

    def __init__(self, registry: ToolRegistry) -> None:
        """TargetレジストリでInitialize.

        Args:
            registry: 発見されたツールを登録するToolRegistry
        """
        self._registry = registry
        self._logger = logging.getLogger(__name__)

    @property
    def registry(self) -> ToolRegistry:
        """レジストリを取得.

        Returns:
            ToolRegistry インスタンス
        """
        return self._registry

    async def discover_all(self) -> int:
        """全発見パイプラインを実行.

        ビルトインツールを発見。Skills と MCP は外部データが必要なため
        このメソッドでは発見しない。

        Returns:
            発見されたツール数
        """
        initial = len(self._registry)

        await self.discover_builtins()

        discovered = len(self._registry) - initial
        self._logger.info(f"発見完了: {discovered} ツール")

        return discovered

    async def discover_builtins(self) -> int:
        """@tool デコレータで定義された関数を発見.

        ToolProvider から登録されたツールを取得してレジストリに登録。

        Returns:
            発見されたビルトインツール数
        """
        try:
            from agentflow.providers.tool_provider import ToolProvider

            provider = ToolProvider()
            tools = provider.discover()

            count = 0
            for tool_info in tools:
                tool_def = ToolDefinition.from_builtin(
                    name=tool_info.get("name", "unknown"),
                    description=tool_info.get("description", ""),
                    input_schema=tool_info.get("input_schema", {}),
                )
                self._registry.register(tool_def)
                count += 1

            self._logger.debug(f"ビルトインツール発見: {count}")
            return count

        except ImportError:
            self._logger.debug("ToolProvider が利用不可")
            return 0
        except Exception as e:
            self._logger.warning(f"ビルトイン発見エラー: {e}")
            return 0

    async def discover_skills(self, skills: list[dict[str, Any]]) -> int:
        """Skillsをツールとして登録.

        スキルエンジンからのスキル定義をツールとしてレジストリに登録。

        Args:
            skills: スキル定義のリスト

        Returns:
            登録されたスキル数
        """
        count = 0
        for skill in skills:
            try:
                tool_def = ToolDefinition.from_skill(skill)
                self._registry.register(tool_def)
                count += 1
            except Exception as e:
                self._logger.warning(f"スキル登録エラー {skill.get('name', '?')}: {e}")

        self._logger.debug(f"スキル発見: {count}")
        return count

    async def discover_mcp_tools(
        self,
        server_name: str,
        tools: list[dict[str, Any]],
    ) -> int:
        """MCPサーバーツールを登録.

        MCPサーバーからのツール定義をレジストリに登録。

        Args:
            server_name: MCPサーバー名
            tools: MCPツール定義のリスト

        Returns:
            登録されたMCPツール数
        """
        count = 0
        for mcp_tool in tools:
            try:
                tool_def = ToolDefinition.from_mcp(mcp_tool, server_name)
                self._registry.register(tool_def)
                count += 1
            except Exception as e:
                self._logger.warning(f"MCPツール登録エラー {mcp_tool.get('name', '?')}: {e}")

        self._logger.debug(f"MCPツール発見 ({server_name}): {count}")
        return count

    def register_builtin(
        self,
        name: str,
        description: str,
        input_schema: dict[str, Any],
        output_schema: dict[str, Any] | None = None,
    ) -> ToolDefinition:
        """ビルトインツールを手動登録.

        @tool デコレータを使わずにツールを登録する場合に使用。

        Args:
            name: ツール名
            description: ツールの説明
            input_schema: 入力スキーマ
            output_schema: 出力スキーマ（オプション）

        Returns:
            登録されたToolDefinition
        """
        tool_def = ToolDefinition(
            uri=f"tool://builtin/{name}",
            name=name,
            description=description,
            source=ToolSource.BUILTIN,
            input_schema=input_schema,
            output_schema=output_schema,
        )
        self._registry.register(tool_def)
        return tool_def

    async def refresh(self) -> int:
        """レジストリをクリアして再発見.

        主にテストやホットリロード用。

        Returns:
            リフレッシュ後のツール数
        """
        self._registry.clear()
        await self.discover_all()

        self._logger.info(f"リフレッシュ完了: {len(self._registry)} ツール")
        return len(self._registry)

    async def discover_skills_from_engine(self) -> int:
        """SkillEngine からスキルを発見してツールとして登録.

        ビルトインスキル・ユーザースキル・アプリ固有スキルを自動発見。

        Returns:
            登録されたスキル数
        """
        try:
            from pathlib import Path

            from agentflow.skills.loader import SkillLoader

            count = 0
            loader = SkillLoader()

            scan_dirs: list[tuple[str, Path]] = [
                ("ビルトイン", Path(__file__).parent.parent / "skills" / "builtin"),
                ("ユーザー", Path.home() / ".agentflow" / "skills"),
            ]

            # アプリ固有スキル（apps/*/skills）を追加
            repo_root = Path(__file__).parent.parent.parent
            apps_dir = repo_root / "apps"
            if apps_dir.exists():
                for app_dir in sorted(path for path in apps_dir.iterdir() if path.is_dir()):
                    app_skills_dir = app_dir / "skills"
                    if app_skills_dir.exists():
                        scan_dirs.append((f"アプリ:{app_dir.name}", app_skills_dir))

            for label, skill_dir in scan_dirs:
                if not skill_dir.exists():
                    continue
                skills = loader.load_directory(skill_dir, recursive=True)
                for skill in skills:
                    try:
                        tool_def = ToolDefinition.from_skill(skill)
                        self._registry.register(tool_def)
                        count += 1
                    except Exception as e:
                        self._logger.warning(
                            f"{label}スキル登録エラー {getattr(skill, 'name', '?')}: {e}"
                        )

            self._logger.debug(f"SkillEngine からスキル発見: {count}")
            return count

        except ImportError:
            self._logger.debug("SkillLoader が利用不可")
            return 0
        except Exception as e:
            self._logger.warning(f"スキル発見エラー: {e}")
            return 0


__all__ = [
    "ToolDiscoveryService",
]
