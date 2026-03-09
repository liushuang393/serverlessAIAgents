"""MCP サーバー管理モジュール.

messaging_hub 向けの MCP サーバーライフサイクル管理を提供。
設定ファイル方式と動的実行方式（npx/uvx）の2種類のインストールをサポート。

使用例:
    >>> manager = MCPManager(config_path)
    >>> servers = manager.list_servers()
    >>> manager.install_server(req)
    >>> manager.delete_server("server_name")
"""

from __future__ import annotations

import json
import logging
from pathlib import Path
from typing import Any

from pydantic import BaseModel, Field


_logger = logging.getLogger(__name__)


class MCPServerConfig(BaseModel):
    """MCP サーバー設定.

    Attributes:
        name: サーバー名（一意識別子）
        command: 起動コマンド（例: "npx", "uvx", "node"）
        args: コマンド引数
        env: 環境変数
        enabled: 有効フラグ
        description: 説明
        install_method: インストール方式 ("config" | "dynamic")
    """

    name: str = Field(..., min_length=1, max_length=100)
    command: str = Field(..., min_length=1)
    args: list[str] = Field(default_factory=list)
    env: dict[str, str] = Field(default_factory=dict)
    enabled: bool = Field(default=True)
    description: str = Field(default="")
    install_method: str = Field(default="config")


class MCPInstallRequest(BaseModel):
    """MCP サーバーインストールリクエスト.

    Attributes:
        name: サーバー名
        command: 起動コマンド
        args: コマンド引数
        env: 環境変数
        description: 説明
        install_method: インストール方式
    """

    name: str = Field(..., min_length=1, max_length=100)
    command: str = Field(..., min_length=1)
    args: list[str] = Field(default_factory=list)
    env: dict[str, str] = Field(default_factory=dict)
    description: str = Field(default="")
    install_method: str = Field(default="config")


class MCPManager:
    """MCP サーバー管理マネージャー.

    app_config.json 内の contracts.mcp セクションを読み書きし、
    MCP サーバー設定のライフサイクルを管理する。
    """

    def __init__(self, config_path: Path | None = None) -> None:
        """初期化.

        Args:
            config_path: app_config.json のパス
        """
        self._config_path = config_path or (Path(__file__).parent / "app_config.json")
        self._logger = logging.getLogger(__name__)
        self._ensure_mcp_section()

    def _read_config(self) -> dict[str, Any]:
        """app_config.json を読み込む."""
        if not self._config_path.exists():
            return {}
        text = self._config_path.read_text(encoding="utf-8")
        loaded = json.loads(text)
        if not isinstance(loaded, dict):
            return {}
        return loaded

    def _write_config(self, config: dict[str, Any]) -> None:
        """app_config.json を書き込む."""
        text = json.dumps(config, ensure_ascii=False, indent=2)
        self._config_path.write_text(text + "\n", encoding="utf-8")

    def _get_mcp_section(self) -> dict[str, Any]:
        """contracts.mcp セクションを取得."""
        config = self._read_config()
        contracts = config.get("contracts", {})
        return dict(contracts.get("mcp", self._default_mcp_section()))

    def _save_mcp_section(self, mcp: dict[str, Any]) -> None:
        """contracts.mcp セクションを保存."""
        config = self._read_config()
        config.setdefault("contracts", {})["mcp"] = mcp
        self._write_config(config)

    def _ensure_mcp_section(self) -> None:
        """contracts.mcp セクションが存在しない場合に初期化."""
        config = self._read_config()
        contracts = config.get("contracts", {})
        if "mcp" not in contracts:
            contracts["mcp"] = self._default_mcp_section()
            config["contracts"] = contracts
            self._write_config(config)
            self._logger.info("contracts.mcp セクションを初期化しました")

    def list_servers(self) -> list[dict[str, Any]]:
        """インストール済み MCP サーバー一覧を取得.

        Returns:
            サーバー設定リスト
        """
        mcp = self._get_mcp_section()
        return list(mcp.get("servers", []))

    def get_server(self, name: str) -> dict[str, Any] | None:
        """指定名の MCP サーバー設定を取得.

        Args:
            name: サーバー名

        Returns:
            サーバー設定、見つからない場合は None
        """
        for server in self.list_servers():
            if server.get("name") == name:
                return dict(server)
        return None

    def install_server(self, request: MCPInstallRequest) -> dict[str, Any]:
        """MCP サーバーをインストール（設定追加）.

        2種類のインストール方式をサポート:
        - config: 設定ファイルに永続的に登録
        - dynamic: npx/uvx 等の動的実行コマンドとして登録

        Args:
            request: インストールリクエスト

        Returns:
            追加されたサーバー設定
        """
        mcp = self._get_mcp_section()
        servers: list[dict[str, Any]] = list(mcp.get("servers", []))

        payload: dict[str, Any] = {
            "name": request.name,
            "command": request.command,
            "args": request.args,
            "env": request.env,
            "enabled": True,
            "description": request.description,
            "install_method": request.install_method,
        }

        # 既存サーバーの上書き
        replaced = False
        for idx, item in enumerate(servers):
            if item.get("name") == request.name:
                servers[idx] = payload
                replaced = True
                break

        if not replaced:
            servers.append(payload)

        mcp["servers"] = servers
        self._save_mcp_section(mcp)

        action = "更新" if replaced else "インストール"
        self._logger.info(
            "MCP サーバー%s: name=%s, method=%s",
            action,
            request.name,
            request.install_method,
        )
        return payload

    def delete_server(self, name: str) -> bool:
        """MCP サーバーを削除.

        Args:
            name: サーバー名

        Returns:
            削除成功したか
        """
        mcp = self._get_mcp_section()
        servers: list[dict[str, Any]] = list(mcp.get("servers", []))
        filtered = [s for s in servers if s.get("name") != name]

        if len(filtered) == len(servers):
            return False

        mcp["servers"] = filtered
        self._save_mcp_section(mcp)
        self._logger.info("MCP サーバー削除: name=%s", name)
        return True

    def enable_server(self, name: str) -> bool:
        """MCP サーバーを有効化."""
        return self._set_enabled(name, enabled=True)

    def disable_server(self, name: str) -> bool:
        """MCP サーバーを無効化."""
        return self._set_enabled(name, enabled=False)

    def _set_enabled(self, name: str, *, enabled: bool) -> bool:
        """サーバーの有効/無効を切り替え."""
        mcp = self._get_mcp_section()
        servers: list[dict[str, Any]] = list(mcp.get("servers", []))
        for server in servers:
            if server.get("name") == name:
                server["enabled"] = enabled
                mcp["servers"] = servers
                self._save_mcp_section(mcp)
                state = "有効化" if enabled else "無効化"
                self._logger.info("MCP サーバー%s: name=%s", state, name)
                return True
        return False

    def get_lazy_loading_config(self) -> dict[str, Any]:
        """懒加載設定を取得."""
        mcp = self._get_mcp_section()
        return dict(mcp.get("lazy_loading", self._default_lazy_loading()))

    def update_lazy_loading_config(
        self,
        patch: dict[str, Any],
    ) -> dict[str, Any]:
        """懒加載設定を部分更新.

        Args:
            patch: 更新するフィールド

        Returns:
            更新後の懒加載設定
        """
        mcp = self._get_mcp_section()
        lazy = dict(mcp.get("lazy_loading", self._default_lazy_loading()))
        lazy.update({k: v for k, v in patch.items() if v is not None})
        mcp["lazy_loading"] = lazy
        self._save_mcp_section(mcp)
        return lazy

    @staticmethod
    def _default_mcp_section() -> dict[str, Any]:
        """MCP セクションのデフォルト値."""
        return {
            "servers": [],
            "lazy_loading": MCPManager._default_lazy_loading(),
            "default_tools": [],
        }

    @staticmethod
    def _default_lazy_loading() -> dict[str, Any]:
        """懒加載のデフォルト値."""
        return {
            "enabled": True,
            "threshold": 10,
            "auto_load_on_call": True,
            "cache_session": True,
            "default_load_count": 3,
        }
