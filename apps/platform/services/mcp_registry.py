"""MCP Registry Service.

`.agentflow/protocols/mcp.yaml` を読み書きし、Platform から
MCP サーバー設定を管理できるようにする。
"""

from __future__ import annotations

from pathlib import Path
from typing import Any

import yaml
from apps.platform.schemas.provisioning_schemas import (
    MCPLazyLoadingPatchRequest,
    MCPServerUpsertRequest,
)


class MCPRegistryService:
    """MCP 設定管理サービス."""

    def __init__(self, config_path: Path | None = None) -> None:
        """初期化.

        Args:
            config_path: MCP 設定ファイルパス
        """
        self._config_path = config_path or (Path.cwd() / ".agentflow/protocols/mcp.yaml")
        self._ensure_file_exists()

    @property
    def config_path(self) -> Path:
        """設定ファイルパス."""
        return self._config_path

    def get_config(self) -> dict[str, Any]:
        """MCP 設定全体を取得."""
        raw = self._read_raw()
        return {
            "servers": raw.get("servers", []),
            "lazy_loading": raw.get("lazy_loading", self._default_lazy_loading()),
        }

    def list_servers(self) -> list[dict[str, Any]]:
        """サーバー一覧を取得."""
        config = self.get_config()
        return list(config["servers"])

    def upsert_server(self, server: MCPServerUpsertRequest) -> dict[str, Any]:
        """サーバーを追加または更新."""
        raw = self._read_raw()
        servers = list(raw.get("servers", []))

        payload = {
            "name": server.name,
            "command": server.command,
            "args": server.args,
            "env": server.env,
            "enabled": server.enabled,
            "description": server.description,
        }

        replaced = False
        for idx, item in enumerate(servers):
            if item.get("name") == server.name:
                servers[idx] = payload
                replaced = True
                break

        if not replaced:
            servers.append(payload)

        raw["servers"] = servers
        raw.setdefault("lazy_loading", self._default_lazy_loading())
        self._write_raw(raw)
        return payload

    def delete_server(self, name: str) -> bool:
        """サーバーを削除."""
        raw = self._read_raw()
        servers = list(raw.get("servers", []))
        filtered = [item for item in servers if item.get("name") != name]

        if len(filtered) == len(servers):
            return False

        raw["servers"] = filtered
        raw.setdefault("lazy_loading", self._default_lazy_loading())
        self._write_raw(raw)
        return True

    def patch_lazy_loading(self, patch: MCPLazyLoadingPatchRequest) -> dict[str, Any]:
        """lazy_loading 設定を部分更新."""
        raw = self._read_raw()
        lazy = dict(raw.get("lazy_loading", self._default_lazy_loading()))

        patch_data = patch.model_dump(exclude_none=True)
        lazy.update(patch_data)

        raw.setdefault("servers", [])
        raw["lazy_loading"] = lazy
        self._write_raw(raw)
        return lazy

    def _ensure_file_exists(self) -> None:
        """設定ファイル未作成時に初期化."""
        if self._config_path.exists():
            return

        self._config_path.parent.mkdir(parents=True, exist_ok=True)
        raw = {
            "servers": [],
            "lazy_loading": self._default_lazy_loading(),
        }
        self._write_raw(raw)

    def _read_raw(self) -> dict[str, Any]:
        """YAML を読み込む."""
        if not self._config_path.exists():
            return {
                "servers": [],
                "lazy_loading": self._default_lazy_loading(),
            }

        loaded = yaml.safe_load(self._config_path.read_text("utf-8"))
        if not isinstance(loaded, dict):
            return {
                "servers": [],
                "lazy_loading": self._default_lazy_loading(),
            }

        loaded.setdefault("servers", [])
        loaded.setdefault("lazy_loading", self._default_lazy_loading())
        return loaded

    def _write_raw(self, raw: dict[str, Any]) -> None:
        """YAML を書き込む."""
        text = yaml.safe_dump(
            raw,
            allow_unicode=True,
            sort_keys=False,
        )
        self._config_path.write_text(text, encoding="utf-8")

    @staticmethod
    def _default_lazy_loading() -> dict[str, Any]:
        """lazy_loading のデフォルト値."""
        return {
            "enabled": True,
            "threshold": 10,
            "auto_load_on_call": True,
            "cache_session": True,
        }
