# -*- coding: utf-8 -*-
"""App Discovery Service — apps/*/app_config.json のスキャンと登録.

apps ディレクトリを走査し、各 App の app_config.json を検証・登録する。
Platform の App 管理 API が依存するコアサービス。

使用例:
    >>> service = AppDiscoveryService()
    >>> await service.scan()
    >>> apps = service.list_apps()
"""

from __future__ import annotations

import json
import logging
from pathlib import Path
from typing import Any

from apps.platform.schemas.app_config_schemas import AppConfig


_logger = logging.getLogger(__name__)

# リポジトリルートからの apps ディレクトリ相対パス
_DEFAULT_APPS_DIR = "apps"
_CONFIG_FILENAME = "app_config.json"


class AppDiscoveryService:
    """App マニフェストの検出・登録・検索サービス.

    Attributes:
        _apps_dir: apps ディレクトリの絶対パス
        _registry: App 名 → AppConfig のマッピング
        _config_paths: App 名 → app_config.json の絶対パス
        _errors: スキャン時に発生した検証エラー
    """

    def __init__(self, apps_dir: Path | None = None) -> None:
        """初期化.

        Args:
            apps_dir: apps ディレクトリの絶対パス。
                      省略時はカレントディレクトリ配下の apps/ を使用。
        """
        if apps_dir is None:
            apps_dir = Path.cwd() / _DEFAULT_APPS_DIR
        self._apps_dir = apps_dir
        self._registry: dict[str, AppConfig] = {}
        self._config_paths: dict[str, Path] = {}
        self._errors: dict[str, str] = {}

    # ------------------------------------------------------------------
    # 公開 API
    # ------------------------------------------------------------------

    async def scan(self) -> int:
        """apps ディレクトリをスキャンし、全 app_config.json を検証・登録.

        Returns:
            正常に登録された App 数
        """
        self._registry.clear()
        self._config_paths.clear()
        self._errors.clear()

        if not self._apps_dir.is_dir():
            _logger.warning("Apps ディレクトリが存在しません: %s", self._apps_dir)
            return 0

        config_paths = sorted(self._apps_dir.glob(f"*/{_CONFIG_FILENAME}"))
        for config_path in config_paths:
            self._load_one(config_path)

        _logger.info(
            "AppDiscovery スキャン完了: %d 件登録, %d 件エラー",
            len(self._registry),
            len(self._errors),
        )
        return len(self._registry)

    def register(self, config: AppConfig, *, config_path: Path | None = None) -> None:
        """手動で AppConfig を登録.

        Args:
            config: 登録する AppConfig
            config_path: app_config.json のパス（任意）
        """
        self._registry[config.name] = config
        if config_path is not None:
            self._config_paths[config.name] = config_path
        _logger.info("App を登録: %s", config.name)

    def get_app(self, name: str) -> AppConfig | None:
        """App 名で検索.

        Args:
            name: App 識別子（snake_case）

        Returns:
            見つかった AppConfig、なければ None
        """
        return self._registry.get(name)

    def get_config_path(self, name: str) -> Path | None:
        """App の app_config.json パスを取得.

        Args:
            name: App 識別子

        Returns:
            設定ファイルパス
        """
        return self._config_paths.get(name)

    def get_raw_config(self, name: str) -> dict[str, Any] | None:
        """App の生 app_config.json を取得.

        Args:
            name: App 識別子

        Returns:
            生JSON辞書
        """
        config_path = self.get_config_path(name)
        if config_path is None or not config_path.is_file():
            return None

        try:
            return json.loads(config_path.read_text("utf-8"))
        except json.JSONDecodeError as exc:
            _logger.warning("JSON パースエラー (%s): %s", config_path, exc)
            return None

    def list_apps(self) -> list[AppConfig]:
        """登録済み全 App を一覧取得.

        Returns:
            AppConfig のリスト（名前順）
        """
        return sorted(self._registry.values(), key=lambda c: c.name)

    def list_errors(self) -> dict[str, str]:
        """スキャン時の検証エラーを取得.

        Returns:
            App ディレクトリ名 → エラーメッセージ
        """
        return dict(self._errors)

    def summary(self) -> dict[str, Any]:
        """全 App の概要統計を返す.

        Returns:
            統計情報辞書
        """
        apps = self.list_apps()
        total_agents = sum(len(a.agents) for a in apps)
        return {
            "total_apps": len(apps),
            "total_agents": total_agents,
            "apps": [
                {
                    "name": a.name,
                    "display_name": a.display_name,
                    "icon": a.icon,
                    "agents_count": len(a.agents),
                    "port": a.ports.api,
                    "config_path": self._to_relative(self._config_paths.get(a.name)),
                    "contracts": {
                        "auth": a.contracts.auth.enabled,
                        "rag": a.contracts.rag.enabled,
                        "skills": bool(a.contracts.skills.default_skills),
                        "release_targets": len(a.contracts.release.targets),
                    },
                }
                for a in apps
            ],
            "errors": self._errors,
        }

    def update_app_config(self, name: str, patch: dict[str, Any]) -> AppConfig:
        """app_config.json を部分更新して再検証する.

        Args:
            name: App 識別子
            patch: 上書きパッチ（deep merge）

        Returns:
            更新後の AppConfig

        Raises:
            KeyError: App が存在しない場合
            ValueError: パッチが不正な場合
        """
        if not patch:
            config = self._registry.get(name)
            if config is None:
                msg = f"App not found: {name}"
                raise KeyError(msg)
            return config

        config_path = self._config_paths.get(name)
        if config_path is None:
            msg = f"Config path not found: {name}"
            raise KeyError(msg)

        if "name" in patch and patch["name"] != name:
            msg = "name フィールドの変更は許可されていません"
            raise ValueError(msg)

        raw = self.get_raw_config(name)
        if raw is None:
            msg = f"Config file is invalid or unreadable: {name}"
            raise ValueError(msg)

        merged = self._deep_merge(raw, patch)
        validated = AppConfig.model_validate(merged)

        if validated.name != name:
            msg = "name フィールドの変更は許可されていません"
            raise ValueError(msg)

        config_path.write_text(
            json.dumps(merged, ensure_ascii=False, indent=2) + "\n",
            encoding="utf-8",
        )

        self._registry[name] = validated
        self._errors.pop(config_path.parent.name, None)
        _logger.info("app_config 更新: %s", config_path)

        return validated

    # ------------------------------------------------------------------
    # 内部メソッド
    # ------------------------------------------------------------------

    def _load_one(self, config_path: Path) -> None:
        """1 つの app_config.json を読み込み・検証・登録."""
        dir_name = config_path.parent.name
        try:
            raw = json.loads(config_path.read_text("utf-8"))
            config = AppConfig.model_validate(raw)
            self._registry[config.name] = config
            self._config_paths[config.name] = config_path
        except json.JSONDecodeError as exc:
            self._errors[dir_name] = f"JSON パースエラー: {exc}"
            _logger.warning("JSON パースエラー (%s): %s", dir_name, exc)
        except Exception as exc:  # noqa: BLE001
            self._errors[dir_name] = f"検証エラー: {exc}"
            _logger.warning("検証エラー (%s): %s", dir_name, exc)

    @staticmethod
    def _deep_merge(base: dict[str, Any], patch: dict[str, Any]) -> dict[str, Any]:
        """辞書を deep merge する（patch 優先）."""
        merged = dict(base)
        for key, value in patch.items():
            if isinstance(value, dict) and isinstance(merged.get(key), dict):
                merged[key] = AppDiscoveryService._deep_merge(merged[key], value)
            else:
                merged[key] = value
        return merged

    @staticmethod
    def _to_relative(path: Path | None) -> str | None:
        """表示用に相対パスへ変換."""
        if path is None:
            return None
        try:
            return str(path.relative_to(Path.cwd()))
        except ValueError:
            return str(path)
