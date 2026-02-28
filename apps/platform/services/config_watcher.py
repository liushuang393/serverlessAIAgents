"""Config Watcher Service — app_config.json の変更を監視してホットリロード.

watchfiles ライブラリを使って apps/*/app_config.json の変更を検知し、
AppDiscoveryService に自動再ロードを要求するバックグラウンドサービス。

特徴:
- 変更・作成・削除いずれも検知（削除は _registry から除去）
- app_config.json 以外の変更は完全に無視（watch_filter）
- asyncio.CancelledError でクリーンに停止
- watchfiles が未インストールの場合は WARNING のみで継続起動

使用例:
    watcher = ConfigWatcherService(discovery)
    task = asyncio.create_task(watcher.watch())
    # サーバー終了時:
    task.cancel()
    await asyncio.gather(task, return_exceptions=True)
"""

from __future__ import annotations

import asyncio
import logging
from pathlib import Path
from typing import TYPE_CHECKING

if TYPE_CHECKING:
    from apps.platform.services.app_discovery import AppDiscoveryService

_logger = logging.getLogger(__name__)
_CONFIG_FILENAME = "app_config.json"


class ConfigWatcherService:
    """app_config.json の変更を監視してホットリロードするバックグラウンドサービス.

    Attributes:
        _discovery: 再ロード通知先の AppDiscoveryService
        _running: 監視稼働中フラグ（外部から確認可能）
    """

    def __init__(self, discovery: AppDiscoveryService) -> None:
        """初期化.

        Args:
            discovery: 変更検知時に _load_one() を呼ぶ AppDiscoveryService
        """
        self._discovery = discovery
        self._running = False

    @property
    def is_running(self) -> bool:
        """監視が稼働中かどうかを返す."""
        return self._running

    async def watch(self) -> None:
        """app_config.json の変更を監視し、変更時に再ロードする.

        watchfiles の awatch() を使って非同期監視を行う。
        変更されたファイルのみを個別に再ロードするため、
        全 App の再スキャンより低コストで実行できる。

        停止方法: asyncio.Task.cancel() で CancelledError を発生させる。
        watchfiles が未インストールの場合は WARNING を記録して即座にリターンする。

        Raises:
            asyncio.CancelledError: タスクがキャンセルされた場合（正常停止）
        """
        try:
            from watchfiles import Change, awatch  # noqa: PLC0415 — 任意依存のため遅延インポート
        except ImportError:
            _logger.warning(
                "watchfiles が未インストールのためホットリロードが無効です。"
                "`pip install watchfiles` でインストールすると有効になります。",
            )
            return

        apps_dir = self._discovery.apps_dir
        if not apps_dir.is_dir():
            _logger.warning("apps ディレクトリが存在しないため監視を開始できません: %s", apps_dir)
            return

        self._running = True
        _logger.info("ConfigWatcher 開始: %s を監視中", apps_dir)

        try:
            async for changes in awatch(
                apps_dir,
                # app_config.json のみを監視対象に絞る
                watch_filter=lambda _change, path: Path(path).name == _CONFIG_FILENAME,
            ):
                for change_type, path_str in changes:
                    config_path = Path(path_str)
                    if config_path.name != _CONFIG_FILENAME:
                        continue

                    _logger.info(
                        "app_config.json 変更検知 [%s]: %s",
                        change_type.name if isinstance(change_type, Change) else change_type,
                        config_path,
                    )
                    # 削除の場合は registry から除去、それ以外は再ロード
                    if change_type == Change.deleted:
                        self._handle_deleted(config_path)
                    else:
                        self._discovery._load_one(config_path)

        except asyncio.CancelledError:
            _logger.info("ConfigWatcher 停止（キャンセル受信）")
            raise
        finally:
            self._running = False

    def _handle_deleted(self, config_path: Path) -> None:
        """削除された app_config.json に対応する App を registry から除去.

        Args:
            config_path: 削除された app_config.json のパス
        """
        dir_name = config_path.parent.name
        # config_paths から dir_name に対応する App 名を逆引き
        target_name: str | None = None
        for app_name, path in list(self._discovery._config_paths.items()):
            if path.parent.name == dir_name:
                target_name = app_name
                break

        if target_name is not None:
            self._discovery._registry.pop(target_name, None)
            self._discovery._config_paths.pop(target_name, None)
            self._discovery._module_warnings.pop(dir_name, None)
            _logger.warning(
                "App '%s' を registry から除去しました (app_config.json 削除検知)",
                target_name,
            )
        else:
            _logger.debug("削除検知: 対応する App が registry に存在しません (%s)", dir_name)

