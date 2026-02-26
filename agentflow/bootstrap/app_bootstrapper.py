"""AppCapabilityBootstrapper - app_config.json を読み込み CapabilityBundle を生成.

contracts.* を書くだけで RAG/Skills/MCP が自動接続される Bootstrapper。
ConfigWatcher も起動し、Platform からのホットリロードを受け付ける。

使用例:
    >>> bundle, bootstrapper = await AppCapabilityBootstrapper.build(
    ...     app_name="faq_system",
    ...     platform_url=os.environ.get("PLATFORM_URL"),
    ... )
    >>> app.state.capability_bundle = bundle
    >>> # ... app shutdown 時 ...
    >>> await bootstrapper.shutdown()
"""

from __future__ import annotations

import asyncio
import contextlib
import json
import logging
from pathlib import Path
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from agentflow.bootstrap.capability_bundle import CapabilityBundle
    from agentflow.bootstrap.config_watcher import ConfigWatcher


logger = logging.getLogger(__name__)

# app_config.json の標準的な検索パス
_APP_CONFIG_FILENAME = "app_config.json"
_DEFAULT_APPS_DIR = "apps"


class AppCapabilityBootstrapper:
    """アプリ能力のブートストラッパー.

    app_config.json から contracts.* を読み込み、対応するサービスを
    自動接続して CapabilityBundle を生成する。
    ConfigWatcher を起動して Platform からの動的設定変更にも対応。

    Attributes:
        _app_name: アプリ識別子
        _platform_url: Platform URL（None なら ConfigWatcher 無効）
        _watcher: ConfigWatcher インスタンス
        _watcher_task: バックグラウンドタスク
    """

    def __init__(
        self,
        app_name: str,
        platform_url: str | None = None,
    ) -> None:
        """初期化.

        Args:
            app_name: アプリ識別子（snake_case）
            platform_url: Platform URL（None なら ConfigWatcher 無効）
        """
        self._app_name = app_name
        self._platform_url = platform_url
        self._watcher_task: asyncio.Task[None] | None = None
        self._watcher: ConfigWatcher | None = None

    @classmethod
    async def build(
        cls,
        app_name: str,
        platform_url: str | None = None,
        apps_dir: str | None = None,
    ) -> tuple[CapabilityBundle, AppCapabilityBootstrapper]:
        """CapabilityBundle を構築してBootstrapperを返す.

        Args:
            app_name: アプリ識別子（snake_case）
            platform_url: Platform URL（None なら ConfigWatcher 無効）
            apps_dir: apps ディレクトリパス（省略時は自動検索）

        Returns:
            (CapabilityBundle, AppCapabilityBootstrapper) タプル
        """
        from agentflow.bootstrap.capability_bundle import CapabilityBundle
        from agentflow.bootstrap.config_watcher import ConfigWatcher
        from agentflow.bootstrap.rag_builder import build_rag_engine
        from agentflow.bootstrap.skill_builder import build_skill_gateway

        bootstrapper = cls(app_name=app_name, platform_url=platform_url)

        # app_config.json を読み込む
        app_config = bootstrapper._load_app_config(apps_dir)
        contracts = app_config.get("contracts", {}) if app_config else {}

        # RAGEngine を構築
        rag_config: dict[str, Any] | None = contracts.get("rag")
        rag_engine = await build_rag_engine(rag_config)

        # SkillGateway を構築
        skills_config: dict[str, Any] | None = contracts.get("skills")
        skill_gateway = await build_skill_gateway(skills_config)

        # CapabilityBundle を生成
        bundle = CapabilityBundle(
            app_name=app_name,
            rag_engine=rag_engine,
            skill_gateway=skill_gateway,
            mcp_client=None,  # Phase 3+ で実装
        )

        # ConfigWatcher を起動（platform_url が設定されている場合）
        if platform_url:
            watcher = ConfigWatcher(app_name=app_name, platform_url=platform_url)
            bootstrapper._watcher_task = asyncio.create_task(
                watcher.watch(bundle),
                name=f"config_watcher_{app_name}",
            )
            bootstrapper._watcher = watcher
            logger.info(
                "ConfigWatcher 起動: app=%s, platform_url=%s",
                app_name,
                platform_url,
            )
        else:
            logger.info(
                "ConfigWatcher 無効（platform_url 未設定）: app=%s",
                app_name,
            )

        logger.info(
            "AppCapabilityBootstrapper 構築完了: app=%s, "
            "rag=%s, skills=%s",
            app_name,
            "有効" if rag_engine is not None else "無効",
            "有効" if skill_gateway is not None else "無効",
        )

        return bundle, bootstrapper

    def _load_app_config(
        self,
        apps_dir: str | None = None,
    ) -> dict[str, Any] | None:
        """app_config.json を探して読み込む.

        Args:
            apps_dir: apps ディレクトリパス

        Returns:
            設定辞書、見つからない場合は None
        """
        # 検索候補パスを構築
        search_paths: list[Path] = []

        # 1. 明示指定パス
        if apps_dir:
            search_paths.append(
                Path(apps_dir) / self._app_name / _APP_CONFIG_FILENAME
            )

        # 2. カレントディレクトリ基準
        cwd = Path.cwd()
        search_paths.extend([
            cwd / _DEFAULT_APPS_DIR / self._app_name / _APP_CONFIG_FILENAME,
            cwd / _APP_CONFIG_FILENAME,  # アプリ自身の ディレクトリで直接起動
        ])

        # 3. このファイルからの相対パス（agentflow/bootstrap/ → ルート）
        pkg_root = Path(__file__).parent.parent.parent
        search_paths.append(
            pkg_root / _DEFAULT_APPS_DIR / self._app_name / _APP_CONFIG_FILENAME
        )

        for config_path in search_paths:
            if config_path.is_file():
                try:
                    text = config_path.read_text(encoding="utf-8")
                    data: dict[str, Any] = json.loads(text)
                    logger.info(
                        "app_config.json 読み込み: %s",
                        config_path,
                    )
                    return data
                except Exception as exc:
                    logger.warning(
                        "app_config.json パース失敗 (%s): %s",
                        config_path,
                        exc,
                    )

        logger.warning(
            "app_config.json が見つかりません: app=%s",
            self._app_name,
        )
        return None

    async def shutdown(self) -> None:
        """ConfigWatcher を停止して Bootstrapper をシャットダウン.

        app shutdown 時に呼び出す。
        """
        if self._watcher_task is not None and not self._watcher_task.done():
            if self._watcher is not None:
                await self._watcher.stop()
            self._watcher_task.cancel()
            with contextlib.suppress(TimeoutError, asyncio.CancelledError):
                await asyncio.wait_for(self._watcher_task, timeout=5.0)
            logger.info("ConfigWatcher 停止完了: %s", self._app_name)


__all__ = ["AppCapabilityBootstrapper"]
