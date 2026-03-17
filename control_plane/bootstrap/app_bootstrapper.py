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
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from control_plane.bootstrap.capability_bundle import CapabilityBundle
    from control_plane.bootstrap.config_watcher import ConfigWatcher


logger = logging.getLogger(__name__)

# app_config.json の標準的な検索パス
_APP_CONFIG_FILENAME = "app_config.json"
_DEFAULT_APPS_DIR = "apps"
_SUPPORTED_CONFIG_ENCODINGS = ("utf-8", "shift_jis")

JsonObject = dict[str, object]


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
        from control_plane.bootstrap.capability_bundle import CapabilityBundle
        from control_plane.bootstrap.config_watcher import ConfigWatcher
        from control_plane.bootstrap.rag_builder import build_rag_engine
        from control_plane.bootstrap.skill_builder import build_skill_gateway

        bootstrapper = cls(app_name=app_name, platform_url=platform_url)

        # app_config.json を読み込む
        app_config = bootstrapper._load_app_config(apps_dir)
        contracts = bootstrapper._extract_contracts(app_config)

        # RAGEngine を構築
        rag_config = bootstrapper._extract_contract_section(contracts, "rag")
        rag_engine = await build_rag_engine(rag_config)

        # SkillGateway を構築
        skills_config = bootstrapper._extract_contract_section(contracts, "skills")
        skill_gateway = await build_skill_gateway(skills_config)

        llm_config = bootstrapper._extract_contract_section(contracts, "llm")

        # CapabilityBundle を生成
        bundle = CapabilityBundle(
            app_name=app_name,
            rag_engine=rag_engine,
            skill_gateway=skill_gateway,
            mcp_client=None,  # Phase 3+ で実装
            llm_contracts=llm_config,
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
            "AppCapabilityBootstrapper 構築完了: app=%s, rag=%s, skills=%s",
            app_name,
            "有効" if rag_engine is not None else "無効",
            "有効" if skill_gateway is not None else "無効",
        )

        return bundle, bootstrapper

    def _load_app_config(
        self,
        apps_dir: str | None = None,
    ) -> JsonObject | None:
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
            search_paths.append(Path(apps_dir) / self._app_name / _APP_CONFIG_FILENAME)

        # 2. カレントディレクトリ基準
        cwd = Path.cwd()
        search_paths.extend(
            [
                cwd / _DEFAULT_APPS_DIR / self._app_name / _APP_CONFIG_FILENAME,
                cwd / _APP_CONFIG_FILENAME,  # アプリ自身の ディレクトリで直接起動
            ]
        )

        # 3. このファイルからの相対パス（control_plane/bootstrap/ → ルート）
        pkg_root = Path(__file__).parent.parent.parent
        search_paths.append(pkg_root / _DEFAULT_APPS_DIR / self._app_name / _APP_CONFIG_FILENAME)

        for config_path in dict.fromkeys(search_paths):
            if config_path.is_file():
                data = self._read_app_config_file(config_path)
                if data is not None:
                    return data

        logger.warning(
            "app_config.json が見つかりません: app=%s",
            self._app_name,
        )
        return None

    def _read_app_config_file(self, config_path: Path) -> JsonObject | None:
        """app_config.json を対応文字コードで読み込む.

        Args:
            config_path: 読み込む設定ファイルパス

        Returns:
            ルートが JSON object の場合は設定辞書、それ以外は None
        """
        for encoding in _SUPPORTED_CONFIG_ENCODINGS:
            try:
                text = config_path.read_text(encoding=encoding)
            except UnicodeDecodeError:
                continue
            except OSError as exc:
                logger.warning(
                    "app_config.json 読み込み失敗 (%s): %s",
                    config_path,
                    exc,
                )
                return None

            try:
                payload: object = json.loads(text)
            except json.JSONDecodeError as exc:
                logger.warning(
                    "app_config.json パース失敗 (%s, encoding=%s): %s",
                    config_path,
                    encoding,
                    exc,
                )
                return None

            app_config = self._coerce_json_object(
                payload,
                context=f"app_config.json ({config_path}, encoding={encoding})",
            )
            if app_config is not None:
                logger.info(
                    "app_config.json 読み込み: %s (encoding=%s)",
                    config_path,
                    encoding,
                )
            return app_config

        logger.warning(
            "app_config.json の文字コード判定失敗 (%s): supported=%s",
            config_path,
            ", ".join(_SUPPORTED_CONFIG_ENCODINGS),
        )
        return None

    def _extract_contracts(self, app_config: JsonObject | None) -> JsonObject:
        """app_config から contracts セクションを安全に抽出する.

        Args:
            app_config: app_config.json のルート辞書

        Returns:
            contracts 辞書。未設定または不正時は空辞書。
        """
        if app_config is None:
            return {}

        contracts = self._coerce_json_object(
            app_config.get("contracts"),
            context=f"contracts section (app={self._app_name})",
            allow_none=True,
        )
        return contracts or {}

    def _extract_contract_section(
        self,
        contracts: JsonObject,
        section_name: str,
    ) -> JsonObject | None:
        """contracts 配下の設定セクションを安全に抽出する.

        Args:
            contracts: contracts 辞書
            section_name: 抽出対象セクション名

        Returns:
            セクション辞書。不正または未設定時は None。
        """
        return self._coerce_json_object(
            contracts.get(section_name),
            context=f"contracts.{section_name} (app={self._app_name})",
            allow_none=True,
        )

    def _coerce_json_object(
        self,
        payload: object,
        *,
        context: str,
        allow_none: bool = False,
    ) -> JsonObject | None:
        """任意オブジェクトを JSON object 相当の辞書へ安全に変換する.

        Args:
            payload: 検査対象オブジェクト
            context: ログ出力用の文脈情報
            allow_none: None を許容するか

        Returns:
            文字列キーを持つ辞書、または None
        """
        if payload is None:
            return None if allow_none else {}

        if not isinstance(payload, dict):
            logger.warning("JSON object 期待値不一致 (%s): type=%s", context, type(payload).__name__)
            return None

        normalized: JsonObject = {}
        for key, value in payload.items():
            if not isinstance(key, str):
                logger.warning("JSON object キー型不正 (%s): type=%s", context, type(key).__name__)
                return None
            normalized[key] = value
        return normalized

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
