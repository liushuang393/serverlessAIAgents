"""Stable runtime bootstrap API for apps."""

from __future__ import annotations

import asyncio
import contextlib
import logging
from pathlib import Path

from kernel.runtime.capability_bundle import CapabilityBundle
from kernel.runtime.config_watcher import ConfigWatcher
from kernel.runtime.rag_builder import build_rag_engine
from kernel.runtime.skill_builder import build_skill_gateway
from shared.config.manifest import load_app_manifest_dict_text


logger = logging.getLogger(__name__)

_APP_CONFIG_FILENAME = "app_config.json"
_DEFAULT_APPS_DIR = "apps"
_SUPPORTED_CONFIG_ENCODINGS = ("utf-8", "shift_jis")

JsonObject = dict[str, object]


class AppCapabilityBootstrapper:
    """Canonical runtime bootstrapper for app capabilities."""

    def __init__(self, app_name: str, platform_url: str | None = None) -> None:
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
        """Build a capability bundle and its lifecycle owner."""
        bootstrapper = cls(app_name=app_name, platform_url=platform_url)
        app_config = bootstrapper._load_app_config(apps_dir)
        contracts = bootstrapper._extract_contracts(app_config)

        rag_config = bootstrapper._extract_contract_section(contracts, "rag")
        skills_config = bootstrapper._extract_contract_section(contracts, "skills")
        llm_config = bootstrapper._extract_contract_section(contracts, "llm")

        rag_engine = await build_rag_engine(rag_config)
        skill_gateway = await build_skill_gateway(skills_config)

        bundle = CapabilityBundle(
            app_name=app_name,
            rag_engine=rag_engine,
            skill_gateway=skill_gateway,
            mcp_client=None,
            llm_contracts=llm_config,
        )

        if platform_url:
            watcher = ConfigWatcher(app_name=app_name, platform_url=platform_url)
            bootstrapper._watcher_task = asyncio.create_task(
                watcher.watch(bundle),
                name=f"config_watcher_{app_name}",
            )
            bootstrapper._watcher = watcher
            logger.info("Runtime watcher started: app=%s platform_url=%s", app_name, platform_url)
        else:
            logger.info("Runtime watcher disabled: app=%s", app_name)

        logger.info(
            "App capability bootstrap ready: app=%s rag=%s skills=%s",
            app_name,
            "enabled" if rag_engine is not None else "disabled",
            "enabled" if skill_gateway is not None else "disabled",
        )
        return bundle, bootstrapper

    def _load_app_config(self, apps_dir: str | None = None) -> JsonObject | None:
        search_paths: list[Path] = []
        if apps_dir:
            search_paths.append(Path(apps_dir) / self._app_name / _APP_CONFIG_FILENAME)

        cwd = Path.cwd()
        search_paths.extend(
            [
                cwd / _DEFAULT_APPS_DIR / self._app_name / _APP_CONFIG_FILENAME,
                cwd / _APP_CONFIG_FILENAME,
            ]
        )
        pkg_root = Path(__file__).resolve().parents[2]
        search_paths.append(pkg_root / _DEFAULT_APPS_DIR / self._app_name / _APP_CONFIG_FILENAME)

        for config_path in dict.fromkeys(search_paths):
            if config_path.is_file():
                return self._read_app_config_file(config_path)

        logger.warning("app_config.json not found for app=%s", self._app_name)
        return None

    def _read_app_config_file(self, config_path: Path) -> JsonObject | None:
        for encoding in _SUPPORTED_CONFIG_ENCODINGS:
            try:
                text = config_path.read_text(encoding=encoding)
            except UnicodeDecodeError:
                continue
            except OSError as exc:
                logger.warning("failed reading app_config.json (%s): %s", config_path, exc)
                return None

            try:
                app_config = load_app_manifest_dict_text(text, manifest_path=config_path)
            except ValueError as exc:
                logger.warning(
                    "canonical manifest load failed (%s, encoding=%s): %s",
                    config_path,
                    encoding,
                    exc,
                )
                return None

            logger.info("Loaded app manifest: %s (encoding=%s)", config_path, encoding)
            return self._coerce_json_object(
                app_config,
                context=f"canonical app_config.json ({config_path}, encoding={encoding})",
            )

        logger.warning("Unsupported app manifest encoding: %s", config_path)
        return None

    def _extract_contracts(self, app_config: JsonObject | None) -> JsonObject:
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
        if payload is None:
            return None if allow_none else {}
        if not isinstance(payload, dict):
            logger.warning("JSON object expected (%s): type=%s", context, type(payload).__name__)
            return None
        normalized: JsonObject = {}
        for key, value in payload.items():
            if not isinstance(key, str):
                logger.warning("JSON object key must be str (%s): type=%s", context, type(key).__name__)
                return None
            normalized[key] = value
        return normalized

    async def shutdown(self) -> None:
        """Stop the watcher and release runtime resources."""
        if self._watcher_task is not None and not self._watcher_task.done():
            if self._watcher is not None:
                await self._watcher.stop()
            self._watcher_task.cancel()
            with contextlib.suppress(TimeoutError, asyncio.CancelledError):
                await asyncio.wait_for(self._watcher_task, timeout=5.0)
            logger.info("Runtime watcher stopped: %s", self._app_name)


__all__ = ["AppCapabilityBootstrapper"]
