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

from copy import deepcopy
import json
import logging
from pathlib import Path
from typing import Any

from apps.platform.schemas.app_config_schemas import (
    AppConfig,
    BlueprintConfig,
    ContractsConfig,
    VisibilityConfig,
)
from apps.platform.services.agent_taxonomy import AgentTaxonomyService


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
        self._taxonomy = AgentTaxonomyService()

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
        product_line_counts: dict[str, int] = {}
        for app in apps:
            key = app.product_line
            product_line_counts[key] = product_line_counts.get(key, 0) + 1
        return {
            "total_apps": len(apps),
            "total_agents": total_agents,
            "product_line_counts": product_line_counts,
            "apps": [
                {
                    "name": a.name,
                    "display_name": a.display_name,
                    "icon": a.icon,
                    "business_base": self._resolve_business_base(a),
                    "product_line": a.product_line,
                    "surface_profile": a.surface_profile,
                    "audit_profile": a.audit_profile,
                    "security_mode": a.security_mode,
                    "agent_count": len(a.agents),
                    "has_api": a.ports.api is not None,
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

    def migrate_manifests(self, *, dry_run: bool = True) -> dict[str, Any]:
        """全 app_config.json を標準契約へ移行する.

        Args:
            dry_run: True の場合はファイルを書き換えない

        Returns:
            移行レポート
        """
        results: list[dict[str, Any]] = []
        changed = 0
        total = len(self._config_paths)

        for app_name in sorted(self._config_paths.keys()):
            config_path = self._config_paths[app_name]
            raw = self.get_raw_config(app_name)
            if raw is None:
                continue

            migrated, updated_fields = self._migrate_one(raw)
            if not updated_fields:
                continue

            changed += 1
            results.append(
                {
                    "name": app_name,
                    "updated_fields": updated_fields,
                },
            )

            if dry_run:
                continue

            config_path.write_text(
                json.dumps(migrated, ensure_ascii=False, indent=2) + "\n",
                encoding="utf-8",
            )
            validated = AppConfig.model_validate(migrated)
            self._registry[validated.name] = validated
            self._errors.pop(config_path.parent.name, None)

        return {
            "total": total,
            "changed": changed,
            "unchanged": total - changed,
            "apps": results,
            "dry_run": dry_run,
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

    def _migrate_one(self, raw: dict[str, Any]) -> tuple[dict[str, Any], list[str]]:
        """単一 manifest を標準契約へ移行."""
        manifest = deepcopy(raw)
        updated_fields: list[str] = []

        contracts_defaults = ContractsConfig().model_dump()
        inferred_rag = self._infer_contract_rag(manifest)
        contracts = manifest.get("contracts")
        if not isinstance(contracts, dict):
            contracts = deepcopy(contracts_defaults)
            contracts["rag"] = inferred_rag
            manifest["contracts"] = contracts
            self._mark_updated(updated_fields, "contracts")
        else:
            for key, default_value in contracts_defaults.items():
                if not isinstance(contracts.get(key), dict):
                    contracts[key] = deepcopy(default_value)
                    self._mark_updated(updated_fields, f"contracts.{key}")

            rag_contract = contracts.get("rag", {})
            if not isinstance(rag_contract, dict):
                contracts["rag"] = inferred_rag
                self._mark_updated(updated_fields, "contracts.rag")
            else:
                for key, value in inferred_rag.items():
                    if key not in rag_contract:
                        rag_contract[key] = deepcopy(value)
                        self._mark_updated(updated_fields, f"contracts.rag.{key}")

        blueprint_defaults = BlueprintConfig().model_dump()
        engine_pattern = self._infer_engine_pattern(manifest)
        flow_pattern = self._infer_flow_pattern(manifest)

        blueprint = manifest.get("blueprint")
        if not isinstance(blueprint, dict):
            blueprint = deepcopy(blueprint_defaults)
            blueprint["engine_pattern"] = engine_pattern
            blueprint["flow_pattern"] = flow_pattern
            manifest["blueprint"] = blueprint
            self._mark_updated(updated_fields, "blueprint")
        else:
            for key, default_value in blueprint_defaults.items():
                if key not in blueprint:
                    blueprint[key] = deepcopy(default_value)
                    self._mark_updated(updated_fields, f"blueprint.{key}")
            if (
                "engine_pattern" not in blueprint
                or not str(blueprint.get("engine_pattern", "")).strip()
            ):
                blueprint["engine_pattern"] = engine_pattern
                self._mark_updated(updated_fields, "blueprint.engine_pattern")
            if "flow_pattern" not in blueprint and flow_pattern is not None:
                blueprint["flow_pattern"] = flow_pattern
                self._mark_updated(updated_fields, "blueprint.flow_pattern")

        app_business_base = self._taxonomy.normalize_business_base(
            manifest.get("business_base"),
        )
        inferred_business_base = self._taxonomy.infer_app_business_base(
            app_name=str(manifest.get("name", "")),
            tags=manifest.get("tags", []) if isinstance(manifest.get("tags"), list) else [],
            contracts_rag_enabled=bool(
                isinstance(manifest.get("contracts"), dict)
                and isinstance(manifest["contracts"].get("rag"), dict)
                and manifest["contracts"]["rag"].get("enabled")
            ),
            agent_capabilities=self._iter_agent_capabilities(manifest.get("agents")),
        )
        if app_business_base is None:
            manifest["business_base"] = inferred_business_base
            self._mark_updated(updated_fields, "business_base")
            app_business_base = inferred_business_base
        elif app_business_base != manifest.get("business_base"):
            manifest["business_base"] = app_business_base
            self._mark_updated(updated_fields, "business_base")

        self._migrate_agent_taxonomy(
            manifest=manifest,
            updated_fields=updated_fields,
            app_business_base=app_business_base or inferred_business_base,
            engine_pattern=engine_pattern,
        )

        visibility_defaults = VisibilityConfig().model_dump()
        visibility = manifest.get("visibility")
        if not isinstance(visibility, dict):
            manifest["visibility"] = deepcopy(visibility_defaults)
            self._mark_updated(updated_fields, "visibility")
        else:
            for key, default_value in visibility_defaults.items():
                if key not in visibility:
                    visibility[key] = deepcopy(default_value)
                    self._mark_updated(updated_fields, f"visibility.{key}")

        inferred_product_line = self._infer_product_line(manifest)
        product_line = manifest.get("product_line")
        if not isinstance(product_line, str) or not product_line.strip():
            manifest["product_line"] = inferred_product_line
            self._mark_updated(updated_fields, "product_line")
        else:
            normalized = product_line.strip().lower()
            if normalized not in {"migration", "faq", "assistant", "framework"}:
                manifest["product_line"] = inferred_product_line
                self._mark_updated(updated_fields, "product_line")
            elif normalized != product_line:
                manifest["product_line"] = normalized
                self._mark_updated(updated_fields, "product_line")

        inferred_surface_profile = self._infer_surface_profile(
            manifest,
            product_line=str(manifest.get("product_line") or "framework"),
        )
        if not isinstance(manifest.get("surface_profile"), str) or not str(
            manifest.get("surface_profile"),
        ).strip():
            manifest["surface_profile"] = inferred_surface_profile
            self._mark_updated(updated_fields, "surface_profile")

        inferred_audit_profile = self._infer_audit_profile(
            product_line=str(manifest.get("product_line") or "framework"),
        )
        if not isinstance(manifest.get("audit_profile"), str) or not str(
            manifest.get("audit_profile"),
        ).strip():
            manifest["audit_profile"] = inferred_audit_profile
            self._mark_updated(updated_fields, "audit_profile")

        plugin_bindings = manifest.get("plugin_bindings")
        if not isinstance(plugin_bindings, list):
            manifest["plugin_bindings"] = []
            self._mark_updated(updated_fields, "plugin_bindings")

        if (
            str(manifest.get("product_line") or "").strip().lower() == "assistant"
            and not str(manifest.get("security_mode") or "").strip()
        ):
            manifest["security_mode"] = "approval_required"
            self._mark_updated(updated_fields, "security_mode")

        return manifest, sorted(updated_fields)

    @staticmethod
    def _infer_engine_pattern(manifest: dict[str, Any]) -> str:
        """services から engine_pattern を推論."""
        services = manifest.get("services")
        if not isinstance(services, dict):
            return "simple"

        engine = services.get("engine")
        if isinstance(engine, dict):
            pattern = engine.get("pattern")
            if isinstance(pattern, str) and pattern.strip():
                return pattern.strip()

        if "pipeline" in services:
            return "pipeline"
        if "workflow" in services:
            return "pipeline"
        if "coordinator" in services or "gateway" in services:
            return "coordinator"
        if "flow" in services:
            return "flow"
        if "deep_agent" in services:
            return "deep_agent"
        return "simple"

    @staticmethod
    def _infer_flow_pattern(manifest: dict[str, Any]) -> str | None:
        """services.engine.flow_pattern を推論."""
        services = manifest.get("services")
        if not isinstance(services, dict):
            return None
        engine = services.get("engine")
        if not isinstance(engine, dict):
            return None
        flow = engine.get("flow_pattern")
        if isinstance(flow, str) and flow.strip():
            return flow.strip()
        return None

    @staticmethod
    def _infer_product_line(manifest: dict[str, Any]) -> str:
        """App 名から product_line を推論."""
        app_name = str(manifest.get("name") or "").strip().lower()
        if app_name == "code_migration_assistant":
            return "migration"
        if app_name == "faq_system":
            return "faq"
        if app_name == "messaging_hub":
            return "assistant"
        return "framework"

    @staticmethod
    def _infer_surface_profile(manifest: dict[str, Any], *, product_line: str) -> str:
        """App と product_line から surface_profile を推論."""
        app_name = str(manifest.get("name") or "").strip().lower()
        if app_name in {"platform", "orchestration_guardian"}:
            return "operator"
        if product_line in {"migration", "faq", "assistant"}:
            return "business"
        return "developer"

    @staticmethod
    def _infer_audit_profile(*, product_line: str) -> str:
        """product_line から audit_profile を推論."""
        if product_line in {"migration", "faq", "assistant"}:
            return "business"
        return "developer"

    @staticmethod
    def _infer_contract_rag(manifest: dict[str, Any]) -> dict[str, Any]:
        """services / tags / agents から contracts.rag を推論."""
        rag_defaults = ContractsConfig().rag.model_dump()

        services = manifest.get("services", {})
        if not isinstance(services, dict):
            services = {}
        rag_service = services.get("rag", {})
        if not isinstance(rag_service, dict):
            rag_service = {}
        vector_service = services.get("vector_db", {})
        if not isinstance(vector_service, dict):
            vector_service = {}

        tags = manifest.get("tags", [])
        tags_lower = {str(tag).lower() for tag in tags} if isinstance(tags, list) else set()

        agents = manifest.get("agents", [])
        has_rag_agent = False
        if isinstance(agents, list):
            for agent in agents:
                if not isinstance(agent, dict):
                    continue
                capabilities = agent.get("capabilities", [])
                if not isinstance(capabilities, list):
                    continue
                if any("rag" in str(cap).lower() for cap in capabilities):
                    has_rag_agent = True
                    break

        enabled: bool
        if "enabled" in rag_service:
            enabled = bool(rag_service.get("enabled"))
        elif rag_service:
            enabled = True
        elif "rag" in tags_lower:
            enabled = True
        else:
            enabled = has_rag_agent

        chunking = rag_service.get("chunking", {})
        if not isinstance(chunking, dict):
            chunking = {}
        retrieval = rag_service.get("retrieval", {})
        if not isinstance(retrieval, dict):
            retrieval = {}

        app_name = str(manifest.get("name", "app")).strip() or "app"
        collection = vector_service.get("collection")
        if not isinstance(collection, str) or not collection.strip():
            collection = f"{app_name}_knowledge" if enabled else None
        else:
            collection = collection.strip()

        inferred = deepcopy(rag_defaults)
        inferred.update(
            {
                "enabled": enabled,
                "pattern": rag_service.get("pattern"),
                "provider": vector_service.get("provider"),
                "collections": [collection] if collection else [],
                "data_sources": (
                    rag_service.get("data_sources")
                    if isinstance(rag_service.get("data_sources"), list)
                    else []
                ),
                "chunk_strategy": chunking.get("strategy", rag_defaults["chunk_strategy"]),
                "chunk_size": chunking.get("size", rag_defaults["chunk_size"]),
                "chunk_overlap": chunking.get("overlap", rag_defaults["chunk_overlap"]),
                "retrieval_method": retrieval.get("method", rag_defaults["retrieval_method"]),
                "embedding_model": rag_service.get("embedding_model"),
                "rerank_model": retrieval.get("reranker"),
                "default_top_k": retrieval.get("top_k", rag_defaults["default_top_k"]),
                "score_threshold": retrieval.get("score_threshold"),
                "indexing_schedule": rag_service.get("indexing_schedule"),
            },
        )
        return inferred

    @staticmethod
    def _mark_updated(updated_fields: list[str], field: str) -> None:
        """更新項目を重複なしで登録."""
        if field not in updated_fields:
            updated_fields.append(field)

    @staticmethod
    def _iter_agent_capabilities(agents_raw: Any) -> list[list[str]]:
        """agents セクションから capabilities 一覧を抽出."""
        if not isinstance(agents_raw, list):
            return []
        rows: list[list[str]] = []
        for agent in agents_raw:
            if not isinstance(agent, dict):
                continue
            capabilities = agent.get("capabilities")
            if not isinstance(capabilities, list):
                continue
            rows.append([str(cap) for cap in capabilities])
        return rows

    def _migrate_agent_taxonomy(
        self,
        *,
        manifest: dict[str, Any],
        updated_fields: list[str],
        app_business_base: str,
        engine_pattern: str,
    ) -> None:
        """agents[].business_base / pattern を補完・正規化する."""
        agents_raw = manifest.get("agents")
        if not isinstance(agents_raw, list):
            return

        for index, agent in enumerate(agents_raw):
            if not isinstance(agent, dict):
                continue

            capabilities = agent.get("capabilities")
            capabilities_list = (
                [str(c) for c in capabilities] if isinstance(capabilities, list) else []
            )

            inferred_base = self._taxonomy.infer_agent_business_base(
                raw_business_base=agent.get("business_base"),
                capabilities=capabilities_list,
                fallback_app_base=app_business_base,
            )
            normalized_base = self._taxonomy.normalize_business_base(agent.get("business_base"))
            if normalized_base is None:
                agent["business_base"] = inferred_base
                self._mark_updated(updated_fields, f"agents[{index}].business_base")
            elif normalized_base != agent.get("business_base"):
                agent["business_base"] = normalized_base
                self._mark_updated(updated_fields, f"agents[{index}].business_base")

            inferred_pattern = self._taxonomy.infer_agent_pattern(
                raw_pattern=agent.get("pattern"),
                name=str(agent.get("name", "")),
                module=str(agent.get("module")) if isinstance(agent.get("module"), str) else None,
                engine_pattern=engine_pattern,
            )
            normalized_pattern = self._taxonomy.normalize_agent_pattern(agent.get("pattern"))
            if normalized_pattern is None:
                agent["pattern"] = inferred_pattern
                self._mark_updated(updated_fields, f"agents[{index}].pattern")
            elif normalized_pattern != agent.get("pattern"):
                agent["pattern"] = normalized_pattern
                self._mark_updated(updated_fields, f"agents[{index}].pattern")

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

    def _resolve_business_base(self, app_config: AppConfig) -> str:
        """AppConfig の business_base を解決（未設定時は推論）."""
        normalized = self._taxonomy.normalize_business_base(app_config.business_base)
        if normalized is not None:
            return normalized
        return self._taxonomy.infer_app_business_base(
            app_name=app_config.name,
            tags=app_config.tags,
            contracts_rag_enabled=app_config.contracts.rag.enabled,
            agent_capabilities=[agent.capabilities for agent in app_config.agents],
        )
