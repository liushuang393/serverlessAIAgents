"""Framework compliance audit service.

各 App manifest と実コードを突合し、Platform 観点での整合性を検証する。
"""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any
from urllib.parse import urlparse

from apps.platform.services.agent_taxonomy import AgentTaxonomyService
from apps.platform.services.protocol_surface_inspector import (
    ProtocolSurfaceReport,
    inspect_protocol_surface,
)

from agentflow.governance.plugin_registry import PluginRegistry


if TYPE_CHECKING:
    from apps.platform.schemas.app_config_schemas import AppConfig
    from apps.platform.services.app_discovery import AppDiscoveryService


_ENGINE_MARKERS: dict[str, re.Pattern[str]] = {
    "coordinator": re.compile(
        r"class\s+\w*Coordinator\b|from\s+\S+\s+import\s+\w*Coordinator\b|\b\w*Coordinator\(",
        re.IGNORECASE,
    ),
    "pipeline": re.compile(r"PipelineEngine|BaseEngine", re.IGNORECASE),
    "flow": re.compile(r"create_flow\(|\bFlow\(", re.IGNORECASE),
    "simple": re.compile(r"SimpleEngine", re.IGNORECASE),
}

_FRAMEWORK_IMPORT_RE = re.compile(r"(^|\n)\s*(from|import)\s+agentflow\b")
_RAG_SURFACE_RE = re.compile(r"\brag\b|vector|retriev|knowledge", re.IGNORECASE)
_SKILLS_SURFACE_RE = re.compile(r"\bskill\b|agentflow\.skills|SKILL\.md", re.IGNORECASE)
_AUTH_ENFORCEMENT_RE = re.compile(
    r"Depends\(\s*require_auth|_require_api_key\(|_require_http_api_key\(|"
    r"_require_ws_api_key\(|_verify_api_key\(|APIKeyHeader|HTTPBearer|Security\(",
    re.IGNORECASE,
)
_MIN_AGENT_COUNT_FOR_A2A = 2


@dataclass(slots=True)
class FrameworkAuditIssue:
    """監査指摘."""

    severity: str
    code: str
    message: str
    hint: str | None = None

    def to_dict(self) -> dict[str, Any]:
        """辞書へ変換."""
        return {
            "severity": self.severity,
            "code": self.code,
            "message": self.message,
            "hint": self.hint,
        }


class FrameworkAuditService:
    """App フレームワーク準拠監査サービス."""

    def __init__(
        self,
        discovery: AppDiscoveryService,
        plugin_registry: PluginRegistry | None = None,
    ) -> None:
        self._discovery = discovery
        self._taxonomy = AgentTaxonomyService()
        self._plugin_registry = plugin_registry or PluginRegistry(
            plugins_dir=self._discovery.apps_dir.parent / "plugins",
            apps_dir=self._discovery.apps_dir,
        )

    def audit_all(self, *, audit_profile: str | None = None) -> dict[str, Any]:
        """全 App を監査."""
        rows: list[dict[str, Any]] = []
        pass_count = 0
        warning_count = 0
        fail_count = 0
        total_issues = 0

        for app_config in self._discovery.list_apps():
            row = self._audit_one(app_config, audit_profile=audit_profile)
            rows.append(row)
            total_issues += row["issue_count"]
            if row["status"] == "pass":
                pass_count += 1
            elif row["status"] == "warning":
                warning_count += 1
            else:
                fail_count += 1

        return {
            "total_apps": len(rows),
            "pass": pass_count,
            "warning": warning_count,
            "fail": fail_count,
            "total_issues": total_issues,
            "audit_profile": audit_profile or "auto",
            "apps": rows,
        }

    def _audit_one(
        self, app_config: AppConfig, *, audit_profile: str | None = None
    ) -> dict[str, Any]:
        """単一 App を監査."""
        issues: list[FrameworkAuditIssue] = []
        config_path = self._discovery.get_config_path(app_config.name)
        app_dir = config_path.parent if config_path is not None else None
        source_text = self._collect_python_sources(app_dir) if app_dir is not None else ""
        resolved_profile = self._resolve_audit_profile(app_config, audit_profile)

        issues.extend(self._check_agent_modules(app_config))
        issues.extend(self._check_runtime_ports(app_config))
        issues.extend(self._check_entry_points(app_config, app_dir, source_text))
        issues.extend(self._check_engine_pattern(app_config, source_text))
        issues.extend(self._check_framework_usage(app_config, source_text))
        issues.extend(self._check_contract_consistency(app_config))
        issues.extend(self._check_plugin_bindings(app_config))
        issues.extend(
            self._check_orchestration_protocols(
                app_config,
                source_text,
                app_dir=app_dir,
                audit_profile=resolved_profile,
            ),
        )
        issues.extend(self._check_auth_runtime_enforcement(app_config, source_text))
        issues.extend(self._check_security_baseline(app_config))
        issues.extend(self._check_taxonomy_fields(app_config))

        has_error = any(issue.severity == "error" for issue in issues)
        has_warning = any(issue.severity == "warning" for issue in issues)
        status = "fail" if has_error else "warning" if has_warning else "pass"

        return {
            "app_name": app_config.name,
            "display_name": app_config.display_name,
            "status": status,
            "audit_profile": resolved_profile,
            "issue_count": len(issues),
            "issues": [issue.to_dict() for issue in issues],
        }

    def _check_agent_modules(self, app_config: AppConfig) -> list[FrameworkAuditIssue]:
        issues: list[FrameworkAuditIssue] = []
        for agent in app_config.agents:
            module = agent.module
            if not isinstance(module, str) or not module.strip():
                issues.append(
                    FrameworkAuditIssue(
                        severity="warning",
                        code="AGENT_MODULE_MISSING_DECLARATION",
                        message=f"agent '{agent.name}' に module 指定がありません",
                        hint="app_config.json agents[].module を指定してください",
                    ),
                )
                continue

            if module.startswith(("apps.", "agentflow.")):
                module_path = Path(*module.split("."))
                file_path = module_path.with_suffix(".py")
                package_path = module_path / "__init__.py"
                if not file_path.is_file() and not package_path.is_file():
                    issues.append(
                        FrameworkAuditIssue(
                            severity="error",
                            code="AGENT_MODULE_NOT_FOUND",
                            message=f"agent '{agent.name}' module が見つかりません: {module}",
                            hint="module パスを実在ファイルへ修正してください",
                        ),
                    )
        return issues

    def _check_plugin_bindings(self, app_config: AppConfig) -> list[FrameworkAuditIssue]:
        """plugin_bindings と plugin_manifest の整合性を検証."""
        issues: list[FrameworkAuditIssue] = []
        strict_line = self._plugin_registry.is_strict_product_line(app_config.product_line)
        severity = "error" if strict_line else "warning"
        bindings = app_config.plugin_bindings

        if (
            strict_line
            and not bindings
            and (
                len(app_config.dependencies.external) > 0
                or isinstance(app_config.services.get("mcp"), dict)
            )
        ):
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="PLUGIN_BINDINGS_EMPTY_WITH_EXTERNAL_SURFACE",
                    message=("外部依存または MCP 面を持つ App だが plugin_bindings が空です"),
                    hint="副作用を伴う統合機能は plugin_bindings で明示管理してください",
                ),
            )

        seen: set[str] = set()
        duplicates: set[str] = set()
        for binding in bindings:
            if binding.id in seen:
                duplicates.add(binding.id)
            seen.add(binding.id)

        if duplicates:
            dup = ", ".join(sorted(duplicates))
            issues.append(
                FrameworkAuditIssue(
                    severity="error",
                    code="PLUGIN_BINDING_DUPLICATED",
                    message=f"plugin_bindings に重複 ID があります: {dup}",
                    hint="同一 plugin は1回だけ宣言してください",
                ),
            )

        for binding in bindings:
            manifest = self._plugin_registry.get_manifest(binding.id)
            if manifest is None:
                issues.append(
                    FrameworkAuditIssue(
                        severity=severity,
                        code="PLUGIN_MANIFEST_NOT_FOUND",
                        message=f"plugin manifest が見つかりません: {binding.id}",
                        hint="plugins/<id>/plugin_manifest.json を配置してください",
                    ),
                )
                continue

            if binding.version != manifest.version:
                issues.append(
                    FrameworkAuditIssue(
                        severity=severity,
                        code="PLUGIN_BINDING_VERSION_MISMATCH",
                        message=(
                            f"plugin '{binding.id}' の binding version({binding.version}) と "
                            f"manifest version({manifest.version}) が一致しません"
                        ),
                        hint="plugin_bindings[].version を manifest.version に合わせてください",
                    ),
                )

            if (
                manifest.compatibility_product_lines
                and app_config.product_line not in manifest.compatibility_product_lines
            ):
                issues.append(
                    FrameworkAuditIssue(
                        severity=severity,
                        code="PLUGIN_PRODUCT_LINE_MISMATCH",
                        message=(
                            f"plugin '{binding.id}' は product_line="
                            f"{manifest.compatibility_product_lines} 専用です"
                        ),
                        hint="App の product_line と互換な plugin を選択してください",
                    ),
                )

        return issues

    def _check_runtime_ports(self, app_config: AppConfig) -> list[FrameworkAuditIssue]:
        issues: list[FrameworkAuditIssue] = []
        runtime_urls = app_config.runtime.urls

        checks: list[tuple[str, str | None, int | None]] = [
            ("runtime.urls.backend", runtime_urls.backend, app_config.ports.api),
            ("runtime.urls.health", runtime_urls.health, app_config.ports.api),
            ("runtime.urls.frontend", runtime_urls.frontend, app_config.ports.frontend),
        ]
        for field, url, expected_port in checks:
            if not url or expected_port is None:
                continue
            parsed = urlparse(url)
            actual_port = parsed.port
            if actual_port is None:
                continue
            if actual_port != expected_port:
                issues.append(
                    FrameworkAuditIssue(
                        severity="warning",
                        code="RUNTIME_PORT_MISMATCH",
                        message=(f"{field} のポート({actual_port})が ports 設定({expected_port})と一致しません"),
                        hint="runtime.urls を ports.* と同じ値へ揃えてください",
                    ),
                )
        return issues

    def _check_entry_points(
        self,
        app_config: AppConfig,
        app_dir: Path | None,
        source_text: str,
    ) -> list[FrameworkAuditIssue]:
        """entry_points / frontend 契約の整合性を検証."""
        issues: list[FrameworkAuditIssue] = []

        api_module = (app_config.entry_points.api_module or "").strip()
        if app_config.ports.api is not None and not api_module:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="API_ENTRYPOINT_MISSING",
                    message="ports.api が設定されているが entry_points.api_module が未設定です",
                    hint="entry_points.api_module に FastAPI モジュールを設定してください",
                ),
            )

        target_path: Path | None = None
        if api_module:
            module_name, _, attr_name = api_module.partition(":")
            module_name = module_name.strip()
            attr_name = (attr_name or "app").strip() or "app"
            if not module_name:
                issues.append(
                    FrameworkAuditIssue(
                        severity="error",
                        code="API_ENTRYPOINT_INVALID",
                        message=f"entry_points.api_module 形式が不正です: {api_module}",
                        hint="`module.path:app` 形式へ修正してください",
                    ),
                )
            else:
                target_path = self._resolve_module_file(module_name)
                if target_path is None:
                    issues.append(
                        FrameworkAuditIssue(
                            severity="error",
                            code="API_MODULE_NOT_FOUND",
                            message=(f"entry_points.api_module が指すモジュールが見つかりません: {module_name}"),
                            hint="実在するモジュールへ修正してください",
                        ),
                    )
                else:
                    target_text = target_path.read_text("utf-8", errors="ignore")
                    attr_re = re.compile(rf"(^|\n)\s*{re.escape(attr_name)}\s*=", re.MULTILINE)
                    if attr_re.search(target_text) is None:
                        issues.append(
                            FrameworkAuditIssue(
                                severity="warning",
                                code="API_APP_OBJECT_NOT_FOUND",
                                message=(f"entry_points.api_module のオブジェクト '{attr_name}' が見つかりません"),
                                hint=(f"{target_path} に `{attr_name} = FastAPI(...)` を定義してください"),
                            ),
                        )

        health_path = (app_config.entry_points.health or "").strip()
        if health_path and source_text and health_path not in source_text:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="HEALTH_PATH_NOT_FOUND",
                    message=f"entry_points.health='{health_path}' の実装が検出できません",
                    hint="FastAPI ルーターの health パスと manifest を一致させてください",
                ),
            )

        frontend_port = app_config.ports.frontend
        if frontend_port is not None:
            frontend_dir = app_dir / "frontend" if app_dir is not None else None
            has_frontend_dir = frontend_dir is not None and frontend_dir.is_dir()
            has_frontend_cmd = bool((app_config.runtime.commands.frontend_dev or "").strip())
            has_frontend_url = bool((app_config.runtime.urls.frontend or "").strip())

            if not has_frontend_dir and not has_frontend_cmd:
                issues.append(
                    FrameworkAuditIssue(
                        severity="warning",
                        code="FRONTEND_DECLARED_BUT_NOT_FOUND",
                        message=("ports.frontend が設定されているが frontend 実装/起動コマンドが見つかりません"),
                        hint=("frontend ディレクトリまたは runtime.commands.frontend_dev を設定してください"),
                    ),
                )

            if not has_frontend_url:
                issues.append(
                    FrameworkAuditIssue(
                        severity="warning",
                        code="FRONTEND_URL_MISSING",
                        message=("ports.frontend が設定されているが runtime.urls.frontend が未設定です"),
                        hint="runtime.urls.frontend を設定し Frontend 接続先を明示してください",
                    ),
                )

        return issues

    def _check_contract_consistency(self, app_config: AppConfig) -> list[FrameworkAuditIssue]:
        """services / contracts / blueprint の整合性を検証."""
        issues: list[FrameworkAuditIssue] = []
        services = app_config.services

        rag_contract = app_config.contracts.rag
        rag_service = services.get("rag")
        rag_service_collections: list[str] = []
        if isinstance(rag_service, dict):
            rag_service_collections = self._normalize_str_list(rag_service.get("collections", []))

        rag_contract_collections = self._normalize_str_list(rag_contract.collections)
        if rag_contract.enabled and not isinstance(rag_service, dict):
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="RAG_CONTRACT_ENABLED_BUT_SERVICE_MISSING",
                    message="contracts.rag.enabled=true だが services.rag が未定義です",
                    hint="services.rag を追加するか contracts.rag.enabled を見直してください",
                ),
            )
        if not rag_contract.enabled and isinstance(rag_service, dict):
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="RAG_SERVICE_PRESENT_BUT_CONTRACT_DISABLED",
                    message="services.rag は定義されているが contracts.rag.enabled=false です",
                    hint="contracts.rag.enabled を true にするか services.rag を整理してください",
                ),
            )
        if rag_contract.enabled:
            if not rag_contract_collections and not rag_service_collections:
                issues.append(
                    FrameworkAuditIssue(
                        severity="warning",
                        code="RAG_COLLECTIONS_EMPTY",
                        message="RAG が有効だが collections が未設定です",
                        hint=("contracts.rag.collections または services.rag.collections を設定してください"),
                    ),
                )
            elif (
                rag_contract_collections
                and rag_service_collections
                and set(rag_contract_collections) != set(rag_service_collections)
            ):
                issues.append(
                    FrameworkAuditIssue(
                        severity="warning",
                        code="RAG_COLLECTION_MISMATCH",
                        message=("contracts.rag.collections と services.rag.collections が一致しません"),
                        hint="RAG の既定コレクション名を 1 つの値へ統一してください",
                    ),
                )

        mcp_service = services.get("mcp")
        mcp_servers = self._normalize_str_list(app_config.blueprint.mcp_servers)
        has_mcp_tools = isinstance(mcp_service, dict) and bool(self._normalize_str_list(mcp_service.get("tools", [])))
        if has_mcp_tools and not mcp_servers:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="MCP_SERVICE_WITHOUT_SERVER_BINDING",
                    message="services.mcp.tools はあるが blueprint.mcp_servers が空です",
                    hint="実際に接続する MCP サーバー名を blueprint.mcp_servers に定義してください",
                ),
            )
        if mcp_servers and not isinstance(mcp_service, dict):
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="MCP_SERVERS_DECLARED_BUT_SERVICE_MISSING",
                    message="blueprint.mcp_servers はあるが services.mcp が未定義です",
                    hint="services.mcp を追加するか blueprint.mcp_servers を整理してください",
                ),
            )

        contract_skills = self._normalize_str_list(app_config.contracts.skills.default_skills)
        blueprint_skills = self._normalize_str_list(app_config.blueprint.default_skills)
        if contract_skills and not blueprint_skills:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="SKILLS_CONTRACT_BLUEPRINT_MISMATCH",
                    message=("contracts.skills.default_skills があるが blueprint.default_skills が空です"),
                    hint="skills の既定値は contracts と blueprint の両方で統一してください",
                ),
            )
        elif contract_skills and set(contract_skills) != set(blueprint_skills):
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="SKILLS_DEFAULT_MISMATCH",
                    message=("contracts.skills.default_skills と blueprint.default_skills が一致しません"),
                    hint="default_skills の重複定義を解消してください",
                ),
            )

        return issues

    def _check_orchestration_protocols(
        self,
        app_config: AppConfig,
        source_text: str,
        *,
        app_dir: Path | None,
        audit_profile: str,
    ) -> list[FrameworkAuditIssue]:
        """Agent 編成とプロトコル面の最低限を検証."""
        issues: list[FrameworkAuditIssue] = []
        if app_dir is None:
            return issues

        engine_pattern = self._taxonomy.normalize_engine_pattern(app_config.blueprint.engine_pattern)
        agent_count = len(app_config.agents)
        enforce_protocol_surface = audit_profile == "developer"
        has_stream_surface = surface_report.has("sse") or surface_report.has("ws")
        if (
            enforce_protocol_surface
            and engine_pattern in {"flow", "pipeline", "coordinator", "deep_agent"}
            and not has_stream_surface
        ):
            issues.append(
                self._build_protocol_missing_issue(
                    code="ORCHESTRATION_STREAM_SURFACE_MISSING",
                    message=(f"engine_pattern='{engine_pattern}' だが 進捗ストリーム面(SSE/WebSocket)が検出できません"),
                    hint="run_stream / SSE / WebSocket の少なくとも 1 つを提供してください",
                ),
            )

        coordinator_like = any(
            self._taxonomy.normalize_agent_pattern(agent.pattern) in {"coordinator", "router"}
            for agent in app_config.agents
        )
        has_a2a_surface = surface_report.has("a2a")
        if (
            enforce_protocol_surface
            and agent_count >= _MIN_AGENT_COUNT_FOR_A2A
            and coordinator_like
            and not has_a2a_surface
        ):
            issues.append(
                self._build_protocol_missing_issue(
                    code="A2A_SURFACE_NOT_FOUND",
                    message="複数 Agent + coordinator/router 構成だが A2A 面が検出できません",
                    hint=("A2A card / A2A routing のいずれかを実装し、Agent 間契約を明示してください"),
                ),
            )

        services = app_config.services
        dependencies_external = {item.lower() for item in app_config.dependencies.external}

        expects_mcp = (
            isinstance(services.get("mcp"), dict)
            or bool(app_config.blueprint.mcp_servers)
            or ("mcp" in dependencies_external)
        )
        if enforce_protocol_surface and expects_mcp and not surface_report.has("mcp"):
            issues.append(
                self._build_protocol_missing_issue(
                    code="MCP_DECLARED_BUT_SURFACE_MISSING",
                    declared="MCP 利用宣言はあるが、MCP 呼び出し面が検出できません",
                    expected="MCP client/tool の呼び出しコードを追加または宣言を整理してください",
                    evidence_protocols=("mcp",),
                    report=surface_report,
                ),
            )

        expects_rag = app_config.contracts.rag.enabled or isinstance(services.get("rag"), dict)
        if expects_rag and _RAG_SURFACE_RE.search(source_text) is None:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="RAG_DECLARED_BUT_SURFACE_MISSING",
                    message="RAG 利用宣言はあるが、RAG 実装シグナルが検出できません",
                    hint="RAG query/indexing の利用コードを確認してください",
                ),
            )

        expects_skills = bool(app_config.contracts.skills.default_skills) or bool(app_config.blueprint.default_skills)
        if expects_skills and _SKILLS_SURFACE_RE.search(source_text) is None:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="SKILLS_DECLARED_BUT_SURFACE_MISSING",
                    message="Skills 利用宣言はあるが、Skills 実装シグナルが検出できません",
                    hint="agentflow.skills の利用コードまたは宣言を見直してください",
                ),
            )

        return issues

    @staticmethod
    def _build_ast_parse_warnings(report: ProtocolSurfaceReport) -> list[FrameworkAuditIssue]:
        issues: list[FrameworkAuditIssue] = []
        for warning in report.parse_warnings:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="AST_PARSE_WARNING",
                    message=(
                        "AST（Python構文木）解析に失敗しました: "
                        f"{warning.file}:{warning.line} {warning.message}"
                    ),
                    hint=f"修正提案: {warning.suggestion} / 影響: {warning.impact}",
                ),
            )
        return issues

    @staticmethod
    def _build_protocol_missing_issue(
        *,
        code: str,
        declared: str,
        expected: str,
        evidence_protocols: tuple[str, ...],
        report: ProtocolSurfaceReport,
    ) -> FrameworkAuditIssue:
        locations: list[str] = []
        for protocol in evidence_protocols:
            locations.extend(report.evidence_locations(protocol))
        has_any_evidence = bool(locations)
        cannot_prove = report.parse_failed_all or (
            report.parse_failed_files > 0 and not has_any_evidence
        )

        if cannot_prove:
            return FrameworkAuditIssue(
                severity="error",
                code=f"{code}_UNVERIFIED",
                message=(f"{declared}。関連ファイルの AST 解析に失敗し、実装有無を立証できません"),
                hint=(f"構文エラーを修正して AST 監査を再実行してください。期待要件: {expected}"),
            )

        evidence_note = ", ".join(sorted(set(locations))[:4]) if locations else "証拠なし"
        return FrameworkAuditIssue(
            severity="warning",
            code=code,
            message=declared,
            hint=f"{expected}（検出証拠: {evidence_note}）",
        )

    @staticmethod
    def _resolve_audit_profile(app_config: AppConfig, override: str | None) -> str:
        """監査プロファイルを解決."""
        if override in {"business", "developer"}:
            return override
        configured = str(app_config.audit_profile or "").strip().lower()
        return configured if configured in {"business", "developer"} else "developer"

    def _check_security_baseline(self, app_config: AppConfig) -> list[FrameworkAuditIssue]:
        """manifest の最低限セキュリティ基準を検証."""
        issues: list[FrameworkAuditIssue] = []

        password = (app_config.runtime.database.password or "").strip()
        if password:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="PLAINTEXT_DB_PASSWORD_IN_MANIFEST",
                    message="runtime.database.password に平文パスワードが保存されています",
                    hint="password を空にし、password_env のみ使用してください",
                ),
            )

        has_external = len(app_config.dependencies.external) > 0
        auth_disabled = not app_config.contracts.auth.enabled
        anonymous_allowed = app_config.contracts.auth.allow_anonymous
        if has_external and auth_disabled and anonymous_allowed:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="EXTERNAL_DEP_WITH_ANONYMOUS_ACCESS",
                    message="外部依存があるが認証が無効かつ匿名アクセス許可です",
                    hint="最低限 API キー/トークン認証を有効化してください",
                ),
            )

        if app_config.visibility.mode == "public" and auth_disabled:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="PUBLIC_APP_WITHOUT_AUTH",
                    message="visibility.mode=public だが認証が無効です",
                    hint="公開アプリは contracts.auth.enabled=true を必須化してください",
                ),
            )

        return issues

    def _check_auth_runtime_enforcement(
        self,
        app_config: AppConfig,
        source_text: str,
    ) -> list[FrameworkAuditIssue]:
        """contracts.auth の宣言と実装シグナルの整合性を検証."""
        issues: list[FrameworkAuditIssue] = []
        auth_contract = app_config.contracts.auth
        if not auth_contract.enabled or auth_contract.allow_anonymous:
            return issues
        if not source_text:
            return issues

        if _AUTH_ENFORCEMENT_RE.search(source_text) is None:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="AUTH_CONTRACT_WITHOUT_RUNTIME_GUARD",
                    message=("contracts.auth で認証必須だが、実装側の認証ガードシグナルが検出できません"),
                    hint=(
                        "Depends(require_auth) または API key 検証(_require_api_key) を HTTP/WS 両方に適用してください"
                    ),
                ),
            )
        return issues

    def _check_engine_pattern(
        self,
        app_config: AppConfig,
        source_text: str,
    ) -> list[FrameworkAuditIssue]:
        issues: list[FrameworkAuditIssue] = []
        if not source_text:
            return issues

        declared = self._taxonomy.normalize_engine_pattern(
            app_config.blueprint.engine_pattern,
        )
        detected, markers = self._detect_engine_pattern(source_text)
        if detected is None:
            return issues

        if declared in markers:
            return issues

        # deep_agent は coordinator 系と互換扱い
        compatible = declared == detected or (declared == "deep_agent" and detected == "coordinator")
        if not compatible:
            marker_text = ", ".join(markers) if markers else "n/a"
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="ENGINE_PATTERN_MISMATCH",
                    message=(
                        f"blueprint.engine_pattern='{app_config.blueprint.engine_pattern}' "
                        f"と実装推定 '{detected}' が不一致です"
                    ),
                    hint=f"検出シグナル: {marker_text}",
                ),
            )
        return issues

    def _check_framework_usage(
        self,
        app_config: AppConfig,
        source_text: str,
    ) -> list[FrameworkAuditIssue]:
        issues: list[FrameworkAuditIssue] = []
        modules = [agent.module for agent in app_config.agents if isinstance(agent.module, str)]
        if any(module.startswith("agentflow.") for module in modules):
            return issues

        if not source_text:
            return issues

        has_import = _FRAMEWORK_IMPORT_RE.search(source_text) is not None

        if not has_import:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="FRAMEWORK_IMPORT_NOT_FOUND",
                    message="agentflow フレームワーク import が検出できませんでした",
                    hint="agentflow の Engine/Agent/Provider API 利用を確認してください",
                ),
            )
        return issues

    def _check_taxonomy_fields(self, app_config: AppConfig) -> list[FrameworkAuditIssue]:
        issues: list[FrameworkAuditIssue] = []
        normalized_base = self._taxonomy.normalize_business_base(app_config.business_base)
        if normalized_base is None:
            issues.append(
                FrameworkAuditIssue(
                    severity="warning",
                    code="APP_BUSINESS_BASE_MISSING",
                    message="business_base が未設定です",
                    hint="app_config.json に business_base を追加してください",
                ),
            )

        for agent in app_config.agents:
            if self._taxonomy.normalize_agent_pattern(agent.pattern) is None:
                issues.append(
                    FrameworkAuditIssue(
                        severity="warning",
                        code="AGENT_PATTERN_MISSING",
                        message=f"agent '{agent.name}' の pattern が未設定です",
                        hint="agents[].pattern を設定してください",
                    ),
                )
        return issues

    def _detect_engine_pattern(self, source_text: str) -> tuple[str | None, list[str]]:
        """実装コードから engine pattern を推定."""
        if not source_text:
            return None, []

        matches = [name for name, pattern in _ENGINE_MARKERS.items() if pattern.search(source_text)]
        if not matches:
            return None, []

        for marker in ("coordinator", "pipeline", "flow", "simple"):
            if marker in matches:
                return marker, matches
        return None, matches

    @staticmethod
    def _resolve_module_file(module_name: str) -> Path | None:
        """Python module path から実ファイルを解決."""
        module_path = Path(*module_name.split("."))
        file_path = module_path.with_suffix(".py")
        if file_path.is_file():
            return file_path
        package_path = module_path / "__init__.py"
        if package_path.is_file():
            return package_path
        return None

    @staticmethod
    def _normalize_str_list(raw: Any) -> list[str]:
        """任意値を重複除去済み文字列リストへ正規化."""
        if not isinstance(raw, list):
            return []
        values: list[str] = []
        seen: set[str] = set()
        for item in raw:
            if not isinstance(item, str):
                continue
            token = item.strip()
            if not token:
                continue
            if token in seen:
                continue
            seen.add(token)
            values.append(token)
        return values

    @staticmethod
    def _collect_python_sources(app_dir: Path) -> str:
        """監査対象の Python ソースを連結."""
        chunks: list[str] = []
        for py_file in app_dir.rglob("*.py"):
            if any(part in {"frontend", "htmlcov", "node_modules", "__pycache__", "tests"} for part in py_file.parts):
                continue
            chunks.append(py_file.read_text("utf-8", errors="ignore"))
        return "\n".join(chunks)
