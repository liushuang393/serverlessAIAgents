# -*- coding: utf-8 -*-
"""Framework compliance audit service.

各 App manifest と実コードを突合し、Platform 観点での整合性を検証する。
"""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path
import re
from typing import Any
from urllib.parse import urlparse

from apps.platform.schemas.app_config_schemas import AppConfig
from apps.platform.services.agent_taxonomy import AgentTaxonomyService
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

    def __init__(self, discovery: AppDiscoveryService) -> None:
        self._discovery = discovery
        self._taxonomy = AgentTaxonomyService()

    def audit_all(self) -> dict[str, Any]:
        """全 App を監査."""
        rows: list[dict[str, Any]] = []
        pass_count = 0
        warning_count = 0
        fail_count = 0
        total_issues = 0

        for app_config in self._discovery.list_apps():
            row = self._audit_one(app_config)
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
            "apps": rows,
        }

    def _audit_one(self, app_config: AppConfig) -> dict[str, Any]:
        """単一 App を監査."""
        issues: list[FrameworkAuditIssue] = []
        config_path = self._discovery.get_config_path(app_config.name)
        app_dir = config_path.parent if config_path is not None else None

        issues.extend(self._check_agent_modules(app_config))
        issues.extend(self._check_runtime_ports(app_config))
        issues.extend(self._check_engine_pattern(app_config, app_dir))
        issues.extend(self._check_framework_usage(app_config, app_dir))
        issues.extend(self._check_taxonomy_fields(app_config))

        has_error = any(issue.severity == "error" for issue in issues)
        has_warning = any(issue.severity == "warning" for issue in issues)
        status = "fail" if has_error else "warning" if has_warning else "pass"

        return {
            "app_name": app_config.name,
            "display_name": app_config.display_name,
            "status": status,
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
                        message=(
                            f"{field} のポート({actual_port})が "
                            f"ports 設定({expected_port})と一致しません"
                        ),
                        hint="runtime.urls を ports.* と同じ値へ揃えてください",
                    ),
                )
        return issues

    def _check_engine_pattern(
        self,
        app_config: AppConfig,
        app_dir: Path | None,
    ) -> list[FrameworkAuditIssue]:
        issues: list[FrameworkAuditIssue] = []
        if app_dir is None or not app_dir.is_dir():
            return issues

        declared = self._taxonomy.normalize_engine_pattern(
            app_config.blueprint.engine_pattern,
        )
        detected, markers = self._detect_engine_pattern(app_dir)
        if detected is None:
            return issues

        if declared in markers:
            return issues

        # deep_agent は coordinator 系と互換扱い
        compatible = declared == detected or (
            declared == "deep_agent" and detected == "coordinator"
        )
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
        app_dir: Path | None,
    ) -> list[FrameworkAuditIssue]:
        issues: list[FrameworkAuditIssue] = []
        modules = [agent.module for agent in app_config.agents if isinstance(agent.module, str)]
        if any(module.startswith("agentflow.") for module in modules):
            return issues

        if app_dir is None or not app_dir.is_dir():
            return issues

        has_import = False
        for py_file in app_dir.rglob("*.py"):
            if any(
                part in {"frontend", "htmlcov", "node_modules", "__pycache__"}
                for part in py_file.parts
            ):
                continue
            content = py_file.read_text("utf-8", errors="ignore")
            if _FRAMEWORK_IMPORT_RE.search(content):
                has_import = True
                break

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

    def _detect_engine_pattern(self, app_dir: Path) -> tuple[str | None, list[str]]:
        """実装コードから engine pattern を推定."""
        text = self._collect_python_sources(app_dir)
        if not text:
            return None, []

        matches = [name for name, pattern in _ENGINE_MARKERS.items() if pattern.search(text)]
        if not matches:
            return None, []

        if "coordinator" in matches:
            return "coordinator", matches
        if "pipeline" in matches:
            return "pipeline", matches
        if "flow" in matches:
            return "flow", matches
        if "simple" in matches:
            return "simple", matches
        return None, matches

    @staticmethod
    def _collect_python_sources(app_dir: Path) -> str:
        """監査対象の Python ソースを連結."""
        chunks: list[str] = []
        for py_file in app_dir.rglob("*.py"):
            if any(
                part in {"frontend", "htmlcov", "node_modules", "__pycache__", "tests"}
                for part in py_file.parts
            ):
                continue
            chunks.append(py_file.read_text("utf-8", errors="ignore"))
        return "\n".join(chunks)
