"""Plugin manifest 実行時ガバナンスレジストリ.

Studio 実行時に、ツールの plugin 情報と app_config の plugin_bindings を突合して
実行可否判定に必要な情報を提供する。
"""

from __future__ import annotations

import json
import logging
import os
import re
import tomllib
from importlib.metadata import PackageNotFoundError, version
from pathlib import Path
from typing import TYPE_CHECKING, Any, Protocol

from pydantic import BaseModel, Field

from contracts.plugin import (
    PluginBinding as ContractPluginBinding,
)
from contracts.plugin import (
    PluginDescriptor,
)
from contracts.plugin import (
    PluginRuntimeAssessment as ContractPluginRuntimeAssessment,
)
from harness.governance.plugin_signature import (
    PluginSignatureVerifier,
    SignatureStatus,
)
from shared.config.manifest import load_app_manifest


if TYPE_CHECKING:
    from harness.governance.engine import ToolExecutionContext


class RegisteredToolLike(Protocol):
    """プラグイン評価に必要なツール契約."""

    name: str
    plugin_id: str | None
    plugin_version: str | None
    required_permissions: list[str]
    operation_type: Any
    risk_level: Any


_SEMVER_PART_RE = re.compile(r"^(\d+)\.(\d+)\.(\d+)")
_KERNEL_CONSTRAINT_RE = re.compile(r"^(>=|<=|==|>|<)?\s*([0-9A-Za-z.+-]+)$")
_SUPPORTED_PRODUCT_LINES = {"migration", "faq", "assistant", "framework"}
_STRICT_PRODUCT_LINES = {"migration", "faq", "assistant"}
_SIDE_EFFECT_OPERATIONS = {"write", "delete", "execute"}
_PLUGIN_SIGNATURE_ENFORCEMENT_ENV = "AGENTFLOW_PLUGIN_SIGNATURE_ENFORCEMENT"
_APP_ENV_ENV = "APP_ENV"
_DEFAULT_PLUGIN_SIGNATURE_ENFORCEMENT = "warn"
_VALID_SIGNATURE_ENFORCEMENTS = {"warn", "deny"}
_CANONICAL_PLUGIN_PACKS_DIR = Path("kernel/plugins/packs")
_LEGACY_PLUGIN_PACKS_DIR = Path("plugins")


_logger = logging.getLogger(__name__)


class PluginManifestRecord(PluginDescriptor):
    """plugin_manifest.json の最小参照情報."""

    manifest_path: Path | None = Field(default=None)
    signature_status: SignatureStatus = Field(default="parse_error")
    signature_reason: str = Field(default="")
    raw: dict[str, Any] = Field(default_factory=dict)


class PluginBindingRecord(ContractPluginBinding):
    """app_config.json plugin_bindings エントリ."""


class AppPluginSnapshot(BaseModel):
    """App 単位の plugin バインディング情報."""

    app_name: str = Field(...)
    product_line: str = Field(...)
    bindings: dict[str, PluginBindingRecord] = Field(default_factory=dict)


class PluginRuntimeAssessment(ContractPluginRuntimeAssessment):
    """ツール実行時 plugin 評価結果."""

    plugin_signature_status: SignatureStatus | None = Field(default=None)
    manifest: PluginManifestRecord | None = Field(default=None)
    binding: PluginBindingRecord | None = Field(default=None)


class PluginManifestLoader:
    """Load canonical plugin manifests from the configured packs root."""

    def __init__(
        self,
        *,
        plugins_dir: Path,
        signature_verifier: PluginSignatureVerifier,
    ) -> None:
        self._plugins_dir = plugins_dir
        self._signature_verifier = signature_verifier

    def load(self) -> dict[str, PluginManifestRecord]:
        """Load plugin manifests under the configured packs root."""
        manifests: dict[str, PluginManifestRecord] = {}
        if not self._plugins_dir.is_dir():
            _logger.debug("plugin packs directory missing: %s", self._plugins_dir)
            return manifests

        for manifest_path in sorted(self._plugins_dir.glob("*/plugin_manifest.json")):
            record = self._load_one(manifest_path)
            if record is not None:
                manifests[record.id] = record
        return manifests

    def _load_one(self, manifest_path: Path) -> PluginManifestRecord | None:
        try:
            raw = json.loads(manifest_path.read_text("utf-8"))
            plugin_id = PluginRegistry._normalize_optional_text(raw.get("id"))
            plugin_version = PluginRegistry._normalize_optional_text(raw.get("version"))
            risk_tier = PluginRegistry._normalize_optional_text(raw.get("risk_tier"))
            compatibility = raw.get("compatibility")
            if plugin_id is None or plugin_version is None or risk_tier is None or not isinstance(compatibility, dict):
                _logger.warning("plugin manifest missing required fields: %s", manifest_path)
                return None

            kernel_constraint = PluginRegistry._normalize_optional_text(compatibility.get("kernel")) or ""
            product_lines = PluginRegistry._normalize_product_lines(compatibility.get("product_lines"))
            signature_result = self._signature_verifier.verify_manifest(
                manifest=raw,
                manifest_path=manifest_path,
            )
            return PluginManifestRecord(
                id=plugin_id,
                version=plugin_version,
                risk_tier=risk_tier,
                compatibility_kernel=kernel_constraint,
                compatibility_product_lines=product_lines,
                manifest_path=manifest_path,
                signature_status=signature_result.status,
                signature_reason=signature_result.reason,
                raw=raw,
            )
        except Exception as exc:
            _logger.warning("plugin manifest load failed (%s): %s", manifest_path, exc)
            return None


class AppBindingResolver:
    """Resolve app-level plugin bindings from canonical app manifests."""

    def __init__(self, apps_dir: Path) -> None:
        self._apps_dir = apps_dir

    def load_snapshot(self, app_name: str) -> AppPluginSnapshot | None:
        """Load one app's plugin binding snapshot from canonical manifest."""
        config_path = self._apps_dir / app_name / "app_config.json"
        if not config_path.is_file():
            return None
        try:
            manifest = load_app_manifest(config_path)
            bindings = {
                binding.id: PluginBindingRecord(
                    id=binding.id,
                    version=binding.version,
                    config=binding.config,
                )
                for binding in manifest.plugin_bindings
            }
            return AppPluginSnapshot(
                app_name=manifest.name,
                product_line=manifest.product_line,
                bindings=bindings,
            )
        except Exception as exc:
            _logger.warning("app manifest load failed (%s): %s", config_path, exc)
            return None


class PluginPolicyEvaluator:
    """Encapsulate runtime policy checks for plugin manifests and bindings."""

    def __init__(self, *, kernel_version: str, signature_enforcement: str) -> None:
        self._kernel_version = kernel_version
        self._signature_enforcement = signature_enforcement

    def evaluate_manifest(
        self,
        assessment: PluginRuntimeAssessment,
        *,
        manifest: PluginManifestRecord,
        plugin_version: str | None,
        tool_permissions: list[str],
    ) -> None:
        """Evaluate manifest compatibility and permissions against one tool."""
        assessment.plugin_risk_tier = manifest.risk_tier
        assessment.plugin_signature_status = manifest.signature_status
        assessment.plugin_signature_reason = manifest.signature_reason
        if manifest.signature_status != "verified":
            message = f"plugin 署名検証 warning: status={manifest.signature_status}, reason={manifest.signature_reason}"
            if self._signature_enforcement == "deny":
                assessment.errors.append(message)
            else:
                assessment.warnings.append(message)

        assessment.manifest_required_permissions = PluginRegistry._normalize_permissions(
            manifest.raw.get("required_permissions")
        )
        missing_permissions = [
            permission for permission in assessment.manifest_required_permissions if permission not in tool_permissions
        ]
        if missing_permissions:
            self._add_violation(
                assessment,
                f"tool.required_permissions が plugin manifest の必須権限を満たしていません: {missing_permissions}",
            )

        if plugin_version and plugin_version != manifest.version:
            self._add_violation(
                assessment,
                f"tool.plugin_version({plugin_version}) と manifest.version({manifest.version}) が一致しません",
            )

        if not PluginRegistry._is_kernel_compatible_static(
            manifest.compatibility_kernel,
            self._kernel_version,
        ):
            self._add_violation(
                assessment,
                (
                    f"plugin '{manifest.id}' は kernel={manifest.compatibility_kernel} を要求しますが "
                    f"現在の kernel は {self._kernel_version} です"
                ),
            )

        if manifest.compatibility_product_lines and assessment.product_line not in manifest.compatibility_product_lines:
            self._add_violation(
                assessment,
                f"plugin '{manifest.id}' は product_line={manifest.compatibility_product_lines} のみ対応です",
            )

    @staticmethod
    def _add_violation(assessment: PluginRuntimeAssessment, message: str) -> None:
        if assessment.strict_mode:
            assessment.errors.append(message)
        else:
            assessment.warnings.append(message)


class PluginRegistry:
    """plugin manifest を読み込み、実行時照合を行うレジストリ."""

    def __init__(
        self,
        *,
        plugins_dir: Path | None = None,
        apps_dir: Path | None = None,
        kernel_version: str | None = None,
        signature_verifier: PluginSignatureVerifier | None = None,
    ) -> None:
        self._plugins_dir = plugins_dir or self._resolve_plugins_dir()
        self._apps_dir = apps_dir or (Path.cwd() / "apps")
        self._kernel_version = kernel_version or self._resolve_kernel_version()
        trust_store_override = os.getenv("AGENTFLOW_PLUGIN_TRUST_STORE")
        self._signature_verifier = signature_verifier or PluginSignatureVerifier(
            trust_store_path=Path(trust_store_override)
            if trust_store_override
            else self._plugins_dir / "trust_store.json",
        )
        self._signature_enforcement = self._resolve_signature_enforcement()
        self._manifest_loader = PluginManifestLoader(
            plugins_dir=self._plugins_dir,
            signature_verifier=self._signature_verifier,
        )
        self._binding_resolver = AppBindingResolver(self._apps_dir)
        self._policy_evaluator = PluginPolicyEvaluator(
            kernel_version=self._kernel_version,
            signature_enforcement=self._signature_enforcement,
        )
        self._manifests: dict[str, PluginManifestRecord] = {}
        self._app_snapshots: dict[str, AppPluginSnapshot | None] = {}
        self._load_manifests()

    def refresh(self) -> None:
        """manifest と app snapshot キャッシュを再読込する."""
        self._load_manifests()
        self._app_snapshots.clear()

    def get_manifest(self, plugin_id: str) -> PluginManifestRecord | None:
        """manifest を取得する."""
        return self._manifests.get(plugin_id)

    def evaluate_tool(
        self,
        tool: RegisteredToolLike,
        context: ToolExecutionContext,
    ) -> PluginRuntimeAssessment:
        """ツール実行時の plugin 評価を行う."""
        app_name = self._normalize_optional_text(context.app_name)
        snapshot = self._load_app_snapshot(app_name) if app_name else None
        product_line = self._resolve_product_line(context, snapshot)
        strict_mode = self.is_strict_product_line(product_line)
        operation_name = str(tool.operation_type.value).lower()
        is_side_effect_tool = operation_name in _SIDE_EFFECT_OPERATIONS
        plugin_id = self._normalize_optional_text(tool.plugin_id)
        plugin_version = self._normalize_optional_text(tool.plugin_version)

        assessment = PluginRuntimeAssessment(
            app_name=app_name,
            product_line=product_line,
            strict_mode=strict_mode,
            is_side_effect_tool=is_side_effect_tool,
            plugin_id=plugin_id,
            plugin_version=plugin_version,
        )

        if not is_side_effect_tool:
            return assessment

        if plugin_id is None:
            self._add_violation(
                assessment,
                "副作用ツールに plugin_id が設定されていません",
            )
            return assessment
        if plugin_version is None:
            self._add_violation(
                assessment,
                "副作用ツールに plugin_version が設定されていません",
            )

        manifest = self._manifests.get(plugin_id)
        assessment.manifest = manifest
        if manifest is None:
            self._add_violation(
                assessment,
                f"plugin manifest が見つかりません: {plugin_id}",
            )
            return assessment

        tool_permissions = self._normalize_permissions(tool.required_permissions)
        self._policy_evaluator.evaluate_manifest(
            assessment,
            manifest=manifest,
            plugin_version=plugin_version,
            tool_permissions=tool_permissions,
        )

        if app_name is None:
            if strict_mode:
                self._add_violation(
                    assessment,
                    "Studio 実行では app_name が必須です（plugin binding 照合不可）",
                )
            else:
                assessment.warnings.append(
                    "app_name が未指定のため plugin binding 照合をスキップしました",
                )
            return assessment

        if snapshot is None:
            self._add_violation(
                assessment,
                f"app_config が見つからず plugin binding を検証できません: {app_name}",
            )
            return assessment

        binding = snapshot.bindings.get(plugin_id)
        assessment.binding = binding
        if binding is None:
            self._add_violation(
                assessment,
                f"App '{app_name}' に plugin binding がありません: {plugin_id}",
            )
            return assessment

        if plugin_version and binding.version != plugin_version:
            self._add_violation(
                assessment,
                (f"App binding version({binding.version}) と tool.plugin_version({plugin_version}) が一致しません"),
            )
        if binding.version != manifest.version:
            self._add_violation(
                assessment,
                (f"App binding version({binding.version}) と manifest.version({manifest.version}) が一致しません"),
            )

        return assessment

    def _add_violation(self, assessment: PluginRuntimeAssessment, message: str) -> None:
        """Studio は error、Framework は warning として扱う."""
        if assessment.strict_mode:
            assessment.errors.append(message)
        else:
            assessment.warnings.append(message)

    @staticmethod
    def _resolve_signature_enforcement() -> str:
        """署名検証ポリシーを解決する."""
        explicit = os.getenv(_PLUGIN_SIGNATURE_ENFORCEMENT_ENV, "").strip().lower()
        if explicit:
            if explicit in _VALID_SIGNATURE_ENFORCEMENTS:
                return explicit
            _logger.warning(
                "未対応の %s=%s が指定されました。%s を使用します",
                _PLUGIN_SIGNATURE_ENFORCEMENT_ENV,
                explicit,
                _DEFAULT_PLUGIN_SIGNATURE_ENFORCEMENT,
            )
            return _DEFAULT_PLUGIN_SIGNATURE_ENFORCEMENT

        app_env = os.getenv(_APP_ENV_ENV, "dev").strip().lower()
        if app_env in {"staging", "stage", "production", "prod"}:
            return "deny"
        return _DEFAULT_PLUGIN_SIGNATURE_ENFORCEMENT

    def _load_manifests(self) -> None:
        """Load plugin manifests from the canonical packs root."""
        self._manifests = self._manifest_loader.load()

    def _load_app_snapshot(self, app_name: str) -> AppPluginSnapshot | None:
        """Load app plugin bindings from canonical app manifests."""
        cached = self._app_snapshots.get(app_name)
        if app_name in self._app_snapshots:
            return cached

        snapshot = self._binding_resolver.load_snapshot(app_name)
        self._app_snapshots[app_name] = snapshot
        return snapshot

    def _resolve_product_line(
        self,
        context: ToolExecutionContext,
        snapshot: AppPluginSnapshot | None,
    ) -> str:
        """実行時 product_line を解決する."""
        explicit = self._normalize_product_line(context.product_line)
        if explicit is not None:
            return explicit
        if snapshot is not None:
            return snapshot.product_line
        return "framework"

    @staticmethod
    def _normalize_optional_text(value: object) -> str | None:
        if not isinstance(value, str):
            return None
        text = value.strip()
        return text if text else None

    @staticmethod
    def _normalize_product_line(value: object) -> str | None:
        if not isinstance(value, str):
            return None
        token = value.strip().lower()
        if token not in _SUPPORTED_PRODUCT_LINES:
            return None
        return token

    @staticmethod
    def _normalize_product_lines(raw: object) -> list[str]:
        if not isinstance(raw, list):
            return []
        lines: list[str] = []
        seen: set[str] = set()
        for item in raw:
            if not isinstance(item, str):
                continue
            line = item.strip().lower()
            if not line or line in seen:
                continue
            seen.add(line)
            lines.append(line)
        return lines

    @staticmethod
    def _normalize_permissions(raw: object) -> list[str]:
        if not isinstance(raw, list):
            return []
        permissions: list[str] = []
        seen: set[str] = set()
        for item in raw:
            if not isinstance(item, str):
                continue
            permission = item.strip()
            if not permission or permission in seen:
                continue
            seen.add(permission)
            permissions.append(permission)
        return permissions

    @staticmethod
    def is_strict_product_line(product_line: str) -> bool:
        """Studio 厳格モード対象の product_line かどうか."""
        return product_line in _STRICT_PRODUCT_LINES

    @staticmethod
    def _resolve_kernel_version() -> str:
        explicit = os.getenv("AGENTFLOW_KERNEL_VERSION", "").strip()
        if explicit:
            return explicit

        pyproject_path = Path.cwd() / "pyproject.toml"
        if pyproject_path.is_file():
            try:
                with pyproject_path.open("rb") as handle:
                    pyproject = tomllib.load(handle)
                project = pyproject.get("project")
                if isinstance(project, dict):
                    pyproject_version = project.get("version")
                    if isinstance(pyproject_version, str) and pyproject_version.strip():
                        return pyproject_version.strip()
            except Exception:
                _logger.debug("pyproject.toml から kernel version を解決できませんでした", exc_info=True)

        try:
            return version("bizcore")
        except PackageNotFoundError:
            return "2.0.0"

    def _is_kernel_compatible(self, constraint: str, current_version: str) -> bool:
        """SemVer 制約との互換性を判定する."""
        return self._is_kernel_compatible_static(constraint, current_version)

    @staticmethod
    def _is_kernel_compatible_static(constraint: str, current_version: str) -> bool:
        """SemVer 制約との互換性を判定する."""
        if not constraint.strip():
            return True

        for token in constraint.split(","):
            term = token.strip()
            if not term:
                continue
            match = _KERNEL_CONSTRAINT_RE.match(term)
            if match is None:
                _logger.warning("未知の kernel 制約形式: %s", term)
                return False
            operator = match.group(1) or "=="
            expected = match.group(2)
            comparison = PluginRegistry._compare_semver(current_version, expected)
            if comparison is None:
                return False
            if operator == "==" and comparison != 0:
                return False
            if operator == ">=" and comparison < 0:
                return False
            if operator == ">" and comparison <= 0:
                return False
            if operator == "<=" and comparison > 0:
                return False
            if operator == "<" and comparison >= 0:
                return False
        return True

    @staticmethod
    def _resolve_plugins_dir() -> Path:
        explicit = os.getenv("AGENTFLOW_PLUGIN_PACKS_DIR", "").strip()
        if explicit:
            return Path(explicit)

        repo_root = Path.cwd()
        canonical_dir = repo_root / _CANONICAL_PLUGIN_PACKS_DIR
        if canonical_dir.is_dir():
            return canonical_dir
        return repo_root / _LEGACY_PLUGIN_PACKS_DIR

    @staticmethod
    def _compare_semver(left: str, right: str) -> int | None:
        left_parts = PluginRegistry._parse_semver(left)
        right_parts = PluginRegistry._parse_semver(right)
        if left_parts is None or right_parts is None:
            return None
        if left_parts < right_parts:
            return -1
        if left_parts > right_parts:
            return 1
        return 0

    @staticmethod
    def _parse_semver(value: str) -> tuple[int, int, int] | None:
        match = _SEMVER_PART_RE.match(value.strip())
        if match is None:
            return None
        return (int(match.group(1)), int(match.group(2)), int(match.group(3)))


__all__ = [
    "AppPluginSnapshot",
    "PluginBindingRecord",
    "PluginManifestRecord",
    "PluginRegistry",
    "PluginRuntimeAssessment",
]
