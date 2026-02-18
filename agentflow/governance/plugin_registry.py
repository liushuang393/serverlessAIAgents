"""Plugin manifest 実行時ガバナンスレジストリ.

Studio 実行時に、ツールの plugin 情報と app_config の plugin_bindings を突合して
実行可否判定に必要な情報を提供する。
"""

from __future__ import annotations

import json
import logging
import os
import re
from dataclasses import dataclass, field
from importlib.metadata import PackageNotFoundError, version
from pathlib import Path
from typing import TYPE_CHECKING, Any

from agentflow.governance.plugin_signature import (
    PluginSignatureVerifier,
    SignatureStatus,
)


if TYPE_CHECKING:
    from agentflow.governance.engine import ToolExecutionContext
    from agentflow.providers.tool_provider import RegisteredTool


_SEMVER_PART_RE = re.compile(r"^(\d+)\.(\d+)\.(\d+)")
_KERNEL_CONSTRAINT_RE = re.compile(r"^(>=|<=|==|>|<)?\s*([0-9A-Za-z.+-]+)$")
_SUPPORTED_PRODUCT_LINES = {"migration", "faq", "assistant", "framework"}
_STRICT_PRODUCT_LINES = {"migration", "faq", "assistant"}
_SIDE_EFFECT_OPERATIONS = {"write", "delete", "execute"}
_PLUGIN_SIGNATURE_ENFORCEMENT_ENV = "AGENTFLOW_PLUGIN_SIGNATURE_ENFORCEMENT"
_DEFAULT_PLUGIN_SIGNATURE_ENFORCEMENT = "warn"


_logger = logging.getLogger(__name__)


@dataclass(slots=True)
class PluginManifestRecord:
    """plugin_manifest.json の最小参照情報."""

    id: str
    version: str
    risk_tier: str
    compatibility_kernel: str
    compatibility_product_lines: list[str] = field(default_factory=list)
    manifest_path: Path | None = None
    signature_status: SignatureStatus = "parse_error"
    signature_reason: str = ""
    raw: dict[str, Any] = field(default_factory=dict)


@dataclass(slots=True)
class PluginBindingRecord:
    """app_config.json plugin_bindings エントリ."""

    id: str
    version: str
    config: dict[str, Any] = field(default_factory=dict)


@dataclass(slots=True)
class AppPluginSnapshot:
    """App 単位の plugin バインディング情報."""

    app_name: str
    product_line: str
    bindings: dict[str, PluginBindingRecord]


@dataclass(slots=True)
class PluginRuntimeAssessment:
    """ツール実行時 plugin 評価結果."""

    app_name: str | None
    product_line: str
    strict_mode: bool
    is_side_effect_tool: bool
    plugin_id: str | None
    plugin_version: str | None
    plugin_risk_tier: str | None = None
    plugin_signature_status: SignatureStatus | None = None
    plugin_signature_reason: str | None = None
    manifest_required_permissions: list[str] = field(default_factory=list)
    manifest: PluginManifestRecord | None = None
    binding: PluginBindingRecord | None = None
    errors: list[str] = field(default_factory=list)
    warnings: list[str] = field(default_factory=list)


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
        self._plugins_dir = plugins_dir or (Path.cwd() / "plugins")
        self._apps_dir = apps_dir or (Path.cwd() / "apps")
        self._kernel_version = kernel_version or self._resolve_kernel_version()
        trust_store_override = os.getenv("AGENTFLOW_PLUGIN_TRUST_STORE")
        self._signature_verifier = signature_verifier or PluginSignatureVerifier(
            trust_store_path=Path(trust_store_override)
            if trust_store_override
            else self._plugins_dir / "trust_store.json",
        )
        self._signature_enforcement = self._resolve_signature_enforcement()
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
        tool: RegisteredTool,
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

        assessment.plugin_risk_tier = manifest.risk_tier
        assessment.plugin_signature_status = manifest.signature_status
        assessment.plugin_signature_reason = manifest.signature_reason
        if (
            self._signature_enforcement == "warn"
            and manifest.signature_status != "verified"
        ):
            assessment.warnings.append(
                "plugin 署名検証 warning: "
                f"status={manifest.signature_status}, reason={manifest.signature_reason}",
            )
        assessment.manifest_required_permissions = self._normalize_permissions(
            manifest.raw.get("required_permissions"),
        )

        tool_permissions = self._normalize_permissions(tool.required_permissions)
        missing_permissions = [
            perm
            for perm in assessment.manifest_required_permissions
            if perm not in tool_permissions
        ]
        if missing_permissions:
            self._add_violation(
                assessment,
                (
                    "tool.required_permissions が plugin manifest の必須権限を満たしていません: "
                    f"{missing_permissions}"
                ),
            )

        if plugin_version and plugin_version != manifest.version:
            self._add_violation(
                assessment,
                (
                    f"tool.plugin_version({plugin_version}) と "
                    f"manifest.version({manifest.version}) が一致しません"
                ),
            )

        if not self._is_kernel_compatible(
            manifest.compatibility_kernel,
            self._kernel_version,
        ):
            self._add_violation(
                assessment,
                (
                    f"plugin '{plugin_id}' は kernel={manifest.compatibility_kernel} を要求しますが "
                    f"現在の kernel は {self._kernel_version} です"
                ),
            )

        if (
            manifest.compatibility_product_lines
            and product_line not in manifest.compatibility_product_lines
        ):
            self._add_violation(
                assessment,
                (
                    f"plugin '{plugin_id}' は product_line="
                    f"{manifest.compatibility_product_lines} のみ対応です"
                ),
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
                (
                    f"App binding version({binding.version}) と "
                    f"tool.plugin_version({plugin_version}) が一致しません"
                ),
            )
        if binding.version != manifest.version:
            self._add_violation(
                assessment,
                (
                    f"App binding version({binding.version}) と "
                    f"manifest.version({manifest.version}) が一致しません"
                ),
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
        """署名検証ポリシーを解決する（P1 は warn 固定運用）。"""
        raw = os.getenv(
            _PLUGIN_SIGNATURE_ENFORCEMENT_ENV,
            _DEFAULT_PLUGIN_SIGNATURE_ENFORCEMENT,
        ).strip().lower()
        if raw != _DEFAULT_PLUGIN_SIGNATURE_ENFORCEMENT:
            _logger.warning(
                "未対応の %s=%s が指定されました。P1 では warn 固定運用です",
                _PLUGIN_SIGNATURE_ENFORCEMENT_ENV,
                raw,
            )
        return _DEFAULT_PLUGIN_SIGNATURE_ENFORCEMENT

    def _load_manifests(self) -> None:
        """plugins/*/plugin_manifest.json を読み込む."""
        self._manifests.clear()
        if not self._plugins_dir.is_dir():
            _logger.debug("plugins ディレクトリが見つかりません: %s", self._plugins_dir)
            return

        for manifest_path in sorted(self._plugins_dir.glob("*/plugin_manifest.json")):
            try:
                raw = json.loads(manifest_path.read_text("utf-8"))
                plugin_id = self._normalize_optional_text(raw.get("id"))
                plugin_version = self._normalize_optional_text(raw.get("version"))
                risk_tier = self._normalize_optional_text(raw.get("risk_tier"))
                compatibility = raw.get("compatibility")
                if (
                    plugin_id is None
                    or plugin_version is None
                    or risk_tier is None
                    or not isinstance(compatibility, dict)
                ):
                    _logger.warning("plugin manifest 必須項目不足: %s", manifest_path)
                    continue

                kernel_constraint = (
                    self._normalize_optional_text(compatibility.get("kernel")) or ""
                )
                product_lines = self._normalize_product_lines(
                    compatibility.get("product_lines"),
                )
                signature_result = self._signature_verifier.verify_manifest(
                    manifest=raw,
                    manifest_path=manifest_path,
                )
                self._manifests[plugin_id] = PluginManifestRecord(
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
            except Exception as exc:  # noqa: BLE001
                _logger.warning("plugin manifest 読み込み失敗 (%s): %s", manifest_path, exc)

    def _load_app_snapshot(self, app_name: str) -> AppPluginSnapshot | None:
        """apps/<app>/app_config.json から plugin binding を読み込む."""
        cached = self._app_snapshots.get(app_name)
        if app_name in self._app_snapshots:
            return cached

        config_path = self._apps_dir / app_name / "app_config.json"
        if not config_path.is_file():
            self._app_snapshots[app_name] = None
            return None

        try:
            raw = json.loads(config_path.read_text("utf-8"))
            product_line = self._normalize_product_line(raw.get("product_line")) or "framework"
            bindings_raw = raw.get("plugin_bindings", [])
            bindings: dict[str, PluginBindingRecord] = {}
            if isinstance(bindings_raw, list):
                for item in bindings_raw:
                    if not isinstance(item, dict):
                        continue
                    binding_id = self._normalize_optional_text(item.get("id"))
                    binding_version = self._normalize_optional_text(item.get("version"))
                    if binding_id is None or binding_version is None:
                        continue
                    bindings[binding_id] = PluginBindingRecord(
                        id=binding_id,
                        version=binding_version,
                        config=item.get("config", {}) if isinstance(item.get("config"), dict) else {},
                    )
            snapshot = AppPluginSnapshot(
                app_name=app_name,
                product_line=product_line,
                bindings=bindings,
            )
            self._app_snapshots[app_name] = snapshot
            return snapshot
        except Exception as exc:  # noqa: BLE001
            _logger.warning("app_config 読み込み失敗 (%s): %s", config_path, exc)
            self._app_snapshots[app_name] = None
            return None

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
        try:
            return version("agentflow")
        except PackageNotFoundError:
            return "2.0.0"

    def _is_kernel_compatible(self, constraint: str, current_version: str) -> bool:
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
            comparison = self._compare_semver(current_version, expected)
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
