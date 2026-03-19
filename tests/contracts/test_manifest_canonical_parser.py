"""Canonical manifest parser wiring tests."""

from __future__ import annotations

import ast
from pathlib import Path


REPO_ROOT = Path(__file__).resolve().parents[2]


def _module_uses_symbol(path: str, *, module: str, symbol: str) -> bool:
    file_path = REPO_ROOT / path
    tree = ast.parse(file_path.read_text(encoding="utf-8"), filename=str(file_path))
    for node in ast.walk(tree):
        if isinstance(node, ast.ImportFrom) and node.module == module:
            if any(alias.name == symbol for alias in node.names):
                return True
    return False


def test_runtime_manifest_resolution_uses_canonical_loader() -> None:
    """kernel runtime manifest 解決は canonical loader を参照する."""
    assert _module_uses_symbol(
        "kernel/runtime/app_manifest.py",
        module="shared.config.manifest",
        symbol="load_app_manifest",
    )


def test_control_plane_app_discovery_uses_canonical_loader() -> None:
    """control plane discovery は canonical loader を参照する."""
    assert _module_uses_symbol(
        "control_plane/services/app_discovery.py",
        module="shared.config.manifest",
        symbol="load_app_manifest",
    )


def test_contract_auth_guard_uses_canonical_loader() -> None:
    """Auth guard も canonical loader を参照する."""
    assert _module_uses_symbol(
        "harness/gating/contract_auth_guard.py",
        module="shared.config.manifest",
        symbol="load_app_manifest_dict",
    )


def test_migration_backend_entry_uses_canonical_loader() -> None:
    """Migration Studio backend 起動入口も canonical loader を参照する."""
    assert _module_uses_symbol(
        "apps/code_migration_assistant/backend/app.py",
        module="shared.config.manifest",
        symbol="load_app_manifest",
    )


def test_orchestration_guardian_uses_canonical_loader() -> None:
    """Orchestration Guardian も canonical loader を参照する."""
    assert _module_uses_symbol(
        "apps/orchestration_guardian/main.py",
        module="shared.config.manifest",
        symbol="load_app_manifest_dict",
    )


def test_faq_system_uses_canonical_loader() -> None:
    """FAQ System も canonical loader を参照する."""
    assert _module_uses_symbol(
        "apps/faq_system/main.py",
        module="shared.config.manifest",
        symbol="load_app_manifest_dict",
    )


def test_market_trend_monitor_uses_canonical_loader() -> None:
    """Market Trend Monitor 設定も canonical loader を参照する."""
    assert _module_uses_symbol(
        "apps/market_trend_monitor/backend/config.py",
        module="shared.config.manifest",
        symbol="load_app_manifest_dict",
    )


def test_app_bootstrapper_uses_canonical_text_loader() -> None:
    """bootstrapper は decoded text を canonical dict loader に流す."""
    assert _module_uses_symbol(
        "control_plane/bootstrap/app_bootstrapper.py",
        module="shared.config.manifest",
        symbol="load_app_manifest_dict_text",
    )


def test_code_migration_api_uses_contract_auth_guard() -> None:
    """Code Migration API は auth guard 経由で canonical app_config を読む."""
    assert _module_uses_symbol(
        "apps/code_migration_assistant/api.py",
        module="harness.gating.contract_auth_guard",
        symbol="ContractAuthGuard",
    )


def test_design_skills_engine_uses_contract_auth_guard() -> None:
    """Design Skills Engine は auth guard 経由で canonical app_config を読む."""
    assert _module_uses_symbol(
        "apps/design_skills_engine/main.py",
        module="harness.gating.contract_auth_guard",
        symbol="ContractAuthGuard",
    )


def test_messaging_hub_uses_contract_auth_guard() -> None:
    """Messaging Hub は auth guard 経由で canonical app_config を読む."""
    assert _module_uses_symbol(
        "apps/messaging_hub/main.py",
        module="harness.gating.contract_auth_guard",
        symbol="ContractAuthGuard",
    )
