"""FrameworkAuditService の追加ルール検証テスト."""

from __future__ import annotations

from copy import deepcopy
from typing import TYPE_CHECKING

from apps.platform.schemas.app_config_schemas import AppConfig
from apps.platform.services.app_discovery import AppDiscoveryService
from apps.platform.services.framework_audit import FrameworkAuditService


if TYPE_CHECKING:
    from pathlib import Path


def _base_manifest() -> dict:
    """監査テスト用の最小 manifest を返す."""
    return {
        "name": "sample_app",
        "display_name": "Sample App",
        "business_base": "operations",
        "product_line": "framework",
        "surface_profile": "developer",
        "audit_profile": "developer",
        "plugin_bindings": [],
        "version": "1.0.0",
        "ports": {"api": None, "frontend": None, "db": None, "redis": None},
        "entry_points": {"api_module": None, "health": None},
        "agents": [
            {
                "name": "CoordinatorAgent",
                "module": "local.coordinator",
                "capabilities": ["coordination"],
                "pattern": "coordinator",
            },
            {
                "name": "WorkerAgent",
                "module": "local.worker",
                "capabilities": ["execution"],
                "pattern": "executor",
            },
        ],
        "services": {},
        "dependencies": {"database": None, "redis": False, "external": []},
        "runtime": {
            "database": {
                "kind": None,
                "url": None,
                "host": None,
                "port": None,
                "name": None,
                "user": None,
                "password": None,
                "password_env": "DB_PASSWORD",
                "note": None,
            },
        },
        "contracts": {
            "auth": {"enabled": False, "allow_anonymous": True},
            "rag": {"enabled": False, "collections": []},
            "skills": {"default_skills": []},
        },
        "blueprint": {
            "engine_pattern": "coordinator",
            "default_skills": [],
            "mcp_servers": [],
        },
        "visibility": {"mode": "private", "tenants": []},
    }


def _audit_service(tmp_path: Path) -> FrameworkAuditService:
    """テスト用監査サービスを構築."""
    discovery = AppDiscoveryService(apps_dir=tmp_path)
    return FrameworkAuditService(discovery)


def test_contract_consistency_detects_rag_and_mcp_mismatch(tmp_path: Path) -> None:
    """RAG/MCP 契約の齟齬を検出できる."""
    service = _audit_service(tmp_path)
    manifest = _base_manifest()
    manifest["contracts"]["rag"] = {"enabled": True, "collections": ["contract_docs"]}
    manifest["services"] = {
        "rag": {"collections": ["service_docs"]},
        "mcp": {"tools": ["search_docs"]},
    }

    config = AppConfig.model_validate(manifest)
    issues = service._check_contract_consistency(config)
    codes = {issue.code for issue in issues}

    assert "RAG_COLLECTION_MISMATCH" in codes
    assert "MCP_SERVICE_WITHOUT_SERVER_BINDING" in codes


def test_orchestration_protocols_warn_when_stream_and_a2a_missing(tmp_path: Path) -> None:
    """coordinator 編成で stream/A2A がなければ警告を出す."""
    service = _audit_service(tmp_path)
    config = AppConfig.model_validate(_base_manifest())

    issues = service._check_orchestration_protocols(
        config,
        source_text="class CoordinatorAgent:\n    pass\n",
        audit_profile="developer",
    )
    codes = {issue.code for issue in issues}

    assert "ORCHESTRATION_STREAM_SURFACE_MISSING" in codes
    assert "A2A_SURFACE_NOT_FOUND" in codes


def test_orchestration_protocols_business_profile_skips_protocol_surface_checks(
    tmp_path: Path,
) -> None:
    """business profile では stream/A2A/MCP の面チェックを強制しない."""
    service = _audit_service(tmp_path)
    manifest = _base_manifest()
    manifest["services"] = {"mcp": {"tools": ["search_docs"]}}
    manifest["blueprint"]["mcp_servers"] = ["filesystem"]
    config = AppConfig.model_validate(manifest)

    issues = service._check_orchestration_protocols(
        config,
        source_text="class CoordinatorAgent:\n    pass\n",
        audit_profile="business",
    )
    codes = {issue.code for issue in issues}
    assert "ORCHESTRATION_STREAM_SURFACE_MISSING" not in codes
    assert "A2A_SURFACE_NOT_FOUND" not in codes
    assert "MCP_DECLARED_BUT_SURFACE_MISSING" not in codes


def test_security_baseline_detects_plaintext_password_and_anonymous_external(
    tmp_path: Path,
) -> None:
    """平文パスワードと匿名外部公開の組み合わせを検出."""
    service = _audit_service(tmp_path)
    manifest = _base_manifest()
    manifest["runtime"]["database"]["password"] = "dev-secret"
    manifest["dependencies"]["external"] = ["slack_api"]

    config = AppConfig.model_validate(manifest)
    issues = service._check_security_baseline(config)
    codes = {issue.code for issue in issues}

    assert "PLAINTEXT_DB_PASSWORD_IN_MANIFEST" in codes
    assert "EXTERNAL_DEP_WITH_ANONYMOUS_ACCESS" in codes


def test_entry_points_reports_missing_api_module(tmp_path: Path) -> None:
    """api_module が不正な場合に error を返す."""
    service = _audit_service(tmp_path)
    manifest = deepcopy(_base_manifest())
    manifest["ports"]["api"] = 8010
    manifest["entry_points"]["api_module"] = "apps.not_exist.main:app"

    config = AppConfig.model_validate(manifest)
    issues = service._check_entry_points(config, app_dir=None, source_text="")
    codes = {issue.code for issue in issues}

    assert "API_MODULE_NOT_FOUND" in codes


def test_auth_runtime_enforcement_warns_when_guard_missing(tmp_path: Path) -> None:
    """auth 必須宣言なのに実装ガードがなければ警告を返す."""
    service = _audit_service(tmp_path)
    manifest = deepcopy(_base_manifest())
    manifest["contracts"]["auth"] = {
        "enabled": True,
        "allow_anonymous": False,
    }
    config = AppConfig.model_validate(manifest)

    issues = service._check_auth_runtime_enforcement(
        config,
        source_text="def endpoint():\n    pass\n",
    )
    codes = {issue.code for issue in issues}
    assert "AUTH_CONTRACT_WITHOUT_RUNTIME_GUARD" in codes


def test_auth_runtime_enforcement_accepts_api_key_guard(tmp_path: Path) -> None:
    """API key 検証シグナルがあれば警告しない."""
    service = _audit_service(tmp_path)
    manifest = deepcopy(_base_manifest())
    manifest["contracts"]["auth"] = {
        "enabled": True,
        "allow_anonymous": False,
    }
    config = AppConfig.model_validate(manifest)

    issues = service._check_auth_runtime_enforcement(
        config,
        source_text="def _require_api_key(request):\n    pass\n",
    )
    codes = {issue.code for issue in issues}
    assert "AUTH_CONTRACT_WITHOUT_RUNTIME_GUARD" not in codes
