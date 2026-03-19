"""Control-plane service public API tests."""

from __future__ import annotations

import json
from pathlib import Path

import pytest
from control_plane import AppRegistryService, DiscoveryService, LifecycleService

from shared import load_app_manifest
from shared.config.manifest import load_app_manifest_dict
from shared.config.manifest import load_app_manifest_dict_payload
from shared.config.manifest import load_app_manifest_dict_text
from shared.config.manifest import load_app_manifest_payload
from shared.config.manifest import load_app_manifest_text

from contracts import AppManifest


def test_load_app_manifest_maps_app_config_to_contract(tmp_path) -> None:
    """app_config.json を AppManifest 契約へ読み替えられること."""
    app_dir = tmp_path / "apps" / "demo_app"
    app_dir.mkdir(parents=True)
    config_path = app_dir / "app_config.json"
    config_path.write_text(
        json.dumps(
            {
                "name": "demo_app",
                "display_name": "Demo App",
                "product_line": "migration",
                "surface_profile": "business",
                "audit_profile": "business",
                "version": "1.2.3",
                "ports": {"api": 8123},
                "runtime": {"commands": {"start": "python -m demo"}, "urls": {"health": "http://localhost:8123/health"}},
            }
        ),
        encoding="utf-8",
    )

    manifest = load_app_manifest(config_path)

    assert manifest.name == "demo_app"
    assert manifest.ports.api == 8123


def test_discovery_service_scans_apps_directory(tmp_path) -> None:
    """DiscoveryService が apps ディレクトリを走査すること."""
    app_dir = tmp_path / "apps" / "demo_app"
    app_dir.mkdir(parents=True)
    (app_dir / "app_config.json").write_text(
        json.dumps(
            {
                "name": "demo_app",
                "display_name": "Demo App",
                "product_line": "faq",
                "surface_profile": "business",
                "audit_profile": "business",
            }
        ),
        encoding="utf-8",
    )

    service = DiscoveryService(tmp_path / "apps")
    manifests = service.scan()

    assert [manifest.name for manifest in manifests] == ["demo_app"]


def test_app_registry_service_syncs_manifests() -> None:
    """AppRegistryService が manifest を保持すること."""
    manifest = AppManifest(
        name="demo_app",
        display_name="Demo App",
        product_line="assistant",
        surface_profile="business",
        audit_profile="business",
        security_mode="approval_required",
    )
    registry = AppRegistryService()

    registry.sync([manifest])

    assert registry.get("demo_app") is not None
    assert registry.list()[0].display_name == "Demo App"


def test_lifecycle_service_builds_command_plan() -> None:
    """manifest の runtime.commands からコマンド計画を組むこと."""
    manifest = AppManifest(
        name="demo_app",
        display_name="Demo App",
        product_line="framework",
        surface_profile="operator",
        audit_profile="developer",
        runtime={"commands": {"start": "python -m demo.start", "stop": "pkill -f demo"}},
    )
    service = LifecycleService()

    assert service.build_command_plan(manifest, "start") == ["python -m demo.start"]
    assert service.build_command_plan(manifest, "stop") == ["pkill -f demo"]


def test_control_plane_package_exports_public_services() -> None:
    """control_plane package が公開サービスをエクスポートすること."""
    assert AppRegistryService is not None
    assert DiscoveryService is not None
    assert LifecycleService is not None


def test_load_app_manifest_preserves_runtime_blueprint_and_plugin_bindings(tmp_path: Path) -> None:
    """canonical manifest loader が runtime/blueprint/plugin を保持すること."""
    app_dir = tmp_path / "apps" / "demo_app"
    app_dir.mkdir(parents=True)
    config_path = app_dir / "app_config.json"
    config_path.write_text(
        json.dumps(
            {
                "name": "demo_app",
                "display_name": "Demo App",
                "product_line": "migration",
                "surface_profile": "business",
                "audit_profile": "business",
                "plugin_bindings": [{"id": "Official.Test-Pack", "version": "1.0.0"}],
                "runtime": {
                    "hosts": {"backend": "0.0.0.0"},
                    "commands": {"start": "python -m demo.start"},
                },
                "blueprint": {"engine_pattern": "pipeline"},
                "visibility": {"mode": "private", "tenants": []},
            }
        ),
        encoding="utf-8",
    )

    manifest = load_app_manifest(config_path)

    assert manifest.runtime.hosts.backend == "0.0.0.0"
    assert manifest.runtime.commands.start == "python -m demo.start"
    assert manifest.blueprint.engine_pattern == "pipeline"
    assert manifest.plugin_bindings[0].id == "official.test-pack"
    assert manifest.metadata["config_path"] == str(config_path.resolve())


def test_manifest_loader_text_and_payload_match_path_output(tmp_path: Path) -> None:
    """path/text/payload 入口が同じ canonical manifest を返すこと."""
    app_dir = tmp_path / "apps" / "demo_app"
    app_dir.mkdir(parents=True)
    config_path = app_dir / "app_config.json"
    payload = {
        "display_name": "Demo App",
        "runtime": {"commands": {"start": "python -m demo.start"}},
        "blueprint": {"engine_pattern": "pipeline"},
        "plugin_bindings": [{"id": "Official.Test-Pack", "version": "1.0.0"}],
    }
    text = json.dumps(payload, ensure_ascii=False)
    config_path.write_text(text, encoding="utf-8")

    from_path = load_app_manifest(config_path)
    from_text = load_app_manifest_text(text, manifest_path=config_path)
    from_payload = load_app_manifest_payload(payload, manifest_path=config_path)

    assert from_path.model_dump(mode="python") == from_text.model_dump(mode="python")
    assert from_path.model_dump(mode="python") == from_payload.model_dump(mode="python")


def test_manifest_dict_loaders_return_consistent_canonical_dict(tmp_path: Path) -> None:
    """path/text/payload dict loader も同一の canonical dict を返すこと."""
    app_dir = tmp_path / "apps" / "demo_app"
    app_dir.mkdir(parents=True)
    config_path = app_dir / "app_config.json"
    payload = {
        "contracts": {"llm": {"provider": "openai"}},
        "visibility": {"mode": "private"},
        "tags": ["migration"],
    }
    text = json.dumps(payload, ensure_ascii=False)
    config_path.write_text(text, encoding="utf-8")

    from_path = load_app_manifest_dict(config_path)
    from_text = load_app_manifest_dict_text(text, manifest_path=config_path)
    from_payload = load_app_manifest_dict_payload(payload, manifest_path=config_path)

    assert from_path == from_text
    assert from_path == from_payload
    assert from_path["name"] == "demo_app"
    assert from_path["metadata"]["config_path"] == str(config_path.resolve())


def test_manifest_text_loader_raises_for_invalid_json(tmp_path: Path) -> None:
    """invalid JSON text は canonical loader で失敗すること."""
    config_path = tmp_path / "apps" / "demo_app" / "app_config.json"
    config_path.parent.mkdir(parents=True)

    with pytest.raises(json.JSONDecodeError):
        load_app_manifest_text("{invalid-json", manifest_path=config_path)
