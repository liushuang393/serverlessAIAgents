"""Control-plane service public API tests."""

from __future__ import annotations

import json
from control_plane import AppRegistryService, DiscoveryService, LifecycleService

from shared import load_app_manifest

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
    assert manifest.ports["api"] == 8123


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
