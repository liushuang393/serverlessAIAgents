"""Public control-plane facades should delegate to canonical services."""

from __future__ import annotations

from pathlib import Path

from contracts import AppManifest
from control_plane.discovery.service import DiscoveryService
from control_plane.lifecycle.service import LifecycleService


class _StubDiscoveryService:
    def __init__(self) -> None:
        self.scanned = False

    async def scan(self) -> int:
        self.scanned = True
        return 1

    def list_apps(self) -> list[AppManifest]:
        return [
            AppManifest(
                name="demo_app",
                display_name="Demo App",
                product_line="framework",
                surface_profile="developer",
                audit_profile="developer",
                plugin_bindings=[],
            )
        ]


def test_discovery_facade_delegates_to_canonical_service() -> None:
    """DiscoveryService facade should delegate to AppDiscoveryService."""
    facade = DiscoveryService(Path("apps"))
    facade._service = _StubDiscoveryService()  # type: ignore[assignment]

    manifests = facade.scan()

    assert len(manifests) == 1
    assert manifests[0].name == "demo_app"
    assert facade._service.scanned is True  # type: ignore[attr-defined]


def test_lifecycle_facade_build_command_plan_uses_manifest_metadata(tmp_path: Path) -> None:
    """Lifecycle facade should resolve commands from the canonical manifest metadata path."""
    app_dir = tmp_path / "demo_app"
    app_dir.mkdir()
    readme_path = app_dir / "README.md"
    readme_path.write_text(
        "```bash\npython -m apps.demo_app.main\n```\n",
        encoding="utf-8",
    )
    config_path = app_dir / "app_config.json"
    config_path.write_text("{}", encoding="utf-8")

    manifest = AppManifest(
        name="demo_app",
        display_name="Demo App",
        product_line="framework",
        surface_profile="developer",
        audit_profile="developer",
        plugin_bindings=[],
        runtime={"commands": {"start": "python -m fallback.demo"}},
        metadata={"config_path": str(config_path)},
    )

    facade = LifecycleService()

    assert facade.build_command_plan(manifest, "start") == ["python -m fallback.demo"]
