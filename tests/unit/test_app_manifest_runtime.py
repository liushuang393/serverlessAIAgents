"""Tests for manifest-driven runtime resolution."""

from __future__ import annotations

import json
from typing import TYPE_CHECKING

from agentflow.runtime import resolve_app_runtime


if TYPE_CHECKING:
    from pathlib import Path


def test_resolve_app_runtime_uses_manifest_hosts_ports_and_urls(tmp_path: Path) -> None:
    config_path = tmp_path / "app_config.json"
    config_path.write_text(
        json.dumps(
            {
                "name": "demo_app",
                "ports": {"api": 8100, "frontend": 3100, "db": None, "redis": None},
                "runtime": {
                    "urls": {
                        "backend": "http://localhost:8100",
                        "frontend": "http://localhost:3100",
                        "health": "http://localhost:8100/health",
                        "database": None,
                    },
                    "hosts": {
                        "backend": "0.0.0.0",
                        "frontend": "127.0.0.1",
                    },
                },
            }
        ),
        encoding="utf-8",
    )

    runtime = resolve_app_runtime(config_path)

    assert runtime.app_name == "demo_app"
    assert runtime.ports.api == 8100
    assert runtime.ports.frontend == 3100
    assert runtime.hosts.backend == "0.0.0.0"
    assert runtime.hosts.frontend == "127.0.0.1"
    assert runtime.urls.health == "http://localhost:8100/health"


def test_resolve_app_runtime_applies_env_port_override_to_urls(tmp_path: Path) -> None:
    config_path = tmp_path / "app_config.json"
    config_path.write_text(
        json.dumps(
            {
                "name": "demo_app",
                "ports": {"api": 8010, "frontend": 3010, "db": None, "redis": None},
                "runtime": {
                    "urls": {
                        "backend": "http://localhost:8010",
                        "frontend": "http://localhost:3010",
                        "health": "http://localhost:8010/health",
                        "database": None,
                    },
                    "hosts": {
                        "backend": "0.0.0.0",
                        "frontend": "0.0.0.0",
                    },
                },
            }
        ),
        encoding="utf-8",
    )

    runtime = resolve_app_runtime(
        config_path,
        env={"APP_PORT": "8900", "APP_HOST": "127.0.0.1"},
        backend_port_env="APP_PORT",
        backend_host_env="APP_HOST",
    )

    assert runtime.ports.api == 8900
    assert runtime.hosts.backend == "127.0.0.1"
    assert runtime.urls.backend == "http://localhost:8900"
    assert runtime.urls.health == "http://localhost:8900/health"
