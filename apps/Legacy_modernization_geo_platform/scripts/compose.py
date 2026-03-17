#!/usr/bin/env python3
"""Manifest-driven Docker wrapper for the GEO platform."""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

from kernel.runtime import resolve_app_runtime


APP_DIR = Path(__file__).resolve().parents[1]
APP_CONFIG_PATH = APP_DIR / "app_config.json"
COMPOSE_PATH = APP_DIR / "docker-compose.yml"


def main() -> int:
    """Run the requested compose action."""
    if len(sys.argv) != 2 or sys.argv[1] not in {"publish", "start", "stop"}:
        print("usage: compose.py [publish|start|stop]", file=sys.stderr)
        return 2

    action = sys.argv[1]
    runtime = resolve_app_runtime(
        APP_CONFIG_PATH,
        env=os.environ,
        backend_host_env="GEO_PLATFORM_HOST",
        backend_port_env="GEO_PLATFORM_PORT",
        backend_url_env="GEO_PLATFORM_PUBLIC_BASE_URL",
    )
    env = os.environ.copy()
    if runtime.hosts.backend is not None:
        env.setdefault("GEO_PLATFORM_HOST", runtime.hosts.backend)
    if runtime.ports.api is not None:
        env.setdefault("GEO_PLATFORM_PORT", str(runtime.ports.api))
    if runtime.urls.backend is not None:
        env.setdefault("GEO_PLATFORM_PUBLIC_BASE_URL", runtime.urls.backend)

    command = ["docker", "compose", "-f", str(COMPOSE_PATH)]
    if action == "publish":
        command.extend(["up", "-d", "--build"])
    elif action == "start":
        command.extend(["up", "-d"])
    else:
        command.append("down")

    result = subprocess.run(command, cwd=str(APP_DIR), env=env, check=False)
    return result.returncode


if __name__ == "__main__":
    raise SystemExit(main())
