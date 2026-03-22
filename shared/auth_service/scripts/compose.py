#!/usr/bin/env python3
"""Manifest-driven Docker wrapper for auth_service."""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path


def _resolve_app_runtime_lazy(*args: object, **kwargs: object) -> object:
    """遅延インポート: kernel.runtime.resolve_app_runtime（L2→L3 違反回避）."""
    from kernel.runtime import resolve_app_runtime as _resolve

    return _resolve(*args, **kwargs)


APP_DIR = Path(__file__).resolve().parents[1]
APP_CONFIG_PATH = APP_DIR / "app_config.json"
COMPOSE_PATH = APP_DIR / "docker-compose.yml"


def main() -> int:
    """Run the requested compose action."""
    if len(sys.argv) != 2 or sys.argv[1] not in {"publish", "start", "stop"}:
        print("usage: compose.py [publish|start|stop]", file=sys.stderr)
        return 2

    action = sys.argv[1]
    runtime = _resolve_app_runtime_lazy(
        APP_CONFIG_PATH,
        env=os.environ,
        backend_host_env="AUTH_SERVICE_HOST",
        backend_port_env="AUTH_SERVICE_PORT",
        frontend_port_env="AUTH_ADMIN_PORT",
    )
    env = os.environ.copy()
    if runtime.hosts.backend is not None:
        env.setdefault("AUTH_SERVICE_HOST", runtime.hosts.backend)
    if runtime.ports.api is not None:
        env.setdefault("AUTH_SERVICE_PORT", str(runtime.ports.api))
    if runtime.ports.frontend is not None:
        env.setdefault("AUTH_ADMIN_PORT", str(runtime.ports.frontend))

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
