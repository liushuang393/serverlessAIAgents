#!/usr/bin/env python3
"""GEO Platform 用の manifest 駆動 Docker ラッパー。"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

from kernel.runtime import resolve_app_runtime


APP_DIR = Path(__file__).resolve().parents[1]
APP_CONFIG_PATH = APP_DIR / "app_config.json"
COMPOSE_PATH = APP_DIR / "docker-compose.yml"


def _build_compose_env() -> dict[str, str]:
    """Compose 実行に必要な host / port / URL を解決する。"""
    runtime = resolve_app_runtime(
        APP_CONFIG_PATH,
        env=os.environ,
        backend_host_env="GEO_PLATFORM_HOST",
        backend_port_env="GEO_PLATFORM_PORT",
        backend_url_env="GEO_PLATFORM_PUBLIC_BASE_URL",
    )
    backend_host = runtime.hosts.backend
    backend_port = runtime.ports.api
    backend_url = runtime.urls.backend
    if backend_host is None or backend_port is None or backend_url is None:
        msg = "GEO Platform の backend host / port / URL は app_config.json または明示 env で指定してください。"
        raise RuntimeError(msg)

    env = os.environ.copy()
    env["GEO_PLATFORM_HOST"] = backend_host
    env["GEO_PLATFORM_PORT"] = str(backend_port)
    env["GEO_PLATFORM_PUBLIC_BASE_URL"] = backend_url
    return env


def main() -> int:
    """要求された compose アクションを実行する。"""
    if len(sys.argv) != 2 or sys.argv[1] not in {"publish", "start", "stop"}:
        print("usage: compose.py [publish|start|stop]", file=sys.stderr)
        return 2

    action = sys.argv[1]
    env = _build_compose_env()

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
