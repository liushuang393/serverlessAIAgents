#!/usr/bin/env python3
"""Decision Governance Engine 用の manifest 駆動 Docker ラッパー。"""

from __future__ import annotations

import os
import subprocess
import sys
from pathlib import Path

from kernel.runtime import resolve_app_runtime


APP_DIR = Path(__file__).resolve().parents[1]
APP_CONFIG_PATH = APP_DIR / "app_config.json"
COMPOSE_FILES = (
    APP_DIR / "docker-compose.yml",
    APP_DIR / "docker-compose.dev.yml",
)


def _build_compose_env() -> dict[str, str]:
    """Compose 実行に必要な host / port を解決する。"""
    runtime = resolve_app_runtime(
        APP_CONFIG_PATH,
        env=os.environ,
        backend_host_env="DGE_HOST",
        backend_port_env="DGE_PORT",
        frontend_host_env="FRONTEND_HOST",
        frontend_port_env="FRONTEND_PORT",
    )
    backend_host = runtime.hosts.backend
    backend_port = runtime.ports.api
    frontend_host = runtime.hosts.frontend
    frontend_port = runtime.ports.frontend
    db_port = runtime.ports.db
    redis_port = runtime.ports.redis
    if backend_host is None or backend_port is None or frontend_host is None or frontend_port is None:
        msg = (
            "Decision Governance Engine の frontend / backend host / port は "
            "app_config.json または明示 env で指定してください。"
        )
        raise RuntimeError(msg)

    env = os.environ.copy()
    env["DGE_HOST"] = backend_host
    env["DGE_PORT"] = str(backend_port)
    env["FRONTEND_HOST"] = frontend_host
    env["FRONTEND_PORT"] = str(frontend_port)
    if db_port is not None:
        env["DB_PORT"] = str(db_port)
    if redis_port is not None:
        env["REDIS_PORT"] = str(redis_port)
    return env


def main() -> int:
    """要求された compose アクションを実行する。"""
    if len(sys.argv) != 2 or sys.argv[1] not in {"publish", "start", "stop"}:
        print("usage: compose.py [publish|start|stop]", file=sys.stderr)
        return 2

    action = sys.argv[1]
    env = _build_compose_env()
    command = ["docker", "compose"]
    for compose_file in COMPOSE_FILES:
        command.extend(["-f", str(compose_file)])
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
