#!/usr/bin/env python3
"""GEO Platform のローカル開発起動ヘルパー。"""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

from kernel.runtime import resolve_app_runtime


def ensure_env_file(app_dir: Path) -> None:
    """`.env` がなければ `.env.example` から生成する。"""
    env_path = app_dir / ".env"
    if env_path.exists():
        return
    example_path = app_dir / ".env.example"
    if example_path.exists():
        shutil.copy(example_path, env_path)


def build_uvicorn_command(app_dir: Path, extra_args: list[str]) -> list[str]:
    """manifest と環境変数から解決した設定で uvicorn コマンドを組み立てる。"""
    runtime = resolve_app_runtime(
        app_dir / "app_config.json",
        env=os.environ,
        backend_host_env="GEO_PLATFORM_HOST",
        backend_port_env="GEO_PLATFORM_PORT",
    )
    host = runtime.hosts.backend or "0.0.0.0"
    port = str(runtime.ports.api or 8100)
    return [
        "uvicorn",
        "apps.legacy_modernization_geo_platform.main:app",
        "--host",
        host,
        "--port",
        port,
        *extra_args,
    ]


def main() -> None:
    """ローカル開発用に uvicorn を起動する。"""
    script_dir = Path(__file__).resolve().parent
    app_dir = script_dir.parent
    ensure_env_file(app_dir)
    extra_args = list(sys.argv[1:])
    if "--reload" not in extra_args:
        extra_args = ["--reload", *extra_args]
    cmd = build_uvicorn_command(app_dir, extra_args)
    if sys.platform != "win32":
        os.execvp(cmd[0], cmd)
    else:
        subprocess.run(cmd, check=False)


if __name__ == "__main__":
    main()
