#!/usr/bin/env python3
"""messaging_hub のローカル開発起動ヘルパー。"""

from __future__ import annotations

import os
import shutil
import subprocess
import sys
from pathlib import Path

from kernel.runtime import resolve_app_runtime


def ensure_env_file(app_dir: Path) -> None:
    """`.env` がなければサンプルファイルから生成する。"""
    env_path = app_dir / ".env"
    if env_path.exists():
        return

    candidates = [".env.example", "env.example", "example.env", "sample.env"]
    for candidate in candidates:
        candidate_path = app_dir / candidate
        if candidate_path.exists():
            print(f"{candidate} から .env を作成します...")
            shutil.copy(candidate_path, env_path)
            return

    print("Warning: .env または .env.example が見つかりません。必要な環境変数は手動で設定してください。")


def start_docker_dependencies(app_dir: Path) -> None:
    """docker compose に定義された依存サービスのみを起動する。"""
    compose_file = app_dir / "docker-compose.yml"
    if not compose_file.exists():
        return

    print(f"{compose_file} 内の依存サービスを確認します...")
    try:
        result = subprocess.run(
            ["docker", "compose", "config", "--services"],
            cwd=str(app_dir),
            capture_output=True,
            text=True,
            check=True,
        )
        all_services = result.stdout.strip().split("\n")
        exclusions = {"backend", "frontend", "app", "api", "web", "worker", "celery", app_dir.name}
        services_to_start = [service for service in all_services if service and service not in exclusions]
        if not services_to_start:
            print("起動対象の依存サービスはありません。")
            return

        print(f"依存サービスを起動します: {', '.join(services_to_start)}")
        subprocess.run(["docker", "compose", "up", "-d", *services_to_start], cwd=str(app_dir), check=True)
        print("依存サービスを起動しました。")
    except subprocess.CalledProcessError as exc:
        print(f"Warning: docker サービスの管理に失敗しました: {exc}")
        print("ローカル起動を継続します...")
    except FileNotFoundError:
        print("Warning: docker コマンドが見つからないため、依存サービス起動をスキップします。")


def _resolve_backend_binding(app_dir: Path) -> tuple[str, str]:
    """manifest と override から backend bind host / port を解決する。"""
    runtime = resolve_app_runtime(
        app_dir / "app_config.json",
        env=os.environ,
        backend_host_env="MSGHUB_HOST",
        backend_port_env="MSGHUB_PORT",
    )
    if runtime.hosts.backend is None or runtime.ports.api is None:
        msg = "messaging_hub/app_config.json の runtime.hosts.backend と ports.api を設定してください。"
        raise RuntimeError(msg)
    return runtime.hosts.backend, str(runtime.ports.api)


def _normalize_reload_args(extra_args: list[str]) -> list[str]:
    """reload 指定を helper の規約に合わせて正規化する。"""
    if "--no-reload" in extra_args:
        return [arg for arg in extra_args if arg != "--no-reload"]
    if "--reload" not in extra_args:
        return ["--reload", *extra_args]
    return extra_args


def build_uvicorn_command(app_dir: Path, extra_args: list[str]) -> list[str]:
    """解決済み設定で uvicorn コマンドを組み立てる。"""
    host, port = _resolve_backend_binding(app_dir)
    normalized_args = _normalize_reload_args(extra_args)
    return [
        "uvicorn",
        "apps.messaging_hub.main:app",
        "--host",
        host,
        "--port",
        port,
        *normalized_args,
    ]


def main() -> None:
    """開発用の依存サービスを起動し、uvicorn を実行する。"""
    script_dir = Path(__file__).resolve().parent
    app_dir = script_dir.parent
    ensure_env_file(app_dir)
    start_docker_dependencies(app_dir)
    cmd = build_uvicorn_command(app_dir, list(sys.argv[1:]))
    print(f"uvicorn を起動します: {' '.join(cmd)}")
    if sys.platform != "win32":
        os.execvp(cmd[0], cmd)
    else:
        subprocess.run(cmd)


if __name__ == "__main__":
    main()
