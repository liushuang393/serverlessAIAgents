#!/usr/bin/env python3
"""Decision Governance Engine のローカル開発起動ヘルパー。"""

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

    # 候補となるサンプルファイル
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
        services_to_start = [s for s in all_services if s and s not in exclusions]

        if not services_to_start:
            print("起動対象の依存サービスはありません。")
            return

        print(f"依存サービスを起動します: {', '.join(services_to_start)}")
        subprocess.run(["docker", "compose", "up", "-d", *services_to_start], cwd=str(app_dir), check=True)
        print("依存サービスを起動しました。")

    except subprocess.CalledProcessError as e:
        print(f"Warning: docker サービスの管理に失敗しました: {e}")
        print("ローカル起動を継続します...")
    except FileNotFoundError:
        print("Warning: docker コマンドが見つからないため、依存サービス起動をスキップします。")


def build_uvicorn_command(app_dir: Path, extra_args: list[str]) -> list[str]:
    """manifest と環境変数から解決した設定で uvicorn コマンドを組み立てる。"""
    runtime = resolve_app_runtime(
        app_dir / "app_config.json",
        env=os.environ,
        backend_host_env="DGE_HOST",
        backend_port_env="DGE_PORT",
    )
    host = runtime.hosts.backend or "0.0.0.0"
    port = str(runtime.ports.api or 8001)
    return [
        "uvicorn",
        "apps.decision_governance_engine.api:app",
        "--host",
        host,
        "--port",
        port,
        *extra_args,
    ]


def main() -> None:
    """開発用の依存サービスを起動し、uvicorn を実行する。"""
    script_dir = Path(__file__).resolve().parent
    app_dir = script_dir.parent

    # 0. .env を準備
    ensure_env_file(app_dir)

    # 1. 依存サービスを起動
    start_docker_dependencies(app_dir)

    # 2. FastAPI 本体は uvicorn で起動
    extra_args = list(sys.argv[1:])
    if "--reload" not in extra_args:
        extra_args = ["--reload", *extra_args]
    cmd = build_uvicorn_command(app_dir, extra_args)

    print(f"uvicorn を起動します: {' '.join(cmd)}")

    if sys.platform != "win32":
        os.execvp(cmd[0], cmd)
    else:
        subprocess.run(cmd)


if __name__ == "__main__":
    main()
