#!/usr/bin/env python3
"""messaging_hub のローカル開発起動ヘルパー。"""

from __future__ import annotations

import argparse
import os
import shutil
import subprocess
from pathlib import Path

from kernel.runtime.uvicorn_launcher import UvicornManifestConfig, launch_from_manifest


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


def _parse_args() -> argparse.Namespace:
    """起動オプションを解釈する。"""
    parser = argparse.ArgumentParser(description="messaging_hub の backend を起動します。")
    parser.set_defaults(reload=True)
    reload_group = parser.add_mutually_exclusive_group()
    reload_group.add_argument("--reload", dest="reload", action="store_true")
    reload_group.add_argument("--no-reload", dest="reload", action="store_false")
    parser.add_argument("--workers", type=int, default=None, help="uvicorn worker 数")
    return parser.parse_args()


def main() -> None:
    """開発用の依存サービスを起動し、uvicorn を実行する。"""
    script_dir = Path(__file__).resolve().parent
    app_dir = script_dir.parent
    args = _parse_args()
    ensure_env_file(app_dir)
    start_docker_dependencies(app_dir)
    launch_from_manifest(
        UvicornManifestConfig(
            app_import="apps.messaging_hub.main:app",
            config_path=app_dir / "app_config.json",
            backend_host_env="MSGHUB_HOST",
            backend_port_env="MSGHUB_PORT",
        ),
        env=os.environ,
        reload=bool(args.reload),
        workers=args.workers,
    )


if __name__ == "__main__":
    main()
