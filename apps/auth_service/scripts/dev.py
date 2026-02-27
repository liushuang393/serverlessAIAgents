#!/usr/bin/env python3
"""auth_service 開発用起動スクリプト.

.env の自動コピーと Docker 依存サービス起動を行い、
ローカルで auth_service を起動する。

使用方法:
    python apps/auth_service/scripts/dev.py
"""

import os
import shutil
import subprocess
import sys
from pathlib import Path


def ensure_env_file(app_dir: Path) -> None:
    """必要に応じて .env.example から .env を作成する."""
    env_path = app_dir / ".env"
    if env_path.exists():
        return

    # .env.example からコピー
    candidates = [".env.example", "env.example", "example.env", "sample.env"]

    for candidate in candidates:
        candidate_path = app_dir / candidate
        if candidate_path.exists():
            print(f".env を {candidate} から作成します...")
            shutil.copy(candidate_path, env_path)
            return

    print("警告: .env または .env.example が見つかりません。環境変数が必要な場合は失敗する可能性があります。")


def start_docker_dependencies(app_dir: Path) -> None:
    """docker-compose.yml があればインフラサービスを起動する."""
    compose_file = app_dir / "docker-compose.yml"
    if not compose_file.exists():
        return

    print(f"インフラサービスを確認中: {compose_file} ...")

    try:
        result = subprocess.run(
            ["docker", "compose", "config", "--services"],
            cwd=str(app_dir),
            capture_output=True,
            text=True,
            check=True,
        )
        all_services = result.stdout.strip().split("\n")

        # メインアプリ以外のサービスのみ起動
        exclusions = {
            "backend",
            "frontend",
            "app",
            "api",
            "web",
            "worker",
            "celery",
            "auth-service",
            app_dir.name,
        }
        services_to_start = [s for s in all_services if s and s not in exclusions]

        if not services_to_start:
            print("起動するインフラサービスはありません。")
            return

        print(f"インフラサービスを起動: {', '.join(services_to_start)}")
        subprocess.run(
            ["docker", "compose", "up", "-d", *services_to_start],
            cwd=str(app_dir),
            check=True,
        )
        print("インフラサービス起動完了。")

    except subprocess.CalledProcessError as e:
        print(f"警告: Docker サービス管理に失敗: {e}")
        print("ローカル起動を続行します...")
    except FileNotFoundError:
        print("警告: 'docker' コマンドが見つかりません。Docker 起動をスキップします。")


def main() -> None:
    """メイン処理."""
    script_dir = Path(__file__).resolve().parent
    app_dir = script_dir.parent

    # 0. .env ファイル確認
    ensure_env_file(app_dir)

    # 1. Docker 依存サービス起動（あれば）
    start_docker_dependencies(app_dir)

    # 2. アプリケーション起動
    target_module = "apps.auth_service.main"
    cmd = [sys.executable, "-m", target_module]

    if len(sys.argv) > 1:
        cmd.extend(sys.argv[1:])

    print(f"auth_service を起動: {' '.join(cmd)}")

    # 現在のプロセスを置き換え
    if sys.platform != "win32":
        os.execvp(cmd[0], cmd)
    else:
        subprocess.run(cmd)


if __name__ == "__main__":
    main()
