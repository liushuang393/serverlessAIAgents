#!/usr/bin/env python3
"""GEO Platform のローカル開発起動ヘルパー。"""

from __future__ import annotations

import argparse
import os
import shutil
from pathlib import Path

from kernel.runtime.uvicorn_launcher import UvicornManifestConfig, launch_from_manifest


def ensure_env_file(app_dir: Path) -> None:
    """`.env` がなければ `.env.example` から生成する。"""
    env_path = app_dir / ".env"
    if env_path.exists():
        return
    example_path = app_dir / ".env.example"
    if example_path.exists():
        shutil.copy(example_path, env_path)


def _parse_args() -> argparse.Namespace:
    """起動オプションを解釈する。"""
    parser = argparse.ArgumentParser(description="GEO Platform の backend を起動します。")
    parser.set_defaults(reload=True)
    reload_group = parser.add_mutually_exclusive_group()
    reload_group.add_argument("--reload", dest="reload", action="store_true")
    reload_group.add_argument("--no-reload", dest="reload", action="store_false")
    parser.add_argument("--workers", type=int, default=None, help="uvicorn worker 数")
    return parser.parse_args()


def main() -> None:
    """ローカル開発用に uvicorn を起動する。"""
    script_dir = Path(__file__).resolve().parent
    app_dir = script_dir.parent
    args = _parse_args()
    ensure_env_file(app_dir)
    launch_from_manifest(
        UvicornManifestConfig(
            app_import="apps.legacy_modernization_geo_platform.main:app",
            config_path=app_dir / "app_config.json",
            backend_host_env="GEO_PLATFORM_HOST",
            backend_port_env="GEO_PLATFORM_PORT",
        ),
        env=os.environ,
        reload=bool(args.reload),
        workers=args.workers,
    )


if __name__ == "__main__":
    main()
