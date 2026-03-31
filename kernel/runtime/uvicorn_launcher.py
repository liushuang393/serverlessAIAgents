"""app_config.json を正本として uvicorn を起動する共通ランチャー。"""

from __future__ import annotations

import argparse
from dataclasses import dataclass
from pathlib import Path
from typing import TYPE_CHECKING

import uvicorn

from kernel.runtime.app_manifest import resolve_app_runtime


if TYPE_CHECKING:
    from collections.abc import Mapping, Sequence


@dataclass(frozen=True, slots=True)
class UvicornManifestConfig:
    """uvicorn 起動に必要な manifest 解決設定。"""

    app_import: str
    config_path: Path
    backend_host_env: str
    backend_port_env: str


def resolve_backend_binding(
    config: UvicornManifestConfig,
    *,
    env: Mapping[str, str] | None = None,
) -> tuple[str, int]:
    """backend bind host / port を env または manifest から解決する。"""
    runtime = resolve_app_runtime(
        config.config_path,
        env=env,
        backend_host_env=config.backend_host_env,
        backend_port_env=config.backend_port_env,
    )
    backend_host = runtime.hosts.backend
    backend_port = runtime.ports.api
    if backend_host is None or backend_port is None:
        msg = f"{config.config_path} の runtime.hosts.backend / ports.api が不足しています。"
        raise RuntimeError(msg)
    return backend_host, backend_port


def build_uvicorn_command(
    config: UvicornManifestConfig,
    *,
    env: Mapping[str, str] | None = None,
    reload: bool = False,
    workers: int | None = None,
    extra_args: Sequence[str] | None = None,
) -> list[str]:
    """解決済み host / port を使う uvicorn CLI コマンドを返す。"""
    backend_host, backend_port = resolve_backend_binding(config, env=env)
    command = [
        "uvicorn",
        config.app_import,
        "--host",
        backend_host,
        "--port",
        str(backend_port),
    ]
    if reload:
        command.append("--reload")
    if workers is not None:
        command.extend(["--workers", str(workers)])
    if extra_args is not None:
        command.extend(extra_args)
    return command


def launch_from_manifest(
    config: UvicornManifestConfig,
    *,
    env: Mapping[str, str] | None = None,
    reload: bool = False,
    workers: int | None = None,
    reload_dirs: Sequence[str] | None = None,
    log_level: str | None = None,
) -> None:
    """manifest 解決済み host / port で uvicorn を起動する。"""
    if workers is not None and workers < 1:
        msg = "workers は 1 以上を指定してください。"
        raise ValueError(msg)
    if reload and workers not in (None, 1):
        msg = "reload 有効時に workers > 1 は指定できません。"
        raise ValueError(msg)

    backend_host, backend_port = resolve_backend_binding(config, env=env)
    uvicorn.run(
        config.app_import,
        host=backend_host,
        port=backend_port,
        reload=reload,
        workers=workers,
        reload_dirs=list(reload_dirs) if reload_dirs is not None else None,
        log_level=log_level,
    )


def _parse_args(argv: Sequence[str]) -> argparse.Namespace:
    """CLI 引数を解釈する。"""
    parser = argparse.ArgumentParser(
        description="app_config.json / env から host / port を解決して uvicorn を起動します。",
    )
    parser.add_argument("--config", required=True, help="app_config.json へのパス")
    parser.add_argument("--app", required=True, help="ASGI app import path")
    parser.add_argument("--host-env", required=True, help="backend host override 用 env 名")
    parser.add_argument("--port-env", required=True, help="backend port override 用 env 名")
    parser.add_argument("--reload", action="store_true", help="uvicorn reload を有効化")
    parser.add_argument("--workers", type=int, default=None, help="uvicorn worker 数")
    parser.add_argument(
        "--reload-dir",
        action="append",
        dest="reload_dirs",
        default=None,
        help="reload 対象ディレクトリ。複数回指定可能。",
    )
    parser.add_argument("--log-level", default=None, help="uvicorn log level")
    return parser.parse_args(list(argv))


def main(argv: Sequence[str] | None = None) -> int:
    """CLI 入口。"""
    import sys

    effective_argv = list(sys.argv[1:] if argv is None else argv)
    args = _parse_args(effective_argv)
    config = UvicornManifestConfig(
        app_import=args.app,
        config_path=Path(args.config).resolve(),
        backend_host_env=args.host_env,
        backend_port_env=args.port_env,
    )
    launch_from_manifest(
        config,
        reload=bool(args.reload),
        workers=args.workers,
        reload_dirs=args.reload_dirs,
        log_level=args.log_level,
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
