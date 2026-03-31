"""Code Migration Assistant のローカル環境変数ロード補助."""

from __future__ import annotations

import os
from collections.abc import Callable
from pathlib import Path


LoadDotenvCallable = Callable[..., bool]
load_dotenv: LoadDotenvCallable | None

try:
    from dotenv import load_dotenv as _dotenv_load_dotenv
except ImportError:  # pragma: no cover - optional dependency
    load_dotenv = None
else:
    load_dotenv = _dotenv_load_dotenv


def load_code_migration_env(app_root: Path) -> None:
    """ローカル実行向けに root/app の .env を順にロードする."""
    if load_dotenv is None:
        return

    repo_root = app_root.parents[1]
    root_env_path = repo_root / ".env"
    app_env_path = app_root / ".env"
    loaded_env_path: Path | None = None

    if root_env_path.is_file():
        load_dotenv(root_env_path, override=False)
        loaded_env_path = root_env_path

    if app_env_path.is_file():
        load_dotenv(app_env_path, override=True)
        loaded_env_path = app_env_path

    if loaded_env_path is not None:
        os.environ.setdefault("AGENTFLOW_ENV_FILE", str(loaded_env_path))


__all__ = ["load_code_migration_env"]
