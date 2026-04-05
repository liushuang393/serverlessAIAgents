"""Code Migration Assistant のローカル環境変数ロード補助."""

from __future__ import annotations

import logging
import os
import secrets
from collections.abc import Callable
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from pathlib import Path

logger = logging.getLogger("migration_server")

LoadDotenvCallable = Callable[..., bool]
load_dotenv: LoadDotenvCallable | None

try:
    from dotenv import load_dotenv as _dotenv_load_dotenv
except ImportError:  # pragma: no cover - optional dependency
    load_dotenv = None
else:
    load_dotenv = _dotenv_load_dotenv


# ---------------------------------------------------------------------------
# API キー自動プロビジョニング
# ---------------------------------------------------------------------------
# CODE_MIGRATION_AUTO_API_KEY=true（デフォルト）の場合、
# CODE_MIGRATION_API_KEY が未設定なら安全なランダムキーを自動生成してセットする。
# 本番環境では CODE_MIGRATION_AUTO_API_KEY=false を設定し、
# 明示的に CODE_MIGRATION_API_KEY を指定すること。
# ---------------------------------------------------------------------------

_AUTO_KEY_ENV = "CODE_MIGRATION_AUTO_API_KEY"
_API_KEY_ENV = "CODE_MIGRATION_API_KEY"


def _ensure_api_key() -> None:
    """API キーが未設定かつ自動生成が有効なら、ランダムキーを生成して環境変数にセットする."""
    auto_flag = os.getenv(_AUTO_KEY_ENV, "true").strip().lower()
    if auto_flag not in {"true", "1", "yes"}:
        return

    existing_key = os.getenv(_API_KEY_ENV, "").strip()
    if existing_key:
        return

    generated_key = f"dev-{secrets.token_urlsafe(24)}"
    os.environ[_API_KEY_ENV] = generated_key

    logger.warning(
        "======================================================================\n"
        "  API キーが未設定のため自動生成しました（ローカル開発用）\n"
        "  生成キー: %s\n"
        "  ブラウザでアクセスする場合:\n"
        "    http://localhost:8003/?api_key=%s\n"
        "  本番環境では環境変数 %s を明示的に設定し、\n"
        "  %s=false にしてください。\n"
        "======================================================================",
        generated_key,
        generated_key,
        _API_KEY_ENV,
        _AUTO_KEY_ENV,
    )


def load_code_migration_env(app_root: Path) -> None:
    """ローカル実行向けに root/app の .env を順にロードし、API キーを確保する."""
    if load_dotenv is not None:
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

    _ensure_api_key()


__all__ = ["load_code_migration_env"]
