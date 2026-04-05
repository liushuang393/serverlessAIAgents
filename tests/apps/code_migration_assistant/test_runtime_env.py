"""Code Migration Assistant のローカル env ロード検証."""

from __future__ import annotations

import os
from typing import TYPE_CHECKING

from apps.code_migration_assistant.runtime_env import load_code_migration_env


if TYPE_CHECKING:
    from pathlib import Path


def _write_env(path: Path, content: str) -> None:
    path.write_text(content, encoding="utf-8")


def test_load_code_migration_env_reads_root_env(monkeypatch, tmp_path: Path) -> None:
    """app .env がなくても root .env を読み込めること."""
    app_root = tmp_path / "apps" / "code_migration_assistant"
    app_root.mkdir(parents=True)
    _write_env(tmp_path / ".env", "CODE_MIGRATION_API_KEY=root-key\n")
    monkeypatch.delenv("CODE_MIGRATION_API_KEY", raising=False)
    monkeypatch.delenv("AGENTFLOW_ENV_FILE", raising=False)

    load_code_migration_env(app_root)

    assert os.getenv("CODE_MIGRATION_API_KEY") == "root-key"
    assert os.getenv("AGENTFLOW_ENV_FILE") == str(tmp_path / ".env")


def test_load_code_migration_env_prefers_app_env(monkeypatch, tmp_path: Path) -> None:
    """app .env がある場合は root .env を上書きすること."""
    app_root = tmp_path / "apps" / "code_migration_assistant"
    app_root.mkdir(parents=True)
    _write_env(tmp_path / ".env", "CODE_MIGRATION_API_KEY=root-key\n")
    _write_env(app_root / ".env", "CODE_MIGRATION_API_KEY=app-key\n")
    monkeypatch.delenv("CODE_MIGRATION_API_KEY", raising=False)
    monkeypatch.delenv("AGENTFLOW_ENV_FILE", raising=False)

    load_code_migration_env(app_root)

    assert os.getenv("CODE_MIGRATION_API_KEY") == "app-key"
    assert os.getenv("AGENTFLOW_ENV_FILE") == str(app_root / ".env")
