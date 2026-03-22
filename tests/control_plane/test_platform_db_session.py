"""Platform DB セッション補助関数のユニットテスト.

テスト対象: control_plane/db/session.py
"""

from __future__ import annotations

from typing import TYPE_CHECKING

from control_plane.db.session import _ensure_sqlite_parent_dir


if TYPE_CHECKING:
    from pathlib import Path


class TestEnsureSqliteParentDir:
    """SQLite 親ディレクトリ作成のテスト."""

    def test_create_parent_dir_for_relative_sqlite_url(self, monkeypatch, tmp_path: Path) -> None:
        """相対 SQLite URL の親ディレクトリを自動作成する."""
        monkeypatch.chdir(tmp_path)

        _ensure_sqlite_parent_dir("sqlite+aiosqlite:///./control_plane/data/platform.db")

        assert (tmp_path / "control_plane/data").is_dir()

    def test_ignore_memory_sqlite_url(self, monkeypatch, tmp_path: Path) -> None:
        """メモリ SQLite URL ではディレクトリを作成しない."""
        monkeypatch.chdir(tmp_path)

        _ensure_sqlite_parent_dir("sqlite+aiosqlite:///:memory:")

        assert not (tmp_path / "apps").exists()
