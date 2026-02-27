"""Unit tests for FileOrganizerAgent."""

from __future__ import annotations

import os
import time

import pytest
from apps.messaging_hub.agents.file_organizer_agent import FileOrganizerAgent


@pytest.mark.asyncio
async def test_organize_moves_files_into_category_folders(tmp_path) -> None:
    """organize がカテゴリフォルダへ移動すること."""
    (tmp_path / "doc.txt").write_text("hello", encoding="utf-8")
    (tmp_path / "photo.jpg").write_bytes(b"\x00\x01")
    agent = FileOrganizerAgent()

    result = await agent.organize(str(tmp_path), dry_run=False)

    assert result.errors == []
    assert result.files_moved >= 2
    assert (tmp_path / "Documents" / "doc.txt").exists()
    assert (tmp_path / "Images" / "photo.jpg").exists()


@pytest.mark.asyncio
async def test_smart_rename_executes_rename(tmp_path) -> None:
    """smart_rename が実際にリネームすること."""
    (tmp_path / "Report Final.txt").write_text("x", encoding="utf-8")
    (tmp_path / "Draft.md").write_text("y", encoding="utf-8")
    agent = FileOrganizerAgent()

    result = await agent.smart_rename(str(tmp_path))

    assert result.errors == []
    assert result.files_renamed == 2
    renamed_files = sorted(name for name in os.listdir(tmp_path) if os.path.isfile(tmp_path / name))  # noqa: PTH208,ASYNC240,PTH113
    assert renamed_files[0].startswith("001_")
    assert renamed_files[1].startswith("002_")


@pytest.mark.asyncio
async def test_find_duplicates_by_content(tmp_path) -> None:
    """同一内容ファイルを重複として検出できること."""
    content = b"duplicate-content"
    (tmp_path / "a.txt").write_bytes(content)
    (tmp_path / "b.txt").write_bytes(content)
    (tmp_path / "c.txt").write_bytes(b"another")
    agent = FileOrganizerAgent()

    groups = await agent.find_duplicates(str(tmp_path), by_content=True)

    assert len(groups) == 1
    assert len(groups[0].files) == 2


@pytest.mark.asyncio
async def test_cleanup_old_files_deletes_targets(tmp_path) -> None:
    """cleanup_old_files が古いファイルを削除すること."""
    old_file = tmp_path / "old.log"
    new_file = tmp_path / "new.log"
    old_file.write_text("old", encoding="utf-8")
    new_file.write_text("new", encoding="utf-8")

    old_timestamp = time.time() - 40 * 24 * 60 * 60
    os.utime(old_file, (old_timestamp, old_timestamp))
    agent = FileOrganizerAgent(days_old_threshold=30)

    result = await agent.cleanup_old_files(str(tmp_path), days_old=30, dry_run=False)

    assert result.errors == []
    assert not old_file.exists()
    assert new_file.exists()
    assert any(action["type"] == "delete" for action in result.actions)
