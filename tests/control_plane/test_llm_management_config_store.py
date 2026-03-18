"""Unit tests for LLM config store atomic persistence."""

from __future__ import annotations

from concurrent.futures import ThreadPoolExecutor
from pathlib import Path

from control_plane.services.llm_management_config_store import LLMConfigStore


def test_load_creates_default_config(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    store = LLMConfigStore(config_path)

    config = store.load()

    assert config_path.exists()
    assert len(config.providers) > 0
    assert store.version() is not None


def test_save_returns_new_version_after_change(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    store = LLMConfigStore(config_path)
    config = store.load()
    old_version = store.version()

    config.gateway.default_role = "coding"
    new_version = store.save(config)

    assert new_version
    assert new_version != old_version
    assert store.version() == new_version


def test_concurrent_saves_keep_config_readable(tmp_path: Path) -> None:
    config_path = tmp_path / ".bizcore" / "llm_gateway.yaml"
    store = LLMConfigStore(config_path)
    store.load()

    def _writer(index: int) -> str:
        config = store.load()
        config.gateway.default_role = f"role_{index}"
        return store.save(config)

    with ThreadPoolExecutor(max_workers=4) as executor:
        versions = list(executor.map(_writer, range(8)))

    assert all(versions)
    latest = store.load()
    assert latest.gateway.default_role.startswith("role_")
