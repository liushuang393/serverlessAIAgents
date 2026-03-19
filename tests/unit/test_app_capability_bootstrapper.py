"""AppCapabilityBootstrapper のユニットテスト.

app_config.json の文字コード互換性と contracts 抽出の堅牢性を検証する。
"""

from __future__ import annotations

import asyncio
import json
from typing import TYPE_CHECKING

import pytest

from control_plane.bootstrap.app_bootstrapper import AppCapabilityBootstrapper
from control_plane.bootstrap.capability_bundle import CapabilityBundle
from shared.rag.rag_pipeline import RAGConfig, RAGPipeline
from kernel.protocols.mcp_client import MCPClient
from kernel.protocols.mcp_config import MCPConfig
from kernel.skills.gateway import SkillGateway
from shared.config.manifest import load_app_manifest_dict


if TYPE_CHECKING:
    from pathlib import Path


def _write_app_config(config_path: Path, payload: dict[str, object], encoding: str) -> None:
    """指定文字コードで app_config.json を作成する."""
    config_path.parent.mkdir(parents=True, exist_ok=True)
    text = json.dumps(payload, ensure_ascii=False)
    config_path.write_bytes(text.encode(encoding))


def test_load_app_config_reads_utf8_file(tmp_path: Path) -> None:
    """UTF-8 の app_config.json を正常に読み込めることを確認する."""
    config_path = tmp_path / "faq_system" / "app_config.json"
    _write_app_config(config_path, {"contracts": {"rag": {"enabled": True}}}, "utf-8")

    bootstrapper = AppCapabilityBootstrapper(app_name="faq_system")

    app_config = bootstrapper._load_app_config(apps_dir=str(tmp_path))

    assert app_config is not None
    assert app_config["contracts"] == {"rag": {"enabled": True}}
    assert app_config["name"] == "faq_system"
    assert app_config["metadata"]["config_path"] == str(config_path.resolve())


def test_load_app_config_reads_shift_jis_file(tmp_path: Path) -> None:
    """Shift_JIS の app_config.json を正常に読み込めることを確認する."""
    config_path = tmp_path / "faq_system" / "app_config.json"
    payload = {"display_name": "FAQシステム", "contracts": {"skills": {"auto_install": True}}}
    _write_app_config(config_path, payload, "shift_jis")

    bootstrapper = AppCapabilityBootstrapper(app_name="faq_system")

    app_config = bootstrapper._load_app_config(apps_dir=str(tmp_path))

    assert app_config is not None
    assert app_config["name"] == "faq_system"
    assert app_config["display_name"] == "FAQシステム"
    assert app_config["contracts"] == {"skills": {"auto_install": True}}
    assert app_config["metadata"]["config_path"] == str(config_path.resolve())


def test_load_app_config_uses_canonical_manifest_loader(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    """bootstrapper が canonical text loader を経由して app_config を読むことを確認する."""
    config_path = tmp_path / "faq_system" / "app_config.json"
    _write_app_config(config_path, {"contracts": {"rag": {"enabled": True}}}, "utf-8")

    seen: list[tuple[str, Path]] = []

    def _fake_load_app_manifest_dict_text(text: str, *, manifest_path: Path) -> dict[str, object]:
        seen.append((text, manifest_path))
        return {"name": "faq_system", "contracts": {"rag": {"enabled": True}}}

    monkeypatch.setattr(
        "control_plane.bootstrap.app_bootstrapper.load_app_manifest_dict_text",
        _fake_load_app_manifest_dict_text,
    )

    bootstrapper = AppCapabilityBootstrapper(app_name="faq_system")
    app_config = bootstrapper._load_app_config(apps_dir=str(tmp_path))

    assert app_config == {"name": "faq_system", "contracts": {"rag": {"enabled": True}}}
    assert len(seen) == 1
    assert '"rag"' in seen[0][0]
    assert seen[0][1] == config_path.resolve()


def test_load_app_config_returns_none_when_missing(tmp_path: Path) -> None:
    """app_config.json が見つからない場合は None を返すことを確認する."""
    missing_app_name = f"missing_config_{tmp_path.name}"
    bootstrapper = AppCapabilityBootstrapper(app_name=missing_app_name)

    app_config = bootstrapper._load_app_config(apps_dir=str(tmp_path))

    assert app_config is None


def test_load_app_config_matches_canonical_manifest_dict(tmp_path: Path) -> None:
    """bootstrapper の戻り値は canonical manifest dict と一致する."""
    config_path = tmp_path / "faq_system" / "app_config.json"
    payload = {
        "runtime": {"commands": {"start": "python -m faq"}},
        "blueprint": {"engine_pattern": "pipeline"},
        "plugin_bindings": [{"id": "Official.Test-Pack", "version": "1.0.0"}],
        "visibility": {"mode": "private"},
    }
    _write_app_config(config_path, payload, "utf-8")

    bootstrapper = AppCapabilityBootstrapper(app_name="faq_system")

    assert bootstrapper._load_app_config(apps_dir=str(tmp_path)) == load_app_manifest_dict(config_path)


@pytest.mark.asyncio
async def test_build_tolerates_missing_contracts_without_watcher(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    """contracts 未設定でも builder が安全に None を受け取ることを確認する."""
    config_path = tmp_path / "faq_system" / "app_config.json"
    _write_app_config(config_path, {"display_name": "FAQ"}, "utf-8")

    captured: dict[str, object | None] = {}

    async def fake_build_rag_engine(config: dict[str, object] | None) -> None:
        await asyncio.sleep(0)
        captured["rag"] = config

    async def fake_build_skill_gateway(config: dict[str, object] | None) -> None:
        await asyncio.sleep(0)
        captured["skills"] = config

    monkeypatch.setattr("control_plane.bootstrap.rag_builder.build_rag_engine", fake_build_rag_engine)
    monkeypatch.setattr("control_plane.bootstrap.skill_builder.build_skill_gateway", fake_build_skill_gateway)

    bundle, bootstrapper = await AppCapabilityBootstrapper.build(
        app_name="faq_system",
        apps_dir=str(tmp_path),
        platform_url=None,
    )

    assert captured == {"rag": None, "skills": None}
    assert bundle.has_rag() is False
    assert bundle.has_skills() is False
    assert bootstrapper._watcher is None
    assert bootstrapper._watcher_task is None

    await bootstrapper.shutdown()


@pytest.mark.asyncio
async def test_build_ignores_invalid_contract_section_types(
    monkeypatch: pytest.MonkeyPatch,
    tmp_path: Path,
) -> None:
    """contracts 配下が非辞書でもクラッシュせずに無効化されることを確認する."""
    config_path = tmp_path / "faq_system" / "app_config.json"
    payload = {"contracts": {"rag": "invalid", "skills": ["invalid"]}}
    _write_app_config(config_path, payload, "utf-8")

    captured: dict[str, object | None] = {}

    async def fake_build_rag_engine(config: dict[str, object] | None) -> None:
        await asyncio.sleep(0)
        captured["rag"] = config

    async def fake_build_skill_gateway(config: dict[str, object] | None) -> None:
        await asyncio.sleep(0)
        captured["skills"] = config

    monkeypatch.setattr("control_plane.bootstrap.rag_builder.build_rag_engine", fake_build_rag_engine)
    monkeypatch.setattr("control_plane.bootstrap.skill_builder.build_skill_gateway", fake_build_skill_gateway)

    bundle, bootstrapper = await AppCapabilityBootstrapper.build(
        app_name="faq_system",
        apps_dir=str(tmp_path),
        platform_url=None,
    )

    assert captured == {"rag": None, "skills": None}
    assert bundle == CapabilityBundle(app_name="faq_system")

    await bootstrapper.shutdown()


def test_capability_bundle_has_flags() -> None:
    """CapabilityBundle の有効判定が concrete type で動作することを確認する."""
    bundle = CapabilityBundle(
        app_name="faq_system",
        rag_engine=RAGPipeline(config=RAGConfig()),
        skill_gateway=SkillGateway(),
        mcp_client=MCPClient(MCPConfig()),
    )

    assert bundle.has_rag() is True
    assert bundle.has_skills() is True
    assert bundle.has_mcp() is True
