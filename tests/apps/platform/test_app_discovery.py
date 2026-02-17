# -*- coding: utf-8 -*-
"""AppDiscoveryService のユニットテスト.

テスト対象: apps/platform/services/app_discovery.py
"""

from __future__ import annotations

import json
from pathlib import Path

import pytest

from apps.platform.schemas.app_config_schemas import AppConfig
from apps.platform.services.app_discovery import AppDiscoveryService


class TestAppDiscoveryInit:
    """初期化テスト."""

    def test_default_apps_dir(self) -> None:
        """デフォルトで cwd/apps を使用する."""
        svc = AppDiscoveryService()
        assert svc._apps_dir.name == "apps"

    def test_custom_apps_dir(self, tmp_path: Path) -> None:
        """カスタム apps_dir を受け付ける."""
        svc = AppDiscoveryService(apps_dir=tmp_path)
        assert svc._apps_dir == tmp_path

    def test_empty_registry_on_init(self) -> None:
        """初期状態でレジストリが空."""
        svc = AppDiscoveryService()
        assert svc.list_apps() == []


class TestAppDiscoveryScan:
    """scan() メソッドテスト."""

    @pytest.mark.asyncio()
    async def test_scan_finds_all_apps(
        self,
        discovery: AppDiscoveryService,
    ) -> None:
        """全 app_config.json を検出・登録する."""
        count = await discovery.scan()
        assert count == 3
        names = [a.name for a in discovery.list_apps()]
        assert "test_app" in names
        assert "minimal_app" in names
        assert "library_app" in names

    @pytest.mark.asyncio()
    async def test_scan_nonexistent_dir(self, tmp_path: Path) -> None:
        """存在しないディレクトリでは 0 を返す."""
        svc = AppDiscoveryService(apps_dir=tmp_path / "nonexistent")
        count = await svc.scan()
        assert count == 0

    @pytest.mark.asyncio()
    async def test_scan_handles_invalid_json(self, tmp_path: Path) -> None:
        """不正な JSON をエラーとして記録する."""
        bad_dir = tmp_path / "bad_app"
        bad_dir.mkdir()
        (bad_dir / "app_config.json").write_text("{invalid", encoding="utf-8")

        svc = AppDiscoveryService(apps_dir=tmp_path)
        count = await svc.scan()
        assert count == 0
        errors = svc.list_errors()
        assert "bad_app" in errors
        assert "JSON パースエラー" in errors["bad_app"]

    @pytest.mark.asyncio()
    async def test_scan_handles_validation_error(self, tmp_path: Path) -> None:
        """スキーマ違反をエラーとして記録する."""
        bad_dir = tmp_path / "invalid_app"
        bad_dir.mkdir()
        (bad_dir / "app_config.json").write_text(
            json.dumps({"name": "INVALID", "display_name": "X"}),
            encoding="utf-8",
        )

        svc = AppDiscoveryService(apps_dir=tmp_path)
        count = await svc.scan()
        assert count == 0
        errors = svc.list_errors()
        assert "invalid_app" in errors

    @pytest.mark.asyncio()
    async def test_scan_clears_previous(
        self,
        discovery: AppDiscoveryService,
    ) -> None:
        """再スキャン時に前回の結果をクリアする."""
        await discovery.scan()
        assert len(discovery.list_apps()) == 3
        # 再スキャンしても同じ結果
        await discovery.scan()
        assert len(discovery.list_apps()) == 3


class TestAppDiscoveryLookup:
    """get_app / list_apps / register テスト."""

    @pytest.mark.asyncio()
    async def test_get_app_found(
        self,
        discovery: AppDiscoveryService,
    ) -> None:
        """登録済み App を取得できる."""
        await discovery.scan()
        app = discovery.get_app("test_app")
        assert app is not None
        assert app.display_name == "テストアプリ"

    @pytest.mark.asyncio()
    async def test_get_app_not_found(
        self,
        discovery: AppDiscoveryService,
    ) -> None:
        """未登録 App は None を返す."""
        await discovery.scan()
        assert discovery.get_app("nonexistent") is None

    def test_register_manual(self) -> None:
        """手動登録が動作する."""
        svc = AppDiscoveryService()
        cfg = AppConfig(
            name="manual_app",
            display_name="Manual",
            product_line="framework",
            surface_profile="developer",
            audit_profile="developer",
            plugin_bindings=[],
        )
        svc.register(cfg)
        assert svc.get_app("manual_app") is not None

    @pytest.mark.asyncio()
    async def test_summary(
        self,
        discovery: AppDiscoveryService,
    ) -> None:
        """summary() が正しい統計を返す."""
        await discovery.scan()
        s = discovery.summary()
        assert s["total_apps"] == 3
        # test_app(2) + minimal_app(0) + library_app(1) = 3
        assert s["total_agents"] == 3
        assert "product_line_counts" in s
        assert len(s["apps"]) == 3
        first = s["apps"][0]
        assert "agent_count" in first
        assert "has_api" in first
        assert "business_base" in first
        assert "product_line" in first
        assert "surface_profile" in first


class TestManifestMigration:
    """migrate_manifests() のテスト."""

    @pytest.mark.asyncio()
    async def test_migrate_manifests_dry_run(self, discovery: AppDiscoveryService) -> None:
        """dry_run で変更計画を返す."""
        await discovery.scan()
        report = discovery.migrate_manifests(dry_run=True)
        assert report["total"] == 3
        assert report["dry_run"] is True
        assert report["changed"] >= 1

    @pytest.mark.asyncio()
    async def test_migrate_manifests_apply_idempotent(self, discovery: AppDiscoveryService) -> None:
        """適用後の再実行は差分 0 になる."""
        await discovery.scan()
        first = discovery.migrate_manifests(dry_run=False)
        await discovery.scan()
        second = discovery.migrate_manifests(dry_run=False)
        assert first["changed"] >= 1
        assert second["changed"] == 0
