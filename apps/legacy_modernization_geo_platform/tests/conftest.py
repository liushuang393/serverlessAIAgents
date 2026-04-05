"""GEO Platform テスト共通フィクスチャ.

harness/testing の標準フィクスチャを活用したテスト基盤。
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import pytest
from apps.legacy_modernization_geo_platform.backend.settings import GeoPlatformSettings
from apps.legacy_modernization_geo_platform.main import create_app

from harness.testing.fixtures import clean_env_fixture
from infrastructure.security.auth_client.client import RemoteUser
from kernel.runtime import resolve_app_runtime


if TYPE_CHECKING:
    from collections.abc import Generator
    from pathlib import Path

    from fastapi import FastAPI
    from fastapi.testclient import TestClient


# ---------------------------------------------------------------------------
# 認証スタブ（harness 標準に合わせた共通スタブ）
# ---------------------------------------------------------------------------


class StubAuthClient:
    """テスト用の認証クライアントスタブ."""

    def __init__(self, **kwargs: object) -> None:
        self.kwargs = kwargs

    async def verify_token(self, token: str) -> RemoteUser | None:
        if token != "valid-token":
            return None
        return RemoteUser(
            user_id="geo-operator",
            username="operator",
            role="operator",
            roles=["operator"],
            tenant_id="tenant-a",
            scopes=["geo.operator"],
            permissions=["geo.publish"],
            azp="geo-platform",
            extra={"tenant_id": "tenant-a"},
        )


# ---------------------------------------------------------------------------
# 共通フィクスチャ
# ---------------------------------------------------------------------------

AUTH_HEADERS: dict[str, str] = {
    "Authorization": "Bearer valid-token",
    "x-tenant-id": "tenant-a",
}


@pytest.fixture
def geo_settings(tmp_path: Path) -> GeoPlatformSettings:
    """テスト用 GeoPlatformSettings を生成する."""
    runtime = resolve_app_runtime("apps/legacy_modernization_geo_platform/app_config.json")
    if runtime.ports.api is None or runtime.urls.backend is None or runtime.urls.frontend is None:
        msg = "legacy_modernization_geo_platform の manifest に必要な runtime 値がありません。"
        raise AssertionError(msg)
    frontend_dist_dir = tmp_path / "frontend-dist"
    frontend_dist_dir.mkdir(parents=True, exist_ok=True)
    (frontend_dist_dir / "index.html").write_text("<html><body>test</body></html>", encoding="utf-8")
    return GeoPlatformSettings(
        app_root=tmp_path,
        host="127.0.0.1",
        port=runtime.ports.api,
        api_key="",
        cors_origins=[runtime.urls.frontend],
        db_path=tmp_path / "data" / "geo.db",
        artifacts_dir=tmp_path / "data" / "artifacts",
        reports_dir=tmp_path / "data" / "reports",
        published_dir=tmp_path / "data" / "published",
        public_base_url=runtime.urls.backend,
        frontend_dist_dir=frontend_dist_dir,
    )


@pytest.fixture
def geo_app(geo_settings: GeoPlatformSettings) -> FastAPI:
    """テスト用 FastAPI アプリケーションを生成する."""
    return create_app(geo_settings, auth_client_factory=StubAuthClient)


@pytest.fixture
def geo_client(geo_app: FastAPI) -> Generator[TestClient]:
    """テスト用 HTTP クライアントを生成する（blocking portal 維持）."""
    from fastapi.testclient import TestClient as _TestClient

    with _TestClient(geo_app) as client:
        yield client


@pytest.fixture
def clean_env() -> Generator[dict[str, str]]:
    """harness/testing 標準のクリーン環境フィクスチャ."""
    with clean_env_fixture() as env:
        yield env
