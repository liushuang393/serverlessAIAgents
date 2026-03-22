"""Platform テスト共有フィクスチャ.

目的: テスト用の一時 app_config.json、TestClient、Mock サービスを提供。
Phase 0-1 および Phase 3 (Agent/Skill/RAG) のテストで共有する。
"""

from __future__ import annotations

import asyncio
import json
import os
from contextlib import asynccontextmanager
from typing import TYPE_CHECKING, Any

import httpx
import pytest
from httpx import Response

from control_plane.schemas.app_config_schemas import AppConfig
from control_plane.services.agent_aggregator import AgentAggregatorService
from control_plane.services.app_discovery import AppDiscoveryService
from control_plane.services.app_lifecycle import (
    AppActionResult,
    AppLifecycleManager,
    AppStatus,
    HealthCheckResult,
)
from control_plane.services.rag_overview import RAGOverviewService
from control_plane.services.skill_catalog import SkillCatalogService
from control_plane.services.tenant_invitation import TenantInvitationService


if TYPE_CHECKING:
    from pathlib import Path


class SyncASGIClient:
    """pytest 用の同期 ASGI クライアント."""

    def __init__(self, app: Any, base_url: str = "http://testserver") -> None:
        self._loop = asyncio.new_event_loop()
        self._transport = httpx.ASGITransport(app=app, raise_app_exceptions=False)
        self._client = httpx.AsyncClient(transport=self._transport, base_url=base_url)

    def request(self, method: str, url: str, **kwargs: Any) -> Response:
        return self._loop.run_until_complete(self._client.request(method, url, **kwargs))

    def get(self, url: str, **kwargs: Any) -> Response:
        return self.request("GET", url, **kwargs)

    def post(self, url: str, **kwargs: Any) -> Response:
        return self.request("POST", url, **kwargs)

    def put(self, url: str, **kwargs: Any) -> Response:
        return self.request("PUT", url, **kwargs)

    def patch(self, url: str, **kwargs: Any) -> Response:
        return self.request("PATCH", url, **kwargs)

    def delete(self, url: str, **kwargs: Any) -> Response:
        return self.request("DELETE", url, **kwargs)

    def close(self) -> None:
        self._loop.run_until_complete(self._client.aclose())
        self._loop.close()


# ------------------------------------------------------------------
# サンプルデータ
# ------------------------------------------------------------------

SAMPLE_APP_CONFIG: dict[str, Any] = {
    "name": "test_app",
    "display_name": "テストアプリ",
    "description": "テスト用アプリケーション",
    "product_line": "migration",
    "surface_profile": "business",
    "audit_profile": "business",
    "plugin_bindings": [],
    "version": "1.0.0",
    "icon": "🧪",
    "ports": {"api": 8099, "frontend": 3099},
    "entry_points": {"api_module": "apps.test_app.main:app", "health": "/health"},
    "agents": [
        {"name": "TestAgent", "module": "apps.test_app.agents.test", "capabilities": ["test"]},
        {"name": "HelperAgent", "capabilities": ["helper", "util"]},
    ],
    "services": {"cache": {"type": "redis"}},
    "dependencies": {"database": "postgresql", "redis": True, "external": ["openai"]},
    "tags": ["test", "sample"],
}

SAMPLE_APP_CONFIG_MINIMAL: dict[str, Any] = {
    "name": "minimal_app",
    "display_name": "最小アプリ",
    "product_line": "framework",
    "surface_profile": "developer",
    "audit_profile": "developer",
    "plugin_bindings": [],
}

SAMPLE_APP_CONFIG_NO_API: dict[str, Any] = {
    "name": "library_app",
    "display_name": "ライブラリアプリ",
    "product_line": "framework",
    "surface_profile": "developer",
    "audit_profile": "developer",
    "plugin_bindings": [],
    "ports": {},
    "entry_points": {"health": None},
    "agents": [{"name": "LibAgent", "capabilities": ["lib"]}],
}

SAMPLE_APP_CONFIG_RAG: dict[str, Any] = {
    "name": "rag_app",
    "display_name": "RAG アプリ",
    "product_line": "framework",
    "surface_profile": "developer",
    "audit_profile": "developer",
    "plugin_bindings": [],
    "icon": "📚",
    "agents": [
        {"name": "RAGAgent", "capabilities": ["rag", "search"]},
        {"name": "IndexAgent", "capabilities": ["indexing"]},
    ],
    "services": {
        "rag": {"collections": ["docs_kb", "faq_kb"]},
        "sql": {"dialect": "postgresql"},
    },
    "dependencies": {"database": "postgresql", "redis": False, "external": []},
    "runtime": {
        "urls": {
            "database": "postgresql+asyncpg://rag:password@localhost:5434/rag_app",
        },
        "database": {
            "kind": "postgresql",
            "url": "postgresql+asyncpg://rag:password@localhost:5434/rag_app",
            "host": "localhost",
            "port": 5434,
            "name": "rag_app",
            "user": "rag",
        },
    },
    "tags": ["rag", "search"],
}

SAMPLE_SKILL_MD_CHATBOT = """\
---
name: chatbot
description: 汎用チャットボットスキル
version: "1.0.0"
author: BizCore Team
tags:
  - chat
  - conversation
  - nlp
triggers:
  - こんにちは
  - 教えて
requirements:
  - openai
examples:
  - "こんにちは、今日の天気は？"
---

# Chatbot Skill

汎用チャットボットスキルの説明。
"""

SAMPLE_SKILL_MD_RAG = """\
---
name: rag
description: RAG 検索スキル
version: "2.0.0"
author: BizCore Team
tags:
  - rag
  - search
  - retrieval
triggers:
  - 検索して
  - 調べて
requirements:
  - chromadb
  - openai
examples:
  - "このドキュメントについて検索して"
---

# RAG Skill

RAG 検索スキルの説明。
"""

SAMPLE_SKILL_MD_INVALID = """\
This file has no frontmatter at all.
Just plain text content.
"""


# ------------------------------------------------------------------
# フィクスチャ
# ------------------------------------------------------------------


@pytest.fixture
def sample_config() -> AppConfig:
    """検証済み AppConfig インスタンスを返す."""
    return AppConfig.model_validate(SAMPLE_APP_CONFIG)


@pytest.fixture
def apps_dir(tmp_path: Path) -> Path:
    """一時 apps ディレクトリに app_config.json を配置して返す."""
    # test_app
    app_dir = tmp_path / "test_app"
    app_dir.mkdir()
    (app_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG, ensure_ascii=False),
        encoding="utf-8",
    )
    # minimal_app
    min_dir = tmp_path / "minimal_app"
    min_dir.mkdir()
    (min_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG_MINIMAL, ensure_ascii=False),
        encoding="utf-8",
    )
    # library_app (API ポートなし)
    lib_dir = tmp_path / "library_app"
    lib_dir.mkdir()
    (lib_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG_NO_API, ensure_ascii=False),
        encoding="utf-8",
    )
    return tmp_path


@pytest.fixture
def discovery(apps_dir: Path) -> AppDiscoveryService:
    """一時ディレクトリを使う AppDiscoveryService を返す."""
    return AppDiscoveryService(apps_dir=apps_dir)


@pytest.fixture
def lifecycle() -> AppLifecycleManager:
    """AppLifecycleManager インスタンスを返す."""
    return AppLifecycleManager()


@pytest.fixture
def test_client(apps_dir: Path) -> SyncASGIClient:
    """Platform FastAPI TestClient を返す（サービス初期化 + scan 済み）."""
    import asyncio

    from control_plane.main import create_app
    from control_plane.routers.apps import init_app_services
    from control_plane.routers.studios import init_studio_services
    from control_plane.routers.tenant_invitations import init_tenant_invitation_services
    from control_plane.services.studio_service import StudioService

    os.environ["PLATFORM_INVITE_ENABLE_OUTBOX_ENDPOINT"] = "true"
    app = create_app()

    # テストでは本番 lifespan を無効化し、明示的に注入したサービスのみを使う
    @asynccontextmanager
    async def _no_lifespan(_app):
        yield

    app.router.lifespan_context = _no_lifespan

    disc = AppDiscoveryService(apps_dir=apps_dir)
    lc = AppLifecycleManager()

    async def _mock_publish(config: AppConfig, *, config_path: Path | None = None) -> AppActionResult:
        del config_path
        return AppActionResult(
            app_name=config.name,
            action="publish",
            success=True,
            command=["echo", "mock-publish"],
            cwd=str(apps_dir / config.name),
            command_source="runtime",
            return_code=0,
            stdout="mock publish ok",
        )

    async def _mock_start(config: AppConfig, *, config_path: Path | None = None) -> AppActionResult:
        del config_path
        return AppActionResult(
            app_name=config.name,
            action="start",
            success=True,
            command=["echo", "mock-start"],
            cwd=str(apps_dir / config.name),
            command_source="readme",
            return_code=0,
            stdout="mock start ok",
        )

    async def _mock_stop(config: AppConfig, *, config_path: Path | None = None) -> AppActionResult:
        del config_path
        return AppActionResult(
            app_name=config.name,
            action="stop",
            success=True,
            command=["echo", "mock-stop"],
            cwd=str(apps_dir / config.name),
            command_source="runtime",
            return_code=0,
            stdout="mock stop ok",
        )

    async def _mock_check_health(config: AppConfig, *, config_path: Path | None = None) -> HealthCheckResult:
        del config_path
        status = AppStatus.UNKNOWN if config.name == "library_app" else AppStatus.HEALTHY
        return HealthCheckResult(
            app_name=config.name,
            status=status,
            response_time_ms=1.0,
            details={"mock": True},
        )

    async def _mock_cli_status(config: AppConfig) -> dict[str, Any]:
        return {
            "preferred": ["codex", "claude"],
            "tools": {
                "codex": {"detected": True, "authenticated": True},
                "claude": {"detected": False, "authenticated": False},
            },
            "app_name": config.name,
        }

    async def _mock_cli_setup(config: AppConfig) -> dict[str, Any]:
        return {
            "ready": True,
            "status": await _mock_cli_status(config),
            "preflight": {"ready": True},
        }

    lc.publish_app = _mock_publish  # type: ignore[method-assign]
    lc.start_app = _mock_start  # type: ignore[method-assign]
    lc.stop_app = _mock_stop  # type: ignore[method-assign]
    lc.check_health = _mock_check_health  # type: ignore[method-assign]
    lc.cli_status = _mock_cli_status  # type: ignore[method-assign]
    lc.cli_setup = _mock_cli_setup  # type: ignore[method-assign]

    init_app_services(disc, lc)
    init_studio_services(StudioService(disc, lc))
    init_tenant_invitation_services(TenantInvitationService())

    # scan() を同期的に実行してレジストリにデータを投入
    asyncio.run(disc.scan())

    client = SyncASGIClient(app)
    try:
        yield client
    finally:
        client.close()


# ------------------------------------------------------------------
# Phase 3: Agent / Skill / RAG フィクスチャ
# ------------------------------------------------------------------


@pytest.fixture
def apps_dir_with_rag(tmp_path: Path) -> Path:
    """RAG 対応 App を含む一時 apps ディレクトリ."""
    # test_app（Agent あり、RAG なし）
    app_dir = tmp_path / "test_app"
    app_dir.mkdir()
    (app_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG, ensure_ascii=False),
        encoding="utf-8",
    )
    # rag_app（RAG 対応 Agent + RAG サービス設定あり）
    rag_dir = tmp_path / "rag_app"
    rag_dir.mkdir()
    (rag_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG_RAG, ensure_ascii=False),
        encoding="utf-8",
    )
    # minimal_app（Agent なし）
    min_dir = tmp_path / "minimal_app"
    min_dir.mkdir()
    (min_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG_MINIMAL, ensure_ascii=False),
        encoding="utf-8",
    )
    return tmp_path


@pytest.fixture
def discovery_with_rag(apps_dir_with_rag: Path) -> AppDiscoveryService:
    """RAG 対応 App を含む AppDiscoveryService."""
    return AppDiscoveryService(apps_dir=apps_dir_with_rag)


@pytest.fixture
def aggregator(discovery: AppDiscoveryService) -> AgentAggregatorService:
    """スキャン済み AppDiscoveryService を使う AgentAggregatorService."""
    asyncio.run(discovery.scan())
    return AgentAggregatorService(discovery)


@pytest.fixture
def aggregator_with_rag(discovery_with_rag: AppDiscoveryService) -> AgentAggregatorService:
    """RAG 対応 App を含む AgentAggregatorService."""
    asyncio.run(discovery_with_rag.scan())
    return AgentAggregatorService(discovery_with_rag)


@pytest.fixture
def skills_dir(tmp_path: Path) -> Path:
    """テスト用スキルディレクトリ（SKILL.md 配置済み）."""
    # chatbot スキル
    chatbot_dir = tmp_path / "chatbot"
    chatbot_dir.mkdir()
    (chatbot_dir / "SKILL.md").write_text(
        SAMPLE_SKILL_MD_CHATBOT,
        encoding="utf-8",
    )
    # rag スキル
    rag_dir = tmp_path / "rag"
    rag_dir.mkdir()
    (rag_dir / "SKILL.md").write_text(
        SAMPLE_SKILL_MD_RAG,
        encoding="utf-8",
    )
    # invalid スキル（frontmatter なし）
    invalid_dir = tmp_path / "invalid_skill"
    invalid_dir.mkdir()
    (invalid_dir / "SKILL.md").write_text(
        SAMPLE_SKILL_MD_INVALID,
        encoding="utf-8",
    )
    return tmp_path


@pytest.fixture
def skill_catalog(skills_dir: Path) -> SkillCatalogService:
    """テスト用 SkillCatalogService（スキャン済み）."""
    catalog = SkillCatalogService(skills_dir=skills_dir)
    asyncio.run(catalog.scan())
    return catalog


@pytest.fixture
def rag_overview(discovery_with_rag: AppDiscoveryService) -> RAGOverviewService:
    """RAG 対応 App を含む RAGOverviewService."""
    asyncio.run(discovery_with_rag.scan())
    return RAGOverviewService(discovery_with_rag)


@pytest.fixture
def phase3_test_client(apps_dir_with_rag: Path, skills_dir: Path) -> SyncASGIClient:
    """Phase 3 全サービス初期化済み TestClient."""
    import asyncio
    from contextlib import asynccontextmanager

    from control_plane.main import create_app
    from control_plane.routers.agents import init_agent_services
    from control_plane.routers.apps import init_app_services
    from control_plane.routers.rag import init_rag_services
    from control_plane.routers.skills import init_skill_services
    from control_plane.routers.studios import init_studio_services
    from control_plane.routers.tenant_invitations import init_tenant_invitation_services
    from control_plane.services.studio_service import StudioService

    os.environ["PLATFORM_INVITE_ENABLE_OUTBOX_ENDPOINT"] = "true"
    app = create_app()

    # テストでは本番 lifespan を無効化し、明示的に注入したサービスのみを利用
    @asynccontextmanager
    async def _no_lifespan(_app):
        yield

    app.router.lifespan_context = _no_lifespan

    disc = AppDiscoveryService(apps_dir=apps_dir_with_rag)
    lc = AppLifecycleManager()
    init_app_services(disc, lc)
    init_studio_services(StudioService(disc, lc))
    init_tenant_invitation_services(TenantInvitationService())
    asyncio.run(disc.scan())

    # Agent サービス
    agg = AgentAggregatorService(disc)
    init_agent_services(agg)

    # Skill サービス
    catalog = SkillCatalogService(skills_dir=skills_dir)
    asyncio.run(catalog.scan())
    init_skill_services(catalog)

    # RAG サービス
    rag = RAGOverviewService(disc)
    init_rag_services(rag)

    client = SyncASGIClient(app)
    try:
        yield client
    finally:
        client.close()
