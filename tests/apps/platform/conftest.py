# -*- coding: utf-8 -*-
"""Platform テスト共有フィクスチャ.

目的: テスト用の一時 app_config.json、TestClient、Mock サービスを提供。
Phase 0-1 および Phase 3 (Agent/Skill/RAG) のテストで共有する。
"""

from __future__ import annotations

import json
from contextlib import asynccontextmanager
from pathlib import Path
from typing import Any

import pytest
from fastapi.testclient import TestClient

from apps.platform.schemas.app_config_schemas import AppConfig
from apps.platform.services.agent_aggregator import AgentAggregatorService
from apps.platform.services.app_discovery import AppDiscoveryService
from apps.platform.services.app_lifecycle import AppLifecycleManager
from apps.platform.services.rag_overview import RAGOverviewService
from apps.platform.services.skill_catalog import SkillCatalogService


# ------------------------------------------------------------------
# サンプルデータ
# ------------------------------------------------------------------

SAMPLE_APP_CONFIG: dict[str, Any] = {
    "name": "test_app",
    "display_name": "テストアプリ",
    "description": "テスト用アプリケーション",
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
}

SAMPLE_APP_CONFIG_NO_API: dict[str, Any] = {
    "name": "library_app",
    "display_name": "ライブラリアプリ",
    "ports": {},
    "entry_points": {"health": None},
    "agents": [{"name": "LibAgent", "capabilities": ["lib"]}],
}

SAMPLE_APP_CONFIG_RAG: dict[str, Any] = {
    "name": "rag_app",
    "display_name": "RAG アプリ",
    "icon": "📚",
    "agents": [
        {"name": "RAGAgent", "capabilities": ["rag", "search"]},
        {"name": "IndexAgent", "capabilities": ["indexing"]},
    ],
    "services": {"rag": {"collections": ["docs_kb", "faq_kb"]}},
    "tags": ["rag", "search"],
}

SAMPLE_SKILL_MD_CHATBOT = """\
---
name: chatbot
description: 汎用チャットボットスキル
version: "1.0.0"
author: AgentFlow Team
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
author: AgentFlow Team
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


@pytest.fixture()
def sample_config() -> AppConfig:
    """検証済み AppConfig インスタンスを返す."""
    return AppConfig.model_validate(SAMPLE_APP_CONFIG)


@pytest.fixture()
def apps_dir(tmp_path: Path) -> Path:
    """一時 apps ディレクトリに app_config.json を配置して返す."""
    # test_app
    app_dir = tmp_path / "test_app"
    app_dir.mkdir()
    (app_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG, ensure_ascii=False), encoding="utf-8",
    )
    # minimal_app
    min_dir = tmp_path / "minimal_app"
    min_dir.mkdir()
    (min_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG_MINIMAL, ensure_ascii=False), encoding="utf-8",
    )
    # library_app (API ポートなし)
    lib_dir = tmp_path / "library_app"
    lib_dir.mkdir()
    (lib_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG_NO_API, ensure_ascii=False), encoding="utf-8",
    )
    return tmp_path


@pytest.fixture()
def discovery(apps_dir: Path) -> AppDiscoveryService:
    """一時ディレクトリを使う AppDiscoveryService を返す."""
    return AppDiscoveryService(apps_dir=apps_dir)


@pytest.fixture()
def lifecycle() -> AppLifecycleManager:
    """AppLifecycleManager インスタンスを返す."""
    return AppLifecycleManager()


@pytest.fixture()
def test_client(apps_dir: Path) -> TestClient:
    """Platform FastAPI TestClient を返す（サービス初期化 + scan 済み）."""
    import asyncio

    from apps.platform.main import create_app
    from apps.platform.routers.apps import init_app_services

    app = create_app()

    # テストでは本番 lifespan を無効化し、明示的に注入したサービスのみを使う
    @asynccontextmanager
    async def _no_lifespan(_app):
        yield

    app.router.lifespan_context = _no_lifespan

    disc = AppDiscoveryService(apps_dir=apps_dir)
    lc = AppLifecycleManager()
    init_app_services(disc, lc)

    # scan() を同期的に実行してレジストリにデータを投入
    asyncio.get_event_loop().run_until_complete(disc.scan())

    with TestClient(app, raise_server_exceptions=False) as client:
        yield client


# ------------------------------------------------------------------
# Phase 3: Agent / Skill / RAG フィクスチャ
# ------------------------------------------------------------------


@pytest.fixture()
def apps_dir_with_rag(tmp_path: Path) -> Path:
    """RAG 対応 App を含む一時 apps ディレクトリ."""
    # test_app（Agent あり、RAG なし）
    app_dir = tmp_path / "test_app"
    app_dir.mkdir()
    (app_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG, ensure_ascii=False), encoding="utf-8",
    )
    # rag_app（RAG 対応 Agent + RAG サービス設定あり）
    rag_dir = tmp_path / "rag_app"
    rag_dir.mkdir()
    (rag_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG_RAG, ensure_ascii=False), encoding="utf-8",
    )
    # minimal_app（Agent なし）
    min_dir = tmp_path / "minimal_app"
    min_dir.mkdir()
    (min_dir / "app_config.json").write_text(
        json.dumps(SAMPLE_APP_CONFIG_MINIMAL, ensure_ascii=False), encoding="utf-8",
    )
    return tmp_path


@pytest.fixture()
def discovery_with_rag(apps_dir_with_rag: Path) -> AppDiscoveryService:
    """RAG 対応 App を含む AppDiscoveryService."""
    return AppDiscoveryService(apps_dir=apps_dir_with_rag)


@pytest.fixture()
def aggregator(discovery: AppDiscoveryService) -> AgentAggregatorService:
    """スキャン済み AppDiscoveryService を使う AgentAggregatorService."""
    import asyncio
    asyncio.get_event_loop().run_until_complete(discovery.scan())
    return AgentAggregatorService(discovery)


@pytest.fixture()
def aggregator_with_rag(discovery_with_rag: AppDiscoveryService) -> AgentAggregatorService:
    """RAG 対応 App を含む AgentAggregatorService."""
    import asyncio
    asyncio.get_event_loop().run_until_complete(discovery_with_rag.scan())
    return AgentAggregatorService(discovery_with_rag)


@pytest.fixture()
def skills_dir(tmp_path: Path) -> Path:
    """テスト用スキルディレクトリ（SKILL.md 配置済み）."""
    # chatbot スキル
    chatbot_dir = tmp_path / "chatbot"
    chatbot_dir.mkdir()
    (chatbot_dir / "SKILL.md").write_text(
        SAMPLE_SKILL_MD_CHATBOT, encoding="utf-8",
    )
    # rag スキル
    rag_dir = tmp_path / "rag"
    rag_dir.mkdir()
    (rag_dir / "SKILL.md").write_text(
        SAMPLE_SKILL_MD_RAG, encoding="utf-8",
    )
    # invalid スキル（frontmatter なし）
    invalid_dir = tmp_path / "invalid_skill"
    invalid_dir.mkdir()
    (invalid_dir / "SKILL.md").write_text(
        SAMPLE_SKILL_MD_INVALID, encoding="utf-8",
    )
    return tmp_path


@pytest.fixture()
def skill_catalog(skills_dir: Path) -> SkillCatalogService:
    """テスト用 SkillCatalogService（スキャン済み）."""
    import asyncio
    catalog = SkillCatalogService(skills_dir=skills_dir)
    asyncio.get_event_loop().run_until_complete(catalog.scan())
    return catalog


@pytest.fixture()
def rag_overview(discovery_with_rag: AppDiscoveryService) -> RAGOverviewService:
    """RAG 対応 App を含む RAGOverviewService."""
    import asyncio
    asyncio.get_event_loop().run_until_complete(discovery_with_rag.scan())
    return RAGOverviewService(discovery_with_rag)


@pytest.fixture()
def phase3_test_client(apps_dir_with_rag: Path, skills_dir: Path) -> TestClient:
    """Phase 3 全サービス初期化済み TestClient."""
    import asyncio
    from contextlib import asynccontextmanager

    from apps.platform.main import create_app
    from apps.platform.routers.agents import init_agent_services
    from apps.platform.routers.apps import init_app_services
    from apps.platform.routers.rag import init_rag_services
    from apps.platform.routers.skills import init_skill_services

    app = create_app()

    # テストでは本番 lifespan を無効化し、明示的に注入したサービスのみを利用
    @asynccontextmanager
    async def _no_lifespan(_app):
        yield

    app.router.lifespan_context = _no_lifespan

    disc = AppDiscoveryService(apps_dir=apps_dir_with_rag)
    lc = AppLifecycleManager()
    init_app_services(disc, lc)
    asyncio.get_event_loop().run_until_complete(disc.scan())

    # Agent サービス
    agg = AgentAggregatorService(disc)
    init_agent_services(agg)

    # Skill サービス
    catalog = SkillCatalogService(skills_dir=skills_dir)
    asyncio.get_event_loop().run_until_complete(catalog.scan())
    init_skill_services(catalog)

    # RAG サービス
    rag = RAGOverviewService(disc)
    init_rag_services(rag)

    with TestClient(app, raise_server_exceptions=False) as client:
        yield client
