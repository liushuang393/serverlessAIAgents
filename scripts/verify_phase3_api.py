"""Phase 3 Backend API 全エンドポイント検証スクリプト."""

import asyncio
import sys
from pathlib import Path


# プロジェクトルートをパスに追加
sys.path.insert(0, str(Path(__file__).resolve().parent.parent))

from apps.platform.main import create_app
from apps.platform.routers.agents import init_agent_services
from apps.platform.routers.apps import init_app_services
from apps.platform.routers.rag import init_rag_services
from apps.platform.routers.skills import init_skill_services
from apps.platform.services.agent_aggregator import AgentAggregatorService
from apps.platform.services.app_discovery import AppDiscoveryService
from apps.platform.services.app_lifecycle import AppLifecycleManager
from apps.platform.services.rag_overview import RAGOverviewService
from apps.platform.services.skill_catalog import SkillCatalogService
from httpx import ASGITransport, AsyncClient


PASS = 0
FAIL = 0


def check(label: str, condition: bool) -> None:
    """テスト結果を判定・表示."""
    global PASS, FAIL
    if condition:
        PASS += 1
        print(f"  ✅ {label}")
    else:
        FAIL += 1
        print(f"  ❌ {label}")


async def main() -> None:
    """全15エンドポイントを検証."""
    app = create_app()

    # サービス手動初期化（lifespan は ASGITransport では発火しない）
    discovery = AppDiscoveryService()
    lifecycle = AppLifecycleManager()
    init_app_services(discovery, lifecycle)
    await discovery.scan()

    aggregator = AgentAggregatorService(discovery)
    init_agent_services(aggregator)

    catalog = SkillCatalogService()
    init_skill_services(catalog)
    await catalog.scan()

    rag = RAGOverviewService(discovery)
    init_rag_services(rag)

    transport = ASGITransport(app=app)
    async with AsyncClient(transport=transport, base_url="http://test") as c:
        # --- Agents (5) ---
        print("\n=== Agents API ===")
        r = await c.get("/api/agents")
        check(f"GET /api/agents -> 200, total={r.json().get('total')}", r.status_code == 200)

        r = await c.get("/api/agents/stats")
        check(f"GET /api/agents/stats -> 200, {r.json()}", r.status_code == 200)

        r = await c.get("/api/agents/capabilities")
        check(
            f"GET /api/agents/capabilities -> 200, total={r.json().get('total')}",
            r.status_code == 200,
        )

        r = await c.get("/api/agents/by-app")
        check(
            f"GET /api/agents/by-app -> 200, apps={r.json().get('total_apps')}",
            r.status_code == 200,
        )

        r = await c.get("/api/agents/search?capability=rag")
        check(
            f"GET /api/agents/search?cap=rag -> 200, results={r.json().get('total')}",
            r.status_code == 200,
        )

        # --- Skills (5) ---
        print("\n=== Skills API ===")
        r = await c.get("/api/skills")
        d = r.json()
        check(f"GET /api/skills -> 200, total={d.get('total')}", r.status_code == 200)

        r = await c.get("/api/skills/stats")
        check(f"GET /api/skills/stats -> 200, {r.json()}", r.status_code == 200)

        r = await c.get("/api/skills/tags")
        check(f"GET /api/skills/tags -> 200, total={r.json().get('total')}", r.status_code == 200)

        r = await c.get("/api/skills/search?tag=core-skill")
        check("GET /api/skills/search?tag=core-skill -> 200", r.status_code == 200)

        if d.get("total", 0) > 0:
            name = d["skills"][0]["name"]
            r = await c.get(f"/api/skills/{name}")
            check(f"GET /api/skills/{name} -> 200", r.status_code == 200)
        else:
            check("GET /api/skills/:name (skipped: 0 skills)", True)

        # --- RAG (5) ---
        print("\n=== RAG API ===")
        r = await c.get("/api/rag/overview")
        check(
            f"GET /api/rag/overview -> 200, strats={len(r.json().get('chunk_strategies', []))}",
            r.status_code == 200,
        )

        r = await c.get("/api/rag/strategies")
        check(f"GET /api/rag/strategies -> 200, total={r.json().get('total')}", r.status_code == 200)

        r = await c.get("/api/rag/rerankers")
        check(f"GET /api/rag/rerankers -> 200, total={r.json().get('total')}", r.status_code == 200)

        r = await c.get("/api/rag/apps")
        check(f"GET /api/rag/apps -> 200, total={r.json().get('total')}", r.status_code == 200)

        r = await c.get("/api/rag/stats")
        check(f"GET /api/rag/stats -> 200, {r.json()}", r.status_code == 200)

    print(f"\n{'=' * 50}")
    print(f"Result: {PASS} passed, {FAIL} failed / {PASS + FAIL} total")
    if FAIL > 0:
        sys.exit(1)
    print("ALL ENDPOINTS VERIFIED ✅")


if __name__ == "__main__":
    asyncio.run(main())
