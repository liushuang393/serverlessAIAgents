"""BizCore control-plane API entrypoint."""

from __future__ import annotations

import argparse
import asyncio
import importlib
import logging
import os
import sys
from contextlib import asynccontextmanager
from pathlib import Path
from typing import TYPE_CHECKING, Any

import uvicorn
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from infrastructure.llm.gateway.config import register_platform_secret_resolver
from kernel.runtime.app_manifest import resolve_app_runtime
from control_plane.engine import PlatformEngine


if TYPE_CHECKING:
    from collections.abc import AsyncIterator


def _load_legacy_symbol(module_path: str, symbol_name: str) -> Any:
    """旧実装モジュールからシンボルを遅延取得する。"""
    module = importlib.import_module(module_path)
    return getattr(module, symbol_name)


def _load_runtime_defaults() -> tuple[str, int]:
    """app_config.json から既定 host/port を解決する。"""
    config_path = Path(__file__).resolve().parents[1] / "app_config.json"
    runtime = resolve_app_runtime(
        config_path,
        env=os.environ,
        backend_host_env="HOST",
        backend_port_env="PORT",
        backend_url_env="PLATFORM_URL",
    )
    return runtime.hosts.backend or "0.0.0.0", runtime.ports.api or 8900


DEFAULT_API_HOST, DEFAULT_API_PORT = _load_runtime_defaults()


def _load_publish_contracts() -> tuple[type[Any], type[Any]]:
    """公開 API 用の publish 契約型を取得する。"""
    request_type = _load_legacy_symbol("control_plane.publish.contracts", "PublishRequest")
    target_type = _load_legacy_symbol("control_plane.publish.contracts", "PublishTarget")
    return request_type, target_type


def _include_legacy_routers(app: FastAPI) -> None:
    """既存 routers を登録する。"""
    routers = importlib.import_module("control_plane.api.routers")
    router_names = (
        "gallery_router",
        "components_router",
        "publish_router",
        "dashboard_router",
        "apps_router",
        "agents_router",
        "skills_router",
        "rag_router",
        "mcp_router",
        "llm_management_router",
        "studios_router",
        "tenant_invitations_router",
    )
    for router_name in router_names:
        app.include_router(getattr(routers, router_name))


@asynccontextmanager
async def _lifespan(app: FastAPI) -> AsyncIterator[None]:
    """アプリケーション起動/終了時のライフサイクル処理."""
    del app
    logger = logging.getLogger(__name__)

    app_discovery_cls = _load_legacy_symbol("control_plane.discovery.runtime", "AppDiscoveryService")
    app_lifecycle_cls = _load_legacy_symbol("control_plane.lifecycle.runtime", "AppLifecycleManager")
    app_scaffolder_cls = _load_legacy_symbol("control_plane.discovery.runtime", "AppScaffolderService")
    port_allocator_cls = _load_legacy_symbol("control_plane.lifecycle.runtime", "PortAllocatorService")
    init_app_config_event_store = _load_legacy_symbol("control_plane.config_center.runtime", "init_app_config_event_store")
    config_watcher_cls = _load_legacy_symbol("control_plane.config_center.runtime", "ConfigWatcherService")
    agent_aggregator_cls = _load_legacy_symbol("control_plane.registry.runtime", "AgentAggregatorService")
    skill_catalog_cls = _load_legacy_symbol("control_plane.registry.runtime", "SkillCatalogService")
    rag_overview_cls = _load_legacy_symbol("control_plane.registry.runtime", "RAGOverviewService")
    init_rag_config_store = _load_legacy_symbol("control_plane.config_center.runtime", "init_rag_config_store")
    mcp_registry_cls = _load_legacy_symbol("control_plane.registry.runtime", "MCPRegistryService")
    studio_service_cls = _load_legacy_symbol("control_plane.registry.runtime", "StudioService")
    tenant_invitation_cls = _load_legacy_symbol("control_plane.tenants.runtime", "TenantInvitationService")
    get_default_llm_management_service = _load_legacy_symbol(
        "control_plane.config_center.runtime",
        "get_default_llm_management_service",
    )
    sync_runtime_secret_cache_from_db = _load_legacy_symbol(
        "control_plane.config_center.runtime",
        "sync_runtime_secret_cache_from_db",
    )
    resolve_platform_cached_secret = _load_legacy_symbol(
        "control_plane.config_center.runtime",
        "resolve_platform_cached_secret",
    )

    ensure_platform_db_ready = _load_legacy_symbol("control_plane.operations.db", "ensure_platform_db_ready")
    close_platform_db = _load_legacy_symbol("control_plane.operations.db", "close_platform_db")

    init_app_services = _load_legacy_symbol("control_plane.api.routers", "init_app_services")
    init_agent_services = _load_legacy_symbol("control_plane.api.routers", "init_agent_services")
    init_skill_services = _load_legacy_symbol("control_plane.api.routers", "init_skill_services")
    init_rag_services = _load_legacy_symbol("control_plane.api.routers", "init_rag_services")
    init_mcp_services = _load_legacy_symbol("control_plane.api.routers", "init_mcp_services")
    init_studio_services = _load_legacy_symbol("control_plane.api.routers", "init_studio_services")
    init_llm_management_service = _load_legacy_symbol(
        "control_plane.api.routers",
        "init_llm_management_service",
    )
    init_tenant_invitation_services = _load_legacy_symbol(
        "control_plane.api.routers",
        "init_tenant_invitation_services",
    )
    discovery = app_discovery_cls()
    lifecycle = app_lifecycle_cls()
    scaffolder = app_scaffolder_cls(discovery)
    port_allocator = port_allocator_cls(discovery)
    app_config_events = init_app_config_event_store()

    init_app_services(
        discovery,
        lifecycle,
        scaffolder=scaffolder,
        port_allocator=port_allocator,
        event_store=app_config_events,
    )
    studio_service = studio_service_cls(discovery, lifecycle)
    init_studio_services(studio_service)

    count = await discovery.scan()
    logger.info("Control plane 起動完了: %d 件の app を検出", count)

    watcher = config_watcher_cls(discovery, event_store=app_config_events)
    watcher_task: asyncio.Task[None] = asyncio.create_task(watcher.watch(), name="config-watcher")

    aggregator = agent_aggregator_cls(discovery)
    init_agent_services(aggregator)

    skill_catalog = skill_catalog_cls()
    init_skill_services(skill_catalog)
    skill_count = await skill_catalog.scan()
    logger.info("SkillCatalog: %d 件のスキルを検出", skill_count)

    rag_overview = rag_overview_cls(discovery)
    init_rag_services(rag_overview)
    init_rag_config_store()

    mcp_registry = mcp_registry_cls()
    init_mcp_services(mcp_registry)

    await ensure_platform_db_ready()
    await sync_runtime_secret_cache_from_db()
    register_platform_secret_resolver(resolve_platform_cached_secret)

    llm_management = get_default_llm_management_service()
    init_llm_management_service(llm_management)

    tenant_invitation = tenant_invitation_cls()
    init_tenant_invitation_services(tenant_invitation)

    yield

    logger.info("Control plane シャットダウン")
    watcher_task.cancel()
    await asyncio.gather(watcher_task, return_exceptions=True)
    await close_platform_db()


def create_app() -> FastAPI:
    """FastAPI アプリケーションを作成する。"""
    app = FastAPI(
        title="BizCore Control Plane",
        description="BizCore control-plane API for studios, lifecycle, governance, and delivery",
        version="2.0.0",
        docs_url="/docs",
        redoc_url="/redoc",
        lifespan=_lifespan,
    )
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )
    _include_legacy_routers(app)

    @app.get("/health")
    async def health_check() -> dict[str, Any]:
        return {"status": "healthy", "service": "control_plane", "version": "2.0.0"}

    @app.get("/")
    async def root() -> dict[str, Any]:
        return {
            "name": "BizCore Control Plane",
            "version": "2.0.0",
            "docs": "/docs",
            "studios_api": "/api/studios",
            "framework_apps_api": "/api/studios/framework/apps",
        }

    return app


def setup_logging(verbose: bool = False) -> None:
    """ログ設定を初期化する。"""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(sys.stdout)],
    )


async def cli_search(query: str, limit: int = 10) -> None:
    """CLI: Gallery 検索。"""
    engine = PlatformEngine()
    result = await engine.search_gallery(query)

    print(f"\n🔍 Search results for '{query}':")
    print("-" * 50)
    if not result.items:
        print("No results found.")
        return

    for item in result.items[:limit]:
        print(f"  {item.icon} {item.name} ({item.id})")
        print(f"    Type: {item.type.value}, Version: {item.version}")
        if item.description:
            print(f"    {item.description[:60]}...")
        print()
    print(f"Total: {result.total} items")


async def cli_publish(
    source: str,
    target: str,
    name: str | None = None,
    gallery: bool = False,
) -> None:
    """CLI: ワンクリック公開。"""
    engine = PlatformEngine()
    publish_request_type, publish_target_type = _load_publish_contracts()

    try:
        publish_target = publish_target_type(target)
    except ValueError:
        print(f"❌ Unknown target: {target}")
        print(f"Available targets: {[item.value for item in publish_target_type]}")
        return

    request = publish_request_type(
        source_path=source,
        target=publish_target,
        name=name,
        publish_to_gallery=gallery,
    )

    print(f"\n🚀 Publishing to {target}...")
    print("-" * 50)

    async for event in engine.publish(request):
        status_icon = {
            "pending": "⏳",
            "validating": "🔍",
            "generating": "⚙️",
            "deploying": "🚀",
            "registering": "📝",
            "completed": "✅",
            "failed": "❌",
            "cancelled": "⚠️",
        }.get(event.status.value, "•")
        print(f"  {status_icon} [{event.phase or 'system'}] {event.message}")

        if event.event_type == "complete":
            print("\n✅ Publish completed!")
            if event.data.get("deployment_url"):
                print(f"   URL: {event.data['deployment_url']}")
            if event.data.get("gallery_id"):
                print(f"   Gallery ID: {event.data['gallery_id']}")
        elif event.event_type == "error":
            print(f"\n❌ Publish failed: {event.message}")


async def cli_components_list(component_type: str | None = None, limit: int = 20) -> None:
    """CLI: コンポーネント一覧。"""
    component_type_enum = _load_legacy_symbol("control_plane.services.component_library", "ComponentType")
    engine = PlatformEngine()

    types = None
    if component_type:
        try:
            types = [component_type_enum(component_type)]
        except ValueError:
            print(f"❌ Unknown type: {component_type}")
            print(f"Available types: {[item.value for item in component_type_enum]}")
            return

    components = engine.search_components(types=types, limit=limit)
    print("\n📦 Components:")
    print("-" * 50)
    if not components:
        print("No components found.")
        return

    for component in components:
        icon = {
            "agent": "🤖",
            "flow": "🔄",
            "tool": "🔧",
            "skill": "⚡",
            "engine": "⚙️",
            "template": "📋",
        }.get(component.type.value, "📦")
        print(f"  {icon} {component.name} ({component.id})")
        print(f"    Type: {component.type.value}, Version: {component.version}")
        if component.description:
            print(f"    {component.description[:60]}...")
        print()
    print(f"Total: {len(components)} components")


async def cli_dashboard(tenant_id: str) -> None:
    """CLI: ダッシュボード表示。"""
    engine = PlatformEngine()
    summary = await engine.get_dashboard_summary(tenant_id)

    print(f"\n📊 Dashboard for tenant: {tenant_id}")
    print("-" * 50)
    stats = summary["stats"]
    print(f"  Components: {stats['component_count']}")
    print(f"    - Agents: {stats['agent_count']}")
    print(f"    - Flows: {stats['flow_count']}")
    print(f"    - Tools: {stats['tool_count']}")
    print(f"    - Skills: {stats['skill_count']}")
    print(f"  Total Usage: {stats['total_usage']}")

    if summary["top_components"]:
        print("\n  Top Components:")
        for component in summary["top_components"][:5]:
            print(f"    - {component['name']} ({component['usage_count']} uses)")

    if summary["recent_activities"]:
        print("\n  Recent Activities:")
        for activity in summary["recent_activities"][:5]:
            print(f"    - [{activity['type']}] {activity['description']}")


def main() -> None:
    """CLI メイン関数。"""
    parser = argparse.ArgumentParser(
        description="BizCore control plane - studios, components, publish, and dashboard",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument("-v", "--verbose", action="store_true", help="詳細ログを出力")

    subparsers = parser.add_subparsers(dest="command", help="コマンド")
    serve_parser = subparsers.add_parser("serve", help="APIサーバーを起動")
    serve_parser.add_argument("--host", default=DEFAULT_API_HOST, help="ホスト")
    serve_parser.add_argument("--port", type=int, default=DEFAULT_API_PORT, help="ポート")
    serve_parser.add_argument("--reload", action="store_true", help="開発モード（ホットリロード有効）")

    search_parser = subparsers.add_parser("search", help="Gallery検索")
    search_parser.add_argument("query", help="検索クエリ")
    search_parser.add_argument("--limit", type=int, default=10, help="最大結果数")

    publish_parser = subparsers.add_parser("publish", help="ワンクリック公開")
    publish_parser.add_argument("source", help="ソースパス")
    publish_parser.add_argument("--target", default="docker", help="発布ターゲット")
    publish_parser.add_argument("--name", help="発布名")
    publish_parser.add_argument("--gallery", action="store_true", help="Galleryに登録")

    components_parser = subparsers.add_parser("components", help="コンポーネント操作")
    components_subparsers = components_parser.add_subparsers(dest="subcommand")
    list_parser = components_subparsers.add_parser("list", help="一覧表示")
    list_parser.add_argument("--type", dest="component_type", help="タイプフィルター")
    list_parser.add_argument("--limit", type=int, default=20, help="最大取得数")

    dashboard_parser = subparsers.add_parser("dashboard", help="ダッシュボード表示")
    dashboard_parser.add_argument("tenant_id", help="テナントID")

    args = parser.parse_args()
    setup_logging(args.verbose)

    if args.command == "serve":
        print(f"[Control Plane] Starting on {args.host}:{args.port} (reload={args.reload})")
        try:
            if args.reload:
                uvicorn.run(
                    "control_plane.api.app:app",
                    host=args.host,
                    port=args.port,
                    reload=True,
                    reload_dirs=["control_plane", "agentflow"],
                )
            else:
                uvicorn.run(create_app(), host=args.host, port=args.port)
        except KeyboardInterrupt:
            pass
    elif args.command == "search":
        asyncio.run(cli_search(args.query, args.limit))
    elif args.command == "publish":
        asyncio.run(cli_publish(args.source, args.target, args.name, args.gallery))
    elif args.command == "components":
        if args.subcommand == "list":
            asyncio.run(cli_components_list(args.component_type, args.limit))
        else:
            components_parser.print_help()
    elif args.command == "dashboard":
        asyncio.run(cli_dashboard(args.tenant_id))
    else:
        parser.print_help()


app = create_app()
