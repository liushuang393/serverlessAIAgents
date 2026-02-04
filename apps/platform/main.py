# -*- coding: utf-8 -*-
"""Platform App - メインエントリーポイント.

CLI および FastAPI サーバーのエントリーポイント。

使用例:
    # サーバー起動
    python -m apps.platform.main serve

    # CLI操作
    python -m apps.platform.main search "PDF"
    python -m apps.platform.main publish ./my-agent --target docker
    python -m apps.platform.main components list
"""

import argparse
import asyncio
import json
import logging
import sys
from pathlib import Path
from typing import Any

import uvicorn
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from apps.platform.engine import PlatformEngine
from apps.platform.routers import (
    gallery_router,
    components_router,
    publish_router,
    dashboard_router,
)
from apps.platform.schemas.publish_schemas import PublishRequest, PublishTarget


def create_app() -> FastAPI:
    """FastAPI アプリケーションを作成.

    Returns:
        FastAPI アプリケーション
    """
    app = FastAPI(
        title="AgentFlow Platform",
        description="AgentFlow Platform API - Gallery, Components, Publish, Dashboard",
        version="1.0.0",
        docs_url="/docs",
        redoc_url="/redoc",
    )

    # CORS設定
    app.add_middleware(
        CORSMiddleware,
        allow_origins=["*"],
        allow_credentials=True,
        allow_methods=["*"],
        allow_headers=["*"],
    )

    # ルーターを登録
    app.include_router(gallery_router)
    app.include_router(components_router)
    app.include_router(publish_router)
    app.include_router(dashboard_router)

    @app.get("/health")
    async def health_check() -> dict[str, Any]:
        """ヘルスチェック."""
        return {"status": "healthy", "service": "platform"}

    @app.get("/")
    async def root() -> dict[str, Any]:
        """ルートエンドポイント."""
        return {
            "name": "AgentFlow Platform",
            "version": "1.0.0",
            "docs": "/docs",
        }

    return app


def setup_logging(verbose: bool = False) -> None:
    """ログ設定を初期化."""
    level = logging.DEBUG if verbose else logging.INFO
    logging.basicConfig(
        level=level,
        format="%(asctime)s - %(name)s - %(levelname)s - %(message)s",
        handlers=[logging.StreamHandler(sys.stdout)],
    )


async def cli_search(query: str, limit: int = 10) -> None:
    """CLI: Gallery検索."""
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
    """CLI: 一键发布."""
    engine = PlatformEngine()

    # ターゲットを変換
    try:
        publish_target = PublishTarget(target)
    except ValueError:
        print(f"❌ Unknown target: {target}")
        print(f"Available targets: {[t.value for t in PublishTarget]}")
        return

    # リクエストを作成
    request = PublishRequest(
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


async def cli_components_list(
    component_type: str | None = None,
    limit: int = 20,
) -> None:
    """CLI: コンポーネント一覧."""
    engine = PlatformEngine()

    from apps.platform.services.component_library import ComponentType

    types = None
    if component_type:
        try:
            types = [ComponentType(component_type)]
        except ValueError:
            print(f"❌ Unknown type: {component_type}")
            print(f"Available types: {[t.value for t in ComponentType]}")
            return

    components = engine.search_components(types=types, limit=limit)

    print(f"\n📦 Components:")
    print("-" * 50)

    if not components:
        print("No components found.")
        return

    for c in components:
        icon = {
            "agent": "🤖",
            "flow": "🔄",
            "tool": "🔧",
            "skill": "⚡",
            "engine": "⚙️",
            "template": "📋",
        }.get(c.type.value, "📦")

        print(f"  {icon} {c.name} ({c.id})")
        print(f"    Type: {c.type.value}, Version: {c.version}")
        if c.description:
            print(f"    {c.description[:60]}...")
        print()

    print(f"Total: {len(components)} components")


async def cli_dashboard(tenant_id: str) -> None:
    """CLI: ダッシュボード."""
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
        for c in summary["top_components"][:5]:
            print(f"    - {c['name']} ({c['usage_count']} uses)")

    if summary["recent_activities"]:
        print("\n  Recent Activities:")
        for a in summary["recent_activities"][:5]:
            print(f"    - [{a['type']}] {a['description']}")


def main() -> None:
    """メイン関数."""
    parser = argparse.ArgumentParser(
        description="AgentFlow Platform - Gallery, Components, Publish, Dashboard",
        formatter_class=argparse.RawDescriptionHelpFormatter,
    )
    parser.add_argument(
        "-v", "--verbose",
        action="store_true",
        help="詳細ログを出力",
    )

    subparsers = parser.add_subparsers(dest="command", help="コマンド")

    # serve コマンド
    serve_parser = subparsers.add_parser("serve", help="APIサーバーを起動")
    serve_parser.add_argument("--host", default="0.0.0.0", help="ホスト")
    serve_parser.add_argument("--port", type=int, default=8000, help="ポート")

    # search コマンド
    search_parser = subparsers.add_parser("search", help="Gallery検索")
    search_parser.add_argument("query", help="検索クエリ")
    search_parser.add_argument("--limit", type=int, default=10, help="最大結果数")

    # publish コマンド
    publish_parser = subparsers.add_parser("publish", help="一键发布")
    publish_parser.add_argument("source", help="ソースパス")
    publish_parser.add_argument("--target", default="docker", help="発布ターゲット")
    publish_parser.add_argument("--name", help="発布名")
    publish_parser.add_argument("--gallery", action="store_true", help="Galleryに登録")

    # components コマンド
    components_parser = subparsers.add_parser("components", help="コンポーネント操作")
    components_subparsers = components_parser.add_subparsers(dest="subcommand")

    list_parser = components_subparsers.add_parser("list", help="一覧表示")
    list_parser.add_argument("--type", dest="component_type", help="タイプフィルター")
    list_parser.add_argument("--limit", type=int, default=20, help="最大取得数")

    # dashboard コマンド
    dashboard_parser = subparsers.add_parser("dashboard", help="ダッシュボード表示")
    dashboard_parser.add_argument("tenant_id", help="テナントID")

    args = parser.parse_args()
    setup_logging(args.verbose)

    if args.command == "serve":
        app = create_app()
        uvicorn.run(app, host=args.host, port=args.port)

    elif args.command == "search":
        asyncio.run(cli_search(args.query, args.limit))

    elif args.command == "publish":
        asyncio.run(cli_publish(
            args.source,
            args.target,
            args.name,
            args.gallery,
        ))

    elif args.command == "components":
        if args.subcommand == "list":
            asyncio.run(cli_components_list(args.component_type, args.limit))
        else:
            components_parser.print_help()

    elif args.command == "dashboard":
        asyncio.run(cli_dashboard(args.tenant_id))

    else:
        parser.print_help()


if __name__ == "__main__":
    main()
