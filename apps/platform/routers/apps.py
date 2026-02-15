# -*- coding: utf-8 -*-
"""Apps Router — App 管理 API エンドポイント.

GET   /api/apps                        — 全 App 一覧
GET   /api/apps/summary                — App 概要統計
POST  /api/apps/refresh                — App 一覧再スキャン
POST  /api/apps/migrate-manifests      — app_config 標準化マイグレーション
GET   /api/apps/ports/conflicts        — ポート重複検出
POST  /api/apps/ports/rebalance        — 重複ポート再割当（dry-run対応）
GET   /api/apps/create/options         — App 作成オプション
POST  /api/apps/create                 — 新規 App 自動生成
GET   /api/apps/{app_name}             — App 詳細
GET   /api/apps/{app_name}/config      — app_config.json 取得
PATCH /api/apps/{app_name}/config      — app_config.json 部分更新
GET   /api/apps/{app_name}/contracts   — 契約設定取得
PATCH /api/apps/{app_name}/contracts   — 契約設定部分更新
GET   /api/apps/{app_name}/health      — ヘルスチェック
POST  /api/apps/{app_name}/publish     — App 発布（docker compose up --build）
POST  /api/apps/{app_name}/start       — App 起動（docker compose up）
POST  /api/apps/{app_name}/stop        — App 停止（docker compose down）
"""

from __future__ import annotations

import asyncio
import logging
from typing import Any
from urllib.parse import urlparse

from fastapi import APIRouter, Body, HTTPException, Query

from apps.platform.schemas.provisioning_schemas import AppCreateRequest
from apps.platform.services.app_scaffolder import AppScaffolderService
from apps.platform.services.app_discovery import AppDiscoveryService
from apps.platform.services.app_lifecycle import (
    AppLifecycleManager,
    AppStatus,
)
from apps.platform.services.port_allocator import PortAllocatorService


router = APIRouter(prefix="/api/apps", tags=["apps"])
_logger = logging.getLogger(__name__)

# モジュールレベルのシングルトン（main.py で初期化）
_discovery: AppDiscoveryService | None = None
_lifecycle: AppLifecycleManager | None = None
_scaffolder: AppScaffolderService | None = None
_port_allocator: PortAllocatorService | None = None
_health_prime_task: asyncio.Task[None] | None = None


def init_app_services(
    discovery: AppDiscoveryService,
    lifecycle: AppLifecycleManager,
    scaffolder: AppScaffolderService | None = None,
    port_allocator: PortAllocatorService | None = None,
) -> None:
    """サービスインスタンスを設定.

    main.py の create_app() から呼ばれる。

    Args:
        discovery: App 検出サービス
        lifecycle: ライフサイクル管理サービス
    """
    global _discovery, _lifecycle, _scaffolder, _port_allocator  # noqa: PLW0603
    _discovery = discovery
    _lifecycle = lifecycle
    _scaffolder = scaffolder or AppScaffolderService(discovery)
    _port_allocator = port_allocator or PortAllocatorService(discovery)


def _get_discovery() -> AppDiscoveryService:
    """AppDiscoveryService を取得（未初期化時はエラー）."""
    if _discovery is None:
        msg = "AppDiscoveryService が未初期化です"
        raise RuntimeError(msg)
    return _discovery


def _get_lifecycle() -> AppLifecycleManager:
    """AppLifecycleManager を取得（未初期化時はエラー）."""
    if _lifecycle is None:
        msg = "AppLifecycleManager が未初期化です"
        raise RuntimeError(msg)
    return _lifecycle


def _get_scaffolder() -> AppScaffolderService:
    """AppScaffolderService を取得（未初期化時はエラー）."""
    if _scaffolder is None:
        msg = "AppScaffolderService が未初期化です"
        raise RuntimeError(msg)
    return _scaffolder


def _get_port_allocator() -> PortAllocatorService:
    """PortAllocatorService を取得（未初期化時はエラー）."""
    if _port_allocator is None:
        msg = "PortAllocatorService が未初期化です"
        raise RuntimeError(msg)
    return _port_allocator


def _get_app_or_404(discovery: AppDiscoveryService, app_name: str):
    """App を取得し、存在しない場合は 404 を返す."""
    config = discovery.get_app(app_name)
    if config is None:
        raise HTTPException(
            status_code=404,
            detail={"message": f"App not found: {app_name}", "error_code": "APP_NOT_FOUND"},
        )
    return config


def _to_database_url(db_kind: str | None, db_port: int | None) -> str | None:
    """DB 接続URLを返す（ローカル公開ポート前提）."""
    if db_port is None:
        return None
    kind = (db_kind or "").lower()
    if "postgres" in kind:
        return f"postgresql://localhost:{db_port}"
    if "mysql" in kind:
        return f"mysql://localhost:{db_port}"
    if "redis" in kind:
        return f"redis://localhost:{db_port}"
    if "sqlite" in kind:
        return "sqlite:///./app.db"
    return f"db://localhost:{db_port}"


def _normalize_text(value: Any) -> str | None:
    """文字列を正規化（空文字は None）."""
    if not isinstance(value, str):
        return None
    text = value.strip()
    return text if text else None


def _runtime_or_local_http_url(
    runtime_url: str | None,
    *,
    port: int | None,
    default_path: str = "",
) -> str | None:
    """localhost 系の runtime URL は ports.* に合わせて補正する."""
    runtime = _normalize_text(runtime_url)
    if port is None:
        return runtime

    fallback = f"http://localhost:{port}{default_path}"
    if runtime is None:
        return fallback

    try:
        parsed = urlparse(runtime)
    except Exception:  # noqa: BLE001
        return fallback

    host = (parsed.hostname or "").lower()
    if host not in {"localhost", "127.0.0.1"}:
        return runtime

    scheme = parsed.scheme or "http"
    path = parsed.path or default_path
    rebuilt = f"{scheme}://{host}:{port}{path}"
    if parsed.query:
        rebuilt = f"{rebuilt}?{parsed.query}"
    return rebuilt


def _runtime_urls(app_config: Any) -> dict[str, str | None]:
    """App 表示用 URL セットを作成（runtime 優先）."""
    configured = app_config.runtime.urls
    health_path = app_config.entry_points.health or "/health"
    if not health_path.startswith("/"):
        health_path = f"/{health_path}"

    backend = _runtime_or_local_http_url(
        _normalize_text(configured.backend),
        port=app_config.ports.api,
    )
    frontend = _runtime_or_local_http_url(
        _normalize_text(configured.frontend),
        port=app_config.ports.frontend,
    )
    health = _runtime_or_local_http_url(
        _normalize_text(configured.health),
        port=app_config.ports.api,
        default_path=health_path,
    )
    database = _normalize_text(configured.database) or _to_database_url(
        app_config.dependencies.database,
        app_config.ports.db,
    )
    return {
        "backend": backend,
        "frontend": frontend,
        "health": health,
        "database": database,
    }


def _runtime_database(app_config: Any, urls: dict[str, str | None]) -> dict[str, Any]:
    """App 表示用 DB 設定を作成（runtime 優先）."""
    runtime_db = app_config.runtime.database.model_dump()
    kind = _normalize_text(runtime_db.get("kind")) or app_config.dependencies.database
    port = runtime_db.get("port")
    if not isinstance(port, int):
        port = app_config.ports.db

    host = _normalize_text(runtime_db.get("host"))
    if host is None and port is not None:
        host = "localhost"

    return {
        "kind": kind,
        "url": (
            _normalize_text(runtime_db.get("url"))
            or urls.get("database")
            or _to_database_url(kind, port)
        ),
        "host": host,
        "port": port,
        "name": _normalize_text(runtime_db.get("name")),
        "user": _normalize_text(runtime_db.get("user")),
        "password": _normalize_text(runtime_db.get("password")),
        "password_env": _normalize_text(runtime_db.get("password_env")),
        "note": _normalize_text(runtime_db.get("note")),
    }


def _runtime_commands(app_config: Any) -> dict[str, str | None]:
    """App 表示用コマンド設定を作成."""
    commands = app_config.runtime.commands.model_dump()
    return {
        "backend_dev": _normalize_text(commands.get("backend_dev")),
        "frontend_dev": _normalize_text(commands.get("frontend_dev")),
        "publish": _normalize_text(commands.get("publish")),
        "start": _normalize_text(commands.get("start")),
        "stop": _normalize_text(commands.get("stop")),
    }


def _runtime_payload(app_config: Any) -> dict[str, Any]:
    """App 表示用 runtime payload を作成."""
    urls = _runtime_urls(app_config)
    return {
        "urls": urls,
        "database": _runtime_database(app_config, urls),
        "commands": _runtime_commands(app_config),
    }


async def _prime_health_cache(
    lifecycle: AppLifecycleManager,
    discovery: AppDiscoveryService,
    app_configs: list[Any],
) -> None:
    """未キャッシュの App ヘルスを一括取得してキャッシュする.

    Args:
        lifecycle: ライフサイクルサービス
        app_configs: AppConfig リスト
    """
    targets = [
        config for config in app_configs
        if lifecycle.get_cached_health(config.name) is None
    ]
    if not targets:
        return

    await asyncio.gather(
        *(
            lifecycle.check_health(
                config,
                config_path=discovery.get_config_path(config.name),
            )
            for config in targets
        ),
    )


def _schedule_health_prime(
    lifecycle: AppLifecycleManager,
    discovery: AppDiscoveryService,
    app_configs: list[Any],
) -> None:
    """ヘルスキャッシュの非同期プリフェッチを開始する."""
    global _health_prime_task  # noqa: PLW0603
    if _health_prime_task is not None and not _health_prime_task.done():
        return

    task = asyncio.create_task(_prime_health_cache(lifecycle, discovery, app_configs))
    _health_prime_task = task

    def _done_callback(done_task: asyncio.Task[None]) -> None:
        global _health_prime_task  # noqa: PLW0603
        if _health_prime_task is done_task:
            _health_prime_task = None
        try:
            done_task.result()
        except Exception as exc:  # noqa: BLE001
            _logger.warning("health cache prefetch failed: %s", exc)

    task.add_done_callback(_done_callback)


# ------------------------------------------------------------------
# エンドポイント
# ------------------------------------------------------------------


@router.get("")
async def list_apps(
    wait_for_health: bool = Query(
        default=True,
        description="true の場合はヘルスチェック完了を待ってから返す",
    ),
    include_runtime: bool = Query(
        default=True,
        description="true の場合は runtime / contracts / visibility / blueprint を含める",
    ),
) -> dict[str, Any]:
    """全 App 一覧を取得."""
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    apps = discovery.list_apps()
    if wait_for_health:
        await _prime_health_cache(lifecycle, discovery, apps)
    else:
        _schedule_health_prime(lifecycle, discovery, apps)

    items: list[dict[str, Any]] = []
    for app_config in apps:
        cached = lifecycle.get_cached_health(app_config.name)
        status = cached.status.value if cached else AppStatus.UNKNOWN.value
        item: dict[str, Any] = {
            "name": app_config.name,
            "display_name": app_config.display_name,
            "description": app_config.description,
            "version": app_config.version,
            "icon": app_config.icon,
            "status": status,
            "ports": app_config.ports.model_dump(),
            "agent_count": len(app_config.agents),
            "tags": app_config.tags,
            "urls": _runtime_urls(app_config),
        }

        if include_runtime:
            config_path = discovery.get_config_path(app_config.name)
            runtime = _runtime_payload(app_config)
            item.update(
                {
                    "config_path": str(config_path) if config_path else None,
                    "contracts": app_config.contracts.model_dump(),
                    "visibility": app_config.visibility.model_dump(),
                    "blueprint": app_config.blueprint.model_dump(),
                    "runtime": runtime,
                },
            )

        items.append(item)

    return {"apps": items, "total": len(items)}


@router.get("/summary")
async def get_summary() -> dict[str, Any]:
    """全 App の概要統計を取得."""
    discovery = _get_discovery()
    return discovery.summary()


@router.post("/refresh")
async def refresh_apps() -> dict[str, Any]:
    """App 一覧を再スキャンする."""
    discovery = _get_discovery()
    old_names = {a.name for a in discovery.list_apps()}
    count = await discovery.scan()
    new_names = {a.name for a in discovery.list_apps()}

    added = sorted(new_names - old_names)
    removed = sorted(old_names - new_names)
    unchanged = sorted(old_names & new_names)

    return {
        "discovered": count,
        "new": added,
        "removed": removed,
        "unchanged": unchanged,
        "errors": discovery.list_errors(),
    }


@router.post("/migrate-manifests")
async def migrate_manifests(
    dry_run: bool = Body(default=True, embed=True, description="true の場合は更新せず計画のみ返す"),
) -> dict[str, Any]:
    """全 app_config.json を標準契約へ移行する."""
    discovery = _get_discovery()
    report = discovery.migrate_manifests(dry_run=dry_run)
    if not dry_run:
        await discovery.scan()
    return report


@router.get("/ports/conflicts")
async def get_port_conflicts() -> dict[str, Any]:
    """app_config 上のポート重複を取得."""
    report = _get_port_allocator().build_conflict_report()
    return report.model_dump()


@router.post("/ports/rebalance")
async def rebalance_ports(
    dry_run: bool = Body(default=True, embed=True, description="true の場合は更新せず計画のみ返す"),
) -> dict[str, Any]:
    """重複ポートの再割当を行う."""
    discovery = _get_discovery()
    allocator = _get_port_allocator()
    plan = allocator.plan_conflict_resolution()

    if dry_run:
        return {
            "dry_run": True,
            "updates": plan,
            "total_updates": len(plan),
        }

    updated_apps: list[str] = []
    for app_name, port_patch in plan.items():
        if not port_patch:
            continue
        discovery.update_app_config(app_name, {"ports": port_patch})
        updated_apps.append(app_name)

    return {
        "dry_run": False,
        "updated_apps": sorted(updated_apps),
        "total_updates": len(updated_apps),
    }


@router.get("/create/options")
async def get_create_options() -> dict[str, Any]:
    """App 作成画面向けの選択肢を取得."""
    return AppScaffolderService.create_options()


@router.post("/create")
async def create_app(request: AppCreateRequest) -> dict[str, Any]:
    """新規 App を自動生成して登録する."""
    scaffolder = _get_scaffolder()
    try:
        created = await scaffolder.create_app(request)
    except FileExistsError as exc:
        raise HTTPException(
            status_code=409,
            detail={"message": str(exc), "error_code": "APP_ALREADY_EXISTS"},
        )
    except ValueError as exc:
        raise HTTPException(
            status_code=400,
            detail={"message": str(exc), "error_code": "APP_CREATE_INVALID"},
        )
    except Exception as exc:  # noqa: BLE001
        raise HTTPException(
            status_code=500,
            detail={"message": str(exc), "error_code": "APP_CREATE_FAILED"},
        )

    return created.model_dump()


@router.get("/{app_name}")
async def get_app_detail(app_name: str) -> dict[str, Any]:
    """App 詳細情報を取得.

    Args:
        app_name: App 識別子（snake_case）
    """
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    if lifecycle.get_cached_health(app_name) is None:
        await lifecycle.check_health(
            config,
            config_path=discovery.get_config_path(app_name),
        )

    cached = lifecycle.get_cached_health(app_name)
    status = cached.status.value if cached else AppStatus.UNKNOWN.value

    config_path = discovery.get_config_path(app_name)
    runtime = _runtime_payload(config)
    return {
        "name": config.name,
        "display_name": config.display_name,
        "description": config.description,
        "version": config.version,
        "icon": config.icon,
        "status": status,
        "ports": config.ports.model_dump(),
        "entry_points": config.entry_points.model_dump(),
        "agents": [a.model_dump() for a in config.agents],
        "services": config.services,
        "dependencies": config.dependencies.model_dump(),
        "contracts": config.contracts.model_dump(),
        "visibility": config.visibility.model_dump(),
        "blueprint": config.blueprint.model_dump(),
        "tags": config.tags,
        "urls": runtime["urls"],
        "runtime": runtime,
        "config_path": str(config_path) if config_path else None,
    }


@router.get("/{app_name}/config")
async def get_app_config(app_name: str) -> dict[str, Any]:
    """app_config.json を取得.

    Args:
        app_name: App 識別子（snake_case）
    """
    discovery = _get_discovery()
    _get_app_or_404(discovery, app_name)

    raw = discovery.get_raw_config(app_name)
    config_path = discovery.get_config_path(app_name)
    if raw is None or config_path is None:
        raise HTTPException(
            status_code=404,
            detail={
                "message": f"app_config.json not found: {app_name}",
                "error_code": "APP_CONFIG_NOT_FOUND",
            },
        )

    return {
        "app_name": app_name,
        "config_path": str(config_path),
        "config": raw,
    }


@router.patch("/{app_name}/config")
async def patch_app_config(
    app_name: str,
    patch: dict[str, Any] = Body(..., description="app_config 更新パッチ"),
) -> dict[str, Any]:
    """app_config.json を部分更新.

    Args:
        app_name: App 識別子（snake_case）
        patch: deep merge パッチ
    """
    discovery = _get_discovery()
    _get_app_or_404(discovery, app_name)

    try:
        updated = discovery.update_app_config(app_name, patch)
    except KeyError:
        raise HTTPException(
            status_code=404,
            detail={
                "message": f"app_config.json not found: {app_name}",
                "error_code": "APP_CONFIG_NOT_FOUND",
            },
        )
    except ValueError as exc:
        raise HTTPException(
            status_code=400,
            detail={"message": str(exc), "error_code": "APP_CONFIG_INVALID"},
        )

    config_path = discovery.get_config_path(app_name)
    return {
        "success": True,
        "app_name": app_name,
        "config_path": str(config_path) if config_path else None,
        "config": updated.model_dump(),
    }


@router.get("/{app_name}/contracts")
async def get_app_contracts(app_name: str) -> dict[str, Any]:
    """App 契約設定を取得.

    Args:
        app_name: App 識別子（snake_case）
    """
    discovery = _get_discovery()
    config = _get_app_or_404(discovery, app_name)

    return {
        "app_name": app_name,
        "contracts": config.contracts.model_dump(),
    }


@router.patch("/{app_name}/contracts")
async def patch_app_contracts(
    app_name: str,
    contracts_patch: dict[str, Any] = Body(..., description="contracts 更新パッチ"),
) -> dict[str, Any]:
    """App 契約設定を部分更新.

    Args:
        app_name: App 識別子（snake_case）
        contracts_patch: contracts セクションの deep merge パッチ
    """
    discovery = _get_discovery()
    _get_app_or_404(discovery, app_name)

    try:
        updated = discovery.update_app_config(app_name, {"contracts": contracts_patch})
    except KeyError:
        raise HTTPException(
            status_code=404,
            detail={
                "message": f"app_config.json not found: {app_name}",
                "error_code": "APP_CONFIG_NOT_FOUND",
            },
        )
    except ValueError as exc:
        raise HTTPException(
            status_code=400,
            detail={"message": str(exc), "error_code": "APP_CONFIG_INVALID"},
        )

    return {
        "success": True,
        "app_name": app_name,
        "contracts": updated.contracts.model_dump(),
    }


@router.get("/{app_name}/health")
async def check_app_health(app_name: str) -> dict[str, Any]:
    """App のヘルスチェックを実行.

    Args:
        app_name: App 識別子（snake_case）
    """
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)

    result = await lifecycle.check_health(
        config,
        config_path=discovery.get_config_path(app_name),
    )
    return result.to_dict()


@router.post("/{app_name}/publish")
async def publish_app(app_name: str) -> dict[str, Any]:
    """App を発布（docker compose up -d --build）."""
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    result = await lifecycle.publish_app(
        config,
        config_path=discovery.get_config_path(app_name),
    )
    return result.to_dict()


@router.post("/{app_name}/start")
async def start_app(app_name: str) -> dict[str, Any]:
    """App を起動（docker compose up -d）."""
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    result = await lifecycle.start_app(
        config,
        config_path=discovery.get_config_path(app_name),
    )
    return result.to_dict()


@router.post("/{app_name}/stop")
async def stop_app(app_name: str) -> dict[str, Any]:
    """App を停止（docker compose down）."""
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    result = await lifecycle.stop_app(
        config,
        config_path=discovery.get_config_path(app_name),
    )
    return result.to_dict()


@router.post("/{app_name}/local-start")
async def local_start_app(app_name: str) -> dict[str, Any]:
    """App をローカル開発モードで起動（バックエンド・フロントエンドをバックグラウンド起動）."""
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    result = await lifecycle.start_local_dev(
        config,
        config_path=discovery.get_config_path(app_name),
    )
    return result.to_dict()
