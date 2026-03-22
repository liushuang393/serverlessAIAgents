"""Apps Router — App 管理 API エンドポイント.

GET   /api/studios/framework/apps                        — 全 App 一覧
GET   /api/studios/framework/apps/summary                — App 概要統計
POST  /api/studios/framework/apps/refresh                — App 一覧再スキャン
POST  /api/studios/framework/apps/migrate-manifests      — app_config 標準化マイグレーション
GET   /api/studios/framework/apps/ports/conflicts        — ポート重複検出
POST  /api/studios/framework/apps/ports/rebalance        — 重複ポート再割当（dry-run対応）
GET   /api/studios/framework/apps/framework-audit        — 基盤/フレームワーク準拠監査
GET   /api/studios/framework/apps/create/options         — App 作成オプション
GET   /api/studios/framework/apps/templates              — App テンプレートカタログ
POST  /api/studios/framework/apps/create                 — 新規 App 自動生成
GET   /api/studios/framework/apps/{app_name}             — App 詳細
GET   /api/studios/framework/apps/{app_name}/config      — app_config.json 取得
PATCH /api/studios/framework/apps/{app_name}/config      — app_config.json 部分更新
GET   /api/studios/framework/apps/{app_name}/contracts   — 契約設定取得
PATCH /api/studios/framework/apps/{app_name}/contracts   — 契約設定部分更新
GET   /api/studios/framework/apps/{app_name}/health      — ヘルスチェック
POST  /api/studios/framework/apps/{app_name}/publish     — App 発布（docker compose up --build）
POST  /api/studios/framework/apps/{app_name}/start       — App 起動（docker compose up）
POST  /api/studios/framework/apps/{app_name}/stop        — App 停止（docker compose down）
POST  /api/studios/framework/apps/{app_name}/restart     — App 再起動（stop → start）
GET   /api/studios/framework/apps/{app_name}/cli/status  — CLI 状態確認
POST  /api/studios/framework/apps/{app_name}/cli/setup   — CLI セットアップ実行
"""

from __future__ import annotations

import asyncio
import json
import logging
import os
from typing import TYPE_CHECKING, Any, Literal
from urllib.parse import urlparse

from fastapi import APIRouter, Body, HTTPException, Query, Request
from fastapi.responses import StreamingResponse

from control_plane.services.agent_taxonomy import AgentTaxonomyService
from control_plane.services.app_lifecycle import (
    AppLifecycleManager,
    AppStatus,
)
from control_plane.services.app_scaffolder import AppScaffolderService
from control_plane.services.framework_audit import FrameworkAuditService
from control_plane.services.port_allocator import PortAllocatorService
from control_plane.services.tenant_dashboard import get_tenant_dashboard


if TYPE_CHECKING:
    from control_plane.schemas.provisioning_schemas import AppCreateRequest
    from control_plane.services.app_config_event_store import AppConfigEventStore
    from control_plane.services.app_discovery import AppDiscoveryService


router = APIRouter(prefix="/api/studios/framework/apps", tags=["framework-apps"])
_logger = logging.getLogger(__name__)

# モジュールレベルのシングルトン（main.py で初期化）
_discovery: AppDiscoveryService | None = None
_lifecycle: AppLifecycleManager | None = None
_scaffolder: AppScaffolderService | None = None
_port_allocator: PortAllocatorService | None = None
_framework_audit: FrameworkAuditService | None = None
_app_config_events: AppConfigEventStore | None = None
_health_prime_task: asyncio.Task[None] | None = None


def init_app_services(
    discovery: AppDiscoveryService,
    lifecycle: AppLifecycleManager,
    scaffolder: AppScaffolderService | None = None,
    port_allocator: PortAllocatorService | None = None,
    framework_audit: FrameworkAuditService | None = None,
    event_store: AppConfigEventStore | None = None,
) -> None:
    """サービスインスタンスを設定.

    main.py の create_app() から呼ばれる。

    Args:
        discovery: App 検出サービス
        lifecycle: ライフサイクル管理サービス
    """
    global _discovery, _lifecycle, _scaffolder, _port_allocator, _framework_audit, _app_config_events
    _discovery = discovery
    _lifecycle = lifecycle
    _scaffolder = scaffolder or AppScaffolderService(discovery)
    _port_allocator = port_allocator or PortAllocatorService(discovery)
    _framework_audit = framework_audit or FrameworkAuditService(discovery)
    _app_config_events = event_store


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


def _get_framework_audit() -> FrameworkAuditService:
    """FrameworkAuditService を取得（未初期化時はエラー）."""
    if _framework_audit is None:
        msg = "FrameworkAuditService が未初期化です"
        raise RuntimeError(msg)
    return _framework_audit


def _get_app_config_event_store() -> AppConfigEventStore:
    """AppConfigEventStore を取得する."""
    if _app_config_events is None:
        msg = "AppConfigEventStore が未初期化です"
        raise RuntimeError(msg)
    return _app_config_events


def _get_app_or_404(discovery: AppDiscoveryService, app_name: str):
    """App を取得し、存在しない場合は 404 を返す."""
    config = discovery.get_app(app_name)
    if config is None:
        raise HTTPException(
            status_code=404,
            detail={"message": f"App not found: {app_name}", "error_code": "APP_NOT_FOUND"},
        )
    return config


@router.get("/events")
async def app_contract_events(
    app: str = Query(..., description="購読対象アプリ名"),
) -> StreamingResponse:
    """app 契約変更の SSE ストリーム."""
    try:
        store = _get_app_config_event_store()
    except RuntimeError as exc:
        raise HTTPException(
            status_code=503,
            detail={"message": str(exc), "error_code": "STORE_NOT_INITIALIZED"},
        ) from exc

    async def event_generator() -> Any:
        connected_event = {"event_type": "connected", "app_name": app}
        yield f"event: connected\ndata: {json.dumps(connected_event)}\n\n"
        async for event in store.subscribe(app):
            event_type = str(event.get("event_type", "message"))
            yield f"event: {event_type}\ndata: {json.dumps(event)}\n\n"

    return StreamingResponse(
        event_generator(),
        media_type="text/event-stream",
        headers={
            "Cache-Control": "no-cache",
            "Connection": "keep-alive",
            "X-Accel-Buffering": "no",
        },
    )


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


def _is_loopback_host(host: str | None) -> bool:
    """loopback 系ホストかどうかを返す."""
    normalized = (host or "").strip().lower()
    return normalized in {"localhost", "127.0.0.1", "0.0.0.0"}


def _normalize_text(value: Any) -> str | None:
    """文字列を正規化（空文字は None）."""
    if not isinstance(value, str):
        return None
    text = value.strip()
    return text if text else None


def _request_access_host(request: Request | None) -> str | None:
    """現在のアクセス元ホストを解決する."""
    configured_public_host = _normalize_text(os.environ.get("AGENTFLOW_PUBLIC_HOST"))
    if request is None:
        return configured_public_host

    forwarded_host = _normalize_text(request.headers.get("x-forwarded-host"))
    if forwarded_host:
        return forwarded_host.split(",")[0].strip().split(":")[0].strip("[]") or None

    host = request.url.hostname
    if isinstance(host, str) and host.strip():
        return host.strip()

    host_header = _normalize_text(request.headers.get("host"))
    if host_header:
        return host_header.split(",")[0].strip().split(":")[0].strip("[]") or None

    return configured_public_host


def _format_url_host(host: str) -> str:
    """URL 用のホスト文字列を返す."""
    return f"[{host}]" if ":" in host and not host.startswith("[") else host


def _replace_loopback_host(raw_url: str | None, *, host: str | None) -> str | None:
    """localhost / 127.0.0.1 / 0.0.0.0 をアクセス元ホストへ置換する."""
    runtime = _normalize_text(raw_url)
    if runtime is None or host is None:
        return runtime

    try:
        parsed = urlparse(runtime)
    except Exception:
        return runtime

    if not _is_loopback_host(parsed.hostname):
        return runtime

    formatted_host = _format_url_host(host)
    target_host = f"{formatted_host}:{parsed.port}" if parsed.port is not None else formatted_host
    if parsed.username is None:
        netloc = target_host
    else:
        credentials = parsed.username
        if parsed.password is not None:
            credentials = f"{credentials}:{parsed.password}"
        netloc = f"{credentials}@{target_host}"
    return parsed._replace(netloc=netloc).geturl()


def _runtime_or_local_http_url(
    runtime_url: str | None,
    *,
    port: int | None,
    default_path: str = "",
    request: Request | None = None,
) -> str | None:
    """localhost 系の runtime URL は ports.* に合わせて補正する."""
    runtime = _normalize_text(runtime_url)
    if port is None:
        return runtime

    fallback = _replace_loopback_host(
        f"http://localhost:{port}{default_path}",
        host=_request_access_host(request),
    )
    if runtime is None:
        return fallback

    try:
        parsed = urlparse(runtime)
    except Exception:
        return fallback

    host = (parsed.hostname or "").lower()
    if not _is_loopback_host(host):
        return runtime

    scheme = parsed.scheme or "http"
    path = parsed.path or default_path
    rebuilt = f"{scheme}://localhost:{port}{path}"
    if parsed.query:
        rebuilt = f"{rebuilt}?{parsed.query}"
    return _replace_loopback_host(rebuilt, host=_request_access_host(request))


def _runtime_urls(app_config: Any, *, request: Request | None = None) -> dict[str, str | None]:
    """App 表示用 URL セットを作成（runtime 優先）."""
    configured = app_config.runtime.urls
    health_path = app_config.entry_points.health or "/health"
    if not health_path.startswith("/"):
        health_path = f"/{health_path}"

    backend = _runtime_or_local_http_url(
        _normalize_text(configured.backend),
        port=app_config.ports.api,
        request=request,
    )
    frontend = _runtime_or_local_http_url(
        _normalize_text(configured.frontend),
        port=app_config.ports.frontend,
        request=request,
    )
    health = _runtime_or_local_http_url(
        _normalize_text(configured.health),
        port=app_config.ports.api,
        default_path=health_path,
        request=request,
    )
    database = _normalize_text(configured.database) or _to_database_url(
        app_config.dependencies.database,
        app_config.ports.db,
    )
    database = _replace_loopback_host(database, host=_request_access_host(request))
    return {
        "backend": backend,
        "frontend": frontend,
        "health": health,
        "database": database,
    }


def _runtime_database(
    app_config: Any,
    urls: dict[str, str | None],
    *,
    request: Request | None = None,
) -> dict[str, Any]:
    """App 表示用 DB 設定を作成（runtime 優先）."""
    runtime_db = app_config.runtime.database.model_dump()
    kind = _normalize_text(runtime_db.get("kind")) or app_config.dependencies.database
    port = runtime_db.get("port")
    if not isinstance(port, int):
        port = app_config.ports.db

    host = _normalize_text(runtime_db.get("host"))
    access_host = _request_access_host(request)
    if host is None and port is not None:
        host = access_host or "localhost"
    elif _is_loopback_host(host) and access_host is not None:
        host = access_host

    return {
        "kind": kind,
        "url": (_normalize_text(runtime_db.get("url")) or urls.get("database") or _to_database_url(kind, port)),
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


def _runtime_cli(app_config: Any) -> dict[str, Any]:
    """App 表示用 CLI 設定を作成."""
    return app_config.runtime.cli.model_dump()


def _runtime_payload(app_config: Any, *, request: Request | None = None) -> dict[str, Any]:
    """App 表示用 runtime payload を作成."""
    urls = _runtime_urls(app_config, request=request)
    return {
        "urls": urls,
        "database": _runtime_database(app_config, urls, request=request),
        "commands": _runtime_commands(app_config),
        "cli": _runtime_cli(app_config),
    }


def _resolve_primary_tenant(app_config: Any) -> str:
    """App 設定から代表テナントIDを解決する."""
    visibility = getattr(app_config, "visibility", None)
    tenants = getattr(visibility, "tenants", None)
    if isinstance(tenants, list):
        for tenant in tenants:
            if isinstance(tenant, str) and tenant.strip():
                return tenant.strip()
    return "global"


def _record_activity_safe(
    *,
    tenant_id: str,
    activity_type: str,
    description: str,
    component_id: str | None = None,
) -> None:
    """活動記録を安全に保存する."""
    try:
        dashboard = get_tenant_dashboard()
        dashboard.record_activity(
            tenant_id=tenant_id,
            activity_type=activity_type,
            description=description,
            component_id=component_id,
        )
    except Exception as exc:
        _logger.warning("failed to record tenant activity: %s", exc)


async def _prime_health_cache(
    lifecycle: AppLifecycleManager,
    discovery: AppDiscoveryService,
    app_configs: list[Any],
) -> None:
    """App ヘルスを一括取得してキャッシュを更新する.

    Args:
        lifecycle: ライフサイクルサービス
        app_configs: AppConfig リスト
    """
    if not app_configs:
        return

    await asyncio.gather(
        *(
            lifecycle.check_health(
                config,
                config_path=discovery.get_config_path(config.name),
            )
            for config in app_configs
        ),
    )


def _schedule_health_prime(
    lifecycle: AppLifecycleManager,
    discovery: AppDiscoveryService,
    app_configs: list[Any],
) -> None:
    """ヘルスキャッシュの非同期プリフェッチを開始する."""
    global _health_prime_task
    if _health_prime_task is not None and not _health_prime_task.done():
        return

    task = asyncio.create_task(_prime_health_cache(lifecycle, discovery, app_configs))
    _health_prime_task = task

    def _done_callback(done_task: asyncio.Task[None]) -> None:
        global _health_prime_task
        if _health_prime_task is done_task:
            _health_prime_task = None
        try:
            done_task.result()
        except asyncio.CancelledError:
            # サーバー終了時などのキャンセルはノイズにしない
            pass
        except Exception as exc:
            _logger.warning("health cache prefetch failed: %s", exc)

    task.add_done_callback(_done_callback)


# ------------------------------------------------------------------
# エンドポイント
# ------------------------------------------------------------------


@router.get("")
async def list_apps(
    request: Request,
    wait_for_health: bool = Query(
        default=False,
        description="true の場合はヘルスチェック完了を待ってから返す",
    ),
    include_runtime: bool = Query(
        default=True,
        description="true の場合は runtime / contracts / visibility / blueprint を含める",
    ),
    surface_profile: Literal["business", "developer", "operator"] = Query(
        default="developer",
        description="表示面プロファイル（business は底層設定を非表示）",
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
            "business_base": app_config.business_base,
            "product_line": app_config.product_line,
            "surface_profile": app_config.surface_profile,
            "audit_profile": app_config.audit_profile,
            "security_mode": app_config.security_mode,
            "version": app_config.version,
            "icon": app_config.icon,
            "status": status,
            "ports": app_config.ports.model_dump(),
            "agent_count": len(app_config.agents),
            "tags": app_config.tags,
            "urls": _runtime_urls(app_config, request=request),
        }

        if include_runtime and surface_profile != "business":
            config_path = discovery.get_config_path(app_config.name)
            runtime = _runtime_payload(app_config, request=request)
            item.update(
                {
                    "config_path": str(config_path) if config_path else None,
                    "contracts": app_config.contracts.model_dump(),
                    "visibility": app_config.visibility.model_dump(),
                    "blueprint": app_config.blueprint.model_dump(),
                    "runtime": runtime,
                },
            )
        elif include_runtime:
            item["runtime"] = {"urls": _runtime_urls(app_config, request=request)}

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


@router.get("/framework-audit")
async def get_framework_audit_report(
    audit_profile: Literal["auto", "business", "developer"] = Query(
        default="auto",
        description="監査プロファイル（auto は app_config.audit_profile を使用）",
    ),
) -> dict[str, Any]:
    """全 App の基盤/フレームワーク準拠監査結果を返す."""
    override = None if audit_profile == "auto" else audit_profile
    return _get_framework_audit().audit_all(audit_profile=override)


@router.get("/create/options")
async def get_create_options(
    surface_profile: Literal["business", "developer", "operator"] = Query(
        default="developer",
        description="作成画面の表示面プロファイル",
    ),
) -> dict[str, Any]:
    """App 作成画面向けの選択肢を取得."""
    return AppScaffolderService.create_options(surface_profile=surface_profile)


@router.get("/templates")
async def get_app_templates() -> dict[str, Any]:
    """App template カタログと展開済み App 一覧を返す."""
    discovery = _get_discovery()
    taxonomy = AgentTaxonomyService()
    template_rows = AppScaffolderService.app_template_options()
    groups: dict[str, list[dict[str, Any]]] = {row["value"]: [] for row in template_rows}
    apps: list[dict[str, Any]] = []

    for app_config in discovery.list_apps():
        normalized_template = taxonomy.normalize_app_template(app_config.blueprint.app_template)
        resolved_template = normalized_template or taxonomy.infer_app_template(
            product_line=app_config.product_line,
            engine_pattern=app_config.blueprint.engine_pattern,
            tags=app_config.tags,
        )
        app_item = {
            "name": app_config.name,
            "display_name": app_config.display_name,
            "icon": app_config.icon,
            "product_line": app_config.product_line,
            "engine_pattern": app_config.blueprint.engine_pattern,
            "app_template": resolved_template,
        }
        apps.append(app_item)
        groups.setdefault(resolved_template, []).append(app_item)

    templates = [
        {
            "app_template": row["value"],
            "label": row["label"],
            "description": row["description"],
            "count": len(groups.get(row["value"], [])),
            "apps": groups.get(row["value"], []),
        }
        for row in template_rows
    ]
    return {
        "templates": templates,
        "apps": apps,
        "total_templates": len(templates),
        "total_apps": len(apps),
    }


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
    except Exception as exc:
        raise HTTPException(
            status_code=500,
            detail={"message": str(exc), "error_code": "APP_CREATE_FAILED"},
        )

    payload = created.model_dump()
    tenant_id = request.tenant_ids[0] if request.tenant_ids else "global"
    _record_activity_safe(
        tenant_id=tenant_id,
        activity_type="create",
        description=f"app created: {request.name}",
        component_id=request.name,
    )
    return payload


@router.get("/{app_name}")
async def get_app_detail(
    request: Request,
    app_name: str,
    wait_for_health: bool = Query(
        default=False,
        description="true の場合はヘルスチェック完了を待ってから返す",
    ),
    surface_profile: Literal["business", "developer", "operator"] = Query(
        default="developer",
        description="表示面プロファイル（business は底層設定を非表示）",
    ),
) -> dict[str, Any]:
    """App 詳細情報を取得.

    Args:
        app_name: App 識別子（snake_case）
    """
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    config_path = discovery.get_config_path(app_name)
    if wait_for_health:
        await lifecycle.check_health(
            config,
            config_path=config_path,
        )
    elif lifecycle.get_cached_health(app_name) is None:
        _schedule_health_prime(lifecycle, discovery, [config])

    cached = lifecycle.get_cached_health(app_name)
    status = cached.status.value if cached else AppStatus.UNKNOWN.value

    runtime = _runtime_payload(config, request=request)
    base_payload: dict[str, Any] = {
        "name": config.name,
        "display_name": config.display_name,
        "description": config.description,
        "business_base": config.business_base,
        "product_line": config.product_line,
        "surface_profile": config.surface_profile,
        "audit_profile": config.audit_profile,
        "security_mode": config.security_mode,
        "version": config.version,
        "icon": config.icon,
        "status": status,
        "ports": config.ports.model_dump(),
        "agent_count": len(config.agents),
        "tags": config.tags,
        "urls": runtime["urls"],
        "runtime": {"urls": runtime["urls"]},
    }

    if surface_profile == "business":
        base_payload["run_flow"] = ["template", "data_permissions", "run", "artifacts"]
        return base_payload

    return {
        **base_payload,
        "entry_points": config.entry_points.model_dump(),
        "agents": [a.model_dump() for a in config.agents],
        "services": config.services,
        "dependencies": config.dependencies.model_dump(),
        "contracts": config.contracts.model_dump(),
        "evolution": config.evolution.model_dump(),
        "visibility": config.visibility.model_dump(),
        "blueprint": config.blueprint.model_dump(),
        "runtime": runtime,
        "config_path": str(config_path) if config_path else None,
    }


@router.get("/{app_name}/config")
async def get_app_config(
    app_name: str,
    surface_profile: Literal["business", "developer", "operator"] = Query(
        default="developer",
        description="表示面プロファイル（business はアクセス不可）",
    ),
) -> dict[str, Any]:
    """app_config.json を取得.

    Args:
        app_name: App 識別子（snake_case）
    """
    if surface_profile == "business":
        raise HTTPException(
            status_code=403,
            detail={
                "message": "business profile では app_config の直接参照はできません",
                "error_code": "SURFACE_ACCESS_DENIED",
            },
        )

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
    patch: dict[str, Any] = Body(..., description="app_config 更新パッチ（読み取り専用のため使用不可）"),
    surface_profile: Literal["business", "developer", "operator"] = Query(
        default="developer",
        description="表示面プロファイル",
    ),
) -> dict[str, Any]:
    """[読み取り専用] Platform は app_config の表示専用です。

    各 App は自己完結型として自身の設定を管理します。
    app_config.json を変更する場合は各 App の設定 API、
    または app_config.json を直接編集してください。

    Args:
        app_name: App 識別子（snake_case）
        patch: 未使用（読み取り専用のため）
        surface_profile: 未使用（読み取り専用のため）

    Raises:
        HTTPException: 405 Method Not Allowed（常に）
    """
    raise HTTPException(
        status_code=405,
        detail={
            "message": (
                f"Platform は '{app_name}' の app_config 変更をサポートしません。"
                " 各 App は自己完結型として自身の設定を管理します。"
                " 変更は各 App の設定 API または app_config.json を直接編集してください。"
            ),
            "error_code": "PLATFORM_READ_ONLY",
            "app_name": app_name,
        },
    )


@router.get("/{app_name}/contracts")
async def get_app_contracts(
    app_name: str,
    surface_profile: Literal["business", "developer", "operator"] = Query(
        default="developer",
        description="表示面プロファイル（business はアクセス不可）",
    ),
) -> dict[str, Any]:
    """App 契約設定を取得.

    Args:
        app_name: App 識別子（snake_case）
    """
    if surface_profile == "business":
        raise HTTPException(
            status_code=403,
            detail={
                "message": "business profile では contracts の参照はできません",
                "error_code": "SURFACE_ACCESS_DENIED",
            },
        )

    discovery = _get_discovery()
    config = _get_app_or_404(discovery, app_name)

    return {
        "app_name": app_name,
        "contracts": config.contracts.model_dump(),
    }


@router.patch("/{app_name}/contracts")
async def patch_app_contracts(
    app_name: str,
    contracts_patch: dict[str, Any] = Body(..., description="contracts 更新パッチ（読み取り専用のため使用不可）"),
    surface_profile: Literal["business", "developer", "operator"] = Query(
        default="developer",
        description="表示面プロファイル",
    ),
) -> dict[str, Any]:
    """[読み取り専用] Platform は contracts 設定の表示専用です。

    各 App は自己完結型として自身の contracts を管理します。
    contracts を変更する場合は各 App の設定 API、
    または app_config.json を直接編集してください。

    Args:
        app_name: App 識別子（snake_case）
        contracts_patch: 未使用（読み取り専用のため）
        surface_profile: 未使用（読み取り専用のため）

    Raises:
        HTTPException: 405 Method Not Allowed（常に）
    """
    raise HTTPException(
        status_code=405,
        detail={
            "message": (
                f"Platform は '{app_name}' の contracts 変更をサポートしません。"
                " 各 App は自己完結型として自身の設定を管理します。"
                " 変更は各 App の設定 API または app_config.json を直接編集してください。"
            ),
            "error_code": "PLATFORM_READ_ONLY",
            "app_name": app_name,
        },
    )


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
    if result.success:
        _record_activity_safe(
            tenant_id=_resolve_primary_tenant(config),
            activity_type="publish",
            description=f"app published: {app_name}",
            component_id=app_name,
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
    if result.success:
        _record_activity_safe(
            tenant_id=_resolve_primary_tenant(config),
            activity_type="start",
            description=f"app started: {app_name}",
            component_id=app_name,
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
    if result.success:
        _record_activity_safe(
            tenant_id=_resolve_primary_tenant(config),
            activity_type="stop",
            description=f"app stopped: {app_name}",
            component_id=app_name,
        )
    return result.to_dict()


@router.post("/{app_name}/restart")
async def restart_app(app_name: str) -> dict[str, Any]:
    """App を再起動（stop -> start）."""
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    config_path = discovery.get_config_path(app_name)

    stop_result = await lifecycle.stop_app(
        config,
        config_path=config_path,
    )
    start_result = await lifecycle.start_app(
        config,
        config_path=config_path,
    )

    success = bool(start_result.success)
    response: dict[str, Any] = {
        "app_name": app_name,
        "action": "restart",
        "success": success,
        "command": "restart",
        "command_source": start_result.command_source,
        "cwd": start_result.cwd,
        "return_code": start_result.return_code,
        "stop": stop_result.to_dict(),
        "start": start_result.to_dict(),
    }
    if start_result.stdout:
        response["stdout"] = start_result.stdout
    if start_result.stderr:
        response["stderr"] = start_result.stderr
    if start_result.checked_health is not None:
        response["health"] = start_result.checked_health.to_dict()
    if not success:
        response["error"] = start_result.error or stop_result.error or "restart_failed"
    else:
        _record_activity_safe(
            tenant_id=_resolve_primary_tenant(config),
            activity_type="restart",
            description=f"app restarted: {app_name}",
            component_id=app_name,
        )
    return response


@router.post("/{app_name}/local-start")
async def local_start_app(app_name: str) -> dict[str, Any]:
    """App をローカル開発モードで起動（バックエンド・フロントエンドをバックグラウンド起動）."""
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    try:
        result = await lifecycle.start_local_dev(
            config,
            config_path=discovery.get_config_path(app_name),
        )
        return result.to_dict()
    except asyncio.CancelledError:
        # サーバー停止中の Ctrl+C で local-start が中断されるケースを明示化する
        return {
            "app_name": app_name,
            "action": "local_start",
            "success": False,
            "command": "",
            "command_source": "fallback",
            "cwd": "",
            "return_code": None,
            "error": "local_start request was cancelled during shutdown",
            "execution_mode": "local",
        }


@router.get("/{app_name}/cli/status")
async def get_app_cli_status(app_name: str) -> dict[str, Any]:
    """App の CLI セットアップ状態を取得."""
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    status = await lifecycle.cli_status(config)
    return {"app_name": app_name, "status": status}


@router.post("/{app_name}/cli/setup")
async def setup_app_cli(app_name: str) -> dict[str, Any]:
    """App の CLI 検出・インストール・認証を実行."""
    discovery = _get_discovery()
    lifecycle = _get_lifecycle()
    config = _get_app_or_404(discovery, app_name)
    result = await lifecycle.cli_setup(config)
    return {"app_name": app_name, "setup": result}
