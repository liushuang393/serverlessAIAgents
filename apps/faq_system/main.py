"""FAQ System - AgentFlow フレームワーク級サービス.

薄い App 層として設計。業務ロジックは agentflow/ フレームワーク側で実装。

アーキテクチャ:
    App層（薄い）          → フレームワーク層（厚い）
    ─────────────────────────────────────────────
    /api/chat              → FAQAgent
    /api/rag/*             → RAGService
    /api/sql/*             → Text2SQLService
    /ws/{client_id}        → WebSocket リアルタイム通信

機能:
- RAG 検索（ナレッジベース）
- Text2SQL（データベースクエリ）
- チャート生成
- フォローアップ提案
- WebSocket 双方向通信
- 富文本レスポンス（Markdown・コード・表・チャート）
- リアルタイム進捗表示
"""

from __future__ import annotations

import json
import logging
import os
from contextlib import asynccontextmanager
from pathlib import Path
from typing import TYPE_CHECKING, Any


try:
    from dotenv import load_dotenv
except ImportError:  # pragma: no cover - optional dependency
    load_dotenv = None

if TYPE_CHECKING:
    from collections.abc import AsyncIterator

# FAQ アプリ専用 .env（存在時）を読み込み、ルート .env より優先する。
_APP_ROOT = Path(__file__).resolve().parent
_APP_ENV_PATH = _APP_ROOT / ".env"


def _load_faq_app_env() -> None:
    """FAQ アプリ専用 .env をロードする."""
    if load_dotenv is None or not _APP_ENV_PATH.is_file():
        return
    load_dotenv(_APP_ENV_PATH, override=True)
    os.environ.setdefault("AGENTFLOW_ENV_FILE", str(_APP_ENV_PATH))


_load_faq_app_env()

# --- 循環参照回避のため、FastAPI 起動前にパッケージパス等を微調整する場合に備え ---

from apps.faq_system.backend.auth import oauth2_router, saml_router
from apps.faq_system.backend.auth.dependencies import (
    get_auth_service,
    get_faq_contract_auth_guard,
)
from apps.faq_system.backend.auth.router import router as auth_router
from apps.faq_system.backend.config import kb_registry
from apps.faq_system.backend.db.models import Base
from apps.faq_system.backend.services.rag_runtime_config import (
    load_rag_runtime_config,
    sync_runtime_env,
)
from apps.faq_system.routers import (
    agents_router,
    chat_router,
    kb_settings_router,
    misc_router,
    rag_router,
    sql_router,
    ws_router,
)
from apps.faq_system.routers.dependencies import (
    start_rag_ingestion_scheduler,
    stop_rag_ingestion_scheduler,
)
from fastapi import FastAPI
from fastapi.middleware.cors import CORSMiddleware

from agentflow.database import DatabaseConfig, DatabaseManager
from agentflow.observability.startup import log_startup_info


logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


# =============================================================================
# アプリケーション設定
# =============================================================================

_APP_CONFIG_PATH = _APP_ROOT / "app_config.json"


def _load_app_config() -> dict[str, Any]:
    """app_config.json からアプリ設定を読み込み.

    Returns:
        設定辞書。ファイル不在・パース失敗時は空辞書。
    """
    if _APP_CONFIG_PATH.is_file():
        try:
            return json.loads(_APP_CONFIG_PATH.read_text("utf-8"))
        except (json.JSONDecodeError, OSError):
            return {}
    return {}


def _sync_runtime_env_from_app_config() -> None:
    """app_config の設定を環境変数へ反映（未設定時のみ）."""
    cfg = load_rag_runtime_config(_APP_CONFIG_PATH)
    sync_runtime_env(cfg)


_sync_runtime_env_from_app_config()


def _log_faq_startup(host: str, port: int) -> None:
    """FAQ System 固有の起動バナーをログ出力.

    Args:
        host: バインドホスト
        port: バインドポート
    """
    cfg = _load_app_config()
    agents_cfg: list[dict[str, Any]] = cfg.get("agents", [])
    agent_names = [f"{a['name']} ({', '.join(a.get('capabilities', []))})" for a in agents_cfg]

    # フレームワーク共通情報（LLM / DB / VectorDB / Embedding）
    log_startup_info(
        app_name=cfg.get("display_name", "FAQ System"),
        extra_info={
            "version": cfg.get("version", "unknown"),
            "agents": agent_names,
        },
    )

    # アクセス情報
    display_host = "localhost" if host in ("0.0.0.0", "::") else host
    logger.info("[Access] http://%s:%s", display_host, port)
    logger.info("[Access] API docs: http://%s:%s/docs", display_host, port)
    logger.info("[Access] Health:   http://%s:%s/api/health", display_host, port)

    # サービス一覧
    svc_names = list(cfg.get("services", {}))
    if svc_names:
        logger.info("[Services] %s", ", ".join(svc_names))
    logger.info("=" * 60)


# グローバル DB 管理
db_manager = DatabaseManager(
    config=DatabaseConfig(),  # URL は環境変数 DB_URL または FAQ_DB_URL から自動解決
    metadata=Base.metadata,
)


@asynccontextmanager
async def lifespan(_app: FastAPI) -> AsyncIterator[None]:
    """アプリ起動/終了時の初期化."""
    from agentflow.bootstrap import AppCapabilityBootstrapper

    get_faq_contract_auth_guard().reset_cache()
    await db_manager.init()
    if db_manager.resolved_url.startswith("sqlite"):
        await db_manager.create_all_tables()

    await kb_registry.ensure_initialized()
    await get_auth_service().ensure_bootstrap_data()
    await start_rag_ingestion_scheduler()

    # --- AppCapabilityBootstrapper: contracts.* から RAG/Skills を自動接続 ---
    bundle, bootstrapper = await AppCapabilityBootstrapper.build(
        app_name="faq_system",
        platform_url=os.environ.get("PLATFORM_URL"),
    )
    _app.state.capability_bundle = bundle
    logger.info(
        "CapabilityBundle 構築完了: rag=%s",
        "有効" if bundle.has_rag() else "無効",
    )

    # 起動バナー出力
    cfg = _load_app_config()
    host = os.getenv("FAQ_HOST", "0.0.0.0")
    port = int(os.getenv("FAQ_PORT", str(cfg.get("ports", {}).get("api", 8005))))
    _log_faq_startup(host, port)

    yield

    # --- シャットダウン: ConfigWatcher 停止 ---
    await bootstrapper.shutdown()
    await stop_rag_ingestion_scheduler()
    await db_manager.close()


app = FastAPI(
    title="FAQ System",
    description="AgentFlow フレームワーク級サービス - WebSocket + 富文本対応",
    version="4.0.0",
    lifespan=lifespan,
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=["*"],
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)


# =============================================================================
# API ルーターの登録
# =============================================================================

app.include_router(auth_router)
app.include_router(oauth2_router.router)
app.include_router(saml_router.router)
app.include_router(chat_router)
app.include_router(rag_router)
app.include_router(sql_router)
app.include_router(kb_settings_router)
app.include_router(agents_router)
app.include_router(ws_router)
app.include_router(misc_router)


# =============================================================================
# エントリポイント
# =============================================================================


if __name__ == "__main__":
    import argparse

    import uvicorn

    parser = argparse.ArgumentParser(description="FAQ System - FastAPI Backend")
    parser.add_argument(
        "--reload",
        action="store_true",
        help="開発模式（ホットリロード有効）",
    )
    parser.add_argument(
        "--host",
        default=None,
        help="ホスト（省略時: 環境変数 FAQ_HOST / デフォルト 0.0.0.0）",
    )
    parser.add_argument(
        "--port",
        type=int,
        default=None,
        help="ポート（省略時: 環境変数 FAQ_PORT / app_config.json）",
    )
    args = parser.parse_args()

    _cfg = _load_app_config()
    _default_port = _cfg.get("ports", {}).get("api", 8005)
    _host = args.host or os.getenv("FAQ_HOST", "0.0.0.0")
    _port = args.port or int(os.getenv("FAQ_PORT", str(_default_port)))
    os.environ["FAQ_HOST"] = _host
    os.environ["FAQ_PORT"] = str(_port)

    # 起動前にポート情報を表示（バインド失敗時でも確認できるように）
    print(f"[FAQ System] Starting on {_host}:{_port} (reload={args.reload})")

    if args.reload:
        # reload モードでは文字列形式の app パスが必要
        uvicorn.run(
            "apps.faq_system.main:app",
            host=_host,
            port=_port,
            reload=True,
            reload_dirs=["apps/faq_system", "agentflow"],
        )
    else:
        uvicorn.run(app, host=_host, port=_port)
