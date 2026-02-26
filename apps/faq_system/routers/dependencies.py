"""FAQ System ルーター共通ユーティリティ・サービス取得.

ルーター間で共有するサービス初期化・ヘルパーを集約する。
直接 Agent/Service を保持せず、遅延初期化でスレッドセーフに取得。
"""

from __future__ import annotations

import json
import logging
import os
from datetime import datetime
from pathlib import Path
from typing import Any
from uuid import uuid4

try:
    from apscheduler.schedulers.asyncio import AsyncIOScheduler
    from apscheduler.triggers.cron import CronTrigger
except ImportError:  # pragma: no cover - optional dependency fallback
    AsyncIOScheduler = Any  # type: ignore[assignment,misc]
    CronTrigger = None
from apps.faq_system.backend.config import kb_registry
from apps.faq_system.backend.services.chat_history_service import ChatHistoryService
from apps.faq_system.backend.services.feedback_service import FeedbackService
from apps.faq_system.backend.services.rag_ingestion import RAGIngestionOrchestrator
from apps.faq_system.backend.services.rag_runtime_config import (
    RAGRuntimeConfig,
    load_rag_runtime_config,
)

from agentflow.agents import (
    FAQAgent,
    FAQAgentConfig,
    SalesAgent,
    SalesAgentConfig,
)
from agentflow.services import (
    ChunkStrategy,
    RAGConfig,
    RAGService,
    RerankerType,
    SQLDialect,
    SuggestionConfig,
    SuggestionService,
    Text2SQLConfig,
    Text2SQLService,
)


logger = logging.getLogger(__name__)


# サービスインスタンスキャッシュ（遅延初期化・私有）
_services: dict[str, Any] = {}
_artifact_registry: dict[str, Path] = {}
_chat_history_service = ChatHistoryService()
_runtime_config: RAGRuntimeConfig | None = None
_ingestion_scheduler: AsyncIOScheduler | None = None


def get_chat_history_service() -> ChatHistoryService:
    """チャット履歴サービスを取得."""
    return _chat_history_service


def get_runtime_rag_config(*, refresh: bool = False) -> RAGRuntimeConfig:
    """RAG/SQL runtime 設定を取得."""
    global _runtime_config
    if _runtime_config is None or refresh:
        _runtime_config = load_rag_runtime_config()
    return _runtime_config


def is_rag_enabled() -> bool:
    """RAG 機能有効判定."""
    return get_runtime_rag_config().rag_enabled


def is_sql_enabled() -> bool:
    """SQL 機能有効判定."""
    return get_runtime_rag_config().sql_enabled


def resolve_default_collection() -> str:
    """既定コレクション名を解決."""
    runtime_cfg = get_runtime_rag_config()
    if runtime_cfg.rag_collection:
        return runtime_cfg.rag_collection
    env_collection = os.getenv("RAG_COLLECTION")
    if env_collection:
        return env_collection
    return kb_registry.resolve_collection()


def resolve_session_id(session_id: str | None) -> str:
    """セッションIDを正規化."""
    if session_id and session_id.strip():
        return session_id.strip()
    return f"session-{uuid4().hex}"


def extract_assistant_content(payload: dict[str, Any]) -> str:
    """保存用に回答本文を抽出."""
    answer = payload.get("answer")
    if isinstance(answer, str) and answer.strip():
        return answer
    try:
        return json.dumps(payload, ensure_ascii=False)
    except TypeError:
        return str(payload)


def get_rag_service(collection: str | None = None) -> RAGService:
    """RAGサービス取得（遅延初期化）."""
    runtime_cfg = get_runtime_rag_config()
    resolved_collection = collection or resolve_default_collection()
    service_key = f"rag:{resolved_collection}"
    if service_key not in _services:
        try:
            chunk_strategy = ChunkStrategy(runtime_cfg.rag_chunk_strategy)
        except ValueError:
            chunk_strategy = ChunkStrategy.RECURSIVE
        try:
            reranker = RerankerType(runtime_cfg.rag_reranker)
        except ValueError:
            reranker = RerankerType.BM25

        _services[service_key] = RAGService(
            RAGConfig(
                collection=resolved_collection,
                chunk_strategy=chunk_strategy,
                reranker=reranker,
                top_k=runtime_cfg.rag_top_k,
            )
        )
    return _services[service_key]


def get_sql_service() -> Text2SQLService:
    """SQLサービス取得（遅延初期化）."""
    if "sql" not in _services:
        runtime_cfg = get_runtime_rag_config()
        schema = runtime_cfg.sql_schema or json.loads(os.getenv("DB_SCHEMA", "{}"))
        try:
            dialect = SQLDialect(runtime_cfg.sql_dialect)
        except ValueError:
            dialect = SQLDialect.POSTGRESQL
        _services["sql"] = Text2SQLService(
            Text2SQLConfig(
                schema=schema,
                dialect=dialect,
                auto_chart=True,
            )
        )
    return _services["sql"]


def get_feedback_service() -> FeedbackService:
    """フィードバックサービス取得（遅延初期化）."""
    if "feedback" not in _services:
        _services["feedback"] = FeedbackService()
    return _services["feedback"]


def get_suggestion_service() -> SuggestionService:
    """提案サービス取得（遅延初期化）."""
    if "suggestion" not in _services:
        _services["suggestion"] = SuggestionService(
            SuggestionConfig(
                max_suggestions=5,
                language="ja",
            )
        )
    return _services["suggestion"]


def get_faq_agent() -> FAQAgent:
    """FAQAgent取得（遅延初期化）."""
    if "faq_agent" not in _services:
        runtime_cfg = get_runtime_rag_config()
        _services["faq_agent"] = FAQAgent(
            FAQAgentConfig(
                rag_collection=resolve_default_collection(),
                rag_chunk_strategy=runtime_cfg.rag_chunk_strategy,
                rag_reranker=runtime_cfg.rag_reranker,
                rag_top_k=runtime_cfg.rag_top_k,
                sql_schema=runtime_cfg.sql_schema or json.loads(os.getenv("DB_SCHEMA", "{}")),
                sql_dialect=runtime_cfg.sql_dialect,
                enable_rag=runtime_cfg.rag_enabled,
                enable_sql=runtime_cfg.sql_enabled,
                enable_hybrid=runtime_cfg.hybrid_enabled,
            )
        )
    return _services["faq_agent"]


def get_sales_agent() -> SalesAgent:
    """SalesAgent取得（遅延初期化）."""
    if "sales_agent" not in _services:
        runtime_cfg = get_runtime_rag_config()
        _services["sales_agent"] = SalesAgent(
            SalesAgentConfig(
                sql_schema=runtime_cfg.sql_schema or json.loads(os.getenv("DB_SCHEMA", "{}")),
            )
        )
    return _services["sales_agent"]


def get_rag_ingestion_orchestrator() -> RAGIngestionOrchestrator:
    """RAG ingest 実行器を取得."""
    if "rag_ingestion_orchestrator" not in _services:
        _services["rag_ingestion_orchestrator"] = RAGIngestionOrchestrator(
            config_loader=get_runtime_rag_config,
            rag_service_getter=get_rag_service,
        )
    return _services["rag_ingestion_orchestrator"]


async def _run_scheduled_ingest_job(source_id: str) -> None:
    """Cron から source 単位 ingest を実行."""
    orchestrator = get_rag_ingestion_orchestrator()
    await orchestrator.ingest(source_ids=[source_id], dry_run=False)


async def start_rag_ingestion_scheduler() -> None:
    """RAG ingest scheduler を開始."""
    global _ingestion_scheduler
    if CronTrigger is None:
        logger.warning("APScheduler is not installed. RAG ingestion scheduler is disabled.")
        return
    if _ingestion_scheduler is not None and _ingestion_scheduler.running:
        return

    orchestrator = get_rag_ingestion_orchestrator()
    entries = orchestrator.resolve_schedule_entries()
    if not entries:
        logger.info("RAG ingestion scheduler: no schedule configured")
        return

    timezone = datetime.now().astimezone().tzinfo
    scheduler = AsyncIOScheduler(timezone=timezone)
    for entry in entries:
        try:
            trigger = CronTrigger.from_crontab(entry.schedule, timezone=timezone)
        except ValueError:
            logger.warning(
                "Invalid cron expression for source=%s schedule=%s",
                entry.source_id,
                entry.schedule,
            )
            continue

        scheduler.add_job(
            _run_scheduled_ingest_job,
            trigger=trigger,
            id=f"rag-ingest:{entry.source_id}",
            replace_existing=True,
            kwargs={"source_id": entry.source_id},
            max_instances=1,
            coalesce=True,
        )

    if not scheduler.get_jobs():
        logger.info("RAG ingestion scheduler: no valid cron jobs")
        return

    scheduler.start()
    _ingestion_scheduler = scheduler
    logger.info("RAG ingestion scheduler started with %s jobs", len(scheduler.get_jobs()))


async def stop_rag_ingestion_scheduler() -> None:
    """RAG ingest scheduler を停止."""
    global _ingestion_scheduler
    if _ingestion_scheduler is None:
        return
    _ingestion_scheduler.shutdown(wait=False)
    _ingestion_scheduler = None
    logger.info("RAG ingestion scheduler stopped")


def register_artifacts(payload: dict[str, Any]) -> dict[str, Any]:
    """生成アセットを登録し、ダウンロードURLを注入."""
    artifacts = payload.get("artifacts")
    if not isinstance(artifacts, list):
        return payload

    for artifact in artifacts:
        if not isinstance(artifact, dict):
            continue
        artifact_id = str(artifact.get("artifact_id", "")).strip()
        file_path = str(artifact.get("file_path", "")).strip()
        if not artifact_id or not file_path:
            continue

        path_obj = Path(file_path)
        if not path_obj.exists() or not path_obj.is_file():
            continue

        _artifact_registry[artifact_id] = path_obj.resolve()
        artifact["download_url"] = f"/api/assets/{artifact_id}/download"

    rich_response = payload.get("rich_response")
    if isinstance(rich_response, dict):
        components = rich_response.get("components")
        if isinstance(components, list):
            for component in components:
                if not isinstance(component, dict):
                    continue
                props = component.get("props")
                if not isinstance(props, dict):
                    continue
                url = props.get("url")
                if not isinstance(url, str) or not url.startswith("artifact://"):
                    continue
                artifact_id = url.replace("artifact://", "", 1)
                if artifact_id in _artifact_registry:
                    props["url"] = f"/api/assets/{artifact_id}/download"
    return payload


def get_artifact_path(artifact_id: str) -> Path | None:
    """登録済みアーティファクトのパスを取得."""
    return _artifact_registry.get(artifact_id)


def invalidate_service_cache(*prefixes: str) -> None:
    """指定プレフィックスにマッチするサービスキャッシュを破棄."""
    global _runtime_config
    stale_keys = [key for key in _services if any(key.startswith(p) or key == p for p in prefixes)]
    for key in stale_keys:
        _services.pop(key, None)
    if any(prefix in {"runtime", "rag", "sql", "faq_agent"} for prefix in prefixes):
        _runtime_config = None
