"""FAQ System ルーター共通ユーティリティ・サービス取得.

ルーター間で共有するサービス初期化・ヘルパーを集約する。
直接 Agent/Service を保持せず、遅延初期化でスレッドセーフに取得。
"""

from __future__ import annotations

import json
import logging
import os
from pathlib import Path
from typing import Any
from uuid import uuid4

from apps.faq_system.backend.config import kb_registry
from apps.faq_system.backend.services.chat_history_service import ChatHistoryService
from apps.faq_system.backend.services.feedback_service import FeedbackService
from agentflow.agents import (
    FAQAgent,
    FAQAgentConfig,
    SalesAgent,
    SalesAgentConfig,
)
from agentflow.services import (
    RAGConfig,
    RAGService,
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


def get_chat_history_service() -> ChatHistoryService:
    """チャット履歴サービスを取得."""
    return _chat_history_service


def resolve_default_collection() -> str:
    """既定コレクション名を解決."""
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
    resolved_collection = collection or resolve_default_collection()
    service_key = f"rag:{resolved_collection}"
    if service_key not in _services:
        _services[service_key] = RAGService(
            RAGConfig(
                collection=resolved_collection,
                chunk_strategy="semantic",
                reranker="bm25",
            )
        )
    return _services[service_key]


def get_sql_service() -> Text2SQLService:
    """SQLサービス取得（遅延初期化）."""
    if "sql" not in _services:
        schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
        _services["sql"] = Text2SQLService(
            Text2SQLConfig(
                schema=schema,
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
        schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
        _services["faq_agent"] = FAQAgent(
            FAQAgentConfig(
                rag_collection=resolve_default_collection(),
                sql_schema=schema,
            )
        )
    return _services["faq_agent"]


def get_sales_agent() -> SalesAgent:
    """SalesAgent取得（遅延初期化）."""
    if "sales_agent" not in _services:
        schema = json.loads(os.getenv("DB_SCHEMA", "{}"))
        _services["sales_agent"] = SalesAgent(
            SalesAgentConfig(
                sql_schema=schema,
            )
        )
    return _services["sales_agent"]


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
    stale_keys = [
        key for key in _services
        if any(key.startswith(p) or key == p for p in prefixes)
    ]
    for key in stale_keys:
        _services.pop(key, None)
