# -*- coding: utf-8 -*-
"""起動時情報ログモジュール.

アプリケーション起動時に設定情報をログ出力する。
- LLM プロバイダー / モデル
- DB 設定
- 登録済み Agent / Skills / MCP ツール
"""

import logging
import os
from pathlib import Path
from typing import Any

import yaml

from agentflow.config import get_settings
from agentflow.observability.logging import get_logger, LogLevel, LogConfig

# アプリ専用ロガー
_startup_logger = get_logger("decision_engine.startup")


def _detect_llm_info() -> dict[str, str]:
    """LLM プロバイダー情報を検出.

    Returns:
        provider, model, base_url などの情報
    """
    info: dict[str, str] = {"provider": "unknown", "model": "unknown"}

    # 環境変数から検出（優先順位順）
    if os.getenv("OPENAI_API_KEY"):
        info["provider"] = "OpenAI"
        info["model"] = os.getenv("OPENAI_MODEL", "gpt-4o-mini")
    elif os.getenv("ANTHROPIC_API_KEY"):
        info["provider"] = "Anthropic"
        info["model"] = os.getenv("ANTHROPIC_MODEL", "claude-3-5-sonnet-20241022")
    elif os.getenv("GOOGLE_API_KEY"):
        info["provider"] = "Google"
        info["model"] = os.getenv("GOOGLE_MODEL", "gemini-2.0-flash-exp")
    elif os.getenv("DEEPSEEK_API_KEY"):
        info["provider"] = "DeepSeek"
        info["model"] = os.getenv("DEEPSEEK_MODEL", "deepseek-chat")
        info["base_url"] = "https://api.deepseek.com"
    elif os.getenv("OLLAMA_BASE_URL"):
        info["provider"] = "Ollama"
        info["model"] = os.getenv("OLLAMA_MODEL", "llama3.3:70b")
        info["base_url"] = os.getenv("OLLAMA_BASE_URL", "")
    else:
        info["provider"] = "Mock (No API Key)"
        info["model"] = "mock"

    return info


def _detect_db_info() -> dict[str, str]:
    """DB 設定情報を検出.

    Returns:
        backend, url などの情報（機密部分はマスク）
    """
    info: dict[str, str] = {"backend": "none", "url": "N/A"}

    if os.getenv("SUPABASE_URL"):
        info["backend"] = "Supabase"
        info["url"] = _mask_url(os.getenv("SUPABASE_URL", ""))
    elif os.getenv("TURSO_URL"):
        info["backend"] = "Turso"
        info["url"] = _mask_url(os.getenv("TURSO_URL", ""))
    elif os.getenv("DATABASE_URL"):
        url = os.getenv("DATABASE_URL", "")
        if url.startswith("postgres"):
            info["backend"] = "PostgreSQL"
        elif url.startswith("sqlite"):
            info["backend"] = "SQLite"
        else:
            info["backend"] = "Unknown"
        info["url"] = _mask_url(url)
    else:
        info["backend"] = "In-Memory (No DB configured)"

    return info


def _mask_url(url: str) -> str:
    """URL の機密部分をマスク."""
    if not url:
        return "N/A"
    # パスワードや認証トークンをマスク
    if "@" in url:
        parts = url.split("@")
        return f"***@{parts[-1]}"
    if len(url) > 30:
        return url[:20] + "..."
    return url


def _load_agent_config() -> dict[str, Any]:
    """agent.yaml から設定を読み込み.

    Returns:
        YAML 設定辞書
    """
    config_path = Path(__file__).parent / "agent.yaml"
    if config_path.exists():
        with open(config_path, encoding="utf-8") as f:
            return yaml.safe_load(f) or {}
    return {}


def _get_registered_agents(config: dict[str, Any]) -> list[str]:
    """登録済み Agent 一覧を取得."""
    agents = config.get("agents", {})
    return [f"{k} ({v.get('name', 'N/A')})" for k, v in agents.items()]


def _get_registered_skills() -> list[str]:
    """登録済み Skills 一覧を取得."""
    try:
        from agentflow import list_skills
        return list_skills()
    except Exception:
        return []


def _get_rag_sources(config: dict[str, Any]) -> list[str]:
    """RAG ソース一覧を取得."""
    rag = config.get("rag", {})
    if not rag.get("enabled", False):
        return ["RAG disabled"]
    sources = rag.get("sources", {})
    return [f"{k}: {v.get('description', '')}" for k, v in sources.items()]


def _detect_vectordb_info() -> dict[str, str]:
    """VectorDB 設定情報を検出.

    Returns:
        backend, persist_dir などの情報
    """
    info: dict[str, str] = {"backend": "none", "path": "N/A"}

    if os.getenv("PINECONE_API_KEY"):
        info["backend"] = "Pinecone"
        info["index"] = os.getenv("PINECONE_INDEX", "default")
    elif os.getenv("QDRANT_URL"):
        info["backend"] = "Qdrant"
        info["path"] = _mask_url(os.getenv("QDRANT_URL", ""))
    elif os.getenv("CHROMA_PERSIST_DIR"):
        info["backend"] = "ChromaDB (Local)"
        info["path"] = os.getenv("CHROMA_PERSIST_DIR", "")
        collection = os.getenv("CHROMA_COLLECTION", "default")
        info["collection"] = collection
    else:
        info["backend"] = "In-Memory (Mock)"

    return info


def log_startup_info() -> None:
    """起動時情報をログ出力."""
    logger = logging.getLogger("decision_engine.startup")

    # ヘッダー
    logger.info("=" * 60)
    logger.info("Decision Governance Engine - 起動情報")
    logger.info("=" * 60)

    # LLM 情報
    llm_info = _detect_llm_info()
    logger.info(f"[LLM] Provider: {llm_info['provider']}")
    logger.info(f"[LLM] Model: {llm_info['model']}")
    if "base_url" in llm_info:
        logger.info(f"[LLM] Base URL: {llm_info['base_url']}")

    # DB 情報
    db_info = _detect_db_info()
    logger.info(f"[DB] Backend: {db_info['backend']}")
    logger.info(f"[DB] URL: {db_info['url']}")

    # VectorDB 情報
    vdb_info = _detect_vectordb_info()
    logger.info(f"[VectorDB] Backend: {vdb_info['backend']}")
    if "path" in vdb_info and vdb_info["path"] != "N/A":
        logger.info(f"[VectorDB] Path: {vdb_info['path']}")
    if "collection" in vdb_info:
        logger.info(f"[VectorDB] Collection: {vdb_info['collection']}")
    if "index" in vdb_info:
        logger.info(f"[VectorDB] Index: {vdb_info['index']}")

    # Agent 設定
    config = _load_agent_config()
    agents = _get_registered_agents(config)
    logger.info(f"[Agents] 登録数: {len(agents)}")
    for agent in agents:
        logger.info(f"  - {agent}")

    # Skills
    skills = _get_registered_skills()
    logger.info(f"[Skills] 登録数: {len(skills)}")
    for skill in skills[:10]:  # 最大10件
        logger.info(f"  - {skill}")
    if len(skills) > 10:
        logger.info(f"  ... and {len(skills) - 10} more")

    # RAG ソース
    rag_sources = _get_rag_sources(config)
    logger.info(f"[RAG] Sources: {len(rag_sources)}")
    for src in rag_sources:
        logger.info(f"  - {src}")

    logger.info("=" * 60)

