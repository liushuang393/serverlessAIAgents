"""RAG設定ヘルパー.

Agent初期化時にRAG設定を動的に適用するためのヘルパー関数。
routers/config.py の設定を参照し、Agent の RAG パラメータを更新する。
"""

import logging
from typing import Any

from agentflow.skills import RAGConfig, RAGSkill


logger = logging.getLogger("decision_api.rag_helper")


def get_rag_config_for_agent(agent_id: str) -> dict[str, Any] | None:
    """特定AgentのRAG設定を取得.

    Args:
        agent_id: Agent ID（shu, qi など）

    Returns:
        RAG設定辞書、または None（設定なしの場合）
    """
    try:
        from apps.decision_governance_engine.routers.config import get_rag_config

        config = get_rag_config(agent_id)
        if config is None:
            return None

        return {
            "use_rag": config.use_rag,
            "top_k": config.top_k,
            "min_similarity": config.min_similarity,
            "sources": [s.name for s in config.rag_sources if s.enabled],
        }
    except ImportError:
        # routers モジュールがない場合はデフォルト設定を使用
        logger.warning(f"Config module not available for agent: {agent_id}")
        return None
    except Exception as e:
        logger.exception(f"Failed to get RAG config for {agent_id}: {e}")
        return None


def apply_rag_config(
    rag_config: RAGConfig,
    agent_id: str,
) -> RAGConfig:
    """RAG設定を適用.

    routers/config.py の設定値で RAGConfig を更新する。

    Args:
        rag_config: 元のRAGConfig
        agent_id: Agent ID

    Returns:
        更新されたRAGConfig
    """
    config_data = get_rag_config_for_agent(agent_id)
    if config_data is None:
        return rag_config

    # 設定値を適用
    if config_data.get("top_k"):
        rag_config.top_k = config_data["top_k"]
    if config_data.get("min_similarity") is not None:
        rag_config.min_similarity = config_data["min_similarity"]

    logger.info(f"Applied RAG config for {agent_id}: top_k={rag_config.top_k}, min_sim={rag_config.min_similarity}")
    return rag_config


def should_use_rag(agent_id: str) -> bool:
    """AgentがRAGを使用すべきかどうかを判定.

    Args:
        agent_id: Agent ID

    Returns:
        RAGを使用すべき場合 True
    """
    config_data = get_rag_config_for_agent(agent_id)
    if config_data is None:
        # デフォルトは使用しない
        return False
    return config_data.get("use_rag", False)


async def initialize_agent_rag(
    agent: Any,
    agent_id: str,
    default_config: RAGConfig,
) -> RAGSkill | None:
    """Agent用のRAGを初期化.

    Args:
        agent: Agentインスタンス
        agent_id: Agent ID
        default_config: デフォルトRAGConfig

    Returns:
        初期化されたRAGSkill、またはNone
    """
    if not should_use_rag(agent_id):
        logger.info(f"RAG disabled for agent: {agent_id}")
        return None

    # 設定を適用
    config = apply_rag_config(default_config, agent_id)

    # RAGSkill初期化
    rag = RAGSkill(rag_config=config)
    await rag.start()

    logger.info(f"RAG initialized for agent: {agent_id}")
    return rag
