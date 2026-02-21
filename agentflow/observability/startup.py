"""起動時情報ログモジュール.

アプリケーション起動時に設定情報をログ出力する。
フレームワーク標準機能として、app層から呼び出して使用する。

使用例:
    ```python
    from agentflow.observability.startup import log_startup_info

    # シンプルな起動ログ
    log_startup_info()

    # アプリ名を指定
    log_startup_info(app_name="My Decision Engine")

    # 追加情報を含める
    log_startup_info(
        app_name="My App",
        extra_info={"agents": ["agent1", "agent2"], "version": "1.0.0"}
    )
    ```
"""

import logging
from typing import Any

from agentflow.config import get_settings


logger = logging.getLogger("agentflow.startup")


def _mask_secret(value: str | None, visible_chars: int = 4) -> str:
    """機密情報をマスク.

    Args:
        value: マスク対象の文字列
        visible_chars: 表示する末尾文字数

    Returns:
        マスクされた文字列
    """
    if not value:
        return "Not configured"
    if len(value) <= visible_chars:
        return "***"
    return f"***{value[-visible_chars:]}"


def _mask_url(url: str | None) -> str:
    """URL内の機密情報をマスク."""
    if not url:
        return "Not configured"
    if "@" in url:
        parts = url.split("@")
        return f"***@{parts[-1]}"
    if len(url) > 40:
        return url[:30] + "..."
    return url


def log_startup_info(
    app_name: str = "AgentFlow Application",
    extra_info: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """起動時情報をログ出力.

    Args:
        app_name: アプリケーション名
        extra_info: 追加情報（agents, skills, rag_sources等）

    Returns:
        設定情報辞書（プログラムで使用可能）
    """
    settings = get_settings()
    info: dict[str, Any] = {}

    # ヘッダー
    logger.info("=" * 60)
    logger.info(f"{app_name} - 起動情報")
    logger.info("=" * 60)

    # LLM 情報
    llm_config = settings.get_active_llm_config()
    info["llm"] = {
        "provider": llm_config["provider"],
        "model": llm_config["model"],
    }
    logger.info(f"[LLM] Provider: {llm_config['provider']}")
    logger.info(f"[LLM] Model: {llm_config['model']}")
    if llm_config.get("base_url"):
        logger.info(f"[LLM] Base URL: {llm_config['base_url']}")

    # DB 情報
    db_config = settings.get_db_config()
    info["db"] = {"backend": db_config["backend"]}
    logger.info(f"[DB] Backend: {db_config['backend']}")
    if db_config.get("url"):
        logger.info(f"[DB] URL: {_mask_url(db_config['url'])}")

    # VectorDB 情報
    vdb_config = settings.get_vectordb_config()
    info["vectordb"] = {"backend": vdb_config["backend"]}
    logger.info(f"[VectorDB] Backend: {vdb_config['backend']}")
    if vdb_config.get("persist_dir"):
        logger.info(f"[VectorDB] Path: {vdb_config['persist_dir']}")
    if vdb_config.get("collection"):
        logger.info(f"[VectorDB] Collection: {vdb_config['collection']}")
    if vdb_config.get("index"):
        logger.info(f"[VectorDB] Index: {vdb_config['index']}")

    # Embedding 情報
    logger.info(f"[Embedding] Model: {settings.openai_embedding_model}")
    info["embedding"] = {"model": settings.openai_embedding_model}

    # 追加情報
    if extra_info:
        info["extra"] = extra_info
        if "agents" in extra_info:
            agents = extra_info["agents"]
            logger.info(f"[Agents] 登録数: {len(agents)}")
            for agent in agents[:10]:
                logger.info(f"  - {agent}")
            if len(agents) > 10:
                logger.info(f"  ... and {len(agents) - 10} more")

        if "skills" in extra_info:
            skills = extra_info["skills"]
            logger.info(f"[Skills] 登録数: {len(skills)}")
            for skill in skills[:5]:
                logger.info(f"  - {skill}")

        if "version" in extra_info:
            logger.info(f"[Version] {extra_info['version']}")

    logger.info("=" * 60)

    return info
