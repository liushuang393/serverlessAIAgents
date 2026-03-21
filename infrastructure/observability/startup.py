"""起動時情報ログモジュール.

アプリケーション起動時に設定情報をログ出力する。
フレームワーク標準機能として、app層から呼び出して使用する。

使用例:
    ```python
    from infrastructure.observability.startup import log_startup_info

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


logger = logging.getLogger("bizcore.startup")


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
    app_name: str = "BizCore Application",
    extra_info: dict[str, Any] | None = None,
) -> dict[str, Any]:
    """起動時情報をログ出力.

    Args:
        app_name: アプリケーション名
        extra_info: 追加情報（agents, skills, rag_sources等）

    Returns:
        設定情報辞書（プログラムで使用可能）
    """
    # 遅延インポート: infrastructure 内部参照
    from infrastructure.config import get_settings

    settings = get_settings()
    info: dict[str, Any] = {}

    # ヘッダー
    logger.info("=" * 60)
    logger.info(f"{app_name} - 起動情報")
    logger.info("=" * 60)

    # LLM 情報（gateway 設定を優先表示、フォールバックで settings）
    llm_provider = "unknown"
    llm_model = "unknown"
    llm_base_url: str | None = None
    try:
        from infrastructure.llm.gateway.config import load_gateway_config

        gw_config = load_gateway_config()
        default_role = gw_config.gateway.default_role
        default_alias = gw_config.registry.get(default_role)
        if default_alias:
            for m in gw_config.models:
                if m.alias == default_alias:
                    llm_provider = m.provider
                    llm_model = m.model
                    llm_base_url = m.api_base
                    break
    except Exception:
        # gateway 読み込み失敗時は settings からフォールバック
        llm_config = settings.get_active_llm_config()
        llm_provider = str(llm_config.get("provider", "unknown"))
        llm_model = str(llm_config.get("model", "unknown"))
        llm_base_url = llm_config.get("base_url")

    info["llm"] = {"provider": llm_provider, "model": llm_model}
    logger.info(f"[LLM] Provider: {llm_provider}")
    logger.info(f"[LLM] Model: {llm_model}")
    if llm_base_url:
        logger.info(f"[LLM] Base URL: {llm_base_url}")

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

    # Embedding 情報（gateway の platform_embedding_default を優先表示）
    embedding_model = settings.openai_embedding_model
    try:
        from infrastructure.llm.gateway.config import load_gateway_config as _load_gw

        _gw = _load_gw()
        for _m in _gw.models:
            if _m.alias == "platform_embedding_default" and _m.model:
                embedding_model = _m.model
                break
    except Exception:
        pass
    logger.info(f"[Embedding] Model: {embedding_model}")
    info["embedding"] = {"model": embedding_model}

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
