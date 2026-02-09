"""起動時情報ログモジュール（App層）.

フレームワーク層の log_startup_info を呼び出し、
App固有の追加情報（登録済みAgent、RAGソース等）を付加する。

使用例:
    from apps.decision_governance_engine.startup import log_startup_info
    log_startup_info()
"""

from pathlib import Path
from typing import Any

import yaml

from agentflow.observability.startup import log_startup_info as _framework_log_startup


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


def _get_rag_sources(config: dict[str, Any]) -> list[str]:
    """RAG ソース一覧を取得."""
    rag = config.get("rag", {})
    if not rag.get("enabled", False):
        return ["RAG disabled"]
    sources = rag.get("sources", {})
    return [f"{k}: {v.get('description', '')}" for k, v in sources.items()]


def log_startup_info() -> dict[str, Any]:
    """起動時情報をログ出力.

    フレームワーク層の機能を呼び出し、App固有の情報を追加。

    Returns:
        設定情報辞書
    """
    # App固有情報を収集
    config = _load_agent_config()
    agents = _get_registered_agents(config)
    rag_sources = _get_rag_sources(config)

    extra_info = {
        "agents": agents,
        "rag_sources": rag_sources,
    }

    # フレームワーク層の関数を呼び出し
    return _framework_log_startup(
        app_name="Decision Governance Engine",
        extra_info=extra_info,
    )

