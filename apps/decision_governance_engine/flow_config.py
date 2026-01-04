# -*- coding: utf-8 -*-
"""Decision Governance Engine - Flow定義設定.

目的: AgentFlow框架と連携するFlow定義を提供
使用: api.py起動時に自動登録

変更履歴:
    - v2.2: YAMLから定義を読み込むように変更（重複定義を排除）

使用例:
    >>> from apps.decision_governance_engine.flow_config import register_flow_definition
    >>> register_flow_definition()  # 起動時に呼び出し
"""

import logging
from pathlib import Path

from agentflow.core.flow_definition import (
    FlowDefinition,
    FlowDefinitionRegistry,
)
from agentflow.core.result_store import MemoryResultStore, ResultStoreManager


# Flow ID（前後端共通）
FLOW_ID = "decision-governance-engine"

# YAML定義ファイルパス
_YAML_PATH = Path(__file__).parent / "agents" / "agent_definitions.yaml"

# Flow定義（遅延読み込み）
_FLOW_DEFINITION: FlowDefinition | None = None
_logger = logging.getLogger("decision_engine.flow_config")


def _load_flow_definition() -> FlowDefinition:
    """YAMLからFlow定義を読み込み（キャッシュ付き）.

    Returns:
        FlowDefinition インスタンス
    """
    global _FLOW_DEFINITION
    if _FLOW_DEFINITION is not None:
        return _FLOW_DEFINITION

    try:
        _FLOW_DEFINITION = FlowDefinition.from_yaml(_YAML_PATH)
        _logger.info(
            f"Loaded flow definition from YAML: "
            f"{len(_FLOW_DEFINITION.agents)} agents"
        )
    except FileNotFoundError:
        _logger.warning(f"YAML not found: {_YAML_PATH}, using fallback")
        _FLOW_DEFINITION = _create_fallback_definition()
    except Exception as e:
        _logger.error(f"Failed to load YAML: {e}, using fallback")
        _FLOW_DEFINITION = _create_fallback_definition()

    return _FLOW_DEFINITION


def _create_fallback_definition() -> FlowDefinition:
    """フォールバック用のFlow定義を作成.

    YAMLが読み込めない場合のデフォルト定義。

    Returns:
        FlowDefinition インスタンス
    """
    from agentflow.core.flow_definition import AgentDefinition

    return FlowDefinition(
        flow_id=FLOW_ID,
        name="Decision Governance Engine",
        version="2.2.0",
        description="意思決定支援エンジン - 認知・門番・診断・道法術器・検証の8ステップ",
        agents=[
            AgentDefinition(
                id="cognitive_gate",
                name="認知",
                label="認知前処理",
                icon="🧠",
            ),
            AgentDefinition(
                id="gatekeeper",
                name="門番",
                label="入口検証",
                icon="🚪",
            ),
            AgentDefinition(
                id="clarification",
                name="診断",
                label="問題診断",
                icon="🔬",
            ),
            AgentDefinition(
                id="dao",
                name="道",
                label="本質分析",
                icon="🎯",
            ),
            AgentDefinition(
                id="fa",
                name="法",
                label="戦略選定",
                icon="🛤️",
            ),
            AgentDefinition(
                id="shu",
                name="術",
                label="実行計画",
                icon="📋",
            ),
            AgentDefinition(
                id="qi",
                name="器",
                label="技術実装",
                icon="🔧",
            ),
            AgentDefinition(
                id="review",
                name="検証",
                label="最終検証",
                icon="🔍",
            ),
        ],
    )


def register_flow_definition() -> None:
    """Flow定義をレジストリに登録.

    アプリ起動時に呼び出す。
    """
    flow_def = _load_flow_definition()
    registry = FlowDefinitionRegistry.get_instance()
    registry.register(flow_def)


def setup_result_store() -> None:
    """結果ストアを設定.

    デフォルトはMemoryResultStore（開発用）。
    本番環境ではFileResultStoreやDB接続に変更。
    """
    ResultStoreManager.set_store(MemoryResultStore(max_size=500))


def get_flow_definition() -> FlowDefinition:
    """Flow定義を取得."""
    return _load_flow_definition()


def get_agent_ids() -> list[str]:
    """Agent IDリストを取得."""
    flow_def = _load_flow_definition()
    return flow_def.get_agent_ids()


def get_agent_definitions() -> list[dict]:
    """Agent定義リストを取得（フロントエンド用）.

    Returns:
        Agent定義リスト（id, name, label, icon）
    """
    flow_def = _load_flow_definition()
    return [a.to_frontend_dict() for a in flow_def.agents]

