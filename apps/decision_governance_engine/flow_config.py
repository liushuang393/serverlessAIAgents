"""Decision Governance Engine - 設定.

API起動時の初期化処理を提供。

使用例:
    >>> from apps.decision_governance_engine.flow_config import (
    ...     get_flow_definition,
    ...     register_flow_definition,
    ...     setup_result_store,
    ... )
    >>> register_flow_definition()
    >>> setup_result_store()
"""

import logging
from pathlib import Path

from agentflow.core.flow_definition import FlowDefinition, FlowDefinitionRegistry
from agentflow.core.result_store import MemoryResultStore, ResultStoreManager


# 定数
FLOW_ID = "decision-governance-engine"
YAML_PATH = Path(__file__).parent / "agents" / "agent_definitions.yaml"

_logger = logging.getLogger("decision_engine.config")

# キャッシュ
_flow_definition: FlowDefinition | None = None


def get_flow_definition() -> FlowDefinition:
    """Flow定義を取得（キャッシュ付き）.

    Returns:
        FlowDefinition インスタンス
    """
    global _flow_definition
    if _flow_definition is None:
        _flow_definition = FlowDefinition.from_yaml(YAML_PATH)
    return _flow_definition


def register_flow_definition() -> None:
    """Flow定義をレジストリに登録."""
    try:
        flow_def = get_flow_definition()
        FlowDefinitionRegistry.get_instance().register(flow_def)
        _logger.info(f"Registered flow: {flow_def.flow_id}")
    except Exception as e:
        _logger.warning(f"Failed to register flow: {e}")


def setup_result_store() -> None:
    """結果ストアを設定."""
    ResultStoreManager.set_store(MemoryResultStore(max_size=500))
