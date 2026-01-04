# -*- coding: utf-8 -*-
"""Decision Governance Engine - 設定.

API起動時の初期化処理を提供。

使用例:
    >>> from apps.decision_governance_engine.flow_config import (
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


def register_flow_definition() -> None:
    """Flow定義をレジストリに登録."""
    try:
        flow_def = FlowDefinition.from_yaml(YAML_PATH)
        FlowDefinitionRegistry.get_instance().register(flow_def)
        _logger.info(f"Registered flow: {flow_def.flow_id}")
    except Exception as e:
        _logger.warning(f"Failed to register flow: {e}")


def setup_result_store() -> None:
    """結果ストアを設定."""
    ResultStoreManager.set_store(MemoryResultStore(max_size=500))

