# -*- coding: utf-8 -*-
"""Decision Governance Engine - 企業級意思決定支援システム.

意思決定を「道・法・術・器」のフレームワークで構造化し、
署名可能な決策レポートを生成するMulti-Agentシステム。

アーキテクチャ:
    GatekeeperAgent → DaoAgent → FaAgent → ShuAgent → QiAgent → ReviewAgent

使用例:
    >>> from apps.decision_governance_engine import DecisionEngine
    >>> engine = DecisionEngine()
    >>> result = await engine.process("新規事業AとBのどちらに投資すべきか")
"""

from apps.decision_governance_engine.workflow import DecisionEngine

__version__ = "1.0.0"
__author__ = "AgentFlow Team"

__all__ = ["DecisionEngine"]

