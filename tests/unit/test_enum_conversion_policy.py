# -*- coding: utf-8 -*-
"""列挙型変換ポリシーテスト.

LLM由来データに対する直接Enum変換を禁止し、
safe_enum() 経由に統一する。
"""

from pathlib import Path


FORBIDDEN_PATTERNS = [
    "StrategyType(data.get(",
    "ReversibilityLevel(data.get(",
    "ProblemType(data.get(",
    "LeverageLevel(data.get(",
    "DeathTrapSeverity(data.get(",
    "SelfCheckStatus(data.get(",
    "FindingSeverity(f.get(",
    "FindingCategory(f.get(",
    "ActionType(f.get(",
]

TARGET_FILES = [
    Path("apps/decision_governance_engine/agents/fa_agent.py"),
    Path("apps/decision_governance_engine/agents/dao_agent.py"),
    Path("apps/decision_governance_engine/agents/review_agent.py"),
]


def test_no_direct_enum_cast_for_llm_payload() -> None:
    """対象ファイルで直接Enum変換を使っていないことを確認."""
    for file_path in TARGET_FILES:
        content = file_path.read_text(encoding="utf-8")
        for pattern in FORBIDDEN_PATTERNS:
            assert pattern not in content, f"{file_path}: forbidden enum cast pattern '{pattern}'"

