"""Decision Governance Engine - 企業級意思決定支援システム.

PipelineEngine パターンを使用した Multi-Agent 意思決定支援システム。
意思決定を「道・法・術・器」のフレームワークで構造化し、
署名可能な決策レポートを生成。

アーキテクチャ:
    Gate層: CognitiveGate → Gatekeeper
    分析層: Clarification → [Dao, Fa, Shu, Qi]
    検証層: ReviewAgent（COACH改善指導 / REVISE対応）

使用例:
    >>> from apps.decision_governance_engine import DecisionEngine
    >>>
    >>> engine = DecisionEngine()
    >>> result = await engine.run({"question": "新規事業に投資すべきか"})
    >>>
    >>> # SSEストリーム
    >>> async for event in engine.run_stream(inputs):
    ...     print(event)
"""

from apps.decision_governance_engine.engine import DecisionEngine


__version__ = "2.0.0"
__author__ = "AgentFlow Team"

__all__ = ["DecisionEngine"]

