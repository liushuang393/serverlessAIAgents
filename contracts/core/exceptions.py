"""contracts.core.exceptions — 全レイヤー共通の基底例外.

L0 契約層に属し、どのレイヤーからも安全に import できる。
kernel.core.exceptions の AgentFlowError と同一階層を提供する。
"""


class AgentFlowError(Exception):
    """AgentFlow 基底例外."""
