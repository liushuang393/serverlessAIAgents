"""Agent 抽象レイヤー (L3 Kernel).

AgentBlock, AgentFactory, AgentRegistry, ResilientAgent, ToolCapableAgent を提供。
遅延インポートにより循環参照を回避する。
"""

from __future__ import annotations

import importlib
from typing import Any


_LAZY_IMPORTS: dict[str, tuple[str, str]] = {
    "AgentBlock": ("kernel.agents.agent_block", "AgentBlock"),
    "AgentDescriptor": ("kernel.agents.contracts", "AgentDescriptor"),
    "AgentEvent": ("kernel.agents.contracts", "AgentEvent"),
    "AgentFeedback": ("kernel.agents.contracts", "AgentFeedback"),
    "AgentFeedbackSummary": ("kernel.agents.contracts", "AgentFeedbackSummary"),
    "AgentFactory": ("kernel.agents.agent_factory", "AgentFactory"),
    "AgentFactoryError": ("kernel.agents.agent_factory", "AgentFactoryError"),
    "AgentFactorySpec": ("kernel.agents.agent_factory", "AgentFactorySpec"),
    "AgentInvocation": ("kernel.agents.contracts", "AgentInvocation"),
    "AgentInstantiationError": ("kernel.agents.agent_factory", "AgentInstantiationError"),
    "AgentResult": ("kernel.agents.contracts", "AgentResult"),
    "AgentSharedContext": ("kernel.agents.agent_factory", "AgentSharedContext"),
    "TieredMemory": ("kernel.agents.agent_factory", "TieredMemory"),
    "create": ("kernel.agents.agent_factory", "create"),
    "get_default_shared_context": ("kernel.agents.agent_factory", "get_default_shared_context"),
    "AgentEntry": ("kernel.agents.agent_registry", "AgentEntry"),
    "AgentRegistry": ("kernel.agents.agent_registry", "AgentRegistry"),
    "LocalAgentBus": ("kernel.agents.local_agent_bus", "LocalAgentBus"),
    "get_global_agent_registry": ("kernel.agents.agent_registry", "get_global_agent_registry"),
    "get_agent_bus": ("kernel.agents.local_agent_bus", "get_agent_bus"),
    "reset_global_agent_registry": ("kernel.agents.agent_registry", "reset_global_agent_registry"),
    "reset_agent_bus": ("kernel.agents.local_agent_bus", "reset_agent_bus"),
    "Agent": ("kernel.agents.resilient_agent", "Agent"),
    "BaseDecisionAgent": ("kernel.agents.resilient_agent", "BaseDecisionAgent"),
    "InputT": ("kernel.agents.resilient_agent", "InputT"),
    "OutputT": ("kernel.agents.resilient_agent", "OutputT"),
    "ResilientAgent": ("kernel.agents.resilient_agent", "ResilientAgent"),
    "ToolCapableAgent": ("kernel.agents.tool_agent", "ToolCapableAgent"),
    "FAQAgent": ("kernel.agents.specialized.faq_agent", "FAQAgent"),
    "FAQAgentConfig": ("kernel.agents.specialized.faq_agent", "FAQAgentConfig"),
    "SalesAgent": ("shared.agents.sales_agent", "SalesAgent"),
    "SalesAgentConfig": ("shared.agents.sales_agent", "SalesAgentConfig"),
}


def __getattr__(name: str) -> Any:
    """遅延インポートを実装.

    Args:
        name: インポートする属性名

    Returns:
        インポートされた属性

    Raises:
        AttributeError: 属性が見つからない場合
    """
    if name in _LAZY_IMPORTS:
        module_path, attr_name = _LAZY_IMPORTS[name]
        module = importlib.import_module(module_path)
        return getattr(module, attr_name)
    msg = f"module {__name__!r} has no attribute {name!r}"
    raise AttributeError(msg)


__all__ = list(_LAZY_IMPORTS.keys())
