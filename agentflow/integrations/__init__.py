# -*- coding: utf-8 -*-
"""フレームワーク統合モジュール.

このモジュールは、FastAPI等のフレームワークとの統合を提供します。

使用例:
    >>> from agentflow.integrations.fastapi import AgentRouter
    >>> app.include_router(AgentRouter(agents=["MyAgent"]), prefix="/api")
    >>>
    >>> # SSEFlowRunner を使用
    >>> from agentflow.integrations import SSEFlowRunner
    >>> runner = SSEFlowRunner(pipeline)
    >>> async for event in runner.run_with_events(data):
    ...     yield event
"""

from agentflow.integrations.fastapi_integration import AgentRouter, create_app
from agentflow.integrations.sse_flow_runner import (
    FlowProtocol,
    SSEConfig,
    SSEFlowRunner,
    SimplePipelineProtocol,
)

__all__ = [
    "AgentRouter",
    "create_app",
    "FlowProtocol",
    "SSEConfig",
    "SSEFlowRunner",
    "SimplePipelineProtocol",
]

