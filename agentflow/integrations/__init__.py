# -*- coding: utf-8 -*-
"""フレームワーク統合モジュール.

このモジュールは、FastAPI等のフレームワークとの統合を提供します。

使用例:
    >>> from agentflow.integrations.fastapi import AgentRouter
    >>> app.include_router(AgentRouter(agents=["MyAgent"]), prefix="/api")
"""

from agentflow.integrations.fastapi_integration import AgentRouter, create_app

__all__ = [
    "AgentRouter",
    "create_app",
]

