# -*- coding: utf-8 -*-
"""Decision Governance Engine - Router模块.

API路由的模块化拆分，提高可维护性和复用性。

模块结构:
    - auth.py: 认证相关端点
    - config.py: RAG设定管理端点
    - decision.py: 决策处理端点
    - knowledge.py: 知识库CRUD（通用工厂）
    - report.py: 报告相关端点
    - workflow.py: 工作流配置端点
"""

from apps.decision_governance_engine.routers.auth import router as auth_router
from apps.decision_governance_engine.routers.config import (
    router as config_router,
    get_rag_config,
)
from apps.decision_governance_engine.routers.decision import router as decision_router
from apps.decision_governance_engine.routers.knowledge import (
    create_knowledge_router,
    router as knowledge_router,
)
from apps.decision_governance_engine.routers.report import router as report_router
from apps.decision_governance_engine.routers.workflow import router as workflow_router

__all__ = [
    "auth_router",
    "config_router",
    "decision_router",
    "knowledge_router",
    "create_knowledge_router",
    "report_router",
    "workflow_router",
    "get_rag_config",
]

