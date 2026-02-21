"""AgentFlow Studio API ルート.

各機能領域ごとに分割されたAPIルーターを提供。
"""

from agentflow.studio.routes.agents import create_agents_router
from agentflow.studio.routes.knowledge import create_knowledge_router
from agentflow.studio.routes.marketplace import create_marketplace_router
from agentflow.studio.routes.preview import create_preview_router
from agentflow.studio.routes.publish import create_publish_router
from agentflow.studio.routes.service_nodes import create_service_nodes_router
from agentflow.studio.routes.workflows import create_workflows_router


__all__ = [
    "create_agents_router",
    "create_knowledge_router",
    "create_marketplace_router",
    "create_preview_router",
    "create_publish_router",
    "create_service_nodes_router",
    "create_workflows_router",
]
