"""shared.rag.resource_manager 後方互換shim → shared.knowledge.resource_manager."""

from shared.knowledge.resource_manager import *  # noqa: F401,F403
from shared.knowledge.resource_manager import ResourceDefinition, ResourceManager

__all__ = ["ResourceDefinition", "ResourceManager"]
