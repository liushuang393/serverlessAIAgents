"""shared.rag.rag_access_control 後方互換shim → shared.access.rag_access_control."""

from shared.access.rag_access_control import *  # noqa: F401,F403
from shared.access.rag_access_control import RAGAccessControl

__all__ = ["RAGAccessControl"]
