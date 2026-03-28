"""Runtime-facing app capability bundle contracts."""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from kernel.protocols.mcp.client import MCPClient
    from kernel.skills.gateway import SkillGateway
    from shared.rag.rag_pipeline import RAGPipeline


@dataclass
class CapabilityBundle:
    """Stable runtime bundle exposed to apps and agents."""

    app_name: str
    rag_engine: RAGPipeline | None = None
    skill_gateway: SkillGateway | None = None
    mcp_client: MCPClient | None = None
    llm_contracts: dict[str, Any] | None = None

    def has_rag(self) -> bool:
        """Return whether RAG is available."""
        return self.rag_engine is not None

    def has_skills(self) -> bool:
        """Return whether skills are available."""
        return self.skill_gateway is not None

    def has_mcp(self) -> bool:
        """Return whether MCP is available."""
        return self.mcp_client is not None


__all__ = ["CapabilityBundle"]
