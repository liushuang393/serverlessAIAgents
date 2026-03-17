"""CapabilityBundle - アプリ能力の集約データクラス.

AppCapabilityBootstrapper が構築し、Agent/Engineに渡す能力の束。
rag_engine / skill_gateway / mcp_client を保持する。

使用例:
    >>> bundle = CapabilityBundle(app_name="faq_system")
    >>> bundle.rag_engine  # None（設定なし / enabled=false）
    >>> # または RAGPipeline インスタンス
"""

from __future__ import annotations

from dataclasses import dataclass
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from shared.rag.rag_pipeline import RAGPipeline
    from kernel.protocols.mcp_client import MCPClient
    from kernel.skills.gateway import SkillGateway


@dataclass
class CapabilityBundle:
    """アプリが利用できる能力の集約.

    Attributes:
        app_name: アプリ識別子
        rag_engine: RAGPipeline インスタンス（enabled=false → None）
        skill_gateway: SkillGateway インスタンス（未設定 → None）
        mcp_client: MCPClient インスタンス（未設定 → None）
        llm_contracts: contracts.llm の生設定
    """

    app_name: str
    rag_engine: RAGPipeline | None = None
    skill_gateway: SkillGateway | None = None
    mcp_client: MCPClient | None = None
    llm_contracts: dict[str, Any] | None = None

    def has_rag(self) -> bool:
        """RAG が有効かどうか."""
        return self.rag_engine is not None

    def has_skills(self) -> bool:
        """SkillGateway が有効かどうか."""
        return self.skill_gateway is not None

    def has_mcp(self) -> bool:
        """MCP が有効かどうか."""
        return self.mcp_client is not None


__all__ = ["CapabilityBundle"]
