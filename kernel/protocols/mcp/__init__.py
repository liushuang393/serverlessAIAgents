"""MCP サブパッケージ — MCP クライアント・ツール・設定の統一エントリポイント."""

from kernel.protocols.mcp.client import MCPClient
from kernel.protocols.mcp.config import LazyLoadingConfig, MCPConfig, MCPServerConfig
from kernel.protocols.mcp.lazy_client import LazyMCPClient
from kernel.protocols.mcp.tool import MCPTool, MCPToolClient

__all__ = [
    "MCPClient",
    "MCPConfig",
    "LazyMCPClient",
    "MCPServerConfig",
    "LazyLoadingConfig",
    "MCPTool",
    "MCPToolClient",
]
