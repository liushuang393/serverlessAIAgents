"""後方互換ブリッジ — 実装は kernel/protocols/mcp/client.py に移動済み."""

from kernel.protocols.mcp.client import *  # noqa: F401, F403
from kernel.protocols.mcp.client import _ensure_mcp_imports  # noqa: F401
