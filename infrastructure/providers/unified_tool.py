"""後方互換shim: infrastructure.providers.unified_tool -> kernel.tools.unified_tool.

統一ツールプロバイダーの正規実装は kernel/tools/unified_tool.py に移動済み。
infrastructure (L1) から kernel (L3) への直接依存を解消するため、
このファイルは kernel.tools.unified_tool からの re-export のみを行う。
"""

from kernel.tools.unified_tool import (
    BuiltinToolProvider,
    MCPToolProvider,
    SkillToolProvider,
    ToolDefinition,
    ToolProvider,
    ToolResult,
    ToolStatus,
    ToolType,
    UnifiedToolDefinition,
    UnifiedToolProvider,
)


# エクスポート
__all__ = [
    "BuiltinToolProvider",
    "MCPToolProvider",
    "SkillToolProvider",
    "ToolDefinition",
    "ToolProvider",
    "ToolResult",
    "ToolStatus",
    "ToolType",
    "UnifiedToolDefinition",
    "UnifiedToolProvider",
]
