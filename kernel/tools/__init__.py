"""Layer 3 の tool runtime 公開 API.

kernel/tools パッケージは以下のツール抽象を提供する:
- ToolDefinition / ToolSource: 統一ツール定義モデル
- ToolRegistry: ツールレジストリ（登録・検索）
- ToolDiscoveryService: ツール自動発見
- ToolBinder / BoundTools / ToolExecutor: Agent へのツールバインド
- ToolCatalogManager: 統一ツールカタログ
- KernelToolExecutor: カーネル層ツール実行
"""

from kernel.tools.executor import KernelToolExecutor
from kernel.tools.tool_binding import BoundTools, ToolBinder, ToolExecutor
from kernel.tools.tool_catalog import (
    CatalogEntry,
    CatalogSource,
    ToolCatalogManager,
    get_tool_catalog,
    reset_tool_catalog,
)
from kernel.tools.tool_definition import ToolDefinition, ToolSource
from kernel.tools.tool_discovery import ToolDiscoveryService
from kernel.tools.tool_registry import (
    ToolRegistry,
    get_global_tool_registry,
    reset_global_tool_registry,
)


__all__ = [
    "BoundTools",
    "CatalogEntry",
    "CatalogSource",
    "KernelToolExecutor",
    "ToolBinder",
    "ToolCatalogManager",
    "ToolDefinition",
    "ToolDiscoveryService",
    "ToolExecutor",
    "ToolRegistry",
    "ToolSource",
    "get_global_tool_registry",
    "get_tool_catalog",
    "reset_global_tool_registry",
    "reset_tool_catalog",
]
