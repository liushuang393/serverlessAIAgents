"""Core Interface Types - 後方互換shim.

実体は contracts.core.types に移設済み。
このファイルは後方互換のため contracts.core.types から全シンボルを re-export する。
"""

from contracts.core.types import (  # noqa: F401
    CodeGenOptions,
    CodeOutputType,
    ConfigField,
    ConfigTemplate,
    DebugEvent,
    DeployConfig,
    DeployEvent,
    DeployResult,
    DeployTarget,
    EdgeDefinition,
    ExecutionEvent,
    FilePreview,
    GeneratedCode,
    NodeDefinition,
    ValidationResult,
    WorkflowDefinition,
)

__all__ = [
    "CodeGenOptions",
    "CodeOutputType",
    "ConfigField",
    "ConfigTemplate",
    "DebugEvent",
    "DeployConfig",
    "DeployEvent",
    "DeployResult",
    "DeployTarget",
    "EdgeDefinition",
    "ExecutionEvent",
    "FilePreview",
    "GeneratedCode",
    "NodeDefinition",
    "ValidationResult",
    "WorkflowDefinition",
]
