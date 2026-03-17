"""contracts.core — コアデータ型とプロトコルの定義.

全レイヤーから参照可能な L0 契約層の核心部分。
kernel/core/interfaces から移設された安定型定義を提供する。
"""

from contracts.core.types import (
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
from contracts.core.workflow_runner import IWorkflowRunner

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
    "IWorkflowRunner",
    "NodeDefinition",
    "ValidationResult",
    "WorkflowDefinition",
]

