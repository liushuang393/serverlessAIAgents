"""interfaces パッケージ — コアインターフェース定義.

型定義とプロトコルは contracts.core (L0) から re-export する。
"""

from contracts.core import (
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
    IWorkflowRunner,
    NodeDefinition,
    ValidationResult,
    WorkflowDefinition,
)
from kernel.core.interfaces.code_generator import ICodeGenerator
from kernel.core.interfaces.config_manager import IConfigManager
from kernel.core.interfaces.deploy_executor import IDeployExecutor

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
    "ICodeGenerator",
    "IConfigManager",
    "IDeployExecutor",
    "IWorkflowRunner",
    "NodeDefinition",
    "ValidationResult",
    "WorkflowDefinition",
]
