"""AgentFlow Core Interfaces - 核心接口定义.

このモジュールは全ての交互モード（Studio/CLI/SDK/API）が使用する
安定した核心インターフェースを定義します。

設計原則:
- 安定性: 一度定義したら変更しない
- 通用性: 特定実装に依存しない
- 拡張性: 新しい実装を容易に追加可能

アーキテクチャ:
    ┌─────────────────────────────────────────────┐
    │          Consumer Layer (調用者)             │
    │  Studio | CLI | SDK | API                   │
    └────────────────────┬────────────────────────┘
                         │
                         ▼
    ┌─────────────────────────────────────────────┐
    │          Core Interface Layer               │
    │  ICodeGenerator | IDeployExecutor |         │
    │  IConfigManager | IWorkflowRunner           │
    └────────────────────┬────────────────────────┘
                         │
                         ▼
    ┌─────────────────────────────────────────────┐
    │          Implementation Layer               │
    │  codegen/ | deploy/ | flow/                 │
    └─────────────────────────────────────────────┘

使用例:
    >>> from agentflow.core.interfaces import (
    ...     ICodeGenerator,
    ...     IDeployExecutor,
    ...     CodeOutputType,
    ...     DeployTarget,
    ... )
"""

# Type definitions
# Interfaces (Protocols)
from agentflow.core.interfaces.code_generator import ICodeGenerator
from agentflow.core.interfaces.config_manager import IConfigManager
from agentflow.core.interfaces.deploy_executor import IDeployExecutor
from agentflow.core.interfaces.types import (
    CodeGenOptions,
    # Enums
    CodeOutputType,
    # Config
    ConfigField,
    ConfigTemplate,
    DebugEvent,
    # Deploy
    DeployConfig,
    DeployEvent,
    DeployResult,
    DeployTarget,
    EdgeDefinition,
    # Execution
    ExecutionEvent,
    FilePreview,
    # Code generation
    GeneratedCode,
    NodeDefinition,
    ValidationResult,
    # Workflow definitions
    WorkflowDefinition,
)
from agentflow.core.interfaces.workflow_runner import IWorkflowRunner


__all__ = [
    "CodeGenOptions",
    # Enums
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
    # Interfaces
    "ICodeGenerator",
    "IConfigManager",
    "IDeployExecutor",
    "IWorkflowRunner",
    "NodeDefinition",
    "ValidationResult",
    # Data types
    "WorkflowDefinition",
]
