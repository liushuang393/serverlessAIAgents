# -*- coding: utf-8 -*-
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
from agentflow.core.interfaces.types import (
    # Enums
    CodeOutputType,
    DeployTarget,
    # Workflow definitions
    WorkflowDefinition,
    NodeDefinition,
    EdgeDefinition,
    # Code generation
    GeneratedCode,
    FilePreview,
    CodeGenOptions,
    # Deploy
    DeployConfig,
    DeployEvent,
    DeployResult,
    # Config
    ConfigField,
    ConfigTemplate,
    ValidationResult,
    # Execution
    ExecutionEvent,
    DebugEvent,
)

# Interfaces (Protocols)
from agentflow.core.interfaces.code_generator import ICodeGenerator
from agentflow.core.interfaces.deploy_executor import IDeployExecutor
from agentflow.core.interfaces.config_manager import IConfigManager
from agentflow.core.interfaces.workflow_runner import IWorkflowRunner


__all__ = [
    # Enums
    "CodeOutputType",
    "DeployTarget",
    # Data types
    "WorkflowDefinition",
    "NodeDefinition",
    "EdgeDefinition",
    "GeneratedCode",
    "FilePreview",
    "CodeGenOptions",
    "DeployConfig",
    "DeployEvent",
    "DeployResult",
    "ConfigField",
    "ConfigTemplate",
    "ValidationResult",
    "ExecutionEvent",
    "DebugEvent",
    # Interfaces
    "ICodeGenerator",
    "IDeployExecutor",
    "IConfigManager",
    "IWorkflowRunner",
]
