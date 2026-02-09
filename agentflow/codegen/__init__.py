"""AgentFlow Code Generator - コード生成モジュール.

Workflow からデプロイ可能なコードを生成します。

機能:
- Frontend コード生成（React）
- Backend コード生成（FastAPI）
- Fullstack コード生成（React + FastAPI）
- テンプレートベースの柔軟な生成

使用例:
    >>> from agentflow.codegen import CodeGenerator, CodeOutputType
    >>>
    >>> generator = CodeGenerator()
    >>> code = await generator.generate(
    ...     workflow=my_workflow,
    ...     output_type=CodeOutputType.FULLSTACK,
    ... )
    >>> print(code.files.keys())
"""

from agentflow.codegen.generator import CodeGenerator
from agentflow.core.interfaces import (
    CodeGenOptions,
    CodeOutputType,
    FilePreview,
    GeneratedCode,
)


__all__ = [
    "CodeGenOptions",
    "CodeGenerator",
    "CodeOutputType",
    "FilePreview",
    "GeneratedCode",
]
