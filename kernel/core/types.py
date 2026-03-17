"""AgentFlow 型定義 — kernel 層.

ワークフロー実行に必要な型定義（ExecutionContext, ExecutionResult, WorkflowConfig 等）。
agentflow/core/types.py から移行。
"""

from collections.abc import Awaitable, Callable
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field, field_validator


class ProtocolType(str, Enum):
    """サポートされるプロトコルタイプ."""

    MCP = "mcp"
    A2A = "a2a"
    AG_UI = "ag-ui"


class AgentMetadata(BaseModel):
    """Agent ブロックのメタデータ.

    agent.yaml ファイル構造を定義する Pydantic モデル。

    Attributes:
        name: エージェント名
        version: セマンティックバージョン
        description: 説明文
        author: 作成者
        protocols: サポートプロトコル
        entry_point: Python モジュールパス
        dependencies: Python パッケージ依存
        tags: カテゴリタグ
        config_schema: JSON スキーマ
    """

    name: str = Field(..., min_length=1, max_length=100, description="Agent name")
    version: str = Field(
        ...,
        pattern=r"^\d+\.\d+\.\d+$",
        description="Semantic version (e.g., 1.0.0)",
    )
    description: str = Field(
        ...,
        min_length=1,
        max_length=500,
        description="Brief description of the agent",
    )
    author: str = Field(..., min_length=1, description="Agent author")
    protocols: list[ProtocolType] = Field(
        default_factory=list,
        description="Supported protocols",
    )
    entry_point: str = Field(
        ...,
        description="Python module path to agent entry point",
    )
    dependencies: list[str] = Field(
        default_factory=list,
        description="Python package dependencies",
    )
    tags: list[str] = Field(
        default_factory=list,
        description="Tags for categorization",
    )
    config_schema: dict[str, Any] = Field(
        default_factory=dict,
        description="JSON schema for agent configuration",
    )

    @field_validator("protocols")
    @classmethod
    def validate_protocols(cls, v: list[ProtocolType]) -> list[ProtocolType]:
        """少なくとも1つのプロトコルが指定されていることを検証.

        Args:
            v: プロトコルリスト

        Returns:
            検証済みプロトコルリスト

        Raises:
            ValueError: プロトコルが空の場合
        """
        if not v:
            msg = "At least one protocol must be specified"
            raise ValueError(msg)
        return v


class WorkflowConfig(BaseModel):
    """ワークフロー設定."""

    workflow_id: str = Field(..., min_length=1, description="Unique workflow ID")
    name: str = Field(..., min_length=1, description="Workflow name")
    description: str = Field(default="", description="Workflow description")
    nodes: list[dict[str, Any]] = Field(
        default_factory=list,
        description="Workflow nodes",
    )
    edges: list[dict[str, Any]] = Field(
        default_factory=list,
        description="Workflow edges",
    )
    config: dict[str, Any] = Field(
        default_factory=dict,
        description="Additional configuration",
    )


class ExecutionContext(BaseModel):
    """ワークフロー実行コンテキスト."""

    workflow_id: str = Field(..., description="Workflow ID being executed")
    execution_id: str = Field(..., description="Unique execution ID")
    inputs: dict[str, Any] = Field(
        default_factory=dict,
        description="Input parameters",
    )
    metadata: dict[str, Any] = Field(
        default_factory=dict,
        description="Execution metadata",
    )
    started_at: datetime = Field(
        default_factory=datetime.now,
        description="Execution start time",
    )

    model_config = {"arbitrary_types_allowed": True}


class ExecutionResult(BaseModel):
    """ワークフロー実行結果."""

    status: str = Field(..., description="Execution status (success/error/timeout)")
    output: dict[str, Any] = Field(
        default_factory=dict,
        description="Execution output",
    )
    error: str | None = Field(
        default=None,
        description="Error message if execution failed",
    )
    duration: float = Field(..., description="Execution duration in seconds")
    context: ExecutionContext = Field(..., description="Execution context")


# フック用型エイリアス
HookCallback = Callable[[ExecutionContext], Awaitable[None]]
NodeHookCallback = Callable[[ExecutionContext, str, dict[str, Any]], Awaitable[None]]

