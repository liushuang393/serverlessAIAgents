"""Type definitions for AgentFlow framework."""

from collections.abc import Awaitable, Callable
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field, field_validator


class ProtocolType(str, Enum):
    """Supported protocol types."""

    MCP = "mcp"
    A2A = "a2a"
    AG_UI = "ag-ui"


class AgentMetadata(BaseModel):
    """Metadata for an agent block.

    This model defines the structure of agent.yaml files that describe
    agent blocks in the AgentFlow ecosystem.
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
        """Validate that at least one protocol is specified.

        Args:
            v: List of protocols.

        Returns:
            Validated list of protocols.

        Raises:
            ValueError: If no protocols are specified.
        """
        if not v:
            msg = "At least one protocol must be specified"
            raise ValueError(msg)
        return v


class WorkflowConfig(BaseModel):
    """Configuration for a workflow."""

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
    """Context for workflow execution."""

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
    """Result of workflow execution."""

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


# Type aliases for hooks
HookCallback = Callable[[ExecutionContext], Awaitable[None]]
NodeHookCallback = Callable[[ExecutionContext, str, dict[str, Any]], Awaitable[None]]

