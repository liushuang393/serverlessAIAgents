"""Custom exceptions for AgentFlow framework."""


class AgentFlowError(Exception):
    """Base exception for all AgentFlow errors."""


class WorkflowError(AgentFlowError):
    """Base exception for workflow-related errors."""


class WorkflowNotFoundError(WorkflowError):
    """Raised when a workflow is not found."""

    def __init__(self, workflow_id: str) -> None:
        """Initialize WorkflowNotFoundError.

        Args:
            workflow_id: The ID of the workflow that was not found.
        """
        super().__init__(f"Workflow not found: {workflow_id}")
        self.workflow_id = workflow_id


class WorkflowExecutionError(WorkflowError):
    """Raised when workflow execution fails."""


class ProtocolError(AgentFlowError):
    """Base exception for protocol-related errors."""


class ProtocolNotSupportedError(ProtocolError):
    """Raised when a protocol is not supported."""

    def __init__(self, protocol: str) -> None:
        """Initialize ProtocolNotSupportedError.

        Args:
            protocol: The name of the unsupported protocol.
        """
        super().__init__(f"Protocol not supported: {protocol}")
        self.protocol = protocol


class ProtocolAdapterError(ProtocolError):
    """Raised when a protocol adapter encounters an error."""


class AgentBlockError(AgentFlowError):
    """Base exception for agent block-related errors."""


class AgentBlockNotFoundError(AgentBlockError):
    """Raised when an agent block is not found."""

    def __init__(self, block_id: str) -> None:
        """Initialize AgentBlockNotFoundError.

        Args:
            block_id: The ID of the agent block that was not found.
        """
        super().__init__(f"Agent block not found: {block_id}")
        self.block_id = block_id


class AgentBlockValidationError(AgentBlockError):
    """Raised when agent block validation fails."""


class ConfigurationError(AgentFlowError):
    """Raised when configuration is invalid."""
