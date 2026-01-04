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


# ============================================================
# Agent 実行エラー（v0.3.0 追加）
# ============================================================


class AgentExecutionError(AgentFlowError):
    """Agent 実行エラー.

    Agent の実行中に発生した一般的なエラーを表します。

    Attributes:
        agent_name: エラーが発生した Agent の名前
        original_error: 元の例外（存在する場合）
    """

    def __init__(
        self,
        agent_name: str,
        message: str,
        original_error: Exception | None = None,
    ) -> None:
        """初期化.

        Args:
            agent_name: Agent 名
            message: エラーメッセージ
            original_error: 元の例外
        """
        self.agent_name = agent_name
        self.original_error = original_error
        super().__init__(f"[{agent_name}] {message}")


class AgentTimeoutError(AgentExecutionError):
    """Agent タイムアウトエラー.

    Agent の実行がタイムアウトした場合に発生します。
    """

    def __init__(
        self,
        agent_name: str,
        timeout_seconds: int,
        attempts: int = 1,
        original_error: Exception | None = None,
    ) -> None:
        """初期化.

        Args:
            agent_name: Agent 名
            timeout_seconds: タイムアウト秒数
            attempts: 試行回数
            original_error: 元の例外
        """
        self.timeout_seconds = timeout_seconds
        self.attempts = attempts
        message = f"タイムアウト ({timeout_seconds}秒) - {attempts}回試行後"
        super().__init__(agent_name, message, original_error)


class AgentRetryExhaustedError(AgentExecutionError):
    """Agent リトライ上限エラー.

    Agent のリトライが上限に達した場合に発生します。
    """

    def __init__(
        self,
        agent_name: str,
        max_retries: int,
        last_error: Exception | None = None,
    ) -> None:
        """初期化.

        Args:
            agent_name: Agent 名
            max_retries: 最大リトライ回数
            last_error: 最後に発生した例外
        """
        self.max_retries = max_retries
        message = f"リトライ上限到達 ({max_retries}回) - 最終エラー: {last_error}"
        super().__init__(agent_name, message, last_error)
