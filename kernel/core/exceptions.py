"""AgentFlow 例外定義 — kernel 層.

フレームワーク全体で使用される例外クラス群。
legacy core surface/exceptions.py から移行。
"""


class AgentFlowError(Exception):
    """AgentFlow 基底例外."""


class WorkflowError(AgentFlowError):
    """ワークフロー関連エラーの基底."""


class WorkflowNotFoundError(WorkflowError):
    """指定されたワークフローが見つからない場合.

    Attributes:
        workflow_id: 見つからなかったワークフローID
    """

    def __init__(self, workflow_id: str) -> None:
        """初期化.

        Args:
            workflow_id: ワークフローID
        """
        super().__init__(f"Workflow not found: {workflow_id}")
        self.workflow_id = workflow_id


class WorkflowExecutionError(WorkflowError):
    """ワークフロー実行失敗."""


class ProtocolError(AgentFlowError):
    """プロトコル関連エラーの基底."""


class ProtocolNotSupportedError(ProtocolError):
    """サポートされていないプロトコル.

    Attributes:
        protocol: 未サポートプロトコル名
    """

    def __init__(self, protocol: str) -> None:
        """初期化.

        Args:
            protocol: プロトコル名
        """
        super().__init__(f"Protocol not supported: {protocol}")
        self.protocol = protocol


class ProtocolAdapterError(ProtocolError):
    """プロトコルアダプタエラー."""


class AgentBlockError(AgentFlowError):
    """Agent ブロック関連エラーの基底."""


class AgentBlockNotFoundError(AgentBlockError):
    """Agent ブロックが見つからない場合.

    Attributes:
        block_id: 見つからなかったブロックID
    """

    def __init__(self, block_id: str) -> None:
        """初期化.

        Args:
            block_id: ブロックID
        """
        super().__init__(f"Agent block not found: {block_id}")
        self.block_id = block_id


class AgentBlockValidationError(AgentBlockError):
    """Agent ブロック検証エラー."""


class ConfigurationError(AgentFlowError):
    """設定エラー."""


class AgentExecutionError(AgentFlowError):
    """Agent 実行エラー.

    Attributes:
        agent_name: エラーが発生した Agent 名
        original_error: 元の例外
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

    Attributes:
        timeout_seconds: タイムアウト秒数
        attempts: 試行回数
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

    Attributes:
        max_retries: 最大リトライ回数
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


class AgentOutputValidationError(AgentExecutionError):
    """Agent 出力検証エラー.

    LLM が返したデータが業務要件を満たさない場合に発生。

    Attributes:
        field_name: 検証失敗フィールド名
        expected: 期待値の説明
        actual: 実際の値
    """

    def __init__(
        self,
        agent_name: str,
        field_name: str,
        expected: str,
        actual: str,
    ) -> None:
        """初期化.

        Args:
            agent_name: Agent 名
            field_name: フィールド名
            expected: 期待値
            actual: 実際の値
        """
        self.field_name = field_name
        self.expected = expected
        self.actual = actual
        message = f"出力検証エラー: {field_name} - 期待: {expected}, 実際: {actual}"
        super().__init__(agent_name, message)
