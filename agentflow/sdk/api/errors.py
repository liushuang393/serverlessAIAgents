"""統一エラーハンドリング.

AgentFlow アプリケーション用の標準エラー処理を提供。
全てのアプリケーションで一貫したエラーレスポンス形式を使用。

Example:
    >>> from agentflow.sdk.api.errors import AgentApiException, ErrorCode
    >>>
    >>> raise AgentApiException(
    ...     code=ErrorCode.VALIDATION_ERROR,
    ...     message="入力が不正です",
    ...     details={"field": "question", "error": "必須項目です"}
    ... )
"""

from enum import Enum
from typing import Any

from fastapi import Request
from fastapi.responses import JSONResponse
from pydantic import BaseModel


class ErrorCode(str, Enum):
    """エラーコード.

    全アプリケーション共通のエラーコード。
    """

    VALIDATION_ERROR = "VALIDATION_ERROR"
    AUTHENTICATION_ERROR = "AUTHENTICATION_ERROR"
    AUTHORIZATION_ERROR = "AUTHORIZATION_ERROR"
    NOT_FOUND = "NOT_FOUND"
    RATE_LIMITED = "RATE_LIMITED"
    SERVER_ERROR = "SERVER_ERROR"
    TIMEOUT = "TIMEOUT"
    AGENT_ERROR = "AGENT_ERROR"
    UNKNOWN = "UNKNOWN"


class ErrorResponse(BaseModel):
    """エラーレスポンス.

    統一されたエラーレスポンス形式。
    """

    error: ErrorCode
    message: str
    details: dict[str, Any] | None = None
    retryable: bool = False


class AgentApiException(Exception):
    """AgentFlow API 例外.

    全ての API エラーをこの例外でラップ。

    Args:
        code: エラーコード
        message: ユーザー向けエラーメッセージ
        details: 追加詳細情報
        retryable: リトライ可能か
        status_code: HTTP ステータスコード
    """

    def __init__(
        self,
        code: ErrorCode,
        message: str,
        details: dict[str, Any] | None = None,
        retryable: bool = False,
        status_code: int = 500,
    ) -> None:
        """初期化."""
        super().__init__(message)
        self.code = code
        self.message = message
        self.details = details
        self.retryable = retryable
        self.status_code = status_code

    def to_response(self) -> ErrorResponse:
        """ErrorResponse に変換."""
        return ErrorResponse(
            error=self.code,
            message=self.message,
            details=self.details,
            retryable=self.retryable,
        )

    @classmethod
    def validation_error(
        cls, message: str = "入力が不正です", details: dict[str, Any] | None = None
    ) -> "AgentApiException":
        """バリデーションエラーを生成."""
        return cls(
            code=ErrorCode.VALIDATION_ERROR,
            message=message,
            details=details,
            retryable=False,
            status_code=400,
        )

    @classmethod
    def not_found(cls, resource: str = "リソース", resource_id: str | None = None) -> "AgentApiException":
        """NotFound エラーを生成."""
        return cls(
            code=ErrorCode.NOT_FOUND,
            message=f"{resource}が見つかりません",
            details={"resource_id": resource_id} if resource_id else None,
            retryable=False,
            status_code=404,
        )

    @classmethod
    def agent_error(
        cls,
        agent_id: str,
        message: str = "Agent 処理中にエラーが発生しました",
        details: dict[str, Any] | None = None,
    ) -> "AgentApiException":
        """Agent エラーを生成."""
        return cls(
            code=ErrorCode.AGENT_ERROR,
            message=message,
            details={"agent_id": agent_id, **(details or {})},
            retryable=True,
            status_code=500,
        )

    @classmethod
    def server_error(cls, message: str = "サーバーエラーが発生しました") -> "AgentApiException":
        """サーバーエラーを生成."""
        return cls(
            code=ErrorCode.SERVER_ERROR,
            message=message,
            details=None,
            retryable=True,
            status_code=500,
        )


async def error_handler(request: Request, exc: AgentApiException) -> JSONResponse:
    """AgentApiException ハンドラー.

    FastAPI の例外ハンドラーとして登録。

    Example:
        >>> from fastapi import FastAPI
        >>> from agentflow.sdk.api.errors import AgentApiException, error_handler
        >>>
        >>> app = FastAPI()
        >>> app.add_exception_handler(AgentApiException, error_handler)
    """
    return JSONResponse(
        status_code=exc.status_code,
        content=exc.to_response().model_dump(),
    )
