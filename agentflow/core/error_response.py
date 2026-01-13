# -*- coding: utf-8 -*-
"""Error Response - 統一エラーレスポンス形式.

このモジュールは、RFC 7807 Problem Details 互換の
統一エラーレスポンス形式を提供します。

接口設計原則:
- RFC 7807 (Problem Details for HTTP APIs) 完全互換
- FastAPI / Flask / Django 等で使用可能
- 将来の Sentry / Datadog 統合を考慮

使用例:
    >>> from agentflow.core.error_response import (
    ...     ErrorResponse, ErrorCode, create_error_response
    ... )
    >>>
    >>> # エラーレスポンス作成
    >>> error = create_error_response(
    ...     code=ErrorCode.AGENT_NOT_FOUND,
    ...     detail="Agent 'MyAgent' was not found",
    ... )
    >>>
    >>> # FastAPI で使用
    >>> @app.exception_handler(AgentFlowError)
    >>> async def handler(request, exc):
    ...     return JSONResponse(
    ...         status_code=exc.status_code,
    ...         content=exc.to_problem_detail(),
    ...     )

参考:
- RFC 7807: Problem Details for HTTP APIs
- FastAPI Error Handling
- Stripe API Error Format
"""

from __future__ import annotations

import traceback
import uuid
from datetime import datetime
from enum import Enum
from typing import Any

from pydantic import BaseModel, Field


# =============================================================================
# エラーコード（ドメイン別に整理）
# =============================================================================


class ErrorCode(str, Enum):
    """統一エラーコード.

    フォーマット: DOMAIN_ERROR_TYPE
    - 各コードは一意で、ドキュメント化される
    - HTTP ステータスコードと対応
    """

    # ==========================================================================
    # 400 Bad Request 系
    # ==========================================================================
    VALIDATION_ERROR = "validation_error"
    INVALID_INPUT = "invalid_input"
    MISSING_PARAMETER = "missing_parameter"
    INVALID_FORMAT = "invalid_format"

    # ==========================================================================
    # 401 Unauthorized 系
    # ==========================================================================
    UNAUTHORIZED = "unauthorized"
    INVALID_TOKEN = "invalid_token"
    TOKEN_EXPIRED = "token_expired"

    # ==========================================================================
    # 403 Forbidden 系
    # ==========================================================================
    FORBIDDEN = "forbidden"
    PERMISSION_DENIED = "permission_denied"
    RATE_LIMITED = "rate_limited"

    # ==========================================================================
    # 404 Not Found 系
    # ==========================================================================
    NOT_FOUND = "not_found"
    AGENT_NOT_FOUND = "agent_not_found"
    WORKFLOW_NOT_FOUND = "workflow_not_found"
    TOOL_NOT_FOUND = "tool_not_found"
    RESOURCE_NOT_FOUND = "resource_not_found"

    # ==========================================================================
    # 408 Timeout 系
    # ==========================================================================
    TIMEOUT = "timeout"
    LLM_TIMEOUT = "llm_timeout"
    TOOL_TIMEOUT = "tool_timeout"
    AGENT_TIMEOUT = "agent_timeout"

    # ==========================================================================
    # 409 Conflict 系
    # ==========================================================================
    CONFLICT = "conflict"
    ALREADY_EXISTS = "already_exists"
    STATE_CONFLICT = "state_conflict"

    # ==========================================================================
    # 500 Internal Error 系
    # ==========================================================================
    INTERNAL_ERROR = "internal_error"
    AGENT_EXECUTION_ERROR = "agent_execution_error"
    TOOL_EXECUTION_ERROR = "tool_execution_error"
    LLM_ERROR = "llm_error"
    WORKFLOW_ERROR = "workflow_error"

    # ==========================================================================
    # 503 Service Unavailable 系
    # ==========================================================================
    SERVICE_UNAVAILABLE = "service_unavailable"
    LLM_UNAVAILABLE = "llm_unavailable"
    EXTERNAL_SERVICE_ERROR = "external_service_error"


# エラーコードとHTTPステータスコードのマッピング
ERROR_CODE_STATUS_MAP: dict[ErrorCode, int] = {
    # 400 系
    ErrorCode.VALIDATION_ERROR: 400,
    ErrorCode.INVALID_INPUT: 400,
    ErrorCode.MISSING_PARAMETER: 400,
    ErrorCode.INVALID_FORMAT: 400,
    # 401 系
    ErrorCode.UNAUTHORIZED: 401,
    ErrorCode.INVALID_TOKEN: 401,
    ErrorCode.TOKEN_EXPIRED: 401,
    # 403 系
    ErrorCode.FORBIDDEN: 403,
    ErrorCode.PERMISSION_DENIED: 403,
    ErrorCode.RATE_LIMITED: 429,
    # 404 系
    ErrorCode.NOT_FOUND: 404,
    ErrorCode.AGENT_NOT_FOUND: 404,
    ErrorCode.WORKFLOW_NOT_FOUND: 404,
    ErrorCode.TOOL_NOT_FOUND: 404,
    ErrorCode.RESOURCE_NOT_FOUND: 404,
    # 408 系
    ErrorCode.TIMEOUT: 408,
    ErrorCode.LLM_TIMEOUT: 408,
    ErrorCode.TOOL_TIMEOUT: 408,
    ErrorCode.AGENT_TIMEOUT: 408,
    # 409 系
    ErrorCode.CONFLICT: 409,
    ErrorCode.ALREADY_EXISTS: 409,
    ErrorCode.STATE_CONFLICT: 409,
    # 500 系
    ErrorCode.INTERNAL_ERROR: 500,
    ErrorCode.AGENT_EXECUTION_ERROR: 500,
    ErrorCode.TOOL_EXECUTION_ERROR: 500,
    ErrorCode.LLM_ERROR: 500,
    ErrorCode.WORKFLOW_ERROR: 500,
    # 503 系
    ErrorCode.SERVICE_UNAVAILABLE: 503,
    ErrorCode.LLM_UNAVAILABLE: 503,
    ErrorCode.EXTERNAL_SERVICE_ERROR: 503,
}

# エラーコードの説明（ドキュメント用）
ERROR_CODE_TITLES: dict[ErrorCode, str] = {
    ErrorCode.VALIDATION_ERROR: "Validation Error",
    ErrorCode.INVALID_INPUT: "Invalid Input",
    ErrorCode.MISSING_PARAMETER: "Missing Required Parameter",
    ErrorCode.INVALID_FORMAT: "Invalid Format",
    ErrorCode.UNAUTHORIZED: "Unauthorized",
    ErrorCode.INVALID_TOKEN: "Invalid Token",
    ErrorCode.TOKEN_EXPIRED: "Token Expired",
    ErrorCode.FORBIDDEN: "Forbidden",
    ErrorCode.PERMISSION_DENIED: "Permission Denied",
    ErrorCode.RATE_LIMITED: "Rate Limited",
    ErrorCode.NOT_FOUND: "Not Found",
    ErrorCode.AGENT_NOT_FOUND: "Agent Not Found",
    ErrorCode.WORKFLOW_NOT_FOUND: "Workflow Not Found",
    ErrorCode.TOOL_NOT_FOUND: "Tool Not Found",
    ErrorCode.RESOURCE_NOT_FOUND: "Resource Not Found",
    ErrorCode.TIMEOUT: "Request Timeout",
    ErrorCode.LLM_TIMEOUT: "LLM Request Timeout",
    ErrorCode.TOOL_TIMEOUT: "Tool Execution Timeout",
    ErrorCode.AGENT_TIMEOUT: "Agent Execution Timeout",
    ErrorCode.CONFLICT: "Conflict",
    ErrorCode.ALREADY_EXISTS: "Resource Already Exists",
    ErrorCode.STATE_CONFLICT: "State Conflict",
    ErrorCode.INTERNAL_ERROR: "Internal Server Error",
    ErrorCode.AGENT_EXECUTION_ERROR: "Agent Execution Error",
    ErrorCode.TOOL_EXECUTION_ERROR: "Tool Execution Error",
    ErrorCode.LLM_ERROR: "LLM Error",
    ErrorCode.WORKFLOW_ERROR: "Workflow Error",
    ErrorCode.SERVICE_UNAVAILABLE: "Service Unavailable",
    ErrorCode.LLM_UNAVAILABLE: "LLM Service Unavailable",
    ErrorCode.EXTERNAL_SERVICE_ERROR: "External Service Error",
}


# =============================================================================
# エラーレスポンスモデル（RFC 7807 互換）
# =============================================================================


class ErrorResponse(BaseModel):
    """統一エラーレスポンス（RFC 7807 Problem Details 互換）.

    RFC 7807 で定義された標準フィールド + AgentFlow 拡張フィールド。

    Attributes:
        type: エラータイプURI（ドキュメントへのリンク）
        title: 人間可読なエラータイトル
        status: HTTPステータスコード
        detail: 詳細なエラー説明
        instance: この特定のエラーインスタンスのURI
        code: AgentFlow内部エラーコード
        trace_id: 分散トレーシング用ID
        timestamp: エラー発生時刻
        retry_after: リトライ推奨秒数（Rate Limit時）
        errors: 複数エラーの詳細（バリデーション時）
        context: 追加コンテキスト情報
    """

    # RFC 7807 標準フィールド
    type: str = Field(
        default="about:blank",
        description="エラータイプURI（ドキュメントリンク）",
    )
    title: str = Field(..., description="人間可読なエラータイトル")
    status: int = Field(..., ge=100, le=599, description="HTTPステータスコード")
    detail: str = Field(..., description="詳細なエラー説明")
    instance: str | None = Field(
        default=None,
        description="このエラーインスタンスのURI",
    )

    # AgentFlow 拡張フィールド
    code: ErrorCode = Field(..., description="内部エラーコード")
    trace_id: str = Field(
        default_factory=lambda: f"trace_{uuid.uuid4().hex[:16]}",
        description="トレーシングID",
    )
    timestamp: datetime = Field(
        default_factory=datetime.utcnow,
        description="エラー発生時刻",
    )
    retry_after: int | None = Field(
        default=None,
        description="リトライ推奨秒数",
    )
    errors: list[dict[str, Any]] = Field(
        default_factory=list,
        description="複数エラー詳細（バリデーション用）",
    )
    context: dict[str, Any] = Field(
        default_factory=dict,
        description="追加コンテキスト",
    )

    def to_dict(self) -> dict[str, Any]:
        """RFC 7807 互換の辞書に変換."""
        result = {
            "type": self.type,
            "title": self.title,
            "status": self.status,
            "detail": self.detail,
            "code": self.code.value,
            "trace_id": self.trace_id,
            "timestamp": self.timestamp.isoformat(),
        }

        # オプションフィールド
        if self.instance:
            result["instance"] = self.instance
        if self.retry_after:
            result["retry_after"] = self.retry_after
        if self.errors:
            result["errors"] = self.errors
        if self.context:
            result["context"] = self.context

        return result

    def to_json_response(self) -> Any:
        """FastAPI JSONResponse 用に変換."""
        try:
            from fastapi.responses import JSONResponse
            return JSONResponse(
                status_code=self.status,
                content=self.to_dict(),
                headers={"Content-Type": "application/problem+json"},
            )
        except ImportError:
            return self.to_dict()


# =============================================================================
# AgentFlow 例外クラス（統一例外）
# =============================================================================


class AgentFlowAPIError(Exception):
    """AgentFlow API エラー基底クラス.

    全ての AgentFlow API エラーはこのクラスを継承。
    ErrorResponse への自動変換をサポート。
    """

    def __init__(
        self,
        code: ErrorCode,
        detail: str,
        *,
        title: str | None = None,
        instance: str | None = None,
        retry_after: int | None = None,
        errors: list[dict[str, Any]] | None = None,
        context: dict[str, Any] | None = None,
        cause: Exception | None = None,
    ) -> None:
        """初期化.

        Args:
            code: エラーコード
            detail: 詳細説明
            title: タイトル（省略時はコードから自動生成）
            instance: インスタンスURI
            retry_after: リトライ推奨秒数
            errors: 複数エラー詳細
            context: 追加コンテキスト
            cause: 元の例外
        """
        self.code = code
        self.detail = detail
        self.title = title or ERROR_CODE_TITLES.get(code, "Error")
        self.status_code = ERROR_CODE_STATUS_MAP.get(code, 500)
        self.instance = instance
        self.retry_after = retry_after
        self.errors = errors or []
        self.context = context or {}
        self.cause = cause
        self.trace_id = f"trace_{uuid.uuid4().hex[:16]}"

        super().__init__(detail)

    def to_response(self) -> ErrorResponse:
        """ErrorResponse に変換."""
        return ErrorResponse(
            type=f"https://agentflow.dev/errors/{self.code.value}",
            title=self.title,
            status=self.status_code,
            detail=self.detail,
            instance=self.instance,
            code=self.code,
            trace_id=self.trace_id,
            retry_after=self.retry_after,
            errors=self.errors,
            context=self.context,
        )

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return self.to_response().to_dict()


# =============================================================================
# 具体的なエラークラス
# =============================================================================


class ValidationError(AgentFlowAPIError):
    """バリデーションエラー."""

    def __init__(
        self,
        detail: str,
        errors: list[dict[str, Any]] | None = None,
        **kwargs: Any,
    ) -> None:
        super().__init__(
            code=ErrorCode.VALIDATION_ERROR,
            detail=detail,
            errors=errors,
            **kwargs,
        )


class NotFoundError(AgentFlowAPIError):
    """リソース未発見エラー."""

    def __init__(
        self,
        resource_type: str,
        resource_id: str,
        **kwargs: Any,
    ) -> None:
        code_map = {
            "agent": ErrorCode.AGENT_NOT_FOUND,
            "workflow": ErrorCode.WORKFLOW_NOT_FOUND,
            "tool": ErrorCode.TOOL_NOT_FOUND,
        }
        code = code_map.get(resource_type.lower(), ErrorCode.NOT_FOUND)

        super().__init__(
            code=code,
            detail=f"{resource_type} '{resource_id}' was not found",
            context={"resource_type": resource_type, "resource_id": resource_id},
            **kwargs,
        )


class TimeoutError(AgentFlowAPIError):
    """タイムアウトエラー."""

    def __init__(
        self,
        operation: str,
        timeout_seconds: float,
        **kwargs: Any,
    ) -> None:
        code_map = {
            "llm": ErrorCode.LLM_TIMEOUT,
            "tool": ErrorCode.TOOL_TIMEOUT,
            "agent": ErrorCode.AGENT_TIMEOUT,
        }
        code = code_map.get(operation.lower(), ErrorCode.TIMEOUT)

        super().__init__(
            code=code,
            detail=f"{operation} timed out after {timeout_seconds} seconds",
            context={"operation": operation, "timeout_seconds": timeout_seconds},
            **kwargs,
        )


class RateLimitError(AgentFlowAPIError):
    """レート制限エラー."""

    def __init__(
        self,
        retry_after: int = 60,
        **kwargs: Any,
    ) -> None:
        super().__init__(
            code=ErrorCode.RATE_LIMITED,
            detail="Rate limit exceeded. Please try again later.",
            retry_after=retry_after,
            **kwargs,
        )


class ExecutionError(AgentFlowAPIError):
    """実行エラー."""

    def __init__(
        self,
        component: str,
        detail: str,
        cause: Exception | None = None,
        **kwargs: Any,
    ) -> None:
        code_map = {
            "agent": ErrorCode.AGENT_EXECUTION_ERROR,
            "tool": ErrorCode.TOOL_EXECUTION_ERROR,
            "llm": ErrorCode.LLM_ERROR,
            "workflow": ErrorCode.WORKFLOW_ERROR,
        }
        code = code_map.get(component.lower(), ErrorCode.INTERNAL_ERROR)

        context = kwargs.pop("context", {})
        context["component"] = component
        if cause:
            context["cause_type"] = type(cause).__name__
            context["cause_message"] = str(cause)

        super().__init__(
            code=code,
            detail=detail,
            cause=cause,
            context=context,
            **kwargs,
        )


# =============================================================================
# ヘルパー関数
# =============================================================================


def create_error_response(
    code: ErrorCode,
    detail: str,
    **kwargs: Any,
) -> ErrorResponse:
    """エラーレスポンスを作成.

    Args:
        code: エラーコード
        detail: 詳細説明
        **kwargs: 追加フィールド

    Returns:
        ErrorResponse インスタンス
    """
    return ErrorResponse(
        type=f"https://agentflow.dev/errors/{code.value}",
        title=ERROR_CODE_TITLES.get(code, "Error"),
        status=ERROR_CODE_STATUS_MAP.get(code, 500),
        detail=detail,
        code=code,
        **kwargs,
    )


def exception_to_response(
    exc: Exception,
    trace_id: str | None = None,
    include_traceback: bool = False,
) -> ErrorResponse:
    """例外を ErrorResponse に変換.

    Args:
        exc: 例外
        trace_id: トレーシングID
        include_traceback: トレースバックを含めるか

    Returns:
        ErrorResponse インスタンス
    """
    if isinstance(exc, AgentFlowAPIError):
        return exc.to_response()

    # 汎用例外の変換
    context: dict[str, Any] = {
        "exception_type": type(exc).__name__,
    }
    if include_traceback:
        context["traceback"] = traceback.format_exc()

    return ErrorResponse(
        type="https://agentflow.dev/errors/internal_error",
        title="Internal Server Error",
        status=500,
        detail=str(exc) or "An unexpected error occurred",
        code=ErrorCode.INTERNAL_ERROR,
        trace_id=trace_id or f"trace_{uuid.uuid4().hex[:16]}",
        context=context,
    )


# =============================================================================
# FastAPI 統合
# =============================================================================


def create_exception_handlers() -> dict[type, Any]:
    """FastAPI 用の例外ハンドラーを作成.

    Returns:
        例外ハンドラーの辞書

    使用例:
        >>> from fastapi import FastAPI
        >>> from agentflow.core.error_response import create_exception_handlers
        >>>
        >>> app = FastAPI()
        >>> for exc_type, handler in create_exception_handlers().items():
        ...     app.add_exception_handler(exc_type, handler)
    """
    try:
        from fastapi import Request
        from fastapi.responses import JSONResponse
    except ImportError:
        return {}

    async def agentflow_error_handler(
        request: Request,
        exc: AgentFlowAPIError,
    ) -> JSONResponse:
        """AgentFlowAPIError ハンドラー."""
        response = exc.to_response()
        return JSONResponse(
            status_code=response.status,
            content=response.to_dict(),
            headers={"Content-Type": "application/problem+json"},
        )

    async def generic_error_handler(
        request: Request,
        exc: Exception,
    ) -> JSONResponse:
        """汎用例外ハンドラー."""
        trace_id = getattr(request.state, "trace_id", None)
        response = exception_to_response(exc, trace_id=trace_id)
        return JSONResponse(
            status_code=response.status,
            content=response.to_dict(),
            headers={"Content-Type": "application/problem+json"},
        )

    return {
        AgentFlowAPIError: agentflow_error_handler,
        Exception: generic_error_handler,
    }


# =============================================================================
# エクスポート
# =============================================================================

__all__ = [
    # エラーコード
    "ErrorCode",
    "ERROR_CODE_STATUS_MAP",
    "ERROR_CODE_TITLES",
    # レスポンスモデル
    "ErrorResponse",
    # 例外クラス
    "AgentFlowAPIError",
    "ValidationError",
    "NotFoundError",
    "TimeoutError",
    "RateLimitError",
    "ExecutionError",
    # ヘルパー
    "create_error_response",
    "exception_to_response",
    "create_exception_handlers",
]
