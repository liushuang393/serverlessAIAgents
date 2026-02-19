"""統一 API レスポンス - 全アプリ共通のレスポンス形式.

設計原則:
- 一貫性: 成功/失敗を問わず同じ構造
- 型安全: Pydantic による厳密な型定義
- 拡張性: メタデータフィールドで拡張可能

使用例:
    >>> from agentflow.api import APIResponse, ErrorCode
    >>>
    >>> # 成功レスポンス
    >>> response = APIResponse.success(data={"result": "ok"})
    >>>
    >>> # エラーレスポンス
    >>> response = APIResponse.error(
    ...     code=ErrorCode.VALIDATION_ERROR,
    ...     message="Invalid input",
    ... )
    >>>
    >>> # ページネーション
    >>> response = PagedResponse(
    ...     items=[...],
    ...     total=100,
    ...     page=1,
    ...     page_size=10,
    ... )
"""

from __future__ import annotations

from datetime import datetime
from enum import Enum
from typing import Any, TypeVar

from pydantic import BaseModel, Field


T = TypeVar("T")


class ErrorCode(str, Enum):
    """エラーコード定義."""

    # 一般
    UNKNOWN = "UNKNOWN"
    INTERNAL_ERROR = "INTERNAL_ERROR"

    # 入力検証
    VALIDATION_ERROR = "VALIDATION_ERROR"
    MISSING_PARAMETER = "MISSING_PARAMETER"
    INVALID_FORMAT = "INVALID_FORMAT"

    # 認証・認可
    UNAUTHORIZED = "UNAUTHORIZED"
    FORBIDDEN = "FORBIDDEN"
    TOKEN_EXPIRED = "TOKEN_EXPIRED"

    # リソース
    NOT_FOUND = "NOT_FOUND"
    ALREADY_EXISTS = "ALREADY_EXISTS"
    CONFLICT = "CONFLICT"

    # レート制限
    RATE_LIMITED = "RATE_LIMITED"
    QUOTA_EXCEEDED = "QUOTA_EXCEEDED"

    # 外部サービス
    EXTERNAL_SERVICE_ERROR = "EXTERNAL_SERVICE_ERROR"
    LLM_ERROR = "LLM_ERROR"
    DATABASE_ERROR = "DATABASE_ERROR"

    # Agent 関連
    AGENT_ERROR = "AGENT_ERROR"
    AGENT_TIMEOUT = "AGENT_TIMEOUT"
    WORKFLOW_ERROR = "WORKFLOW_ERROR"


class APIError(BaseModel):
    """APIエラー詳細."""

    code: ErrorCode = Field(..., description="エラーコード")
    message: str = Field(..., description="エラーメッセージ")
    details: dict[str, Any] | None = Field(None, description="詳細情報")
    field: str | None = Field(None, description="エラーが発生したフィールド")


class APIResponse[T](BaseModel):
    """統一 API レスポンス.

    Attributes:
        success: 成功かどうか
        data: レスポンスデータ
        error: エラー情報
        meta: メタデータ
        timestamp: タイムスタンプ
    """

    success: bool = Field(..., description="成功かどうか")
    data: T | None = Field(None, description="レスポンスデータ")
    error: APIError | None = Field(None, description="エラー情報")
    meta: dict[str, Any] = Field(default_factory=dict, description="メタデータ")
    timestamp: str = Field(
        default_factory=lambda: datetime.now().isoformat(),
        description="タイムスタンプ",
    )

    @classmethod
    def success_response(
        cls,
        data: T | None = None,
        meta: dict[str, Any] | None = None,
    ) -> APIResponse[T]:
        """成功レスポンスを作成.

        Args:
            data: レスポンスデータ
            meta: メタデータ

        Returns:
            成功レスポンス
        """
        return cls(
            success=True,
            data=data,
            meta=meta or {},
        )

    @classmethod
    def error_response(
        cls,
        code: ErrorCode,
        message: str,
        details: dict[str, Any] | None = None,
        field: str | None = None,
    ) -> APIResponse[None]:
        """エラーレスポンスを作成.

        Args:
            code: エラーコード
            message: エラーメッセージ
            details: 詳細情報
            field: エラーフィールド

        Returns:
            エラーレスポンス
        """
        return cls(
            success=False,
            error=APIError(
                code=code,
                message=message,
                details=details,
                field=field,
            ),
        )

    # エイリアス
    @classmethod
    def ok(cls, data: T | None = None, **kwargs: Any) -> APIResponse[T]:
        """成功レスポンス（エイリアス）."""
        return cls.success_response(data=data, meta=kwargs.get("meta"))

    @classmethod
    def fail(
        cls,
        code: ErrorCode = ErrorCode.UNKNOWN,
        message: str = "An error occurred",
        **kwargs: Any,
    ) -> APIResponse[None]:
        """エラーレスポンス（エイリアス）."""
        return cls.error_response(
            code=code,
            message=message,
            details=kwargs.get("details"),
            field=kwargs.get("field"),
        )


class PagedResponse[T](BaseModel):
    """ページネーション対応レスポンス."""

    items: list[T] = Field(..., description="アイテムリスト")
    total: int = Field(..., description="総件数")
    page: int = Field(1, description="現在ページ")
    page_size: int = Field(10, description="ページサイズ")
    has_next: bool = Field(False, description="次ページあり")
    has_prev: bool = Field(False, description="前ページあり")

    @classmethod
    def create(
        cls,
        items: list[T],
        total: int,
        page: int = 1,
        page_size: int = 10,
    ) -> PagedResponse[T]:
        """ページネーションレスポンスを作成.

        Args:
            items: アイテムリスト
            total: 総件数
            page: 現在ページ
            page_size: ページサイズ

        Returns:
            ページネーションレスポンス
        """
        total_pages = (total + page_size - 1) // page_size
        return cls(
            items=items,
            total=total,
            page=page,
            page_size=page_size,
            has_next=page < total_pages,
            has_prev=page > 1,
        )


class StreamEventType(str, Enum):
    """ストリームイベント種別."""

    PROGRESS = "progress"
    LOG = "log"
    DATA = "data"
    RESULT = "result"
    ERROR = "error"
    COMPLETE = "complete"


class StreamEvent(BaseModel):
    """ストリームイベント."""

    type: StreamEventType = Field(..., description="イベント種別")
    data: dict[str, Any] = Field(default_factory=dict, description="イベントデータ")
    timestamp: str = Field(
        default_factory=lambda: datetime.now().isoformat(),
        description="タイムスタンプ",
    )

    @classmethod
    def progress(cls, value: int, message: str = "", **kwargs: Any) -> StreamEvent:
        """進捗イベントを作成."""
        return cls(
            type=StreamEventType.PROGRESS,
            data={"value": value, "message": message, **kwargs},
        )

    @classmethod
    def result(cls, data: dict[str, Any]) -> StreamEvent:
        """結果イベントを作成."""
        return cls(type=StreamEventType.RESULT, data=data)

    @classmethod
    def error(cls, message: str, code: str = "ERROR") -> StreamEvent:
        """エラーイベントを作成."""
        return cls(
            type=StreamEventType.ERROR,
            data={"message": message, "code": code},
        )

    def to_sse(self) -> str:
        """SSE形式に変換."""
        import json

        return f"data: {json.dumps(self.model_dump(), ensure_ascii=False, default=str)}\n\n"


__all__ = [
    "APIError",
    "APIResponse",
    "ErrorCode",
    "PagedResponse",
    "StreamEvent",
    "StreamEventType",
]
