"""SQL / 売上分析ルーター.

/api/sql/query, /api/sales/analyze
"""

from __future__ import annotations

from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.auth.dependencies import require_auth
from apps.faq_system.routers.dependencies import (
    get_sales_agent,
    get_sql_service,
    is_sql_enabled,
)
from fastapi import APIRouter, Depends, HTTPException
from pydantic import BaseModel, Field


if TYPE_CHECKING:
    from apps.faq_system.backend.auth.models import UserInfo
else:
    UserInfo = Any


router = APIRouter(tags=["SQL / 売上分析"])


# ---------------------------------------------------------------------------
# リクエストモデル
# ---------------------------------------------------------------------------


class SQLQueryRequest(BaseModel):
    """SQLクエリリクエスト."""

    question: str = Field(..., description="質問")


# ---------------------------------------------------------------------------
# エンドポイント
# ---------------------------------------------------------------------------


@router.post("/api/sql/query")
async def sql_query(
    request: SQLQueryRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """SQL クエリ API (認証必須)."""
    if not is_sql_enabled():
        raise HTTPException(
            status_code=400,
            detail={"message": "SQL is disabled by app config", "error_code": "sql_disabled"},
        )

    service = get_sql_service()
    result = await service.execute(action="query", question=request.question)
    if not result.success:
        status_code = 400 if result.error_code and result.error_code.startswith("invalid_") else 500
        raise HTTPException(
            status_code=status_code,
            detail={
                "message": result.error_message or "SQL query failed",
                "error_code": result.error_code or "sql_error",
            },
        )
    return result.data


@router.post("/api/sales/analyze")
async def sales_analyze(
    request: SQLQueryRequest,
    _user: UserInfo = Depends(require_auth),
) -> dict[str, Any]:
    """売上分析 API (認証必須).

    SalesAgent を使用して売上データを分析します。
    """
    agent = get_sales_agent()
    return await agent.run({"question": request.question})
