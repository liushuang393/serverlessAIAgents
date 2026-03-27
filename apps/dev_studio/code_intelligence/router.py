"""code_intelligence ルーター.

エンドポイント:
    POST /parse  - ソースコードを解析してUnifiedASTを返す
    GET  /parsers - 利用可能なパーサー一覧を返す
"""

from __future__ import annotations

import logging
from typing import Any

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel, Field


_logger = logging.getLogger(__name__)

router = APIRouter(tags=["code_intelligence"])


class ParseRequest(BaseModel):
    """コード解析リクエスト."""

    source_code: str = Field(..., description="解析するソースコード")
    language: str = Field(..., description="言語 (python / java)")
    include_comments: bool = Field(True, description="コメントを含めるか")
    module_name: str = Field("module", description="モジュール名（Python用）")


class ParseResponse(BaseModel):
    """コード解析レスポンス."""

    success: bool
    language: str
    ast: dict[str, Any] | None = None
    errors: list[dict[str, Any]] = Field(default_factory=list)
    warnings: list[str] = Field(default_factory=list)
    metadata: dict[str, Any] = Field(default_factory=dict)


@router.post("/parse", response_model=ParseResponse)
async def parse_code(request: ParseRequest) -> ParseResponse:
    """ソースコードを解析してUnifiedASTを返す.

    Args:
        request: 解析リクエスト

    Returns:
        解析結果（UnifiedAST）
    """
    try:
        from apps.dev_studio.code_intelligence.parsers.base import CodeParser, ParseContext

        parser: CodeParser
        if request.language == "python":
            from apps.dev_studio.code_intelligence.parsers.modern.python_parser import PythonParser

            parser = PythonParser()
        elif request.language == "java":
            from apps.dev_studio.code_intelligence.parsers.modern.java_parser import JavaParser

            parser = JavaParser()
        else:
            raise HTTPException(
                status_code=400,
                detail=f"未サポートの言語: {request.language}。使用可能: python, java",
            )

        context = ParseContext(
            include_comments=request.include_comments,
            metadata={"module_name": request.module_name},
        )
        result = parser.parse(request.source_code, context)

        return ParseResponse(
            success=result.success,
            language=request.language,
            ast=result.ast.to_dict() if result.ast else None,
            errors=[
                {
                    "message": e.message,
                    "line": e.line,
                    "column": e.column,
                }
                for e in result.errors
            ],
            warnings=result.warnings,
            metadata=result.metadata,
        )

    except HTTPException:
        raise
    except Exception as e:
        _logger.exception(f"コード解析エラー: {e}")
        raise HTTPException(status_code=500, detail=f"解析失敗: {e}") from e


@router.get("/parsers")
async def list_parsers() -> dict[str, Any]:
    """利用可能なパーサー一覧を返す."""
    return {
        "parsers": [
            {"language": "python", "extensions": [".py", ".pyi"], "engine": "ast"},
            {"language": "java", "extensions": [".java"], "engine": "regex"},
        ]
    }


__all__ = ["router"]
