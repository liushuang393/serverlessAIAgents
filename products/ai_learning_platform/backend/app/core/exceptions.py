# app/core/exceptions.py
from fastapi import HTTPException
from typing import Any, Dict, Optional

class APIException(HTTPException):
    def __init__(
        self,
        status_code: int,
        detail: Any = None,
        headers: Optional[Dict[str, Any]] = None,
        error_code: Optional[str] = None
    ):
        super().__init__(status_code, detail, headers)
        self.error_code = error_code

class DatabaseException(APIException):
    def __init__(self, detail: str = "データベースエラーが発生しました"):
        super().__init__(500, detail, error_code="DB_ERROR")

class OpenAIException(APIException):
    def __init__(self, detail: str = "AI処理でエラーが発生しました"):
        super().__init__(503, detail, error_code="AI_ERROR")