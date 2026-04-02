import logging
import os
from typing import Any

import httpx


logger = logging.getLogger("decision_api.auth_client")
_DEFAULT_AUTH_SERVICE_URL = "http://localhost:8010"


def _response_json_dict(response: httpx.Response) -> dict[str, Any]:
    """HTTP レスポンス JSON を辞書へ正規化."""
    payload = response.json()
    if isinstance(payload, dict):
        return payload
    return {"success": False, "message": "認証サービスのレスポンス形式が不正です"}


class AuthClient:
    """Auth Service と通信するためのクライアント."""

    def __init__(self, base_url: str | None = None):
        resolved_base_url = str(base_url or os.getenv("AUTH_SERVICE_URL", _DEFAULT_AUTH_SERVICE_URL))
        self.base_url = resolved_base_url.rstrip("/")

    async def login(self, username: str, password: str) -> dict[str, Any]:
        """ログイン要求をプロキシ."""
        async with httpx.AsyncClient() as client:
            try:
                response = await client.post(
                    f"{self.base_url}/auth/login", json={"username": username, "password": password}, timeout=10.0
                )
                # Auth Service は AuthResponse (success, message, user, access_token...) を返す
                # セッション管理のためにクッキーから auth_session を抽出する必要があるかもしれないが、
                # AuthClient の呼び出し元 (routers/auth.py) で response.cookies を扱う
                data = _response_json_dict(response)
                if response.cookies.get("auth_session"):
                    data["session_token"] = response.cookies.get("auth_session")
                return data
            except Exception as e:
                logger.exception(f"Auth login error: {e}")
                return {"success": False, "message": f"認証サービスとの通信に失敗しました: {e}"}

    async def get_me(self, token: str) -> dict[str, Any]:
        """現在のユーザー情報を取得."""
        async with httpx.AsyncClient() as client:
            try:
                response = await client.get(f"{self.base_url}/auth/me", cookies={"auth_session": token}, timeout=5.0)
                return _response_json_dict(response)
            except Exception as e:
                logger.exception(f"Auth verify error: {e}")
                return {"success": False, "message": "セッションの検証に失敗しました"}

    async def logout(self, token: str) -> dict[str, Any]:
        """ログアウト要求をプロキシ."""
        async with httpx.AsyncClient() as client:
            try:
                response = await client.post(
                    f"{self.base_url}/auth/logout", cookies={"auth_session": token}, timeout=5.0
                )
                return _response_json_dict(response)
            except Exception as e:
                logger.exception(f"Auth logout error: {e}")
                return {"success": True, "message": "ログアウト通知に失敗しましたが、ローカルセッションは破棄されます"}
