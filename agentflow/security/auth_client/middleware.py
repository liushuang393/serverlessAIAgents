"""auth_client FastAPI ミドルウェア.

リクエストの Authorization ヘッダーから JWT を検証し、
コンテキストにユーザー情報をセットする Starlette ミドルウェア。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING, Any

from starlette.middleware.base import BaseHTTPMiddleware


if TYPE_CHECKING:
    from collections.abc import Awaitable, Callable

    from starlette.requests import Request
    from starlette.responses import Response

    from agentflow.security.auth_client.client import AuthClient


logger = logging.getLogger(__name__)

# リクエストスコープのユーザーキー
_AUTH_USER_KEY = "auth_client_user"


class AuthMiddleware(BaseHTTPMiddleware):
    """JWT 検証ミドルウェア.

    リクエストの Authorization ヘッダーを検証し、
    request.state.user にユーザー情報をセットする。

    使用例:
        from agentflow.security.auth_client import AuthMiddleware

        app.add_middleware(
            AuthMiddleware,
            auth_client=auth_client_instance,
            exclude_paths=["/health", "/docs"],
        )
    """

    def __init__(
        self,
        app: Any,
        auth_client: AuthClient,
        exclude_paths: list[str] | None = None,
    ) -> None:
        """初期化.

        Args:
            app: ASGI アプリ
            auth_client: AuthClient インスタンス
            exclude_paths: 認証をスキップするパス一覧（前方一致）
        """
        super().__init__(app)
        self._client = auth_client
        self._exclude_paths = exclude_paths or ["/health", "/docs", "/redoc", "/openapi.json"]

    async def dispatch(
        self,
        request: Request,
        call_next: Callable[[Request], Awaitable[Response]],
    ) -> Response:
        """ミドルウェア処理."""
        # 除外パスはスキップ
        for path in self._exclude_paths:
            if request.url.path.startswith(path):
                return await call_next(request)

        # Authorization ヘッダーから JWT を取得
        authorization = request.headers.get("Authorization")
        if authorization and authorization.startswith("Bearer "):
            token = authorization[7:]
            user = await self._client.verify_token(token)
            if user is not None:
                request.state.user = user
            else:
                request.state.user = None
        else:
            request.state.user = None

        return await call_next(request)
