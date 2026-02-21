"""auth_client 設定モジュール.

AUTH_SERVICE_URL と AUTH_SERVICE_JWT_SECRET の 2 環境変数だけで動作する。
"""

from __future__ import annotations

import os


class AuthClientConfig:
    """auth_client 設定.

    環境変数から読み込む最小構成。
    """

    def __init__(
        self,
        base_url: str | None = None,
        jwt_secret: str | None = None,
        jwt_algorithm: str | None = None,
        jwt_issuer: str | None = None,
        jwt_audience: str | None = None,
        timeout: float = 10.0,
    ) -> None:
        """初期化.

        Args:
            base_url: auth_service の URL（例: http://localhost:8010）
                      未指定時は AUTH_SERVICE_URL 環境変数を使用
            jwt_secret: JWT 署名シークレット
                        未指定時は AUTH_SERVICE_JWT_SECRET 環境変数を使用
            jwt_algorithm: JWT アルゴリズム（デフォルト: HS256）
            jwt_issuer: JWT 発行者（デフォルト: auth-service）
            jwt_audience: JWT 対象者（デフォルト: auth-service）
            timeout: HTTP リクエストタイムアウト（秒）
        """
        resolved_base_url = base_url or os.getenv("AUTH_SERVICE_URL") or "http://localhost:8010"
        self.base_url = resolved_base_url.rstrip("/")
        self.jwt_secret = jwt_secret or os.getenv("AUTH_SERVICE_JWT_SECRET", "")
        self.jwt_algorithm = jwt_algorithm or os.getenv("AUTH_SERVICE_JWT_ALGORITHM", "HS256")
        self.jwt_issuer = jwt_issuer or os.getenv("AUTH_SERVICE_JWT_ISSUER", "auth-service")
        self.jwt_audience = jwt_audience or os.getenv("AUTH_SERVICE_JWT_AUDIENCE", "auth-service")
        self.timeout = timeout
