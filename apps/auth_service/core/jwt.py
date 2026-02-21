"""JWT 発行/検証/リフレッシュユーティリティ.

HS256 または RS256 対応。JWKS エンドポイント用の公開鍵情報も提供する。
"""

from __future__ import annotations

import logging
import secrets
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from typing import Any


logger = logging.getLogger(__name__)


@dataclass
class TokenPair:
    """アクセストークンとリフレッシュトークンのペア."""

    access_token: str
    refresh_token: str
    token_type: str = "bearer"
    expires_in: int = 1800  # 秒


@dataclass
class TokenClaims:
    """デコードされた JWT クレーム."""

    sub: str  # ユーザーID
    username: str
    display_name: str
    department: str
    position: str
    role: str
    email: str | None
    jti: str
    iat: datetime
    exp: datetime
    iss: str
    aud: str


class JWTManager:
    """JWT トークン管理クラス.

    アクセストークン発行・検証・リフレッシュ、JWKS 情報提供を担う。
    """

    def __init__(
        self,
        secret_key: str,
        algorithm: str = "HS256",
        access_expire_minutes: int = 30,
        refresh_expire_days: int = 7,
        issuer: str = "auth-service",
        audience: str = "auth-service",
    ) -> None:
        """初期化.

        Args:
            secret_key: JWT 署名シークレット
            algorithm: 署名アルゴリズム（HS256）
            access_expire_minutes: アクセストークン有効期限（分）
            refresh_expire_days: リフレッシュトークン有効期限（日）
            issuer: 発行者
            audience: 対象者
        """
        self._secret_key = secret_key
        self._algorithm = algorithm
        self._access_expire_minutes = access_expire_minutes
        self._refresh_expire_days = refresh_expire_days
        self._issuer = issuer
        self._audience = audience

    def create_access_token(
        self,
        user_id: str,
        username: str,
        display_name: str,
        department: str,
        position: str,
        role: str,
        email: str | None = None,
        extra_claims: dict[str, Any] | None = None,
    ) -> str:
        """アクセストークンを発行.

        Args:
            user_id: ユーザーID
            username: ユーザー名
            display_name: 表示名
            department: 部署
            position: 役職
            role: ロール
            email: メールアドレス
            extra_claims: 追加クレーム

        Returns:
            JWT アクセストークン文字列
        """
        now = datetime.now(UTC)
        expire = now + timedelta(minutes=self._access_expire_minutes)
        jti = secrets.token_hex(16)

        payload: dict[str, Any] = {
            "sub": user_id,
            "iat": int(now.timestamp()),
            "exp": int(expire.timestamp()),
            "iss": self._issuer,
            "aud": self._audience,
            "jti": jti,
            "username": username,
            "display_name": display_name,
            "department": department,
            "position": position,
            "role": role,
        }
        if email:
            payload["email"] = email
        if extra_claims:
            payload.update(extra_claims)

        return self._encode(payload)

    def create_refresh_token(
        self,
        user_id: str,
        family: str | None = None,
    ) -> tuple[str, str]:
        """リフレッシュトークンを発行.

        Args:
            user_id: ユーザーID
            family: トークンファミリーID（リプレイ攻撃検出用）

        Returns:
            (token_string, family_id) タプル
        """
        family_id = family or secrets.token_hex(16)
        now = datetime.now(UTC)
        expire = now + timedelta(days=self._refresh_expire_days)

        payload: dict[str, Any] = {
            "sub": user_id,
            "iat": int(now.timestamp()),
            "exp": int(expire.timestamp()),
            "iss": self._issuer,
            "aud": self._audience,
            "jti": secrets.token_hex(16),
            "type": "refresh",
            "family": family_id,
        }

        return self._encode(payload), family_id

    def decode_access_token(self, token: str) -> TokenClaims | None:
        """アクセストークンを検証・デコード.

        Args:
            token: JWT トークン文字列

        Returns:
            TokenClaims または None（無効/期限切れの場合）
        """
        payload = self._decode(token)
        if payload is None:
            return None
        if payload.get("type") == "refresh":
            logger.warning("アクセストークン検証にリフレッシュトークンが使用されました")
            return None

        try:
            return TokenClaims(
                sub=str(payload["sub"]),
                username=str(payload.get("username", payload["sub"])),
                display_name=str(payload.get("display_name", "")),
                department=str(payload.get("department", "")),
                position=str(payload.get("position", "")),
                role=str(payload.get("role", "employee")),
                email=payload.get("email"),
                jti=str(payload.get("jti", "")),
                iat=datetime.fromtimestamp(int(payload["iat"]), tz=UTC),
                exp=datetime.fromtimestamp(int(payload["exp"]), tz=UTC),
                iss=str(payload.get("iss", "")),
                aud=str(payload.get("aud", "")),
            )
        except (KeyError, TypeError, ValueError) as e:
            logger.debug("TokenClaims 変換エラー: %s", e)
            return None

    def decode_refresh_token(self, token: str) -> dict[str, Any] | None:
        """リフレッシュトークンを検証・デコード.

        Args:
            token: JWT リフレッシュトークン文字列

        Returns:
            クレーム dict または None
        """
        payload = self._decode(token)
        if payload is None:
            return None
        if payload.get("type") != "refresh":
            logger.warning("リフレッシュトークン検証に通常トークンが使用されました")
            return None
        return payload

    def get_jwks(self) -> dict[str, Any]:
        """JWKS (JSON Web Key Set) を返す.

        HS256 の場合は対称鍵なので、クライアントが独自検証できる形式で返す。
        公開鍵共有方式（RS256）への移行時はここを変更する。

        Returns:
            JWKS 形式の dict
        """
        # HS256 は対称鍵なので公開できない。代わりに鍵IDとアルゴリズム情報を返す。
        # クライアントは共有シークレットを使ってローカル検証を行う。
        return {
            "keys": [
                {
                    "kty": "oct",
                    "use": "sig",
                    "alg": self._algorithm,
                    "kid": "auth-service-key-1",
                    "note": "共有シークレット方式。AUTH_SERVICE_JWT_SECRET 環境変数で検証してください。",
                }
            ]
        }

    def _encode(self, payload: dict[str, Any]) -> str:
        """JWT をエンコード."""
        try:
            import jwt

            return jwt.encode(payload, self._secret_key, algorithm=self._algorithm)
        except ImportError as e:
            msg = "PyJWT をインストールしてください: pip install pyjwt"
            raise ImportError(msg) from e

    def _decode(self, token: str) -> dict[str, Any] | None:
        """JWT をデコード・検証."""
        try:
            import jwt

            return jwt.decode(
                token,
                self._secret_key,
                algorithms=[self._algorithm],
                issuer=self._issuer,
                audience=self._audience,
            )
        except ImportError:
            logger.exception("PyJWT がインストールされていません")
            return None
        except Exception as e:
            logger.debug("JWT デコードエラー: %s", e)
            return None

    @property
    def access_expire_seconds(self) -> int:
        """アクセストークン有効期限（秒）."""
        return self._access_expire_minutes * 60
