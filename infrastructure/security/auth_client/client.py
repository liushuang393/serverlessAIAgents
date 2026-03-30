"""infrastructure.security.auth_client.client — Auth Service クライアント.

auth_service への HTTP クライアントおよびローカル JWT 検証を提供する。
verify_token はローカル JWT 検証を優先し、失敗時は /auth/me へフォールバックする。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any


logger = logging.getLogger(__name__)


@dataclass
class RemoteUser:
    """auth_service から解決したユーザー情報.

    Attributes:
        user_id: ユーザー固有 ID（JWT の sub クレーム）。
        username: ログインユーザー名。
        role: プライマリロール（例: "admin", "employee"）。
        roles: ロール一覧。
        scopes: 付与済みスコープ一覧（例: ["api", "faq.access"]）。
        permissions: パーミッション一覧（例: ["faq:read"]）。
        tenant_id: テナント ID（マルチテナント時）。
        azp: 認可パーティ（authorized party）。
        email: メールアドレス。
        display_name: 表示名。
        department: 部署名。
        position: 役職。
        extra: その他の JWT クレーム / メタデータ。
    """

    user_id: str = ""
    username: str = ""
    role: str = "employee"
    roles: list[str] = field(default_factory=list)
    scopes: list[str] = field(default_factory=list)
    permissions: list[str] = field(default_factory=list)
    tenant_id: str | None = None
    azp: str | None = None
    email: str | None = None
    display_name: str = ""
    department: str = ""
    position: str = ""
    extra: dict[str, Any] = field(default_factory=dict)


class AuthClient:
    """auth_service JWT 検証 + リモート /auth/me フォールバッククライアント.

    Args:
        base_url: auth_service のベース URL（例: http://localhost:8010）。
        jwt_secret: JWT 署名シークレット（ローカル検証用）。
        jwt_algorithm: JWT アルゴリズム（デフォルト: "HS256"）。
        jwt_issuer: JWT 発行者（iss クレーム検証用）。
        jwt_audience: JWT 受信者（aud クレーム検証用）。
    """

    def __init__(
        self,
        base_url: str = "",
        jwt_secret: str = "",
        jwt_algorithm: str = "HS256",
        jwt_issuer: str = "",
        jwt_audience: str = "",
    ) -> None:
        """初期化."""
        self._base_url = base_url.rstrip("/") if base_url else ""
        self._jwt_secret = jwt_secret
        self._jwt_algorithm = jwt_algorithm
        self._jwt_issuer = jwt_issuer
        self._jwt_audience = jwt_audience

    @property
    def base_url(self) -> str:
        """auth_service のベース URL を返す（末尾スラッシュなし）."""
        return self._base_url

    async def verify_token(self, token: str) -> RemoteUser | None:
        """JWT トークンを検証してユーザー情報を返す.

        検証順:
        1. ローカル JWT 検証（jwt_secret が設定されている場合）
        2. リモート /auth/me エンドポイント（base_url が設定されている場合）

        Args:
            token: Bearer トークン文字列（"Bearer " プレフィックスなし）。

        Returns:
            検証成功時は RemoteUser、失敗時は None。
        """
        # 1. ローカル JWT 検証
        if self._jwt_secret:
            user = self._verify_locally(token)
            if user is not None:
                return user
            logger.debug("ローカル JWT 検証失敗。リモートにフォールバック")

        # 2. リモート /auth/me 検証
        if self._base_url:
            return await self._verify_remote(token)

        logger.warning("AUTH_SERVICE_JWT_SECRET が未設定です。ローカル検証できません。")
        return None

    def _verify_locally(self, token: str) -> RemoteUser | None:
        """PyJWT でローカル検証する."""
        try:
            import jwt as pyjwt

            kwargs: dict[str, Any] = {
                "algorithms": [self._jwt_algorithm],
            }
            if self._jwt_issuer:
                kwargs["issuer"] = self._jwt_issuer
            if self._jwt_audience:
                kwargs["audience"] = self._jwt_audience

            payload = pyjwt.decode(token, self._jwt_secret, **kwargs)
            return self._payload_to_remote_user(payload)
        except Exception:
            logger.debug("ローカル JWT デコード失敗", exc_info=True)
            return None

    async def _verify_remote(self, token: str) -> RemoteUser | None:
        """リモートの /auth/me エンドポイントで検証する."""
        try:
            import httpx

            url = f"{self._base_url}/auth/me"
            async with httpx.AsyncClient(timeout=5.0) as client:
                resp = await client.get(url, headers={"Authorization": f"Bearer {token}"})
                if resp.status_code != 200:
                    return None
                data: dict[str, Any] = resp.json()
                return self._auth_me_to_remote_user(data)
        except Exception:
            logger.debug("リモート /auth/me 検証失敗", exc_info=True)
            return None

    @staticmethod
    def _payload_to_remote_user(payload: dict[str, Any]) -> RemoteUser:
        """JWT ペイロードを RemoteUser へ変換する."""
        meta: dict[str, Any] = payload.get("metadata", {}) or {}
        roles_raw = payload.get("roles", [])
        role_raw = payload.get("role", "")
        if not role_raw and roles_raw:
            role_raw = roles_raw[0]
        return RemoteUser(
            user_id=str(payload.get("sub", "")),
            username=str(meta.get("username", payload.get("sub", ""))),
            role=str(role_raw or "employee"),
            roles=list(roles_raw),
            scopes=list(payload.get("scp", payload.get("scopes", []))),
            permissions=list(payload.get("permissions", [])),
            tenant_id=str(payload["tenant_id"]) if payload.get("tenant_id") else None,
            azp=str(payload["azp"]) if payload.get("azp") else None,
            email=str(payload.get("email", "")),
            display_name=str(meta.get("display_name", "")),
            department=str(meta.get("department", "")),
            position=str(meta.get("position", "")),
            extra={k: v for k, v in payload.items() if k not in {"sub", "iat", "exp", "iss", "aud", "metadata"}},
        )

    @staticmethod
    def _auth_me_to_remote_user(data: dict[str, Any]) -> RemoteUser:
        """auth/me レスポンスを RemoteUser へ変換する.

        auth_service のレスポンスは {"success": true, "user": {...}} 形式の
        ネスト構造を返す場合があるため、"user" キーが存在すればそちらを参照する。
        """
        # ネストされた "user" キーがあればそちらを使う
        if "user" in data and isinstance(data.get("user"), dict):
            data = data["user"]
        role = str(data.get("role", "employee"))
        return RemoteUser(
            user_id=str(data.get("user_id", data.get("id", ""))),
            username=str(data.get("username", "")),
            role=role,
            roles=list(data.get("roles", [role])),
            scopes=list(data.get("scopes", data.get("scp", []))),
            permissions=list(data.get("permissions", [])),
            tenant_id=str(data["tenant_id"]) if data.get("tenant_id") else None,
            azp=str(data["azp"]) if data.get("azp") else None,
            email=str(data.get("email", "")),
            display_name=str(data.get("display_name", "")),
            department=str(data.get("department", "")),
            position=str(data.get("position", "")),
            extra={},
        )


__all__ = ["AuthClient", "RemoteUser"]
