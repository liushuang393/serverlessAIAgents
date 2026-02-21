from abc import ABC, abstractmethod
from typing import Any

from pydantic import BaseModel


class OAuth2Token(BaseModel):
    """OAuth2 アクセストークン."""

    access_token: str
    refresh_token: str | None = None
    expires_in: int
    token_type: str = "bearer"
    id_token: str | None = None  # OIDC の場合


class ExternalIdentity(BaseModel):
    """外部認証プロバイダーからのユーザー情報."""

    sub: str
    username: str | None = None
    email: str | None = None
    display_name: str | None = None
    picture: str | None = None
    provider: str
    raw_info: dict[str, Any] | None = None



class OAuth2Provider(ABC):
    """OAuth2 プロバイダー基底クラス."""

    def __init__(self, client_id: str, client_secret: str, redirect_uri: str) -> None:
        self.client_id = client_id
        self.client_secret = client_secret
        self.redirect_uri = redirect_uri

    @abstractmethod
    async def get_authorization_url(self, state: str) -> str:
        """認可 URL を生成."""

    @abstractmethod
    async def exchange_code(self, code: str) -> OAuth2Token:
        """認証コードをトークンに交換."""

    @abstractmethod
    async def get_user_info(self, token: OAuth2Token) -> ExternalIdentity:
        """ユーザー情報を取得."""
