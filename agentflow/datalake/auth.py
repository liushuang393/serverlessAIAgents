"""AuthProvider - 認証プロバイダ抽象.

認証ロジックを外部から注入可能にするためのプロトコル。
これにより、既存の認証システムとの統合が容易になる。

設計原則:
    - 認証分離: コネクタは認証情報を保持しない
    - 外部注入: 企業の認証システムを注入可能
    - トークン管理: 自動リフレッシュをサポート
"""

import logging
from abc import ABC, abstractmethod
from datetime import UTC, datetime
from typing import Any, Literal

from pydantic import BaseModel, Field


logger = logging.getLogger(__name__)


class AuthCredentials(BaseModel):
    """認証情報.

    Attributes:
        credential_type: 認証タイプ
        access_token: アクセストークン
        refresh_token: リフレッシュトークン
        expires_at: 有効期限
        api_key: APIキー
        username: ユーザー名
        password: パスワード
        extra: 追加情報
    """

    credential_type: Literal[
        "oauth2", "api_key", "basic", "iam_role", "service_account"
    ] = Field(default="api_key", description="認証タイプ")

    # OAuth2
    access_token: str | None = Field(default=None, description="アクセストークン")
    refresh_token: str | None = Field(default=None, description="リフレッシュトークン")
    expires_at: datetime | None = Field(default=None, description="有効期限")

    # API Key
    api_key: str | None = Field(default=None, description="APIキー")

    # Basic Auth
    username: str | None = Field(default=None, description="ユーザー名")
    password: str | None = Field(default=None, description="パスワード")

    # Additional
    extra: dict[str, Any] = Field(default_factory=dict, description="追加情報")

    @property
    def is_expired(self) -> bool:
        """トークンが期限切れかどうか.

        Returns:
            期限切れの場合True
        """
        if self.expires_at is None:
            return False
        return datetime.now(UTC) >= self.expires_at


class AuthProvider(ABC):
    """認証プロバイダ抽象基底クラス.

    企業の認証システムを統合するためのインターフェース。

    実装例:
        >>> class MyAuthProvider(AuthProvider):
        ...     async def get_credentials(self, connector_type, resource_id):
        ...         # 認証システムから認証情報を取得
        ...         return AuthCredentials(api_key="xxx")
    """

    @abstractmethod
    async def get_credentials(
        self,
        connector_type: str,
        resource_id: str | None = None,
    ) -> AuthCredentials:
        """認証情報を取得.

        Args:
            connector_type: コネクタタイプ（'s3', 'onedrive'等）
            resource_id: リソース識別子（バケット名、ドライブID等）

        Returns:
            AuthCredentials
        """
        ...

    async def refresh_token(
        self,
        connector_type: str,
        refresh_token: str,
    ) -> AuthCredentials:
        """トークンをリフレッシュ.

        デフォルト実装はNotImplementedErrorを発生。
        OAuth2対応が必要な場合はオーバーライドすること。

        Args:
            connector_type: コネクタタイプ
            refresh_token: リフレッシュトークン

        Returns:
            新しいAuthCredentials

        Raises:
            NotImplementedError: 未実装の場合
        """
        msg = f"Token refresh not implemented for {connector_type}"
        raise NotImplementedError(
            msg
        )


class SimpleAuthProvider(AuthProvider):
    """シンプルな認証プロバイダ（開発・テスト用）.

    環境変数や設定ファイルから認証情報を取得。

    使用例:
        >>> auth = SimpleAuthProvider()
        >>> auth.register("s3", AuthCredentials(api_key="xxx"))
        >>> creds = await auth.get_credentials("s3")
    """

    def __init__(self) -> None:
        """初期化."""
        self._credentials: dict[str, AuthCredentials] = {}
        self._resource_credentials: dict[str, dict[str, AuthCredentials]] = {}

    def register(
        self,
        connector_type: str,
        credentials: AuthCredentials,
        resource_id: str | None = None,
    ) -> None:
        """認証情報を登録.

        Args:
            connector_type: コネクタタイプ
            credentials: 認証情報
            resource_id: リソース識別子（オプション）
        """
        if resource_id:
            if connector_type not in self._resource_credentials:
                self._resource_credentials[connector_type] = {}
            self._resource_credentials[connector_type][resource_id] = credentials
        else:
            self._credentials[connector_type] = credentials

        logger.debug(f"Registered credentials for {connector_type}/{resource_id}")

    async def get_credentials(
        self,
        connector_type: str,
        resource_id: str | None = None,
    ) -> AuthCredentials:
        """認証情報を取得.

        Args:
            connector_type: コネクタタイプ
            resource_id: リソース識別子

        Returns:
            AuthCredentials

        Raises:
            KeyError: 認証情報が見つからない場合
        """
        # リソース固有の認証情報を優先
        if resource_id and connector_type in self._resource_credentials:
            if resource_id in self._resource_credentials[connector_type]:
                return self._resource_credentials[connector_type][resource_id]

        # デフォルトの認証情報
        if connector_type in self._credentials:
            return self._credentials[connector_type]

        msg = f"No credentials found for {connector_type}/{resource_id}"
        raise KeyError(msg)

