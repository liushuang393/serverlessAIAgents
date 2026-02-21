"""認証プロバイダー抽象基底クラス.

Strategy パターンの抽象インターフェース。
各プロバイダーはこのクラスを継承して認証ロジックを実装する。
"""

from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass, field
from typing import TYPE_CHECKING, Any


if TYPE_CHECKING:
    from apps.auth_service.config import Settings


@dataclass(slots=True)
class ExternalIdentity:
    """外部認証で解決されたユーザー情報."""

    username: str
    display_name: str
    role: str = "employee"
    department: str = ""
    position: str = ""
    email: str | None = None
    raw_info: dict[str, Any] = field(default_factory=dict)


@dataclass
class AuthResult:
    """認証結果."""

    success: bool
    message: str
    identity: ExternalIdentity | None = None


class AuthProvider(ABC):
    """認証プロバイダー抽象基底クラス.

    全プロバイダーはこのインターフェースを実装する。
    """

    def __init__(self, settings: Settings) -> None:
        """初期化.

        Args:
            settings: auth_service 設定
        """
        self._settings = settings

    @abstractmethod
    async def authenticate(self, username: str, password: str) -> AuthResult:
        """ユーザーを認証.

        Args:
            username: ユーザー名
            password: パスワード

        Returns:
            AuthResult インスタンス
        """

    @property
    @abstractmethod
    def provider_name(self) -> str:
        """プロバイダー名."""

    def supports_registration(self) -> bool:
        """ユーザー登録をサポートするか.

        Returns:
            local_db のみ True
        """
        return False

    def supports_password_change(self) -> bool:
        """パスワード変更をサポートするか.

        Returns:
            local_db のみ True
        """
        return False
