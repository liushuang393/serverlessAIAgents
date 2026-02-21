"""パスワードハッシュユーティリティ.

PBKDF2-HMAC-SHA256 を使用したパスワードハッシュ/検証を提供する。
"""

from __future__ import annotations

import hashlib
import secrets


class PasswordManager:
    """パスワードハッシュ管理クラス."""

    def __init__(self, iterations: int = 200000) -> None:
        """初期化.

        Args:
            iterations: PBKDF2 反復回数（デフォルト: 200000）
        """
        self._iterations = iterations

    def generate_salt(self) -> str:
        """ランダムなソルトを生成.

        Returns:
            16バイトの16進数ソルト文字列
        """
        return secrets.token_hex(16)

    def hash_password(self, password: str, salt: str) -> str:
        """パスワードをハッシュ化.

        Args:
            password: 平文パスワード
            salt: ソルト文字列

        Returns:
            PBKDF2 ハッシュの16進数文字列
        """
        return hashlib.pbkdf2_hmac(
            "sha256",
            password.encode("utf-8"),
            salt.encode("utf-8"),
            self._iterations,
        ).hex()

    def verify_password(self, password: str, salt: str, expected_hash: str) -> bool:
        """パスワードを検証.

        タイミング攻撃を防ぐため compare_digest を使用する。

        Args:
            password: 検証する平文パスワード
            salt: ソルト文字列
            expected_hash: 期待するハッシュ

        Returns:
            パスワードが正しければ True
        """
        actual_hash = self.hash_password(password, salt)
        return secrets.compare_digest(actual_hash, expected_hash)

    @staticmethod
    def hash_token(raw_token: str) -> str:
        """トークンを SHA-256 でハッシュ化.

        Args:
            raw_token: 平文トークン

        Returns:
            SHA-256 ハッシュの16進数文字列
        """
        return hashlib.sha256(raw_token.encode("utf-8")).hexdigest()

    @staticmethod
    def validate_password_strength(password: str) -> tuple[bool, str]:
        """パスワード強度を検証.

        Args:
            password: 検証するパスワード

        Returns:
            (is_valid, reason) タプル
        """
        if len(password) < 8:
            return False, "パスワードは8文字以上にしてください"
        if password.isalpha() or password.isdigit():
            return False, "パスワードは英字と数字を含めてください"
        return True, "OK"

    @staticmethod
    def build_user_id(username: str) -> str:
        """ユーザー名からユーザーIDを生成.

        Args:
            username: ユーザー名

        Returns:
            ユーザーID（最大64文字）
        """
        candidate = f"user-{username}"
        if len(candidate) <= 64:
            return candidate
        digest = hashlib.sha256(username.encode("utf-8")).hexdigest()[:59]
        return f"user-{digest}"
