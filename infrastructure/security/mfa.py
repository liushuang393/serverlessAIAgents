"""infrastructure.security.mfa — MFA（多要素認証）モジュール.

TOTP ベースの多要素認証（RFC 6238）を提供する。
pyotp パッケージが必要。未インストール時は fallback を使用する。

使用例:
    secret = TimeBasedMFA.generate_secret()
    uri = TimeBasedMFA.get_provisioning_uri(secret, "user@example.com", "MyApp")
    is_valid = TimeBasedMFA.verify_totp(secret, "123456")
"""

from __future__ import annotations

import base64
import hashlib
import hmac
import logging
import struct
import time


logger = logging.getLogger(__name__)


class TimeBasedMFA:
    """TOTP ベースの多要素認証ヘルパー（静的メソッドのみ）.

    pyotp が利用可能な場合はそちらを優先し、
    ない場合は組み込みの RFC 6238 実装を使用する。
    """

    @staticmethod
    def generate_secret(length: int = 32) -> str:
        """TOTP 用のランダムシークレットを生成する.

        Args:
            length: シークレットのバイト長。デフォルト 32。

        Returns:
            Base32 エンコードされたシークレット文字列。
        """
        try:
            import pyotp

            return pyotp.random_base32(length)
        except ImportError:
            import secrets

            raw = secrets.token_bytes(length)
            return base64.b32encode(raw).decode("ascii")

    @staticmethod
    def get_provisioning_uri(secret: str, username: str, app_name: str) -> str:
        """Authenticator アプリ向けのプロビジョニング URI を生成する.

        Args:
            secret: Base32 エンコードされた TOTP シークレット。
            username: ユーザー名（例: user@example.com）。
            app_name: アプリケーション名（例: AgentFlowFAQ）。

        Returns:
            otpauth://totp/... 形式の URI 文字列。
        """
        try:
            import pyotp

            totp = pyotp.TOTP(secret)
            return totp.provisioning_uri(name=username, issuer_name=app_name)
        except ImportError:
            from urllib.parse import quote

            label = f"{app_name}:{username}"
            return (
                f"otpauth://totp/{quote(label)}"
                f"?secret={secret}&issuer={quote(app_name)}&algorithm=SHA1&digits=6&period=30"
            )

    @staticmethod
    def verify_totp(secret: str, code: str, window: int = 1) -> bool:
        """TOTP コードを検証する.

        Args:
            secret: Base32 エンコードされた TOTP シークレット。
            code: ユーザーが入力した 6 桁のコード。
            window: 許容タイムウィンドウ数（前後 window ステップ）。

        Returns:
            コードが有効な場合 True。
        """
        if not secret or not code:
            return False

        try:
            import pyotp

            totp = pyotp.TOTP(secret)
            return totp.verify(code, valid_window=window)
        except ImportError:
            return TimeBasedMFA._verify_totp_rfc6238(secret, code, window)

    @staticmethod
    def _verify_totp_rfc6238(secret: str, code: str, window: int) -> bool:
        """RFC 6238 準拠の TOTP 検証（pyotp なし版）."""
        try:
            key = base64.b32decode(secret.upper())
        except Exception:
            return False

        current_step = int(time.time()) // 30
        for offset in range(-window, window + 1):
            step = current_step + offset
            step_bytes = struct.pack(">Q", step)
            hmac_result = hmac.new(key, step_bytes, hashlib.sha1).digest()
            offset_idx = hmac_result[-1] & 0x0F
            truncated = struct.unpack(">I", hmac_result[offset_idx : offset_idx + 4])[0] & 0x7FFFFFFF
            expected = str(truncated % 1_000_000).zfill(6)
            if hmac.compare_digest(expected, code.strip()):
                return True
        return False


__all__ = ["TimeBasedMFA"]
