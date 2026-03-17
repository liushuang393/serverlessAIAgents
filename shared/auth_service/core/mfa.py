"""TOTP MFA ユーティリティ.

pyotp を使用した TOTP（Time-based One-Time Password）MFA を提供する。
"""

from __future__ import annotations

import logging


logger = logging.getLogger(__name__)


class MFAManager:
    """TOTP MFA 管理クラス."""

    def __init__(self, issuer_name: str = "AuthService") -> None:
        """初期化.

        Args:
            issuer_name: OTP Auth URI に表示される発行者名
        """
        self._issuer_name = issuer_name

    def generate_secret(self) -> str:
        """MFA シークレットを生成.

        Returns:
            Base32 エンコードされたシークレット文字列
        """
        try:
            import pyotp

            return pyotp.random_base32()
        except ImportError as e:
            msg = "pyotp をインストールしてください: pip install pyotp"
            raise ImportError(msg) from e

    def get_provisioning_uri(self, secret: str, username: str) -> str:
        """OTP Auth URI を生成（QR コード用）.

        Args:
            secret: MFA シークレット
            username: ユーザー名

        Returns:
            otpauth:// URI 文字列
        """
        try:
            import pyotp

            return pyotp.TOTP(secret).provisioning_uri(
                name=username,
                issuer_name=self._issuer_name,
            )
        except ImportError as e:
            msg = "pyotp をインストールしてください: pip install pyotp"
            raise ImportError(msg) from e

    @staticmethod
    def verify_totp(secret: str, code: str) -> bool:
        """TOTP コードを検証.

        Args:
            secret: MFA シークレット
            code: ユーザーが入力した 6 桁コード

        Returns:
            コードが有効な場合 True
        """
        if not secret or not code:
            return False
        try:
            import pyotp

            totp = pyotp.TOTP(secret)
            # valid_window=1 で前後 30 秒の時刻ずれを許容
            return totp.verify(code, valid_window=1)
        except ImportError:
            logger.exception("pyotp がインストールされていません")
            return False
        except Exception as e:
            logger.debug("TOTP 検証エラー: %s", e)
            return False
