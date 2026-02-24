"""MFA (多要素認証) モジュール.

TOTP (Time-based One-Time Password) を用いた MFA 機能を提供する。
pyotp パッケージが未インストールの場合でもモジュールのインポート自体は
成功し、メソッド呼び出し時に ImportError を発生させる防御的設計。

依存: pyotp>=2.9.0（pip install pyotp）
"""

from __future__ import annotations

import logging

logger = logging.getLogger(__name__)

_PYOTP_INSTALL_MSG = (
    "pyotp がインストールされていません。"
    "MFA 機能を使用するには 'pip install pyotp' を実行してください。"
)


class TimeBasedMFA:
    """TOTP ベースの多要素認証実装.

    pyotp パッケージを用いて TOTP シークレット生成・QR コード URI 生成・
    コード検証を行う。pyotp が未インストールの場合、各メソッド呼び出し時に
    ImportError を送出する（verify_totp のみ False を返す）。
    """

    @staticmethod
    def generate_secret() -> str:
        """ランダムな Base32 MFA シークレットを生成.

        Returns:
            Base32 エンコードされたシークレット文字列

        Raises:
            ImportError: pyotp がインストールされていない場合
        """
        try:
            import pyotp  # noqa: PLC0415 — 遅延インポート
        except ImportError:
            raise ImportError(_PYOTP_INSTALL_MSG) from None
        return pyotp.random_base32()

    @staticmethod
    def get_provisioning_uri(secret: str, username: str, issuer_name: str) -> str:
        """QR コード用の OTP プロビジョニング URI を生成.

        Args:
            secret: MFA シークレット
            username: ユーザー名
            issuer_name: 発行者名（アプリ名など）

        Returns:
            otpauth:// 形式の URI 文字列

        Raises:
            ImportError: pyotp がインストールされていない場合
        """
        try:
            import pyotp  # noqa: PLC0415 — 遅延インポート
        except ImportError:
            raise ImportError(_PYOTP_INSTALL_MSG) from None
        return pyotp.TOTP(secret).provisioning_uri(
            name=username, issuer_name=issuer_name,
        )

    @staticmethod
    def verify_totp(secret: str, code: str) -> bool:
        """TOTP コードをシークレットに対して検証.

        Args:
            secret: MFA シークレット
            code: ユーザーが入力した 6 桁コード

        Returns:
            コードが有効な場合 True、無効または pyotp 未インストールの場合 False

        注意:
            pyotp がインストールされていない場合は False を返し、
            ログに警告を出力する（ImportError は送出しない）。
        """
        if not secret or not code:
            return False
        try:
            import pyotp  # noqa: PLC0415 — 遅延インポート

            totp = pyotp.TOTP(secret)
            result: bool = totp.verify(code)
            return result
        except ImportError:
            logger.warning("pyotp 未インストールのため TOTP 検証をスキップ")
            return False
