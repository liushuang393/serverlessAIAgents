"""SAML 2.0 認証プロバイダー (スタブ実装).

本番実装には python3-saml または pysaml2 が必要。
現在はスタブとして設定チェックのみ行う。
"""

from __future__ import annotations

import logging
from typing import TYPE_CHECKING

from apps.auth_service.providers.base import AuthProvider, AuthResult, ExternalIdentity


if TYPE_CHECKING:
    from apps.auth_service.config import Settings


logger = logging.getLogger(__name__)


class SAMLProvider(AuthProvider):
    """SAML 2.0 プロバイダー.

    完全実装には python3-saml のインストールが必要:
        pip install python3-saml

    現在はスタブ実装。SAML ACS URL での応答処理は api/router.py に追加予定。
    """

    def __init__(self, settings: Settings) -> None:
        super().__init__(settings)

    @property
    def provider_name(self) -> str:
        return "saml"

    async def authenticate(self, username: str, password: str) -> AuthResult:
        """パスワード認証は非対応（SAML はブラウザリダイレクト専用）."""
        return AuthResult(
            success=False,
            message="SAML 認証はパスワード方式に対応していません。SAML SSO フローを使用してください。",
        )

    def get_sso_url(self) -> str | None:
        """SAML SSO URL を返す.

        Returns:
            SSO URL または None（設定なしの場合）
        """
        if not self._settings.SAML_IDP_METADATA_URL:
            logger.error("SAML_IDP_METADATA_URL が未設定です")
            return None
        return self._settings.SAML_IDP_METADATA_URL

    async def process_assertion(self, saml_response: str) -> ExternalIdentity | None:
        """SAML アサーションを処理.

        Args:
            saml_response: Base64 エンコードされた SAML レスポンス

        Returns:
            ExternalIdentity または None

        Note:
            完全実装には python3-saml が必要です。
        """
        try:
            import importlib.util

            if importlib.util.find_spec("onelogin.saml2.auth") is None:
                raise ImportError
        except ImportError:
            logger.exception("python3-saml がインストールされていません: pip install python3-saml")
            return None

        # python3-saml の実装はここに入る
        # auth = OneLogin_Saml2_Auth(request_data, saml_settings)
        # auth.process_response()
        # ...
        logger.warning("SAML アサーション処理は未実装です")
        return None
