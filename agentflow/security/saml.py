from abc import ABC, abstractmethod
from typing import Dict, Any
from pydantic import BaseModel

class SAMLIdentity(BaseModel):
     """SAML 認証で取得されたアイデンティティ."""
     name_id: str
     attributes: Dict[str, list]
     session_index: str | None = None

class SAMLProvider:
    """SAML プロバイダーの実装クラス."""

    def __init__(self, settings: Dict[str, Any]):
        """
        python3_saml の設定を受け取る.
        settings 構造:
        {
            "strict": True,
            "debug": True,
            "sp": { ... },
            "idp": { ... }
        }
        """
        try:
            from onelogin.saml2.auth import OneLogin_Saml2_Auth
            self.auth_class = OneLogin_Saml2_Auth
        except ImportError:
            raise ImportError("python3-saml is not installed. Please run 'pip install python3-saml'")
            
        self.settings = settings

    def prepare_request(self, request_data: Dict[str, Any]) -> Dict[str, Any]:
        """FastAPI/Flask のリクエストを python3-saml 用に変換."""
        # 通常は https か http か、ホスト、パス、GET/POST パラメータを含む
        return {
            'https': 'on' if request_data.get('https') else 'off',
            'http_host': request_data.get('http_host'),
            'server_port': request_data.get('server_port'),
            'script_name': request_data.get('script_name'),
            'get_data': request_data.get('get_data', {}),
            'post_data': request_data.get('post_data', {}),
            'query_string': request_data.get('query_string', '')
        }

    def get_auth(self, saml_prepare: Dict[str, Any]):
        """OneLogin_Saml2_Auth インスタンスを取得."""
        return self.auth_class(saml_prepare, self.settings)

    async def get_sso_url(self, saml_prepare: Dict[str, Any], return_to: str | None = None) -> str:
        """SSO 開始用のリダイレクト URL を取得."""
        auth = self.get_auth(saml_prepare)
        return auth.login(return_to=return_to)

    async def get_slo_url(self, saml_prepare: Dict[str, Any], return_to: str | None = None) -> str:
        """SLO 開始用のリダイレクト URL を取得."""
        auth = self.get_auth(saml_prepare)
        return auth.logout(return_to=return_to)

    async def process_response(self, saml_prepare: Dict[str, Any]) -> SAMLIdentity | None:
        """ACS (Assertion Consumer Service) でのレスポンス処理."""
        auth = self.get_auth(saml_prepare)
        auth.process_response()
        errors = auth.get_errors()
        if not errors and auth.is_authenticated():
            return SAMLIdentity(
                name_id=auth.get_nameid(),
                attributes=auth.get_attributes(),
                session_index=auth.get_session_index()
            )
        return None
