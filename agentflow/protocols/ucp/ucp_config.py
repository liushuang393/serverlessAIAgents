"""UCP設定モデル.

UCPクライアント・サーバーの設定を管理。
"""

from __future__ import annotations

from typing import Any

from pydantic import BaseModel, Field, SecretStr


class UCPEndpoint(BaseModel):
    """UCPエンドポイント設定."""

    name: str = Field(..., description="エンドポイント名")
    url: str = Field(..., description="エンドポイントURL")
    enabled: bool = Field(default=True, description="有効/無効")
    timeout: float = Field(default=30.0, ge=1.0, description="タイムアウト（秒）")
    max_retries: int = Field(default=3, ge=0, le=10, description="最大リトライ回数")
    headers: dict[str, str] = Field(default_factory=dict, description="追加ヘッダー")
    metadata: dict[str, Any] = Field(default_factory=dict, description="メタデータ")

    model_config = {"extra": "allow"}


class UCPSecurityConfig(BaseModel):
    """UCPセキュリティ設定."""

    api_key: SecretStr | None = Field(default=None, description="APIキー")
    auth_token: SecretStr | None = Field(default=None, description="認証トークン")
    enable_audit_log: bool = Field(default=True, description="監査ログ有効化")
    enable_whitelist: bool = Field(default=True, description="ホワイトリスト有効化")
    allowed_actions: list[str] = Field(
        default_factory=lambda: [
            "intent_analysis",
            "offer_request",
            "transaction_create",
            "transaction_update",
        ],
        description="許可アクション",
    )
    rate_limit_per_minute: int = Field(default=60, ge=1, description="レート制限")
    enable_encryption: bool = Field(default=False, description="暗号化有効化")


class UCPConfig(BaseModel):
    """UCP設定."""

    endpoints: list[UCPEndpoint] = Field(
        default_factory=list, description="エンドポイントリスト"
    )
    default_endpoint: str | None = Field(default=None, description="デフォルトエンドポイント")
    security: UCPSecurityConfig = Field(
        default_factory=UCPSecurityConfig, description="セキュリティ設定"
    )
    version: str = Field(default="1.0", description="UCPバージョン")
    enable_caching: bool = Field(default=True, description="キャッシュ有効化")
    cache_ttl: float = Field(default=300.0, ge=0.0, description="キャッシュTTL（秒）")
    log_level: str = Field(default="INFO", description="ログレベル")

    model_config = {"extra": "allow"}

    def get_endpoint(self, name: str | None = None) -> UCPEndpoint | None:
        """エンドポイントを取得.

        Args:
            name: エンドポイント名（Noneの場合はデフォルト）

        Returns:
            UCPEndpointまたはNone
        """
        target_name = name or self.default_endpoint
        if not target_name:
            # 最初の有効なエンドポイントを返す
            for endpoint in self.endpoints:
                if endpoint.enabled:
                    return endpoint
            return None
        for endpoint in self.endpoints:
            if endpoint.name == target_name and endpoint.enabled:
                return endpoint
        return None

    @classmethod
    def from_env(cls) -> UCPConfig:
        """環境変数から設定を読み込み.

        Returns:
            UCPConfig インスタンス
        """
        import os

        endpoints = []
        ucp_url = os.environ.get("UCP_ENDPOINT_URL")
        if ucp_url:
            endpoints.append(
                UCPEndpoint(
                    name="default",
                    url=ucp_url,
                )
            )

        security = UCPSecurityConfig(
            api_key=SecretStr(os.environ.get("UCP_API_KEY", ""))
            if os.environ.get("UCP_API_KEY")
            else None,
            auth_token=SecretStr(os.environ.get("UCP_AUTH_TOKEN", ""))
            if os.environ.get("UCP_AUTH_TOKEN")
            else None,
        )

        return cls(
            endpoints=endpoints,
            default_endpoint="default" if endpoints else None,
            security=security,
        )

