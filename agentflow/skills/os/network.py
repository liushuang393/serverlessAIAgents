# -*- coding: utf-8 -*-
"""ネットワークスキル.

安全なHTTPリクエストAPIを提供。ドメインホワイトリスト制限付き。

Example:
    >>> net = NetworkSkill(config)
    >>> response = await net.http_request("GET", "https://api.example.com/data")
"""

from __future__ import annotations

from dataclasses import dataclass, field
from datetime import datetime
from typing import Any, Literal
from urllib.parse import urlparse

from agentflow.skills.os.base import OSSkillBase, OSSkillError
from agentflow.skills.os.config import OSSkillConfig

# httpx は遅延インポート（オプション依存）
HttpMethod = Literal["GET", "POST", "PUT", "PATCH", "DELETE", "HEAD", "OPTIONS"]


@dataclass
class HttpResponse:
    """HTTP レスポンス."""

    status_code: int
    headers: dict[str, str]
    body: str
    url: str
    method: str
    duration_ms: float = 0.0
    requested_at: datetime = field(default_factory=datetime.now)

    @property
    def success(self) -> bool:
        """成功レスポンスか."""
        return 200 <= self.status_code < 300

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "status_code": self.status_code,
            "headers": self.headers,
            "body": self.body[:1000] if len(self.body) > 1000 else self.body,
            "body_truncated": len(self.body) > 1000,
            "url": self.url,
            "method": self.method,
            "success": self.success,
            "duration_ms": self.duration_ms,
        }


class NetworkSecurityError(OSSkillError):
    """ネットワークセキュリティ違反."""

    pass


class NetworkSkill(OSSkillBase):
    """ネットワークスキル.

    ドメインホワイトリストに基づく安全なHTTP通信を提供。
    """

    def __init__(self, config: OSSkillConfig | None = None) -> None:
        """初期化."""
        super().__init__(config)
        self._client: Any = None

    def _get_client(self) -> Any:
        """httpx クライアントを取得（遅延初期化）."""
        if self._client is None:
            try:
                import httpx
                self._client = httpx.AsyncClient(timeout=self._config.max_timeout_seconds)
            except ImportError:
                msg = "httpx がインストールされていません: pip install httpx"
                raise OSSkillError(msg, skill_name="NetworkSkill")
        return self._client

    def _validate_url(self, url: str) -> str:
        """URLを検証.

        Args:
            url: 検証するURL

        Returns:
            検証済みURL

        Raises:
            NetworkSecurityError: ドメインが許可されていない場合
        """
        parsed = urlparse(url)

        if not parsed.scheme or parsed.scheme not in ("http", "https"):
            msg = f"無効なURLスキーム: {parsed.scheme}"
            raise NetworkSecurityError(msg, skill_name="NetworkSkill")

        domain = parsed.netloc.split(":")[0]  # ポート番号を除去

        if not self._config.is_domain_allowed(domain):
            msg = f"ドメイン '{domain}' は許可されていません"
            self._logger.warning(msg)
            raise NetworkSecurityError(
                msg,
                skill_name="NetworkSkill",
                details={"domain": domain, "whitelist": self._config.domain_whitelist},
            )

        return url

    async def http_request(
        self,
        method: HttpMethod,
        url: str,
        headers: dict[str, str] | None = None,
        body: str | dict[str, Any] | None = None,
        timeout: float | None = None,
    ) -> HttpResponse:
        """HTTP リクエストを送信.

        Args:
            method: HTTP メソッド
            url: リクエストURL
            headers: リクエストヘッダー
            body: リクエストボディ
            timeout: タイムアウト秒

        Returns:
            HTTP レスポンス

        Raises:
            NetworkSecurityError: ドメインが許可されていない場合
            OSSkillError: リクエストエラーの場合
        """
        import asyncio

        validated_url = self._validate_url(url)
        timeout = timeout or self._config.max_timeout_seconds

        self._audit_log("http_request", {
            "method": method,
            "url": validated_url,
            "has_body": body is not None,
        })

        client = self._get_client()
        start_time = asyncio.get_event_loop().time()

        try:
            # リクエスト準備
            kwargs: dict[str, Any] = {
                "method": method,
                "url": validated_url,
                "headers": headers or {},
                "timeout": timeout,
            }

            if body is not None:
                if isinstance(body, dict):
                    kwargs["json"] = body
                else:
                    kwargs["content"] = body

            response = await client.request(**kwargs)
            duration_ms = (asyncio.get_event_loop().time() - start_time) * 1000

            return HttpResponse(
                status_code=response.status_code,
                headers=dict(response.headers),
                body=response.text,
                url=validated_url,
                method=method,
                duration_ms=duration_ms,
            )

        except Exception as e:
            msg = f"HTTPリクエスト失敗: {e}"
            raise OSSkillError(msg, skill_name="NetworkSkill")

    async def close(self) -> None:
        """クライアントを閉じる."""
        if self._client is not None:
            await self._client.aclose()
            self._client = None

    async def __aenter__(self) -> "NetworkSkill":
        """async with サポート."""
        return self

    async def __aexit__(self, exc_type: Any, exc_val: Any, exc_tb: Any) -> None:
        """クリーンアップ."""
        await self.close()

