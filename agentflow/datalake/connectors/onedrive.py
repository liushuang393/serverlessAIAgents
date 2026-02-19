from __future__ import annotations

"""OneDriveConnector - Microsoft Graph API コネクタ.

OneDrive / SharePoint へのアクセスを提供。
Microsoft Graph API を使用。

使用例:
    >>> connector = OneDriveConnector()
    >>> items = await connector.list("/Documents")
    >>> result = await connector.read("/Documents/report.xlsx")

環境変数:
    MICROSOFT_CLIENT_ID: アプリケーションID
    MICROSOFT_CLIENT_SECRET: クライアントシークレット
    MICROSOFT_TENANT_ID: テナントID
"""

import logging
import mimetypes
import os
from datetime import UTC, datetime
from typing import Any

from pydantic import Field

from agentflow.datalake.auth import AuthProvider
from agentflow.datalake.connector import ConnectorConfig, DataConnector
from agentflow.datalake.core import DataItem, ReadResult


logger = logging.getLogger(__name__)


class OneDriveConfig(ConnectorConfig):
    """OneDriveコネクタ設定.

    Attributes:
        client_id: アプリケーションID
        client_secret: クライアントシークレット
        tenant_id: テナントID
        drive_id: ドライブID（省略時はデフォルトドライブ）
        site_id: SharePointサイトID（SharePoint使用時）
    """

    client_id: str | None = Field(default=None, description="アプリケーションID")
    client_secret: str | None = Field(default=None, description="クライアントシークレット")
    tenant_id: str | None = Field(default=None, description="テナントID")
    drive_id: str | None = Field(default=None, description="ドライブID")
    site_id: str | None = Field(default=None, description="SharePointサイトID")
    scopes: list[str] = Field(
        default_factory=lambda: ["https://graph.microsoft.com/.default"],
        description="認証スコープ",
    )


class OneDriveConnector(DataConnector):
    """OneDrive / SharePoint コネクタ.

    onedrive:// スキームでOneDrive/SharePointにアクセス。

    使用例:
        >>> # 個人用OneDrive
        >>> connector = OneDriveConnector()
        >>>
        >>> # SharePoint
        >>> connector = OneDriveConnector(OneDriveConfig(
        ...     site_id="your-site-id",
        ... ))
    """

    def __init__(
        self,
        config: OneDriveConfig | None = None,
        auth_provider: AuthProvider | None = None,
    ) -> None:
        """初期化.

        Args:
            config: コネクタ設定
            auth_provider: 認証プロバイダ
        """
        self._config = config or OneDriveConfig()
        self._auth_provider = auth_provider
        self._access_token: str | None = None
        self._token_expires: datetime | None = None
        self._session = None

    @property
    def scheme(self) -> str:
        """URIスキーム."""
        return "onedrive"

    async def _get_session(self):
        """HTTPセッションを取得."""
        if self._session is None:
            try:
                import aiohttp
            except ImportError as e:
                msg = "aiohttp is required for OneDrive support. Install with: pip install aiohttp"
                raise ImportError(msg) from e

            timeout = aiohttp.ClientTimeout(total=self._config.timeout)
            self._session = aiohttp.ClientSession(timeout=timeout)

        return self._session

    async def _get_access_token(self) -> str:
        """アクセストークンを取得.

        Returns:
            アクセストークン
        """
        # トークンが有効な場合は再利用
        if self._access_token and self._token_expires:
            if datetime.now(UTC) < self._token_expires:
                return self._access_token

        # 認証プロバイダから取得
        if self._auth_provider:
            try:
                creds = await self._auth_provider.get_credentials("onedrive", "")
                if creds.access_token:
                    self._access_token = creds.access_token
                    return self._access_token
            except KeyError:
                pass

        # OAuth2 クライアント認証
        client_id = self._config.client_id or os.getenv("MICROSOFT_CLIENT_ID")
        client_secret = self._config.client_secret or os.getenv("MICROSOFT_CLIENT_SECRET")
        tenant_id = self._config.tenant_id or os.getenv("MICROSOFT_TENANT_ID", "common")

        if not client_id or not client_secret:
            msg = (
                "Microsoft credentials required. Set MICROSOFT_CLIENT_ID and "
                "MICROSOFT_CLIENT_SECRET environment variables."
            )
            raise ValueError(msg)

        session = await self._get_session()
        token_url = f"https://login.microsoftonline.com/{tenant_id}/oauth2/v2.0/token"

        data = {
            "client_id": client_id,
            "client_secret": client_secret,
            "scope": " ".join(self._config.scopes),
            "grant_type": "client_credentials",
        }

        async with session.post(token_url, data=data) as response:
            response.raise_for_status()
            result = await response.json()

            self._access_token = result["access_token"]
            expires_in = result.get("expires_in", 3600)
            self._token_expires = datetime.now(UTC).replace(
                second=datetime.now(UTC).second + expires_in - 60
            )

            return self._access_token

    def _get_base_url(self) -> str:
        """Graph APIベースURLを取得."""
        if self._config.site_id:
            # SharePoint
            base = f"https://graph.microsoft.com/v1.0/sites/{self._config.site_id}"
            if self._config.drive_id:
                return f"{base}/drives/{self._config.drive_id}"
            return f"{base}/drive"
        if self._config.drive_id:
            return f"https://graph.microsoft.com/v1.0/drives/{self._config.drive_id}"
        return "https://graph.microsoft.com/v1.0/me/drive"

    async def _request(
        self,
        method: str,
        path: str,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """Graph APIリクエスト.

        Args:
            method: HTTPメソッド
            path: APIパス
            **kwargs: 追加パラメータ

        Returns:
            レスポンスJSON
        """
        session = await self._get_session()
        token = await self._get_access_token()

        headers = kwargs.pop("headers", {})
        headers["Authorization"] = f"Bearer {token}"

        url = f"{self._get_base_url()}{path}"

        async with session.request(method, url, headers=headers, **kwargs) as response:
            response.raise_for_status()
            if response.content_type == "application/json":
                return await response.json()
            return {"content": await response.read()}

    async def list(
        self,
        path: str,
        recursive: bool = False,
        pattern: str | None = None,
        limit: int | None = None,
    ) -> list[DataItem]:
        """フォルダ内のアイテム一覧.

        Args:
            path: フォルダパス
            recursive: 再帰的に取得
            pattern: フィルタパターン
            limit: 最大件数

        Returns:
            DataItemのリスト
        """
        import fnmatch

        items: list[DataItem] = []
        api_path = f"/root:{path}:/children" if path and path != "/" else "/root/children"

        result = await self._request("GET", api_path)

        for item in result.get("value", []):
            name = item["name"]

            if pattern and not fnmatch.fnmatch(name, pattern):
                continue

            is_folder = "folder" in item
            modified_at = None
            if "lastModifiedDateTime" in item:
                modified_at = datetime.fromisoformat(
                    item["lastModifiedDateTime"].replace("Z", "+00:00")
                )

            items.append(
                DataItem(
                    uri=f"onedrive://{item.get('parentReference', {}).get('path', '')}/{name}",
                    name=name,
                    size=item.get("size"),
                    modified_at=modified_at,
                    content_type=item.get("file", {}).get("mimeType"),
                    is_directory=is_folder,
                    metadata={
                        "id": item["id"],
                        "webUrl": item.get("webUrl", ""),
                    },
                )
            )

            if limit and len(items) >= limit:
                break

        # 再帰的取得
        if recursive:
            folders = [i for i in items if i.is_directory]
            for folder in folders:
                if limit and len(items) >= limit:
                    break
                sub_path = f"{path.rstrip('/')}/{folder.name}"
                sub_items = await self.list(sub_path, recursive=True, pattern=pattern)
                items.extend(sub_items)

        return items[:limit] if limit else items

    async def read(self, path: str) -> ReadResult:
        """ファイル読み取り.

        Args:
            path: ファイルパス

        Returns:
            ReadResult
        """
        api_path = f"/root:{path}:/content"
        result = await self._request("GET", api_path)

        content = result.get("content", b"")
        content_type = mimetypes.guess_type(path)[0] or "application/octet-stream"

        return ReadResult(
            uri=f"onedrive://{path}",
            content=content,
            content_type=content_type,
            size=len(content),
        )

    async def write(
        self,
        path: str,
        content: bytes | str,
        content_type: str | None = None,
        metadata: dict[str, str] | None = None,
    ) -> DataItem:
        """ファイル書き込み.

        Args:
            path: ファイルパス
            content: 書き込み内容
            content_type: Content-Type
            metadata: メタデータ

        Returns:
            DataItem
        """
        session = await self._get_session()
        token = await self._get_access_token()

        if isinstance(content, str):
            content = content.encode("utf-8")

        api_path = f"/root:{path}:/content"
        url = f"{self._get_base_url()}{api_path}"

        headers = {
            "Authorization": f"Bearer {token}",
            "Content-Type": content_type or "application/octet-stream",
        }

        async with session.put(url, data=content, headers=headers) as response:
            response.raise_for_status()
            result = await response.json()

            return DataItem(
                uri=f"onedrive://{path}",
                name=result["name"],
                size=result.get("size"),
                modified_at=datetime.now(UTC),
                content_type=content_type,
                metadata={"id": result["id"]},
            )

    async def exists(self, path: str) -> bool:
        """ファイル存在確認."""
        try:
            api_path = f"/root:{path}"
            await self._request("GET", api_path)
            return True
        except Exception:
            return False

    async def delete(self, path: str) -> bool:
        """ファイル削除."""
        try:
            session = await self._get_session()
            token = await self._get_access_token()

            api_path = f"/root:{path}"
            url = f"{self._get_base_url()}{api_path}"

            headers = {"Authorization": f"Bearer {token}"}

            async with session.delete(url, headers=headers) as response:
                return response.status == 204
        except Exception as e:
            logger.warning(f"Failed to delete {path}: {e}")
            return False

    async def close(self) -> None:
        """セッションをクローズ."""
        if self._session:
            await self._session.close()
            self._session = None
