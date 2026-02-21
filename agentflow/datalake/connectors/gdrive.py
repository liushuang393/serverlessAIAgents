"""GoogleDriveConnector - Google Drive API コネクタ.

Google Drive へのアクセスを提供。
Google Drive API v3 を使用。

使用例:
    >>> connector = GoogleDriveConnector()
    >>> items = await connector.list("/My Drive/Documents")
    >>> result = await connector.read("/My Drive/Documents/report.xlsx")

環境変数:
    GOOGLE_CLIENT_ID: クライアントID
    GOOGLE_CLIENT_SECRET: クライアントシークレット
    GOOGLE_REFRESH_TOKEN: リフレッシュトークン
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

# Google Drive MIME types
FOLDER_MIME_TYPE = "application/vnd.google-apps.folder"
GOOGLE_DOCS_EXPORT = {
    "application/vnd.google-apps.document": "application/pdf",
    "application/vnd.google-apps.spreadsheet": "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet",
    "application/vnd.google-apps.presentation": "application/pdf",
}


class GoogleDriveConfig(ConnectorConfig):
    """Google Driveコネクタ設定.

    Attributes:
        client_id: クライアントID
        client_secret: クライアントシークレット
        refresh_token: リフレッシュトークン
        service_account_file: サービスアカウントJSONファイルパス
    """

    client_id: str | None = Field(default=None, description="クライアントID")
    client_secret: str | None = Field(default=None, description="クライアントシークレット")
    refresh_token: str | None = Field(default=None, description="リフレッシュトークン")
    service_account_file: str | None = Field(default=None, description="サービスアカウントJSONファイル")


class GoogleDriveConnector(DataConnector):
    """Google Drive コネクタ.

    gdrive:// スキームでGoogle Driveにアクセス。

    使用例:
        >>> # OAuth2認証
        >>> connector = GoogleDriveConnector()
        >>>
        >>> # サービスアカウント
        >>> connector = GoogleDriveConnector(GoogleDriveConfig(
        ...     service_account_file="/path/to/service-account.json",
        ... ))
    """

    BASE_URL = "https://www.googleapis.com/drive/v3"
    UPLOAD_URL = "https://www.googleapis.com/upload/drive/v3"

    def __init__(
        self,
        config: GoogleDriveConfig | None = None,
        auth_provider: AuthProvider | None = None,
    ) -> None:
        """初期化.

        Args:
            config: コネクタ設定
            auth_provider: 認証プロバイダ
        """
        self._config = config or GoogleDriveConfig()
        self._auth_provider = auth_provider
        self._access_token: str | None = None
        self._token_expires: datetime | None = None
        self._session: Any = None

    @property
    def scheme(self) -> str:
        """URIスキーム."""
        return "gdrive"

    async def _get_session(self) -> Any:
        """HTTPセッションを取得."""
        if self._session is None:
            try:
                import aiohttp
            except ImportError as e:
                msg = "aiohttp is required for Google Drive support. Install with: pip install aiohttp"
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
                creds = await self._auth_provider.get_credentials("gdrive", "")
                if creds.access_token:
                    self._access_token = creds.access_token
                    return self._access_token
            except KeyError:
                pass

        # サービスアカウント認証
        sa_file = self._config.service_account_file or os.getenv("GOOGLE_APPLICATION_CREDENTIALS")
        if sa_file:
            return await self._get_service_account_token(sa_file)

        # OAuth2 リフレッシュトークン認証
        return await self._refresh_oauth_token()

    async def _get_service_account_token(self, sa_file: str) -> str:
        """サービスアカウントトークンを取得."""
        import json
        import time

        try:
            import jwt
        except ImportError as e:
            msg = "PyJWT is required for service account auth. Install with: pip install PyJWT"
            raise ImportError(msg) from e

        with open(sa_file) as f:
            sa_info = json.load(f)

        now = int(time.time())
        payload = {
            "iss": sa_info["client_email"],
            "scope": "https://www.googleapis.com/auth/drive",
            "aud": "https://oauth2.googleapis.com/token",
            "iat": now,
            "exp": now + 3600,
        }

        signed_jwt = jwt.encode(payload, sa_info["private_key"], algorithm="RS256")

        session = await self._get_session()
        async with session.post(
            "https://oauth2.googleapis.com/token",
            data={
                "grant_type": "urn:ietf:params:oauth:grant-type:jwt-bearer",
                "assertion": signed_jwt,
            },
        ) as response:
            response.raise_for_status()
            result = await response.json()
            self._access_token = result["access_token"]
            self._token_expires = datetime.now(UTC).replace(second=datetime.now(UTC).second + 3540)
            return self._access_token

    async def _refresh_oauth_token(self) -> str:
        """OAuth2トークンをリフレッシュ."""
        client_id = self._config.client_id or os.getenv("GOOGLE_CLIENT_ID")
        client_secret = self._config.client_secret or os.getenv("GOOGLE_CLIENT_SECRET")
        refresh_token = self._config.refresh_token or os.getenv("GOOGLE_REFRESH_TOKEN")

        if not all([client_id, client_secret, refresh_token]):
            msg = "Google credentials required. Set GOOGLE_CLIENT_ID, GOOGLE_CLIENT_SECRET, and GOOGLE_REFRESH_TOKEN."
            raise ValueError(msg)

        session = await self._get_session()
        async with session.post(
            "https://oauth2.googleapis.com/token",
            data={
                "client_id": client_id,
                "client_secret": client_secret,
                "refresh_token": refresh_token,
                "grant_type": "refresh_token",
            },
        ) as response:
            response.raise_for_status()
            result = await response.json()
            self._access_token = result["access_token"]
            expires_in = result.get("expires_in", 3600)
            self._token_expires = datetime.now(UTC).replace(second=datetime.now(UTC).second + expires_in - 60)
            return self._access_token

    async def _request(
        self,
        method: str,
        path: str,
        base_url: str | None = None,
        **kwargs: Any,
    ) -> dict[str, Any]:
        """Drive APIリクエスト."""
        session = await self._get_session()
        token = await self._get_access_token()

        headers = kwargs.pop("headers", {})
        headers["Authorization"] = f"Bearer {token}"

        url = f"{base_url or self.BASE_URL}{path}"

        async with session.request(method, url, headers=headers, **kwargs) as response:
            response.raise_for_status()
            if response.content_type == "application/json":
                payload = await response.json()
                return payload if isinstance(payload, dict) else {"data": payload}
            return {"content": await response.read()}

    async def _resolve_path(self, path: str) -> str | None:
        """パスをファイルIDに解決.

        Args:
            path: ファイルパス

        Returns:
            ファイルID（見つからない場合はNone）
        """
        if not path or path == "/":
            return "root"

        parts = [p for p in path.strip("/").split("/") if p]
        parent_id = "root"

        for part in parts:
            query = f"name='{part}' and '{parent_id}' in parents and trashed=false"
            result = await self._request(
                "GET",
                "/files",
                params={"q": query, "fields": "files(id,name,mimeType)"},
            )
            files = result.get("files", [])
            if not files:
                return None
            parent_id = files[0]["id"]

        return parent_id

    async def list(
        self,
        path: str,
        recursive: bool = False,
        pattern: str | None = None,
        limit: int | None = None,
    ) -> list[DataItem]:
        """フォルダ内のアイテム一覧."""
        import fnmatch

        folder_id = await self._resolve_path(path)
        if not folder_id:
            logger.warning(f"Path not found: {path}")
            return []

        items: list[DataItem] = []
        query = f"'{folder_id}' in parents and trashed=false"

        result = await self._request(
            "GET",
            "/files",
            params={
                "q": query,
                "fields": "files(id,name,mimeType,size,modifiedTime,webViewLink)",
                "pageSize": min(limit or 100, 100),
            },
        )

        for file in result.get("files", []):
            name = file["name"]

            if pattern and not fnmatch.fnmatch(name, pattern):
                continue

            is_folder = file["mimeType"] == FOLDER_MIME_TYPE
            modified_at = None
            if "modifiedTime" in file:
                modified_at = datetime.fromisoformat(file["modifiedTime"].replace("Z", "+00:00"))

            items.append(
                DataItem(
                    uri=f"gdrive://{path.rstrip('/')}/{name}",
                    name=name,
                    size=int(file.get("size", 0)) if "size" in file else None,
                    modified_at=modified_at,
                    content_type=file["mimeType"] if not is_folder else None,
                    is_directory=is_folder,
                    metadata={
                        "id": file["id"],
                        "webViewLink": file.get("webViewLink", ""),
                    },
                )
            )

            if limit and len(items) >= limit:
                break

        return items[:limit] if limit else items

    async def read(self, path: str) -> ReadResult:
        """ファイル読み取り."""
        file_id = await self._resolve_path(path)
        if not file_id:
            msg = f"File not found: {path}"
            raise FileNotFoundError(msg)

        # ファイル情報取得
        info = await self._request(
            "GET",
            f"/files/{file_id}",
            params={"fields": "id,name,mimeType,size"},
        )

        mime_type = info["mimeType"]

        # Google Docsの場合はエクスポート
        if mime_type in GOOGLE_DOCS_EXPORT:
            export_mime = GOOGLE_DOCS_EXPORT[mime_type]
            result = await self._request(
                "GET",
                f"/files/{file_id}/export",
                params={"mimeType": export_mime},
            )
            content = result.get("content", b"")
            return ReadResult(
                uri=f"gdrive://{path}",
                content=content,
                content_type=export_mime,
                size=len(content),
            )

        # 通常ファイル
        result = await self._request("GET", f"/files/{file_id}?alt=media")
        content = result.get("content", b"")

        return ReadResult(
            uri=f"gdrive://{path}",
            content=content,
            content_type=mime_type,
            size=len(content),
        )

    async def write(
        self,
        path: str,
        content: bytes | str,
        content_type: str | None = None,
        metadata: dict[str, str] | None = None,
    ) -> DataItem:
        """ファイル書き込み."""
        if isinstance(content, str):
            content = content.encode("utf-8")

        # 親フォルダを解決
        parts = path.strip("/").rsplit("/", 1)
        if len(parts) == 2:
            parent_path, name = parts
            parent_id = await self._resolve_path(parent_path)
        else:
            name = parts[0]
            parent_id = "root"

        if not parent_id:
            msg = f"Parent folder not found: {parent_path}"
            raise FileNotFoundError(msg)

        # 既存ファイルを確認
        existing_id = await self._resolve_path(path)

        session = await self._get_session()
        token = await self._get_access_token()

        headers = {
            "Authorization": f"Bearer {token}",
            "Content-Type": content_type or mimetypes.guess_type(name)[0] or "application/octet-stream",
        }

        if existing_id:
            # 更新
            url = f"{self.UPLOAD_URL}/files/{existing_id}?uploadType=media"
            async with session.patch(url, data=content, headers=headers) as response:
                response.raise_for_status()
                result = await response.json()
        else:
            # 新規作成
            import json

            file_metadata = {"name": name, "parents": [parent_id]}
            boundary = "boundary_string"
            body = (
                (
                    f"--{boundary}\r\n"
                    f"Content-Type: application/json; charset=UTF-8\r\n\r\n"
                    f"{json.dumps(file_metadata)}\r\n"
                    f"--{boundary}\r\n"
                    f"Content-Type: {headers['Content-Type']}\r\n\r\n"
                ).encode()
                + content
                + f"\r\n--{boundary}--".encode()
            )

            headers["Content-Type"] = f"multipart/related; boundary={boundary}"
            url = f"{self.UPLOAD_URL}/files?uploadType=multipart"

            async with session.post(url, data=body, headers=headers) as response:
                response.raise_for_status()
                result = await response.json()

        return DataItem(
            uri=f"gdrive://{path}",
            name=result["name"],
            size=len(content),
            modified_at=datetime.now(UTC),
            content_type=content_type,
            metadata={"id": result["id"]},
        )

    async def exists(self, path: str) -> bool:
        """ファイル存在確認."""
        file_id = await self._resolve_path(path)
        return file_id is not None

    async def delete(self, path: str) -> bool:
        """ファイル削除."""
        file_id = await self._resolve_path(path)
        if not file_id:
            return False

        try:
            session = await self._get_session()
            token = await self._get_access_token()

            url = f"{self.BASE_URL}/files/{file_id}"
            headers = {"Authorization": f"Bearer {token}"}

            async with session.delete(url, headers=headers) as response:
                return bool(response.status == 204)
        except Exception as e:
            logger.warning(f"Failed to delete {path}: {e}")
            return False

    async def close(self) -> None:
        """セッションをクローズ."""
        if self._session:
            await self._session.close()
            self._session = None
