from __future__ import annotations


"""RestAPIConnector - REST/GraphQL APIコネクタ.

REST APIおよびGraphQL APIへのアクセスを提供。

使用例:
    >>> connector = RestAPIConnector()
    >>> result = await connector.read("https://api.example.com/users/1")
    >>> data = await connector.query("https://api.example.com/users", "name=John")
"""

import contextlib
import json
import logging
from typing import TYPE_CHECKING, Any
from urllib.parse import parse_qs

from pydantic import Field

from agentflow.datalake.connector import ConnectorConfig, DataConnector
from agentflow.datalake.core import DataItem, ReadResult


if TYPE_CHECKING:
    import builtins

    from agentflow.datalake.auth import AuthProvider


logger = logging.getLogger(__name__)


class RestAPIConfig(ConnectorConfig):
    """REST APIコネクタ設定.

    Attributes:
        base_url: ベースURL
        default_headers: デフォルトヘッダー
        auth_header_name: 認証ヘッダー名
    """

    base_url: str | None = Field(default=None, description="ベースURL")
    default_headers: dict[str, str] = Field(default_factory=dict, description="デフォルトヘッダー")
    auth_header_name: str = Field(default="Authorization", description="認証ヘッダー名")


class RestAPIConnector(DataConnector):
    """REST/GraphQL APIコネクタ.

    rest://, http://, https:// スキームでAPIにアクセス。

    使用例:
        >>> connector = RestAPIConnector()
        >>> result = await connector.read("https://api.example.com/users/1")
        >>> # POSTリクエスト
        >>> await connector.write(
        ...     "https://api.example.com/users",
        ...     {"name": "John"},
        ... )
    """

    def __init__(
        self,
        config: RestAPIConfig | None = None,
        auth_provider: AuthProvider | None = None,
    ) -> None:
        """初期化.

        Args:
            config: コネクタ設定
            auth_provider: 認証プロバイダ
        """
        self._config = config or RestAPIConfig()
        self._auth_provider = auth_provider
        self._session: Any = None

    @property
    def scheme(self) -> str:
        """URIスキーム."""
        return "rest"

    async def _get_session(self) -> Any:
        """HTTPセッションを取得.

        Returns:
            aiohttp.ClientSession
        """
        if self._session is None:
            try:
                import aiohttp
            except ImportError as e:
                msg = "aiohttp is required for REST API support. Install with: pip install aiohttp"
                raise ImportError(msg) from e

            timeout = aiohttp.ClientTimeout(total=self._config.timeout)
            self._session = aiohttp.ClientSession(
                headers=self._config.default_headers,
                timeout=timeout,
            )

        return self._session

    def _build_url(self, path: str) -> str:
        """完全URLを構築.

        Args:
            path: パス

        Returns:
            完全URL
        """
        # rest:// を https:// に変換
        if path.startswith("rest://"):
            path = "https://" + path[7:]

        # ベースURLがあれば結合
        if self._config.base_url and not path.startswith(("http://", "https://")):
            base = self._config.base_url.rstrip("/")
            path = f"{base}/{path.lstrip('/')}"

        return path

    async def _get_auth_headers(self, url: str) -> dict[str, str]:
        """認証ヘッダーを取得.

        Args:
            url: リクエストURL

        Returns:
            認証ヘッダー辞書
        """
        headers: dict[str, str] = {}

        if self._auth_provider:
            try:
                creds = await self._auth_provider.get_credentials("rest", url)

                if creds.access_token:
                    headers[self._config.auth_header_name] = f"Bearer {creds.access_token}"
                elif creds.api_key:
                    headers[self._config.auth_header_name] = creds.api_key
                elif creds.username and creds.password:
                    import base64

                    auth = base64.b64encode(f"{creds.username}:{creds.password}".encode()).decode()
                    headers[self._config.auth_header_name] = f"Basic {auth}"

            except KeyError:
                pass

        return headers

    async def list(
        self,
        path: str,
        recursive: bool = False,
        pattern: str | None = None,
        limit: int | None = None,
    ) -> list[DataItem]:
        """APIエンドポイント一覧（REST APIでは通常サポートされない）.

        Args:
            path: パス
            recursive: 再帰的に取得
            pattern: フィルタパターン
            limit: 最大件数

        Returns:
            空のリスト（REST APIでは一覧取得は通常 query() で行う）
        """
        logger.warning("list() is not typically supported for REST API. Use query() instead.")
        return []

    async def read(self, path: str) -> ReadResult:
        """GETリクエストでデータ取得.

        Args:
            path: URL/パス

        Returns:
            ReadResult
        """
        session = await self._get_session()
        url = self._build_url(path)
        headers = await self._get_auth_headers(url)

        async with session.get(url, headers=headers) as response:
            response.raise_for_status()
            content = await response.read()
            content_type = response.headers.get("Content-Type", "application/json")

            # JSONの場合はパース
            parsed_content: Any = content
            if "application/json" in content_type:
                with contextlib.suppress(json.JSONDecodeError):
                    parsed_content = json.loads(content)

            return ReadResult(
                uri=url,
                content=parsed_content,
                content_type=content_type,
                size=len(content),
                metadata={
                    "status_code": response.status,
                    "headers": dict(response.headers),
                },
            )

    async def write(
        self,
        path: str,
        content: bytes | str | dict[str, Any] | builtins.list[Any],
        content_type: str | None = None,
        metadata: dict[str, str] | None = None,
    ) -> DataItem:
        """POSTリクエストでデータ送信.

        Args:
            path: URL/パス
            content: 送信内容
            content_type: Content-Type
            metadata: 追加ヘッダー

        Returns:
            DataItem
        """
        session = await self._get_session()
        url = self._build_url(path)
        headers = await self._get_auth_headers(url)

        if metadata:
            headers.update(metadata)

        # コンテンツ準備
        if isinstance(content, (dict, list)):
            data = json.dumps(content, ensure_ascii=False)
            headers.setdefault("Content-Type", "application/json")
        elif isinstance(content, str):
            data = content
        else:
            data = content.decode("utf-8") if isinstance(content, bytes) else str(content)

        async with session.post(url, data=data, headers=headers) as response:
            response.raise_for_status()
            response_content = await response.read()

            return DataItem(
                uri=url,
                name=path.rsplit("/", maxsplit=1)[-1],
                size=len(response_content),
                metadata={
                    "status_code": response.status,
                    "response": response_content.decode("utf-8", errors="replace"),
                },
            )

    async def exists(self, path: str) -> bool:
        """HEADリクエストで存在確認.

        Args:
            path: URL/パス

        Returns:
            存在する場合True
        """
        session = await self._get_session()
        url = self._build_url(path)
        headers = await self._get_auth_headers(url)

        try:
            async with session.head(url, headers=headers) as response:
                return bool(response.status < 400)
        except Exception:
            return False

    async def delete(self, path: str) -> bool:
        """DELETEリクエスト.

        Args:
            path: URL/パス

        Returns:
            成功の場合True
        """
        session = await self._get_session()
        url = self._build_url(path)
        headers = await self._get_auth_headers(url)

        try:
            async with session.delete(url, headers=headers) as response:
                return bool(response.status < 400)
        except Exception as e:
            logger.warning(f"DELETE failed for {url}: {e}")
            return False

    async def query(
        self,
        path: str,
        query: str | dict[str, Any],
        method: str = "GET",
        **kwargs: Any,
    ) -> builtins.list[dict[str, Any]]:
        """APIクエリ実行.

        Args:
            path: URL/パス
            query: クエリパラメータ（文字列 or 辞書）
            method: HTTPメソッド
            **kwargs: 追加パラメータ

        Returns:
            レスポンスデータのリスト
        """
        session = await self._get_session()
        url = self._build_url(path)
        headers = await self._get_auth_headers(url)

        # クエリパラメータ処理
        if isinstance(query, str):
            # "key=value&key2=value2" 形式
            raw_params = parse_qs(query, keep_blank_values=True)
            params: dict[str, Any] = {
                key: values[0] if len(values) == 1 else values for key, values in raw_params.items()
            }
        else:
            params = query

        if method.upper() == "GET":
            async with session.get(url, params=params, headers=headers) as response:
                response.raise_for_status()
                data = await response.json()
        elif method.upper() == "POST":
            async with session.post(url, json=params, headers=headers) as response:
                response.raise_for_status()
                data = await response.json()
        else:
            msg = f"Unsupported method: {method}"
            raise ValueError(msg)

        # リストでない場合はラップ
        if isinstance(data, list):
            return [item for item in data if isinstance(item, dict)]
        if isinstance(data, dict):
            return [data]
        return [{"value": data}]

    async def graphql(
        self,
        endpoint: str,
        query: str,
        variables: dict[str, Any] | None = None,
    ) -> dict[str, Any]:
        """GraphQLクエリ実行.

        Args:
            endpoint: GraphQLエンドポイント
            query: GraphQLクエリ
            variables: 変数

        Returns:
            レスポンスデータ
        """
        session = await self._get_session()
        url = self._build_url(endpoint)
        headers = await self._get_auth_headers(url)
        headers["Content-Type"] = "application/json"

        payload: dict[str, Any] = {"query": query}
        if variables:
            payload["variables"] = variables

        async with session.post(url, json=payload, headers=headers) as response:
            response.raise_for_status()
            result = await response.json()

            if isinstance(result, dict) and "errors" in result:
                logger.warning(f"GraphQL errors: {result['errors']}")

            if isinstance(result, dict):
                data = result.get("data", {})
                return data if isinstance(data, dict) else {}
            return {}

    async def close(self) -> None:
        """セッションをクローズ."""
        if self._session:
            await self._session.close()
            self._session = None
