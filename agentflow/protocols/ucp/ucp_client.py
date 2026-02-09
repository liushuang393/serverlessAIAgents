"""UCPクライアント実装.

Universal Commerce Protocol のクライアント。
リモートUCPサービスとの通信を管理。

設計原則:
- 非同期I/O: httpxを使用した非同期HTTP通信
- リトライロジック: 指数バックオフによる再試行
- セキュリティ: 認証・監査ログ・レート制限
- キャッシュ: レスポンスキャッシュによるパフォーマンス最適化
"""

from __future__ import annotations

import asyncio
import logging
import time
import uuid
from typing import TYPE_CHECKING, Any

import httpx

from agentflow.protocols.ucp.ucp_messages import (
    UCPIntentRequest,
    UCPIntentResponse,
    UCPMessage,
    UCPOfferRequest,
    UCPOfferResponse,
    UCPTransactionRequest,
    UCPTransactionResponse,
)


if TYPE_CHECKING:
    from agentflow.protocols.ucp.ucp_config import UCPConfig, UCPEndpoint


class UCPClient:
    """UCPクライアント.

    Universal Commerce Protocolのクライアント実装。
    リモートUCPサービスとの通信、意図分析、オファー取得、トランザクション処理を行う。

    Example:
        >>> config = UCPConfig.from_env()
        >>> async with UCPClient(config) as client:
        ...     intent = await client.analyze_intent("ノートPCが欲しい")
        ...     offers = await client.get_offers(intent.intent_id)
    """

    def __init__(
        self,
        config: UCPConfig,
        *,
        logger: logging.Logger | None = None,
    ) -> None:
        """UCPクライアントを初期化.

        Args:
            config: UCP設定
            logger: ロガーインスタンス（オプション）
        """
        self._config = config
        self._logger = logger or logging.getLogger(__name__)
        self._http_client: httpx.AsyncClient | None = None
        self._cache: dict[str, tuple[Any, float]] = {}
        self._request_count = 0
        self._last_reset_time = time.time()

    async def _get_http_client(self) -> httpx.AsyncClient:
        """HTTPクライアントを取得（遅延初期化）."""
        if self._http_client is None:
            endpoint = self._config.get_endpoint()
            timeout = endpoint.timeout if endpoint else 30.0
            self._http_client = httpx.AsyncClient(timeout=timeout)
        return self._http_client

    async def close(self) -> None:
        """クライアントをクローズ."""
        if self._http_client:
            await self._http_client.aclose()
            self._http_client = None

    async def __aenter__(self) -> UCPClient:
        """非同期コンテキストマネージャーのエントリー."""
        return self

    async def __aexit__(self, *_args: Any) -> None:
        """非同期コンテキストマネージャーの終了."""
        await self.close()

    def _generate_message_id(self) -> str:
        """メッセージIDを生成."""
        return f"ucp-{uuid.uuid4().hex[:12]}"

    def _check_rate_limit(self) -> bool:
        """レート制限をチェック.

        Returns:
            制限内ならTrue、超過ならFalse
        """
        current_time = time.time()
        if current_time - self._last_reset_time >= 60:
            self._request_count = 0
            self._last_reset_time = current_time
        return self._request_count < self._config.security.rate_limit_per_minute

    def _get_cache_key(self, operation: str, params: dict[str, Any]) -> str:
        """キャッシュキーを生成."""
        import hashlib
        import json
        param_str = json.dumps(params, sort_keys=True)
        return f"{operation}:{hashlib.md5(param_str.encode()).hexdigest()}"

    def _get_cached(self, key: str) -> Any | None:
        """キャッシュから取得."""
        if not self._config.enable_caching:
            return None
        if key in self._cache:
            value, timestamp = self._cache[key]
            if time.time() - timestamp < self._config.cache_ttl:
                return value
            del self._cache[key]
        return None

    def _set_cache(self, key: str, value: Any) -> None:
        """キャッシュに設定."""
        if self._config.enable_caching:
            self._cache[key] = (value, time.time())

    def _build_headers(self, endpoint: UCPEndpoint) -> dict[str, str]:
        """リクエストヘッダーを構築."""
        headers = {
            "Content-Type": "application/json",
            "X-UCP-Version": self._config.version,
            **endpoint.headers,
        }
        if self._config.security.api_key:
            headers["X-API-Key"] = self._config.security.api_key.get_secret_value()
        if self._config.security.auth_token:
            headers["Authorization"] = (
                f"Bearer {self._config.security.auth_token.get_secret_value()}"
            )
        return headers

    async def _send_request(
        self,
        endpoint: UCPEndpoint,
        path: str,
        message: UCPMessage,
    ) -> dict[str, Any]:
        """リクエストを送信（リトライロジック付き）.

        Args:
            endpoint: エンドポイント設定
            path: APIパス
            message: UCPメッセージ

        Returns:
            レスポンスデータ

        Raises:
            httpx.HTTPError: HTTP通信エラー
            ValueError: レート制限超過
        """
        if not self._check_rate_limit():
            msg = "Rate limit exceeded"
            raise ValueError(msg)

        self._request_count += 1
        client = await self._get_http_client()
        headers = self._build_headers(endpoint)
        url = f"{endpoint.url.rstrip('/')}/{path.lstrip('/')}"
        last_error: Exception | None = None

        for attempt in range(endpoint.max_retries):
            try:
                response = await client.post(
                    url,
                    json=message.model_dump(mode="json"),
                    headers=headers,
                )
                response.raise_for_status()
                return response.json()
            except httpx.HTTPError as e:
                last_error = e
                if attempt < endpoint.max_retries - 1:
                    wait_time = 2 ** attempt
                    self._logger.warning(
                        f"UCP request failed (attempt {attempt + 1}), "
                        f"retrying in {wait_time}s: {e}"
                    )
                    await asyncio.sleep(wait_time)

        self._logger.error(f"UCP request failed after {endpoint.max_retries} attempts")
        raise last_error or RuntimeError("Request failed")

    async def analyze_intent(
        self,
        user_input: str,
        *,
        user_id: str | None = None,
        session_id: str | None = None,
        conversation_history: list[dict[str, str]] | None = None,
        context: dict[str, Any] | None = None,
    ) -> UCPIntentResponse:
        """購買意図を分析.

        Args:
            user_input: ユーザー入力テキスト
            user_id: ユーザーID（オプション）
            session_id: セッションID（オプション）
            conversation_history: 会話履歴（オプション）
            context: 追加コンテキスト（オプション）

        Returns:
            意図分析レスポンス

        Raises:
            ValueError: エンドポイントが見つからない場合
        """
        endpoint = self._config.get_endpoint()
        if not endpoint:
            msg = "No available UCP endpoint"
            raise ValueError(msg)

        request = UCPIntentRequest(
            message_id=self._generate_message_id(),
            user_input=user_input,
            user_id=user_id,
            session_id=session_id,
            conversation_history=conversation_history or [],
            context=context or {},
        )

        # キャッシュチェック
        cache_key = self._get_cache_key("intent", {"input": user_input})
        cached = self._get_cached(cache_key)
        if cached:
            self._logger.debug(f"Cache hit for intent analysis: {cache_key}")
            return cached

        self._logger.info(f"Analyzing intent: {user_input[:50]}...")
        response_data = await self._send_request(endpoint, "/intent/analyze", request)
        response = UCPIntentResponse(**response_data)

        self._set_cache(cache_key, response)
        return response

    async def get_offers(
        self,
        intent_id: str,
        *,
        user_id: str | None = None,
        product_ids: list[str] | None = None,
        keywords: list[str] | None = None,
        limit: int = 10,
        filters: dict[str, Any] | None = None,
    ) -> UCPOfferResponse:
        """オファーを取得.

        Args:
            intent_id: 意図ID
            user_id: ユーザーID（オプション）
            product_ids: 商品IDリスト（オプション）
            keywords: キーワード（オプション）
            limit: 最大取得数
            filters: フィルター条件（オプション）

        Returns:
            オファー取得レスポンス
        """
        endpoint = self._config.get_endpoint()
        if not endpoint:
            msg = "No available UCP endpoint"
            raise ValueError(msg)

        request = UCPOfferRequest(
            message_id=self._generate_message_id(),
            intent_id=intent_id,
            user_id=user_id,
            product_ids=product_ids or [],
            keywords=keywords or [],
            limit=limit,
            filters=filters or {},
        )

        self._logger.info(f"Getting offers for intent: {intent_id}")
        response_data = await self._send_request(endpoint, "/offers/get", request)
        return UCPOfferResponse(**response_data)

    async def create_transaction(
        self,
        cart_id: str,
        items: list[dict[str, Any]],
        *,
        offers: list[str] | None = None,
    ) -> UCPTransactionResponse:
        """トランザクションを作成.

        Args:
            cart_id: カートID
            items: 商品アイテムリスト
            offers: 適用オファーID（オプション）

        Returns:
            トランザクションレスポンス
        """
        endpoint = self._config.get_endpoint()
        if not endpoint:
            msg = "No available UCP endpoint"
            raise ValueError(msg)

        request = UCPTransactionRequest(
            message_id=self._generate_message_id(),
            action="create",
            cart_id=cart_id,
            items=items,
            offers=offers or [],
        )

        self._logger.info(f"Creating transaction for cart: {cart_id}")
        response_data = await self._send_request(endpoint, "/transaction/create", request)
        return UCPTransactionResponse(**response_data)

    async def complete_transaction(
        self,
        transaction_id: str,
        payment_method: str,
        payment_details: dict[str, Any],
    ) -> UCPTransactionResponse:
        """トランザクションを完了.

        Args:
            transaction_id: トランザクションID
            payment_method: 決済方法
            payment_details: 決済詳細

        Returns:
            トランザクションレスポンス
        """
        endpoint = self._config.get_endpoint()
        if not endpoint:
            msg = "No available UCP endpoint"
            raise ValueError(msg)

        request = UCPTransactionRequest(
            message_id=self._generate_message_id(),
            action="complete",
            transaction_id=transaction_id,
            payment_method=payment_method,
            payment_details=payment_details,
        )

        self._logger.info(f"Completing transaction: {transaction_id}")
        response_data = await self._send_request(endpoint, "/transaction/complete", request)
        return UCPTransactionResponse(**response_data)

    async def health_check(self) -> dict[str, Any]:
        """ヘルスチェック.

        Returns:
            ヘルスチェック結果
        """
        endpoint = self._config.get_endpoint()
        if not endpoint:
            return {"status": "error", "message": "No available endpoint"}

        try:
            client = await self._get_http_client()
            response = await client.get(
                f"{endpoint.url.rstrip('/')}/health",
                headers=self._build_headers(endpoint),
            )
            return {"status": "ok", "endpoint": endpoint.name, "data": response.json()}
        except Exception as e:
            return {"status": "error", "endpoint": endpoint.name, "message": str(e)}

