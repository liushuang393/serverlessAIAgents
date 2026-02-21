"""A2A クライアント実装.

このモジュールはリモート A2A エージェントを発見して呼び出すクライアントを提供します。
"""

import asyncio
import logging
import time
from typing import Any

import httpx

from agentflow.protocols.a2a_card import AgentCard


class A2AClient:
    """リモート A2A エージェントを発見して呼び出すクライアント.

    このクラスはリモートエージェントの AgentCard を取得し、タスクを実行します。
    AgentCard はキャッシュされ、ネットワークリクエストを削減します。

    Example:
        >>> client = A2AClient()
        >>> card = await client.discover_agent("https://example.com/agent")
        >>> result = await client.call_remote_agent(
        ...     "https://example.com/agent",
        ...     "skill_name",
        ...     {"input": "data"}
        ... )
    """

    def __init__(
        self,
        *,
        logger: logging.Logger | None = None,
        default_timeout: float = 30.0,
        max_retries: int = 3,
        cache_ttl: float = 300.0,
    ) -> None:
        """A2A クライアントを初期化.

        Args:
            logger: ロガーインスタンス (オプション)
            default_timeout: リクエストのデフォルトタイムアウト (秒)
            max_retries: 失敗したリクエストの最大再試行回数
            cache_ttl: AgentCard キャッシュの有効期限 (秒)
        """
        self._logger = logger or logging.getLogger(__name__)
        self._default_timeout = default_timeout
        self._max_retries = max_retries
        self._cache_ttl = cache_ttl
        self._cache: dict[str, tuple[AgentCard, float]] = {}
        self._http_client = httpx.AsyncClient(timeout=default_timeout)

    async def close(self) -> None:
        """HTTP クライアントをクローズ."""
        await self._http_client.aclose()

    async def __aenter__(self) -> "A2AClient":
        """非同期コンテキストマネージャーのエントリー."""
        return self

    async def __aexit__(self, *_args: Any) -> None:
        """非同期コンテキストマネージャーの終了."""
        await self.close()

    def _is_cache_valid(self, endpoint: str) -> bool:
        """キャッシュが有効かどうかを確認.

        Args:
            endpoint: エージェントエンドポイント URL

        Returns:
            キャッシュが有効な場合は True
        """
        if endpoint not in self._cache:
            return False

        _card, timestamp = self._cache[endpoint]
        return time.time() - timestamp < self._cache_ttl

    async def discover_agent(self, endpoint: str, *, force_refresh: bool = False) -> AgentCard:
        """リモートエージェントを発見して AgentCard を取得.

        Args:
            endpoint: エージェントエンドポイント URL
            force_refresh: キャッシュを無視して強制的に再取得

        Returns:
            AgentCard インスタンス

        Raises:
            httpx.HTTPError: HTTP リクエストが失敗した場合
            ValueError: AgentCard のパースに失敗した場合
        """
        # キャッシュをチェック
        if not force_refresh and self._is_cache_valid(endpoint):
            card, _timestamp = self._cache[endpoint]
            self._logger.debug(f"Using cached AgentCard for {endpoint}")
            return card

        # AgentCard を取得
        self._logger.info(f"Discovering agent at {endpoint}")

        for attempt in range(self._max_retries):
            try:
                response = await self._http_client.get(f"{endpoint}/card")
                response.raise_for_status()

                card_data = response.json()
                card = AgentCard(**card_data)

                # キャッシュに保存
                self._cache[endpoint] = (card, time.time())

                self._logger.info(f"Successfully discovered agent: {card.name} at {endpoint}")

            except httpx.HTTPError:
                if attempt == self._max_retries - 1:
                    self._logger.exception(f"Failed to discover agent at {endpoint} after {self._max_retries} attempts")
                    raise

                # 指数バックオフで再試行
                wait_time = 2**attempt
                self._logger.warning(f"Attempt {attempt + 1} failed, retrying in {wait_time}s")
                await asyncio.sleep(wait_time)
            else:
                return card

        # この行には到達しないはずだが、型チェッカーを満足させるため
        msg = f"Failed to discover agent at {endpoint}"
        raise RuntimeError(msg)

    async def call_remote_agent(
        self,
        endpoint: str,
        skill_name: str,
        inputs: dict[str, Any],
        *,
        timeout: float | None = None,
    ) -> dict[str, Any]:
        """リモートエージェントのスキルを呼び出す.

        Args:
            endpoint: エージェントエンドポイント URL
            skill_name: スキル名
            inputs: タスク入力
            timeout: タイムアウト (秒)、None の場合はデフォルト値を使用

        Returns:
            タスク実行結果

        Raises:
            httpx.HTTPError: HTTP リクエストが失敗した場合
            ValueError: スキルが見つからない場合
        """
        # AgentCard を取得してスキルの存在を確認
        card = await self.discover_agent(endpoint)

        if skill_name not in [skill.name for skill in card.skills]:
            msg = f"Skill not found: {skill_name} in agent {card.name}"
            raise ValueError(msg)

        # タイムアウトを設定
        request_timeout = timeout if timeout is not None else self._default_timeout

        # タスクを実行
        self._logger.debug(f"Calling remote agent: endpoint={endpoint}, skill={skill_name}")

        for attempt in range(self._max_retries):
            try:
                response = await self._http_client.post(
                    f"{endpoint}/task",
                    json={"skill": skill_name, "inputs": inputs},
                    timeout=request_timeout,
                )
                response.raise_for_status()

                result: dict[str, Any] = response.json()

                self._logger.debug(f"Remote task completed: endpoint={endpoint}, skill={skill_name}")

            except httpx.HTTPError:
                if attempt == self._max_retries - 1:
                    self._logger.exception(
                        f"Failed to call remote agent at {endpoint} after {self._max_retries} attempts"
                    )
                    raise

                # 指数バックオフで再試行
                wait_time = 2**attempt
                self._logger.warning(f"Attempt {attempt + 1} failed, retrying in {wait_time}s")
                await asyncio.sleep(wait_time)
            else:
                return result

        # この行には到達しないはずだが、型チェッカーを満足させるため
        msg = f"Failed to call remote agent at {endpoint}"
        raise RuntimeError(msg)

    def clear_cache(self, endpoint: str | None = None) -> None:
        """AgentCard キャッシュをクリア.

        Args:
            endpoint: クリアするエンドポイント、None の場合は全てクリア
        """
        if endpoint is None:
            self._cache.clear()
            self._logger.debug("Cleared all AgentCard cache")
        elif endpoint in self._cache:
            del self._cache[endpoint]
            self._logger.debug(f"Cleared AgentCard cache for {endpoint}")

    def get_cached_endpoints(self) -> list[str]:
        """キャッシュされているエンドポイントのリストを取得.

        Returns:
            エンドポイント URL のリスト
        """
        return list(self._cache.keys())
