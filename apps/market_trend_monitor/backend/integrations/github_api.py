"""GitHub API 連携."""

from __future__ import annotations

import asyncio
import logging
import time
from typing import Any

import httpx

from apps.market_trend_monitor.backend.integrations.rate_limiter import rate_limiter

# レート制限リトライ設定
_MIN_RATE_LIMIT_WAIT: float = 2.0  # 最低待機秒数（0s ループ防止）
_MAX_RATE_LIMIT_WAIT: float = 60.0  # 上限待機秒数
_FALLBACK_RATE_LIMIT_WAIT: float = 5.0  # reset ヘッダ無し時のフォールバック


class GitHubAPIClient:
    """GitHub API クライアント."""

    def __init__(
        self,
        token: str | None = None,
        base_url: str = "https://api.github.com",
        max_retries: int = 3,
    ) -> None:
        """初期化.

        Args:
            token: GitHub Personal Access Token（任意）
            base_url: API ベース URL
            max_retries: 最大リトライ回数（レート制限含む）
        """
        self._token = token
        self._base_url = base_url
        self._logger = logging.getLogger(self.__class__.__name__)
        self._max_retries = max_retries

    async def search_repositories(
        self,
        query: str,
        per_page: int = 5,
    ) -> list[dict[str, Any]]:
        """リポジトリ検索."""
        headers = {"Accept": "application/vnd.github+json"}
        if self._token:
            headers["Authorization"] = f"Bearer {self._token}"

        params = {
            "q": query,
            "per_page": per_page,
            "sort": "updated",
            "order": "desc",
        }

        url = f"{self._base_url}/search/repositories"

        for attempt in range(self._max_retries + 1):
            try:
                await rate_limiter.acquire("github")
                async with httpx.AsyncClient(timeout=20.0) as client:
                    response = await client.get(url, params=params, headers=headers)

                if response.status_code == 200:
                    payload = response.json()
                    return payload.get("items", [])

                if response.status_code == 403:
                    wait = self._calc_rate_limit_wait(response)
                    self._logger.warning(
                        "GitHubレート制限、%.1fs 待機 (attempt=%d/%d)",
                        wait, attempt + 1, self._max_retries + 1,
                    )
                    await asyncio.sleep(wait)
                    continue

                self._logger.warning(
                    "GitHub APIエラー: status=%s", response.status_code
                )
            except Exception as exc:
                self._logger.warning("GitHub API 失敗: %s", exc)

            await asyncio.sleep(1.0 * (attempt + 1))

        return []

    # ------------------------------------------------------------------
    # 内部ヘルパー
    # ------------------------------------------------------------------

    @staticmethod
    def _calc_rate_limit_wait(response: httpx.Response) -> float:
        """X-RateLimit-Reset ヘッダから待機秒数を算出.

        ヘッダが無い、またはパース不能な場合はフォールバック値を返す。
        0s ループを防ぐため最低待機秒数を保証する。

        Args:
            response: GitHub API レスポンス

        Returns:
            待機秒数（_MIN ～ _MAX の範囲）
        """
        reset = response.headers.get("X-RateLimit-Reset")
        if not reset:
            return _FALLBACK_RATE_LIMIT_WAIT

        try:
            wait = float(reset) - time.time()
        except (ValueError, TypeError):
            return _FALLBACK_RATE_LIMIT_WAIT

        # 最低・最大の範囲に収める
        return max(_MIN_RATE_LIMIT_WAIT, min(wait, _MAX_RATE_LIMIT_WAIT))
