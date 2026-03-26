"""ExternalAPIBackend - 外部 REST API 経由の検索バックエンド.

外部の検索 API（Google Custom Search, Tavily, Serper 等）を
統一 RetrievalBackend インターフェースでラップ。

使用例:
    >>> backend = ExternalAPIBackend(
    ...     api_url="https://api.example.com/search",
    ...     api_key="sk-xxx",
    ...     response_mapping={"documents": "results", "content": "snippet"},
    ... )
    >>> result = await backend.retrieve(RetrievalQuery(query="最新ニュース"))
"""

from __future__ import annotations

import logging
import uuid
from typing import Any

from apps.faq_system.backend.mcp.backends.base import (
    BackendType,
    RetrievalBackend,
    RetrievalQuery,
    RetrievalResult,
    RetrievedDocument,
)


logger = logging.getLogger(__name__)


class ExternalAPIBackend(RetrievalBackend):
    """外部 REST API 検索バックエンド.

    任意の REST API を検索バックエンドとして利用可能。
    レスポンスのフィールドマッピングで柔軟に対応。
    """

    def __init__(
        self,
        api_url: str,
        api_key: str = "",
        method: str = "GET",
        headers: dict[str, str] | None = None,
        query_param: str = "q",
        top_k_param: str = "num",
        response_mapping: dict[str, str] | None = None,
        timeout_seconds: int = 30,
    ) -> None:
        """初期化.

        Args:
            api_url: API エンドポイント URL
            api_key: API キー（Authorization ヘッダーに設定）
            method: HTTP メソッド（GET / POST）
            headers: 追加 HTTP ヘッダー
            query_param: クエリパラメータ名（GET時）
            top_k_param: 件数パラメータ名
            response_mapping: レスポンスフィールドのマッピング
            timeout_seconds: リクエストタイムアウト（秒）
        """
        super().__init__(backend_type=BackendType.EXTERNAL_API, name="external_api")
        self._api_url = api_url
        self._api_key = api_key
        self._method = method.upper()
        self._headers = headers or {}
        self._query_param = query_param
        self._top_k_param = top_k_param
        self._timeout = timeout_seconds

        # レスポンスフィールドマッピング（API の JSON 構造 → 内部フィールド）
        default_mapping: dict[str, str] = {
            "results_key": "results",  # 結果リストのキー
            "content_key": "content",  # コンテンツフィールド
            "title_key": "title",  # タイトルフィールド
            "url_key": "url",  # URL フィールド
            "score_key": "score",  # スコアフィールド
        }
        if response_mapping:
            default_mapping.update(response_mapping)
        self._mapping = default_mapping

    async def retrieve(self, query: RetrievalQuery) -> RetrievalResult:
        """外部 API 経由で検索を実行.

        Args:
            query: 統一検索クエリ

        Returns:
            統一検索結果
        """
        try:
            import httpx
        except ImportError:
            self._logger.exception("httpx 未インストール。pip install httpx を実行してください")
            return RetrievalResult(
                query=query.query,
                backend_type=BackendType.EXTERNAL_API,
                metadata={"error": "httpx 未インストール"},
            )

        # リクエストヘッダー構築
        req_headers = {**self._headers}
        if self._api_key:
            req_headers["Authorization"] = f"Bearer {self._api_key}"

        try:
            async with httpx.AsyncClient(timeout=self._timeout) as client:
                if self._method == "GET":
                    params = {
                        self._query_param: query.query,
                        self._top_k_param: str(query.top_k),
                    }
                    # フィルターをクエリパラメータに追加
                    params.update({k: str(v) for k, v in query.filters.items()})
                    resp = await client.get(self._api_url, params=params, headers=req_headers)
                else:
                    body: dict[str, Any] = {
                        self._query_param: query.query,
                        self._top_k_param: query.top_k,
                        **query.filters,
                    }
                    resp = await client.post(self._api_url, json=body, headers=req_headers)

                resp.raise_for_status()
                data = resp.json()

        except Exception as e:
            self._logger.exception("外部 API リクエストエラー: %s", self._api_url)
            return RetrievalResult(
                query=query.query,
                backend_type=BackendType.EXTERNAL_API,
                metadata={"error": str(e), "api_url": self._api_url},
            )

        # レスポンスを RetrievedDocument に変換
        documents = self._parse_response(data, query.top_k)

        return RetrievalResult(
            documents=documents,
            query=query.query,
            total_found=len(documents),
            backend_type=BackendType.EXTERNAL_API,
            metadata={"api_url": self._api_url},
        )

    async def health_check(self) -> bool:
        """ヘルスチェック（API URL が設定されているか）."""
        return bool(self._api_url)

    def _parse_response(
        self,
        data: dict[str, Any] | list[Any],
        top_k: int,
    ) -> list[RetrievedDocument]:
        """API レスポンスを RetrievedDocument に変換.

        Args:
            data: API レスポンス JSON
            top_k: 上位K件

        Returns:
            変換済みドキュメントリスト
        """
        # 結果リストを取得
        results_key = self._mapping["results_key"]
        if isinstance(data, list):
            items = data
        elif isinstance(data, dict):
            items = data.get(results_key, [])
            if not isinstance(items, list):
                items = [data]

        content_key = self._mapping["content_key"]
        title_key = self._mapping["title_key"]
        url_key = self._mapping["url_key"]
        score_key = self._mapping["score_key"]

        documents: list[RetrievedDocument] = []
        for i, item in enumerate(items[:top_k]):
            if not isinstance(item, dict):
                continue

            content = str(item.get(content_key, ""))
            title = str(item.get(title_key, ""))
            url = str(item.get(url_key, ""))
            score = float(item.get(score_key, max(0.0, 1.0 - (i * 0.05))))

            # コンテンツが空の場合はタイトルを使う
            if not content and title:
                content = title

            documents.append(
                RetrievedDocument(
                    doc_id=uuid.uuid4().hex[:12],
                    content=content,
                    score=score,
                    source=url or self._api_url,
                    metadata={"title": title, "url": url, "raw": item},
                )
            )

        return documents
