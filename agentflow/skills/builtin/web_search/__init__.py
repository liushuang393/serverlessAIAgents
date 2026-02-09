"""Web Search Skill - 網絡検索能力.

実時間でインターネットから情報を検索・取得する。

使用例:
    >>> from agentflow.skills.builtin.web_search import WebSearchSkill
    >>> search = WebSearchSkill()
    >>> results = await search.search("AgentFlow")
"""

from __future__ import annotations

import asyncio
import logging
from dataclasses import dataclass, field
from typing import Any
from urllib.parse import quote_plus

import httpx


# ロガー設定
logger = logging.getLogger("web_search")


@dataclass
class SearchConfig:
    """検索設定."""

    engine: str = "duckduckgo"  # google, bing, duckduckgo
    max_results: int = 5
    language: str = "ja"
    timeout: float = 10.0
    safe_search: bool = True


@dataclass
class SearchResult:
    """検索結果."""

    title: str
    url: str
    snippet: str
    source: str = ""


@dataclass
class SearchSummary:
    """検索要約結果."""

    answer: str
    sources: list[SearchResult] = field(default_factory=list)
    query: str = ""


class SearchError(Exception):
    """検索エラー."""


class RateLimitError(SearchError):
    """レートリミットエラー."""


class WebSearchSkill:
    """Web Search Skill - 網絡検索能力.

    DuckDuckGo を使用してプライバシーを保護しながら検索。
    """

    # DuckDuckGo HTML 検索 URL
    DDG_URL = "https://html.duckduckgo.com/html/"

    def __init__(self, config: SearchConfig | None = None) -> None:
        """初期化.

        Args:
            config: 検索設定
        """
        self._config = config or SearchConfig()
        self._client: httpx.AsyncClient | None = None
        self._logger = logger

    async def _get_client(self) -> httpx.AsyncClient:
        """HTTP クライアント取得."""
        if self._client is None:
            self._client = httpx.AsyncClient(
                timeout=self._config.timeout,
                headers={
                    "User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
                },
                follow_redirects=True,
            )
        return self._client

    async def close(self) -> None:
        """リソース解放."""
        if self._client:
            await self._client.aclose()
            self._client = None

    async def search(self, query: str) -> list[SearchResult]:
        """検索実行.

        Args:
            query: 検索クエリ

        Returns:
            検索結果リスト
        """
        if self._config.engine == "duckduckgo":
            return await self._search_duckduckgo(query)
        # 他のエンジンは未実装
        self._logger.warning(f"Engine {self._config.engine} not implemented, using DuckDuckGo")
        return await self._search_duckduckgo(query)

    async def _search_duckduckgo(self, query: str) -> list[SearchResult]:
        """DuckDuckGo で検索."""
        client = await self._get_client()

        try:
            # HTML 検索を使用（API 不要）
            response = await client.post(
                self.DDG_URL,
                data={"q": query, "kl": f"{self._config.language}-{self._config.language}"},
            )
            response.raise_for_status()

            # HTML パース（BeautifulSoup 不要の簡易パース）
            results = self._parse_ddg_html(response.text)
            return results[: self._config.max_results]

        except httpx.HTTPStatusError as e:
            if e.response.status_code == 429:
                msg = "DuckDuckGo rate limit exceeded"
                raise RateLimitError(msg) from e
            msg = f"Search failed: {e}"
            raise SearchError(msg) from e
        except Exception as e:
            msg = f"Search error: {e}"
            raise SearchError(msg) from e

    def _parse_ddg_html(self, html: str) -> list[SearchResult]:
        """DuckDuckGo HTML をパース."""
        results: list[SearchResult] = []

        # 簡易 HTML パース（class="result" を探す）
        import re

        # 結果ブロックを抽出
        result_pattern = re.compile(
            r'<a class="result__a" href="([^"]+)"[^>]*>([^<]+)</a>.*?'
            r'<a class="result__snippet"[^>]*>([^<]*)</a>',
            re.DOTALL,
        )

        for match in result_pattern.finditer(html):
            url, title, snippet = match.groups()
            results.append(
                SearchResult(
                    title=title.strip(),
                    url=url.strip(),
                    snippet=snippet.strip(),
                    source="duckduckgo",
                )
            )

        return results

    async def fetch_content(self, url: str, max_chars: int = 5000) -> str:
        """URL からコンテンツを取得.

        Args:
            url: 対象 URL
            max_chars: 最大文字数

        Returns:
            抽出されたテキスト
        """
        client = await self._get_client()
        try:
            response = await client.get(url)
            response.raise_for_status()

            # 簡易テキスト抽出（HTML タグ除去）
            import re

            text = re.sub(r"<script[^>]*>.*?</script>", "", response.text, flags=re.DOTALL)
            text = re.sub(r"<style[^>]*>.*?</style>", "", text, flags=re.DOTALL)
            text = re.sub(r"<[^>]+>", " ", text)
            text = re.sub(r"\s+", " ", text).strip()

            return text[:max_chars]
        except Exception as e:
            self._logger.warning(f"Failed to fetch {url}: {e}")
            return ""

    async def search_and_summarize(
        self,
        query: str,
        question: str | None = None,
        llm_client: Any = None,
    ) -> SearchSummary:
        """検索して結果を要約.

        Args:
            query: 検索クエリ
            question: 具体的な質問（要約の焦点）
            llm_client: LLM クライアント（省略時は検索結果のみ）

        Returns:
            検索要約
        """
        # 1. 検索実行
        results = await self.search(query)
        if not results:
            return SearchSummary(answer="検索結果が見つかりませんでした。", sources=[], query=query)

        # 2. 各結果の内容を取得
        contents = []
        for r in results[:3]:  # 上位3件のみ
            content = await self.fetch_content(r.url, max_chars=2000)
            if content:
                contents.append(f"【{r.title}】\n{content[:1000]}")

        # 3. LLM がない場合はスニペットを結合
        if not llm_client:
            combined = "\n\n".join([f"- {r.title}: {r.snippet}" for r in results])
            return SearchSummary(answer=combined, sources=results, query=query)

        # 4. LLM で要約生成
        context = "\n\n---\n\n".join(contents) if contents else "\n".join([r.snippet for r in results])
        prompt = f"""以下の検索結果を基に質問に回答してください。

検索クエリ: {query}
質問: {question or query}

検索結果:
{context}

回答（簡潔に、日本語で）:"""

        try:
            response = await llm_client.chat(messages=[{"role": "user", "content": prompt}])
            answer = response.get("content", "") if isinstance(response, dict) else str(response)
        except Exception as e:
            self._logger.exception(f"LLM summarization failed: {e}")
            answer = "\n".join([f"- {r.snippet}" for r in results])

        return SearchSummary(answer=answer, sources=results, query=query)


# エクスポート
__all__ = [
    "RateLimitError",
    "SearchConfig",
    "SearchError",
    "SearchResult",
    "SearchSummary",
    "WebSearchSkill",
]

