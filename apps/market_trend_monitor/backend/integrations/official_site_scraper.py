"""Official Site Scraper.

監視対象企業の公式サイトからニュースやプレスリリースを直接収集します。
"""

import asyncio
import logging
import re
import uuid
from datetime import UTC, datetime
from typing import Any
from urllib.parse import urlparse

from apps.market_trend_monitor.backend.models import Article, SourceType

from kernel import get_llm

try:
    import httpx
except ImportError:  # pragma: no cover - optional dependency
    httpx = None

try:
    import trafilatura
except ImportError:  # pragma: no cover - optional dependency
    trafilatura = None


_TITLE_RE = re.compile(r"<title[^>]*>(.*?)</title>", flags=re.IGNORECASE | re.DOTALL)


class OfficialSiteScraper:
    """公式サイトスクレイパー.

    特定の企業のプレスリリース・ニュースページを巡回します。
    """

    def __init__(self, llm: Any | None = None) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._llm = llm

    def _get_llm(self) -> Any:
        """LLMインスタンスを取得."""
        if self._llm is None:
            self._llm = get_llm(temperature=0.0)
        return self._llm

    async def scrape_official_news(
        self,
        competitor_name: str,
        urls: list[str],
    ) -> list[Article]:
        """公式サイトからニュースを収集する."""
        self._logger.info("公式サイト収集開始: %s (%d urls)", competitor_name, len(urls))
        if httpx is None:
            self._logger.warning("httpx が未導入のため公式サイト収集をスキップします")
            return []

        candidate_urls = self._normalize_urls(competitor_name, urls)
        if not candidate_urls:
            return []

        timeout_seconds = 20
        max_retries = 3
        articles: list[Article] = []

        async with httpx.AsyncClient(timeout=timeout_seconds, follow_redirects=True) as client:
            for target_url in candidate_urls:
                html = await self._fetch_html_with_retry(client, target_url, max_retries=max_retries)
                if not html:
                    continue

                extracted = self._extract_content(html)
                content = extracted.get("content", "").strip()
                if not content:
                    continue

                title = extracted.get("title") or f"{competitor_name} Official Update"
                now = datetime.now(tz=UTC)
                article = Article(
                    id=str(uuid.uuid4()),
                    title=title,
                    url=target_url,
                    source=SourceType.OFFICIAL_SITE,
                    published_at=now,
                    content=content,
                    keywords=[competitor_name, "official", "press release"],
                    collected_at=now,
                    metadata={
                        "competitor": competitor_name,
                        "official_site": True,
                        "provider": extracted.get("provider", "httpx"),
                        "fallback_used": bool(extracted.get("fallback_used", False)),
                    },
                )
                articles.append(article)

        return articles

    def _normalize_urls(self, competitor_name: str, urls: list[str]) -> list[str]:
        """URL 候補を正規化して重複を排除する."""
        candidates: list[str] = []
        for raw in urls:
            text = str(raw).strip()
            if text and self._is_http_url(text):
                candidates.append(text)

        if not candidates:
            normalized_name = re.sub(r"[^a-z0-9]", "", competitor_name.lower())
            if normalized_name:
                candidates.extend(
                    [
                        f"https://www.{normalized_name}.com/news",
                        f"https://www.{normalized_name}.com/press",
                    ]
                )

        deduped: list[str] = []
        for item in candidates:
            if item not in deduped:
                deduped.append(item)
        return deduped[:8]

    async def _fetch_html_with_retry(
        self,
        client: Any,
        url: str,
        *,
        max_retries: int,
    ) -> str:
        """ページ取得をリトライ付きで実行する."""
        for attempt in range(max_retries):
            try:
                response = await client.get(url)
                response.raise_for_status()
                return response.text
            except Exception as exc:
                self._logger.warning(
                    "公式サイト取得失敗: url=%s attempt=%d/%d error=%s",
                    url,
                    attempt + 1,
                    max_retries,
                    exc,
                )
                if attempt < max_retries - 1:
                    await asyncio.sleep(0.5 * (2**attempt))
        return ""

    def _extract_content(self, html: str) -> dict[str, Any]:
        """HTML から本文を抽出する."""
        title_match = _TITLE_RE.search(html)
        title = self._clean_text(title_match.group(1)) if title_match else None

        if trafilatura is not None:
            try:
                extracted = trafilatura.extract(html, include_comments=False, include_tables=True)
            except Exception:
                extracted = None
            if extracted:
                return {
                    "title": title,
                    "content": extracted,
                    "provider": "trafilatura",
                    "fallback_used": False,
                }

        stripped = re.sub(r"<script[\s\S]*?</script>", " ", html, flags=re.IGNORECASE)
        stripped = re.sub(r"<style[\s\S]*?</style>", " ", stripped, flags=re.IGNORECASE)
        stripped = re.sub(r"<[^>]+>", " ", stripped)
        stripped = re.sub(r"\s+", " ", stripped).strip()
        return {
            "title": title,
            "content": stripped,
            "provider": "html_strip",
            "fallback_used": True,
        }

    def _is_http_url(self, value: str) -> bool:
        parsed = urlparse(value)
        return parsed.scheme in {"http", "https"} and bool(parsed.netloc)

    def _clean_text(self, value: str | None) -> str | None:
        if not value:
            return None
        text = re.sub(r"\s+", " ", value).strip()
        return text or None
