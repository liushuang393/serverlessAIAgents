"""Official Site Scraper.

監視対象企業の公式サイトからニュースやプレスリリースを直接収集します。
"""

import logging
import uuid
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.models import Article, SourceType

from agentflow import get_llm


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
        """公式サイトからニュースを収集（モック実装ベース）.

        実際のプロダクション環境では、Playwright などのブラウザツールを使用して
        動的にページを巡回し、LLM でニュース項目を抽出します。
        """
        self._logger.info("公式サイト収集開始: %s (%d urls)", competitor_name, len(urls))
        articles: list[Article] = []

        # TODO: Phase 7 実装において、browser_api を使用した実スクレイピングを統合
        # 現時点では、特定企業の主要な動向をシミュレートする高品質な記事を生成

        # モックロジック（後続の統合確認用）
        mock_news = [
            {
                "title": f"{competitor_name}: クラウドネイティブな基幹系刷新ソリューションを発表",
                "content": f"{competitor_name}は本日、COBOLからJavaへの移行を加速させる自動変換エンジンの最新版をリリースしました。",
                "url": urls[0] if urls else f"https://www.{competitor_name.lower().replace(' ', '')}.com/news/1",
            }
        ]

        for item in mock_news:
            article = Article(
                id=str(uuid.uuid4()),
                title=item["title"],
                url=item["url"],
                source=SourceType.OFFICIAL_SITE,
                published_at=datetime.now(),
                content=item["content"],
                keywords=[competitor_name, "press release"],
                collected_at=datetime.now(),
                metadata={
                    "competitor": competitor_name,
                    "official_site": True,
                },
            )
            articles.append(article)

        return articles
