"""Web provider 公開 API."""

from infrastructure.providers.web.browser.registry import get_browser_operator
from infrastructure.providers.web.crawl.registry import get_crawl_provider
from infrastructure.providers.web.extract.registry import get_extraction_provider
from infrastructure.providers.web.fetch.registry import get_fetch_provider
from infrastructure.providers.web.search.registry import get_search_provider


__all__ = [
    "get_browser_operator",
    "get_crawl_provider",
    "get_extraction_provider",
    "get_fetch_provider",
    "get_search_provider",
]
