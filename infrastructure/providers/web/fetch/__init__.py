"""fetch provider 公開 API."""

from infrastructure.providers.web.fetch.base import ContentFetchProvider, FetchResult
from infrastructure.providers.web.fetch.registry import FetchProviderRegistry, get_fetch_provider


__all__ = [
    "ContentFetchProvider",
    "FetchProviderRegistry",
    "FetchResult",
    "get_fetch_provider",
]
