"""browser provider 公開 API."""

from infrastructure.providers.web.browser.base import (
    BrowserOperationRequest,
    BrowserOperationResult,
    BrowserOperator,
)
from infrastructure.providers.web.browser.registry import BrowserProviderRegistry, get_browser_operator


__all__ = [
    "BrowserOperationRequest",
    "BrowserOperationResult",
    "BrowserOperator",
    "BrowserProviderRegistry",
    "get_browser_operator",
]
