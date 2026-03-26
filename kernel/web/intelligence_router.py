"""Kernel facade for the shared Web Router implementation."""

from __future__ import annotations

from contracts.web import WebRetrievalRouter, WebRouterInput, WebRouterOutput
from shared.web.router_service import DefaultWebRetrievalRouter


class WebIntelligenceRouter(WebRetrievalRouter):
    """Kernel から公開する薄い facade."""

    def __init__(self) -> None:
        self._delegate: WebRetrievalRouter = DefaultWebRetrievalRouter()

    async def execute(self, req: WebRouterInput) -> WebRouterOutput:
        """共有実装へ委譲する."""
        return await self._delegate.execute(req)
