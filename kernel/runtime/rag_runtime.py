"""Canonical runtime-facing RAG protocol."""

from __future__ import annotations

from typing import Any, Protocol


class RAGRuntime(Protocol):
    """Stable RAG runtime protocol exposed to apps and agents."""

    async def search(
        self,
        query: str,
        top_k: int | None = None,
        filters: dict[str, Any] | None = None,
    ) -> list[dict[str, Any]]:
        """Search the configured knowledge base."""

    async def query(
        self,
        query: str,
        top_k: int | None = None,
        filters: dict[str, Any] | None = None,
    ) -> Any:
        """Run a RAG query and return the generated response."""

    async def close(self) -> None:
        """Release any underlying resources."""


__all__ = ["RAGRuntime"]
