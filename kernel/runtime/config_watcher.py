"""Runtime config watcher for hot-reloading capability bundles from platform SSE."""

from __future__ import annotations

import asyncio
import json
import logging
from typing import Any

from kernel.runtime.capability_bundle import CapabilityBundle
from kernel.runtime.rag_builder import build_rag_engine


logger = logging.getLogger(__name__)

_INITIAL_BACKOFF = 1.0
_MAX_BACKOFF = 60.0
_BACKOFF_FACTOR = 2.0
_SSE_PATH = "/api/studios/framework/apps/events"


class ConfigWatcher:
    """Subscribe to platform SSE and hot-reload runtime capabilities."""

    def __init__(self, app_name: str, platform_url: str | None = None) -> None:
        self._app_name = app_name
        self._platform_url = platform_url
        self._stop_event = asyncio.Event()

    async def watch(self, bundle: CapabilityBundle) -> None:
        """Watch for platform changes and mutate the bundle in place."""
        if not self._platform_url:
            logger.debug("platform_url not configured; runtime watcher disabled")
            return

        backoff = _INITIAL_BACKOFF
        sse_url = f"{self._platform_url.rstrip('/')}{_SSE_PATH}?app={self._app_name}"
        while not self._stop_event.is_set():
            try:
                await self._subscribe_and_process(sse_url, bundle)
                break
            except asyncio.CancelledError:
                logger.info("Runtime config watcher cancelled: %s", self._app_name)
                break
            except Exception as exc:
                if self._stop_event.is_set():
                    break
                logger.warning(
                    "Runtime config watcher disconnected; retrying in %.1fs: %s",
                    backoff,
                    exc,
                )
                try:
                    await asyncio.wait_for(asyncio.shield(self._stop_event.wait()), timeout=backoff)
                    break
                except TimeoutError:
                    backoff = min(backoff * _BACKOFF_FACTOR, _MAX_BACKOFF)

        logger.info("Runtime config watcher stopped: %s", self._app_name)

    async def _subscribe_and_process(self, sse_url: str, bundle: CapabilityBundle) -> None:
        import httpx
        from httpx_sse import aconnect_sse

        async with httpx.AsyncClient(timeout=None) as client, aconnect_sse(client, "GET", sse_url) as event_source:
            logger.info("Runtime config watcher connected: %s", sse_url)
            async for sse in event_source.aiter_sse():
                if self._stop_event.is_set():
                    return
                await self._handle_event(sse.event or "message", sse.data, bundle)

    async def _handle_event(self, event_type: str, data: str, bundle: CapabilityBundle) -> None:
        if event_type not in ("app_contracts_changed", "rag_config_changed", "message"):
            return

        try:
            payload = json.loads(data)
        except Exception:
            logger.debug("SSE JSON parse failed: %s", data[:200])
            return

        if not isinstance(payload, dict):
            logger.debug("SSE payload must be an object: %s", type(payload).__name__)
            return

        contracts_llm = _extract_contracts_llm(payload)
        if contracts_llm is not None:
            bundle.llm_contracts = contracts_llm
            logger.info("Runtime LLM contracts reloaded: %s", self._app_name)

        contracts_rag = _extract_contracts_rag(payload)
        if contracts_rag is None:
            return

        logger.info("Runtime RAG contracts changed: %s", self._app_name)
        bundle.rag_engine = await build_rag_engine(contracts_rag)
        logger.info(
            "Runtime RAG bundle updated: %s rag=%s",
            self._app_name,
            "enabled" if bundle.rag_engine is not None else "disabled",
        )

    async def stop(self) -> None:
        """Stop the watcher."""
        self._stop_event.set()


def _extract_contracts_rag(payload: dict[str, Any]) -> dict[str, Any] | None:
    """Extract canonical contracts.rag from SSE payloads."""
    canonical = payload.get("contracts_rag")
    if isinstance(canonical, dict):
        return canonical

    legacy = payload.get("rag_config")
    if isinstance(legacy, dict):
        return _legacy_to_contracts_rag(legacy)

    if isinstance(payload.get("enabled"), bool):
        return _legacy_to_contracts_rag(payload)
    return None


def _extract_contracts_llm(payload: dict[str, Any]) -> dict[str, Any] | None:
    """Extract canonical contracts.llm from SSE payloads."""
    canonical = payload.get("contracts_llm")
    return canonical if isinstance(canonical, dict) else None


def _legacy_to_contracts_rag(legacy: dict[str, Any]) -> dict[str, Any]:
    """Convert legacy rag_config payloads to canonical contracts.rag."""
    enabled = bool(legacy.get("enabled", False))
    collection = legacy.get("vector_collection")
    collections: list[str] = []
    if isinstance(collection, str) and collection.strip():
        collections = [collection.strip()]

    return {
        "enabled": enabled,
        "pattern": legacy.get("pattern"),
        "provider": legacy.get("vector_provider") if enabled else None,
        "collections": collections if enabled else [],
        "data_sources": legacy.get("data_sources", []),
        "chunk_strategy": legacy.get("chunk_strategy", "recursive"),
        "chunk_size": legacy.get("chunk_size", 800),
        "chunk_overlap": legacy.get("chunk_overlap", 120),
        "retrieval_method": legacy.get("retrieval_method", "hybrid"),
        "embedding_model": legacy.get("embedding_model"),
        "rerank_model": legacy.get("reranker"),
        "default_top_k": legacy.get("top_k", 5),
        "score_threshold": legacy.get("score_threshold"),
        "indexing_schedule": legacy.get("indexing_schedule"),
    }


__all__ = [
    "ConfigWatcher",
    "_extract_contracts_llm",
    "_extract_contracts_rag",
]
