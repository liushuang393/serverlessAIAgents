"""RagConfigStore tests."""

from __future__ import annotations

import asyncio

import pytest
from apps.platform.services.rag_config_store import RagConfigStore


@pytest.mark.asyncio
async def test_fire_config_change_emits_canonical_and_legacy() -> None:
    store = RagConfigStore()
    events: list[dict[str, object]] = []

    async def _collect_one() -> None:
        async for event in store.subscribe("faq_system"):
            events.append(event)
            break

    consumer = asyncio.create_task(_collect_one())
    await asyncio.sleep(0)

    count = await store.fire_config_change(
        "faq_system",
        contracts_rag={"enabled": True, "collections": ["faq_docs"]},
        rag_config={"enabled": True, "vector_collection": "faq_docs"},
        config_version="123",
        updated_at="2026-03-01T00:00:00+00:00",
    )
    assert count == 1

    await asyncio.wait_for(consumer, timeout=1.0)
    assert len(events) == 1
    event = events[0]
    assert event["event_type"] == "rag_config_changed"
    assert event["contracts_rag"] == {"enabled": True, "collections": ["faq_docs"]}
    assert event["rag_config"] == {"enabled": True, "vector_collection": "faq_docs"}
    assert event["config_version"] == "123"
    assert event["updated_at"] == "2026-03-01T00:00:00+00:00"
