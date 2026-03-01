"""RAG hot-reload payload compatibility tests."""

from __future__ import annotations

import pytest

from agentflow.bootstrap.config_watcher import _extract_contracts_rag
from agentflow.bootstrap.rag_builder import _normalize_rag_payload


def test_extract_contracts_rag_prefers_canonical() -> None:
    payload = {
        "contracts_rag": {
            "enabled": True,
            "collections": ["faq_knowledge"],
            "default_top_k": 8,
        },
        "rag_config": {"enabled": False},
    }
    extracted = _extract_contracts_rag(payload)
    assert extracted is not None
    assert extracted["enabled"] is True
    assert extracted["collections"] == ["faq_knowledge"]


def test_extract_contracts_rag_accepts_legacy() -> None:
    payload = {
        "rag_config": {
            "enabled": True,
            "vector_provider": "qdrant",
            "vector_collection": "faq_docs",
            "top_k": 6,
        }
    }
    extracted = _extract_contracts_rag(payload)
    assert extracted is not None
    assert extracted["enabled"] is True
    assert extracted["provider"] == "qdrant"
    assert extracted["collections"] == ["faq_docs"]
    assert extracted["default_top_k"] == 6


def test_rag_builder_normalize_legacy_payload() -> None:
    legacy = {
        "enabled": True,
        "vector_provider": "qdrant",
        "vector_collection": "faq_docs",
        "top_k": 10,
    }
    normalized = _normalize_rag_payload(legacy)
    assert normalized is not None
    assert normalized["enabled"] is True
    assert normalized["provider"] == "qdrant"
    assert normalized["collections"] == ["faq_docs"]
    assert normalized["default_top_k"] == 10


@pytest.mark.parametrize(
    "payload",
    [
        None,
    ],
)
def test_rag_builder_normalize_none(payload: dict[str, object] | None) -> None:
    assert _normalize_rag_payload(payload) in {None, payload}
