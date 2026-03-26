"""Web 根拠の rerank."""

from __future__ import annotations

from typing import TYPE_CHECKING

from shared.gateway.rerank.service import SharedRerankGateway


if TYPE_CHECKING:
    from contracts.web import EvidenceItem


async def rerank_evidence(query: str, evidence: list[EvidenceItem], *, top_k: int = 5) -> list[EvidenceItem]:
    """既存 rerank gateway で根拠を再順位付けする."""
    if not evidence:
        return []
    documents = [(item.markdown or item.snippet or item.url).strip() for item in evidence]
    gateway = SharedRerankGateway()
    try:
        ranked = await gateway.rerank(query=query, documents=documents, top_k=min(top_k, len(documents)))
    except Exception:
        return evidence[:top_k]

    sorted_items: list[EvidenceItem] = []
    for result in ranked:
        if 0 <= result.index < len(evidence):
            candidate = evidence[result.index]
            sorted_items.append(
                candidate.model_copy(
                    update={"confidence": max(candidate.confidence, min(1.0, float(result.score)))},
                )
            )
    if not sorted_items:
        return evidence[:top_k]
    return sorted_items[:top_k]
