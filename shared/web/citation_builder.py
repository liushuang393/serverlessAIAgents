"""引用生成."""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from contracts.web import EvidenceItem


def build_citations(items: list[EvidenceItem]) -> list[str]:
    """根拠一覧から引用行を生成する."""
    citations: list[str] = []
    for index, item in enumerate(items, start=1):
        title = item.title.strip() if isinstance(item.title, str) and item.title.strip() else item.url
        citations.append(f"[{index}] {title} - {item.url}")
    return citations
