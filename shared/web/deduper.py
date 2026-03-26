"""重複除去."""

from __future__ import annotations

import hashlib
from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from contracts.web import EvidenceItem


def dedupe_evidence(items: list[EvidenceItem]) -> list[EvidenceItem]:
    """同一 URL または同一本文を持つ根拠を除去する."""
    seen_urls: set[str] = set()
    seen_hashes: set[str] = set()
    deduped: list[EvidenceItem] = []
    for item in items:
        url_key = item.url.strip().lower()
        body = (item.markdown or item.snippet or "").strip()
        body_hash = hashlib.sha256(body.encode("utf-8")).hexdigest() if body else ""
        if url_key and url_key in seen_urls:
            continue
        if body_hash and body_hash in seen_hashes:
            continue
        if url_key:
            seen_urls.add(url_key)
        if body_hash:
            seen_hashes.add(body_hash)
        deduped.append(item)
    return deduped
