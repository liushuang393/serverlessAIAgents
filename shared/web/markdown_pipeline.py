"""Markdown パイプライン."""

from __future__ import annotations

from shared.web.normalizer import normalize_markdown


def finalize_markdown(markdown: str, max_chars: int = 30000) -> str:
    """最終入力用 Markdown を生成する."""
    normalized = normalize_markdown(markdown)
    return normalized[:max_chars]
