"""Markdown 正規化."""

from __future__ import annotations

import re


_NAV_PATTERN = re.compile(r"(?im)^(navigation|nav|menu|footer|copyright).*$")
_MULTI_EMPTY = re.compile(r"\n{3,}")
_SCRIPT_STYLE_HINT = re.compile(r"(?im)<(script|style)[^>]*>.*?</\\1>")


def normalize_markdown(markdown: str) -> str:
    """Markdown を正規化する."""
    text = markdown.replace("\r\n", "\n").replace("\r", "\n")
    text = _SCRIPT_STYLE_HINT.sub("", text)
    lines = [line for line in text.splitlines() if not _NAV_PATTERN.match(line.strip())]
    normalized = "\n".join(lines)
    normalized = re.sub(r"[ \t]{2,}", " ", normalized)
    return _MULTI_EMPTY.sub("\n\n", normalized).strip()
