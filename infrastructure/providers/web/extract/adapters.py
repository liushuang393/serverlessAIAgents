"""Web extraction provider 実装."""

from __future__ import annotations

import re
from typing import Any

from infrastructure.providers.web.extract.base import ExtractionResult, StructuredExtractionProvider


_DATE_RE = re.compile(r"\b(20\d{2}[-/]\d{1,2}[-/]\d{1,2})\b")
_PRICE_RE = re.compile(r"([$¥€]\s?\d+(?:[.,]\d+)*)")
_TITLE_RE = re.compile(r"^\s{0,3}#\s+(.+)$", re.MULTILINE)
_FAQ_RE = re.compile(r"(?im)^(?:q[:：]\s*(?P<question>.+?)\n+a[:：]\s*(?P<answer>.+?))(?=\n{2,}|\Z)", re.DOTALL)
_TABLE_RE = re.compile(r"(?ms)(^\|.+\|\n^\|[- :|]+\|\n(?:^\|.*\|\n?)*)")


def _extract_basic_fields(markdown: str) -> dict[str, Any]:
    """汎用フィールド抽出."""
    title_match = _TITLE_RE.search(markdown)
    date_match = _DATE_RE.search(markdown)
    price_match = _PRICE_RE.search(markdown)
    snippet = markdown.strip().splitlines()
    summary = " ".join(snippet[:3]).strip()[:300]
    table_match = _TABLE_RE.search(markdown)
    faq_items = [
        {"question": match.group("question").strip(), "answer": match.group("answer").strip()}
        for match in _FAQ_RE.finditer(markdown)
    ]
    return {
        "title": title_match.group(1).strip() if title_match else None,
        "date": date_match.group(1).strip() if date_match else None,
        "price": price_match.group(1).strip() if price_match else None,
        "summary": summary or None,
        "table": table_match.group(1).strip() if table_match else None,
        "faq": faq_items or None,
    }


class JsonSchemaExtractor(StructuredExtractionProvider):
    """JSON schema 指定抽出."""

    name = "json_schema_extractor"

    async def extract(self, markdown: str, schema: dict[str, Any] | None = None) -> ExtractionResult:
        if not markdown.strip():
            return ExtractionResult(ok=False, data=None, metadata={"provider": self.name, "reason": "empty_markdown"})
        basic = _extract_basic_fields(markdown)
        if schema is None:
            return ExtractionResult(ok=True, data=basic, metadata={"provider": self.name})

        properties_obj = schema.get("properties")
        if not isinstance(properties_obj, dict):
            return ExtractionResult(
                ok=True, data=basic, metadata={"provider": self.name, "reason": "schema_no_properties"}
            )

        extracted: dict[str, Any] = {}
        partial_fields: list[str] = []
        for field_name in properties_obj:
            value = basic.get(field_name)
            if value is None:
                partial_fields.append(field_name)
                continue
            extracted[field_name] = value
        return ExtractionResult(
            ok=True,
            data=extracted,
            metadata={"provider": self.name, "partial_fields": partial_fields},
        )


class BrowserJsonExtractor(StructuredExtractionProvider):
    """ブラウザ由来 JSON 抽出の差し替え枠."""

    name = "browser_json_extractor"

    async def extract(self, markdown: str, schema: dict[str, Any] | None = None) -> ExtractionResult:
        _ = schema
        if not markdown.strip():
            return ExtractionResult(ok=False, data=None, metadata={"provider": self.name, "reason": "empty_markdown"})
        return ExtractionResult(ok=True, data={"raw_markdown": markdown[:2000]}, metadata={"provider": self.name})
