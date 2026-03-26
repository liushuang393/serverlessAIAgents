"""API データソースからのドキュメントインジェスト.

外部 REST API を RAG データソースとして取り込む。
返り値の型（JSON array / {"data": [...]} / text）を自動判別。
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from typing import Any

import httpx


logger = logging.getLogger(__name__)


@dataclass
class APIIngestResult:
    """API インジェスト結果（型付き）."""

    source_uri: str
    chunks_created: int = 0
    content_type: str = "unknown"
    success: bool = True
    error_message: str = ""
    raw_item_count: int = 0
    warnings: list[str] = field(default_factory=list)


class APIIngestor:
    """外部 API からドキュメントを取り込む.

    対応する返り値型:
    - JSON array: [{"text": "..."}, ...]
    - JSON object with data key: {"data": [...], "total": N}
    - JSON object with items/results key: {"items": [...]}
    - Plain text
    """

    def __init__(
        self,
        source_uri: str,
        headers: dict[str, str] | None = None,
        text_fields: list[str] | None = None,
        timeout: float = 30.0,
    ) -> None:
        self._uri = source_uri
        self._headers = headers or {}
        self._text_fields = text_fields or [
            "text",
            "content",
            "body",
            "answer",
            "description",
            "question",
        ]
        self._timeout = timeout

    async def ingest(self, collection_name: str) -> APIIngestResult:
        """API を呼び出してドキュメントを取り込む."""
        result = APIIngestResult(source_uri=self._uri)
        try:
            async with httpx.AsyncClient(timeout=self._timeout) as client:
                resp = await client.get(self._uri, headers=self._headers)
                resp.raise_for_status()

            content_type = resp.headers.get("content-type", "").split(";")[0].strip()
            result.content_type = content_type

            if "json" in content_type:
                items = self._extract_items(resp.json())
            else:
                items = [{"text": resp.text}]

            result.raw_item_count = len(items)
            texts = self._extract_texts(items)
            result.chunks_created = len(texts)
            result.success = True

        except Exception as exc:
            logger.warning("API インジェスト失敗 %s: %s", self._uri, exc)
            result.success = False
            result.error_message = str(exc)

        return result

    def _extract_items(self, payload: Any) -> list[dict[str, Any]]:
        """JSON ペイロードからアイテムリストを抽出."""
        if isinstance(payload, list):
            return [item for item in payload if isinstance(item, dict)]
        if isinstance(payload, dict):
            for key in ("data", "items", "results", "records", "documents"):
                maybe_items = payload.get(key)
                if isinstance(maybe_items, list):
                    return [item for item in maybe_items if isinstance(item, dict)]
            return [payload]
        return []

    def _extract_texts(self, items: list[Any]) -> list[str]:
        """アイテムリストからテキストを抽出."""
        texts: list[str] = []
        for item in items:
            if isinstance(item, str):
                texts.append(item)
            elif isinstance(item, dict):
                for field_name in self._text_fields:
                    if isinstance(item.get(field_name), str) and item[field_name].strip():
                        texts.append(item[field_name])
                        break
                else:
                    # テキストフィールドなし: 全文字列値を結合
                    combined = " ".join(str(v) for v in item.values() if isinstance(v, str) and str(v).strip())
                    if combined:
                        texts.append(combined)
        return [t for t in texts if t.strip()]


__all__ = ["APIIngestResult", "APIIngestor"]
