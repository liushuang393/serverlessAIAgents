"""Web extraction provider 抽象."""

from __future__ import annotations

from typing import Any, Protocol

from pydantic import Field

from contracts.base import ContractModel


class ExtractionResult(ContractModel):
    """構造化抽出結果."""

    ok: bool = Field(..., description="成功可否")
    data: dict[str, Any] | list[dict[str, Any]] | None = Field(default=None, description="抽出データ")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタ情報")


class StructuredExtractionProvider(Protocol):
    """構造化抽出 provider 契約."""

    name: str

    async def extract(self, markdown: str, schema: dict[str, Any] | None = None) -> ExtractionResult:
        """Markdown から構造化データを抽出する."""
