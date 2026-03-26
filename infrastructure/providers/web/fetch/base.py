"""Web fetch provider 抽象."""

from typing import Any, Protocol

from pydantic import Field

from contracts.base import ContractModel
from contracts.web import WebRetrievalMode


class FetchResult(ContractModel):
    """fetch 結果."""

    ok: bool = Field(..., description="成功可否")
    url: str = Field(..., description="対象 URL")
    mode: WebRetrievalMode = Field(..., description="実行モード")
    markdown: str | None = Field(default=None, description="取得 Markdown")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタ情報")


class ContentFetchProvider(Protocol):
    """コンテンツ取得 provider 契約."""

    name: str

    async def fetch(self, url: str) -> FetchResult:
        """URL から本文を取得する."""
