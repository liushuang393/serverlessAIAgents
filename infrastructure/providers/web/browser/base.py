"""Web browser operation provider 抽象."""

from typing import Any, Protocol

from pydantic import Field

from contracts.base import ContractModel
from contracts.web.contracts import BrowserActionStep


class BrowserOperationRequest(ContractModel):
    """ブラウザ操作要求."""

    url: str = Field(..., description="開始 URL")
    steps: list[BrowserActionStep] = Field(default_factory=list, description="操作ステップ")
    return_markdown: bool = Field(default=True, description="Markdown 返却可否")
    allowed_domains: list[str] | None = Field(default=None, description="許可ドメイン")


class BrowserOperationResult(ContractModel):
    """ブラウザ操作結果."""

    ok: bool = Field(..., description="成功可否")
    markdown: str | None = Field(default=None, description="取得 Markdown")
    artifacts: dict[str, Any] = Field(default_factory=dict, description="成果物情報")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加メタ情報")


class BrowserOperator(Protocol):
    """ブラウザ操作 provider 契約."""

    name: str

    async def run(self, request: BrowserOperationRequest) -> BrowserOperationResult:
        """ブラウザ操作を実行する."""
