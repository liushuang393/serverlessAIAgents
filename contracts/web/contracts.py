"""Web Intelligence 契約."""

from __future__ import annotations

from enum import StrEnum
from typing import Any, Literal, Protocol, runtime_checkable

from pydantic import Field, model_validator

from contracts.base import ContractModel


class WebIntentType(StrEnum):
    """Web 要求の意図種別."""

    SEARCH = "search"
    READ_URL = "read_url"
    EXTRACT = "extract"
    COMPARE = "compare"
    MONITOR = "monitor"
    OPERATE = "operate"
    CRAWL = "crawl"


class AccuracyLevel(StrEnum):
    """精度要求."""

    HIGH = "high"
    NORMAL = "normal"
    LOW = "low"


class LatencyLevel(StrEnum):
    """遅延要求."""

    LOW = "low"
    NORMAL = "normal"
    RELAXED = "relaxed"


class BudgetLevel(StrEnum):
    """コスト要求."""

    LOW = "low"
    NORMAL = "normal"
    HIGH = "high"


class FreshnessLevel(StrEnum):
    """鮮度要求."""

    REALTIME = "realtime"
    RECENT = "recent"
    STABLE = "stable"


class WebRetrievalMode(StrEnum):
    """取得モード."""

    DIRECT_MARKDOWN = "direct_markdown"
    HTML_READABILITY = "html_readability"
    RENDERED_MARKDOWN = "rendered_markdown"
    SEARCH_THEN_FETCH = "search_then_fetch"
    BROWSER_MCP = "browser_mcp"
    CRAWL_MODE = "crawl_mode"


class EstimatedCostLevel(StrEnum):
    """推定コストレベル."""

    LOW = "low"
    MEDIUM = "medium"
    HIGH = "high"


class BrowserActionStep(ContractModel):
    """ブラウザ操作 1 ステップ."""

    action: Literal["navigate", "click", "type", "fill", "wait", "expand", "paginate", "download"] = Field(
        ...,
        description="操作種別",
    )
    selector: str | None = Field(default=None, description="対象セレクタ")
    text: str | None = Field(default=None, description="入力テキスト")
    wait_for: str | None = Field(default=None, description="待機対象テキストまたは状態")
    timeout_ms: int | None = Field(default=None, ge=0, description="ステップ単位タイムアウト")
    download_name: str | None = Field(default=None, description="ダウンロード成果物名")

    @model_validator(mode="after")
    def validate_required_fields(self) -> BrowserActionStep:
        """action ごとの必須フィールドを検証する."""
        if self.action in {"click", "expand", "paginate", "download"} and not self.selector:
            msg = f"{self.action} requires selector"
            raise ValueError(msg)
        if self.action in {"type", "fill"}:
            if not self.selector:
                msg = f"{self.action} requires selector"
                raise ValueError(msg)
            if self.text is None:
                msg = f"{self.action} requires text"
                raise ValueError(msg)
        if self.action == "wait" and self.wait_for is None and self.selector is None and self.timeout_ms is None:
            msg = "wait requires wait_for, selector, or timeout_ms"
            raise ValueError(msg)
        if self.action == "navigate" and not self.wait_for:
            msg = "navigate requires wait_for as target URL"
            raise ValueError(msg)
        return self


class WebIntent(ContractModel):
    """Web 取得の意図."""

    intent: WebIntentType = Field(..., description="意図")
    query: str | None = Field(default=None, description="検索クエリ")
    url: str | None = Field(default=None, description="単一 URL")
    urls: list[str] | None = Field(default=None, description="複数 URL")
    task: str | None = Field(default=None, description="タスク説明")


class QualityConstraints(ContractModel):
    """品質制約."""

    accuracy: AccuracyLevel = Field(default=AccuracyLevel.NORMAL, description="精度要求")
    latency: LatencyLevel = Field(default=LatencyLevel.NORMAL, description="遅延要求")
    budget: BudgetLevel = Field(default=BudgetLevel.NORMAL, description="コスト要求")
    freshness: FreshnessLevel = Field(default=FreshnessLevel.RECENT, description="鮮度要求")
    auth_required: bool = Field(default=False, description="認証が必要か")
    interaction_required: bool = Field(default=False, description="操作が必要か")
    structured_output: bool = Field(default=False, description="構造化出力が必要か")


class ExtractionSchema(ContractModel):
    """抽出スキーマ."""

    name: str = Field(..., description="スキーマ名")
    json_schema: dict[str, Any] | None = Field(default=None, description="JSON Schema")


class WebRouterInput(ContractModel):
    """Router 入力契約."""

    request_id: str = Field(..., description="要求識別子")
    intent: WebIntent = Field(..., description="意図")
    constraints: QualityConstraints = Field(default_factory=QualityConstraints, description="品質制約")
    extraction_schema: ExtractionSchema | None = Field(default=None, description="抽出スキーマ")
    operation_steps: list[BrowserActionStep] = Field(default_factory=list, description="ブラウザ操作ステップ")
    allowed_domains: list[str] | None = Field(default=None, description="許可ドメイン")
    blocked_domains: list[str] | None = Field(default=None, description="禁止ドメイン")


class EvidenceItem(ContractModel):
    """根拠情報."""

    url: str = Field(..., description="根拠 URL")
    title: str | None = Field(default=None, description="タイトル")
    snippet: str | None = Field(default=None, description="要約断片")
    markdown: str | None = Field(default=None, description="本文 Markdown")
    metadata: dict[str, Any] = Field(default_factory=dict, description="追加情報")
    confidence: float = Field(default=0.0, ge=0.0, le=1.0, description="根拠信頼度")


class WebRouterOutput(ContractModel):
    """Router 出力契約."""

    mode_used: WebRetrievalMode = Field(..., description="使用モード")
    answer_markdown: str | None = Field(default=None, description="回答 Markdown")
    extracted_data: dict[str, Any] | list[dict[str, Any]] | None = Field(default=None, description="抽出データ")
    evidence: list[EvidenceItem] = Field(default_factory=list, description="根拠一覧")
    citations: list[str] = Field(..., description="引用一覧")
    latency_ms: int = Field(..., ge=0, description="処理時間")
    estimated_cost_level: EstimatedCostLevel = Field(..., description="推定コストレベル")
    confidence: float = Field(..., ge=0.0, le=1.0, description="全体信頼度")
    fallback_used: bool = Field(..., description="フォールバック使用有無")
    metadata: dict[str, Any] = Field(default_factory=dict, description="mode ごとの観測情報")


@runtime_checkable
class WebRetrievalRouter(Protocol):
    """Web Router の canonical 契約."""

    async def execute(self, req: WebRouterInput) -> WebRouterOutput:
        """要求を実行する."""
