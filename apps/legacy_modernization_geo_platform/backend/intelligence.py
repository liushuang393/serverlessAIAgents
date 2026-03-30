"""External intelligence integration for GEO demand discovery.

DI パターンにより decision_governance_engine への直接依存を排除。
IntelligenceServiceProtocol を満たす任意のサービスを注入可能。
"""

from __future__ import annotations

import logging
import os
from dataclasses import dataclass
from datetime import UTC, datetime, timedelta
from typing import TYPE_CHECKING, Any, Protocol, runtime_checkable

if TYPE_CHECKING:
    from apps.legacy_modernization_geo_platform.backend.schemas import GeoExecuteRequest

logger = logging.getLogger(__name__)


# ---------------------------------------------------------------------------
# DI 用 Protocol（クロスアプリ依存を排除）
# ---------------------------------------------------------------------------


@runtime_checkable
class IntelligenceServiceProtocol(Protocol):
    """IntelligenceService が満たすべき構造型."""

    async def gather(self, query: str, topics: list[str] | None = None) -> Any: ...


# ---------------------------------------------------------------------------
# データクラス
# ---------------------------------------------------------------------------


@dataclass(slots=True)
class NormalizedEvidenceSource:
    """パイプライン全体で使用する正規化検索結果."""

    url: str
    title: str
    publisher: str
    summary: str
    snippet: str
    reliability: str
    published_at: datetime | None
    retrieved_at: datetime
    provider: str
    tags: list[str]

    @property
    def is_fresh(self) -> bool:
        """ソースがまだ新鮮とみなせるか."""
        anchor = _ensure_utc(self.published_at or self.retrieved_at)
        return datetime.now(UTC) - anchor <= timedelta(days=30)

    @property
    def citation_ready(self) -> bool:
        """引用に十分なメタデータを持つか."""
        return bool(self.url and self.summary and (self.snippet or self.title))

    def to_evidence_dict(self) -> dict[str, str | bool]:
        """JSON シリアライズ可能な evidence ペイロードに変換する."""
        return {
            "url": self.url,
            "title": self.title,
            "publisher": self.publisher,
            "summary": self.summary,
            "snippet": self.snippet,
            "reliability": self.reliability,
            "citation_ready": self.citation_ready,
            "fresh": self.is_fresh,
            "provider": self.provider,
        }


@dataclass(slots=True)
class IntelligenceSnapshot:
    """外部検索プロバイダからの集約結果."""

    sources: list[NormalizedEvidenceSource]
    warnings: list[str]
    primary_provider: str


# ---------------------------------------------------------------------------
# アダプター
# ---------------------------------------------------------------------------


class GeoIntelligenceAdapter:
    """共有 Intelligence Service を GEO 固有デフォルトでラップする.

    service 引数で DI すると decision_governance_engine への依存なしに動作する。
    service=None の場合、フォールバックとして decision_governance_engine を遅延インポートする。
    Docker 等コンテナ環境では service を明示注入することを推奨する。
    tool_gate を注入すると、外部 API 呼び出し前にガバナンス評価を挿入する。
    """

    def __init__(
        self,
        service: IntelligenceServiceProtocol | None = None,
        *,
        tool_gate: Any | None = None,
    ) -> None:
        if service is None:
            service = _create_default_intelligence_service()
        self._service: IntelligenceServiceProtocol = service
        self._tool_gate = tool_gate

    async def gather_market_intelligence(self, request: GeoExecuteRequest) -> IntelligenceSnapshot:
        """リクエストに基づき正規化インテリジェンスを収集する."""
        if os.getenv("GEO_PLATFORM_USE_SAMPLE_INTELLIGENCE") == "1":
            return self._sample_snapshot(request)

        await self._evaluate_tool_gate(request)

        queries = self._build_queries(request)
        sources: list[NormalizedEvidenceSource] = []
        warnings: list[str] = []
        primary_provider = "none"

        for query in queries:
            result = await self._service.gather(query, topics=self._build_topics(request))
            provider = getattr(result, "provider", "unknown")
            if primary_provider == "none" and provider:
                primary_provider = provider
            warnings.extend(getattr(result, "warnings", []))
            sources.extend(self._normalize_result(result))

        deduplicated = self._deduplicate_sources(sources)
        return IntelligenceSnapshot(
            sources=deduplicated[:8],
            warnings=warnings,
            primary_provider=primary_provider,
        )

    async def _evaluate_tool_gate(self, request: GeoExecuteRequest) -> None:
        """ToolGate によるガバナンス評価を実行する."""
        if self._tool_gate is None:
            return
        from infrastructure.sandbox.tool_provider import RegisteredTool

        tool_descriptor = RegisteredTool(
            name="intelligence_service.gather",
            description="外部検索プロバイダへのインテリジェンス収集",
            provider_type="method",
            operation_type="read",
            risk_level="medium",
        )
        decision = await self._tool_gate.evaluate(
            tool_descriptor,
            tool_call_id=None,
            arguments={"campaign_name": request.campaign_name},
        )
        if decision.decision not in {"allow", "allowed"}:
            msg = f"ToolGate がインテリジェンス収集を拒否: {decision.reason}"
            raise PermissionError(msg)

    def _build_queries(self, request: GeoExecuteRequest) -> list[str]:
        """実行リクエストから検索クエリを構築する."""
        accounts = list(request.inputs.target_accounts)
        industries = list(request.targets.industries)
        stacks = list(request.targets.legacy_stacks)
        regions = request.inputs.regions or request.targets.regions or ["Japan"]
        region_text = " ".join(regions[:2])
        stack_text = " ".join(stacks[:2]) if stacks else "legacy modernization"

        queries: list[str] = []
        for account in accounts[:2]:
            queries.append(f"{account} {stack_text} modernization {region_text}")
        for industry in industries[:2]:
            queries.append(f"{industry} {stack_text} migration demand {region_text}")
        if not queries:
            queries.append(f"legacy modernization demand {region_text}")
        return queries

    def _build_topics(self, request: GeoExecuteRequest) -> list[str]:
        """Intelligence Service 用の関連トピックを構築する."""
        topics = list(request.targets.legacy_stacks)
        topics.extend(request.inputs.target_services)
        topics.extend(request.targets.industries)
        return [item for item in topics if item][:5]

    def _normalize_result(self, result: Any) -> list[NormalizedEvidenceSource]:
        """共有サービスのレスポンスを正規化する."""
        normalized: list[NormalizedEvidenceSource] = []
        evidence_items = getattr(result, "evidence", [])
        result_provider = getattr(result, "provider", "unknown")
        for item in evidence_items:
            reliability_raw = getattr(item, "reliability", "MEDIUM")
            reliability_str = reliability_raw.value if hasattr(reliability_raw, "value") else str(reliability_raw)
            normalized.append(
                NormalizedEvidenceSource(
                    url=getattr(item, "url", ""),
                    title=getattr(item, "title", ""),
                    publisher=getattr(item, "publisher", None) or result_provider,
                    summary=getattr(item, "summary", None) or getattr(item, "snippet", ""),
                    snippet=getattr(item, "snippet", ""),
                    reliability=reliability_str,
                    published_at=getattr(item, "published_at", None),
                    retrieved_at=getattr(item, "retrieved_at", datetime.now(UTC)),
                    provider=result_provider,
                    tags=list(getattr(item, "tags", [])),
                ),
            )
        return normalized

    def _deduplicate_sources(
        self,
        sources: list[NormalizedEvidenceSource],
    ) -> list[NormalizedEvidenceSource]:
        """URL で重複排除し、順序を保持する."""
        seen: set[str] = set()
        deduplicated: list[NormalizedEvidenceSource] = []
        for source in sources:
            if source.url in seen:
                continue
            seen.add(source.url)
            deduplicated.append(source)
        return deduplicated

    def _sample_snapshot(self, request: GeoExecuteRequest) -> IntelligenceSnapshot:
        """自動テスト用の決定論的インテリジェンスを返す."""
        stack = (request.targets.legacy_stacks or ["COBOL"])[0]
        industry = (request.targets.industries or ["manufacturing"])[0]
        now = datetime.now(UTC)
        sources = [
            NormalizedEvidenceSource(
                url=f"https://example.test/{industry}/{stack.lower()}-modernization",
                title=f"{industry} {stack} modernization demand signal",
                publisher="sample-intel",
                summary=f"{industry} organizations are evaluating incremental {stack} modernization programs.",
                snippet="Sample evidence for automated tests.",
                reliability="HIGH",
                published_at=now,
                retrieved_at=now,
                provider="sample",
                tags=["sample", industry, stack],
            ),
            NormalizedEvidenceSource(
                url=f"https://example.test/{industry}/migration-roadmap",
                title=f"{industry} migration roadmap",
                publisher="sample-intel",
                summary="Roadmap evidence indicating phased migration is preferred over big bang rewrites.",
                snippet="Phased migration reduces delivery risk.",
                reliability="MEDIUM",
                published_at=now,
                retrieved_at=now,
                provider="sample",
                tags=["sample", "roadmap"],
            ),
            NormalizedEvidenceSource(
                url=f"https://example.test/{industry}/skills-gap",
                title="Legacy skills gap report",
                publisher="sample-intel",
                summary="The maintenance talent gap is driving modernization urgency.",
                snippet="Skills shortages increase modernization urgency.",
                reliability="HIGH",
                published_at=now,
                retrieved_at=now,
                provider="sample",
                tags=["sample", "skills-gap"],
            ),
        ]
        return IntelligenceSnapshot(sources=sources, warnings=[], primary_provider="sample")


# ---------------------------------------------------------------------------
# ヘルパー
# ---------------------------------------------------------------------------


def _load_intelligence_providers() -> list[str]:
    """app_config.json の services.intelligence.providers を読み取る."""
    import json
    from pathlib import Path

    config_path = Path(__file__).resolve().parent.parent / "app_config.json"
    if not config_path.is_file():
        return ["serpapi", "bing", "duckduckgo_search"]
    try:
        config = json.loads(config_path.read_text(encoding="utf-8"))
        providers = config.get("services", {}).get("intelligence", {}).get("providers")
        if isinstance(providers, list) and providers:
            return [str(p) for p in providers]
    except (json.JSONDecodeError, OSError):
        logger.warning("app_config.json の intelligence 設定の読み取りに失敗しました")
    return ["serpapi", "bing", "duckduckgo_search"]


def _create_default_intelligence_service() -> IntelligenceServiceProtocol:
    """decision_governance_engine から遅延ロードでデフォルトサービスを生成する.

    app_config.json の services.intelligence.providers を正本として使用する。
    Docker コンテナ等で decision_governance_engine が利用不可の場合は
    ImportError を送出するため、呼び出し元で DI すること。
    """
    try:
        from apps.decision_governance_engine.services.intelligence_service import (
            IntelligenceConfig,
            IntelligenceService,
        )
    except ImportError:
        msg = (
            "decision_governance_engine が見つかりません。"
            "GeoIntelligenceAdapter(service=...) で IntelligenceServiceProtocol を"
            "満たすサービスを明示注入してください。"
        )
        raise ImportError(msg) from None
    providers = _load_intelligence_providers()
    return IntelligenceService(
        IntelligenceConfig(
            provider_preference=providers,
            mode="STANDARD",
        ),
    )


def _ensure_utc(value: datetime) -> datetime:
    """datetime を UTC タイムゾーン付きに正規化する."""
    if value.tzinfo is None:
        return value.replace(tzinfo=UTC)
    return value.astimezone(UTC)
