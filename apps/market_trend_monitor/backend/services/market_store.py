"""Market Trend Monitor のインメモリストア.

API から参照される最新スナップショットを保持します。
"""

import asyncio
import logging
from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.db import init_db
from apps.market_trend_monitor.backend.db.models import ReportHistoryModel
from apps.market_trend_monitor.backend.db.session import async_session
from sqlalchemy import select


class MarketStore:
    """インメモリストア."""

    _MAX_REPORT_HISTORY = 50

    def __init__(self, evidence_service=None, session_factory=None) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._lock = asyncio.Lock()
        self._articles: list[dict[str, Any]] = []
        self._trends: list[dict[str, Any]] = []
        self._reports: list[dict[str, Any]] = []
        self._notifications: list[dict[str, Any]] = []
        self._signals: list[dict[str, Any]] = []
        self._evidences: list[dict[str, Any]] = []
        self._updated_at: str | None = None
        self._evidence_service = evidence_service
        self._session_factory = session_factory or async_session

    async def update_from_flow(self, flow_results: dict[str, Any]) -> None:
        """フロー結果からストアを更新."""
        async with self._lock:
            collector = flow_results.get("collector", {})
            analyzer = flow_results.get("analyzer", {})
            reporter = flow_results.get("reporter", {})
            notifier = flow_results.get("notifier", {})
            evidence = flow_results.get("evidence_ledger", {})
            signal = flow_results.get("signal_scorer", {})

            new_articles = list(collector.get("articles", []))
            new_trends = list(analyzer.get("trends", []))

            report = reporter.get("report")
            if new_articles:
                self._articles = new_articles
            if new_trends:
                self._trends = new_trends

            if isinstance(report, dict):
                report_with_snapshot = self._attach_report_snapshot(report, new_trends)
                self._reports = self._prepend_unique_report(report_with_snapshot, self._reports)
                self._reports = self._reports[: self._MAX_REPORT_HISTORY]
                await self._upsert_report_history(report_with_snapshot)

            self._notifications = list(notifier.get("notifications", []))
            self._evidences = list(evidence.get("evidences", []))
            self._signals = list(signal.get("signals", []))
            self._updated_at = datetime.now().isoformat()

    async def initialize(self) -> None:
        """データベースから以前の状態をロード."""
        if not self._evidence_service:
            return

        async with self._lock:
            try:
                # 証拠を取得（間接的に記事情報を復元）
                evidences = await self._evidence_service.list_evidences()
                self._evidences = [self._evidence_to_dict(e) for e in evidences]

                # 記事情報を復元
                self._articles = [
                    {
                        "id": e.id,
                        "title": e.title,
                        "url": e.url,
                        "source": e.source_type.value,
                        "published_at": e.collected_at.isoformat(),
                        "content": e.extracted_data.get("content", ""),
                        "keywords": e.extracted_data.get("keywords", []),
                        "collected_at": e.collected_at.isoformat(),
                        "metadata": e.metadata,
                    }
                    for e in evidences
                ]

                self._reports = await self._load_report_history(limit=self._MAX_REPORT_HISTORY)
                if self._reports and not self._trends:
                    latest_snapshot = self._reports[0].get("trend_snapshot", [])
                    if isinstance(latest_snapshot, list):
                        self._trends = latest_snapshot

                if self._evidences or self._reports:
                    self._updated_at = datetime.now().isoformat()
            except Exception as e:
                self._logger.warning("MarketStore初期化でDB読込に失敗: %s", e)

    def _evidence_to_dict(self, e: Any) -> dict[str, Any]:
        """Evidence オブジェクトを辞書に変換."""
        return {
            "id": e.id,
            "source_id": e.source_id,
            "source_type": e.source_type.value,
            "url": e.url,
            "title": e.title,
            "extracted_data": e.extracted_data,
            "collected_at": e.collected_at.isoformat(),
            "reliability_score": e.reliability_score,
            "metadata": e.metadata,
        }

    def _prepend_unique_report(
        self,
        report: dict[str, Any],
        existing_reports: list[dict[str, Any]],
    ) -> list[dict[str, Any]]:
        """レポート履歴に重複なく先頭追加."""
        report_id = str(report.get("id", ""))
        if not report_id:
            return existing_reports

        filtered = [item for item in existing_reports if str(item.get("id", "")) != report_id]
        return [report, *filtered]

    def _attach_report_snapshot(
        self,
        report: dict[str, Any],
        trends: list[dict[str, Any]],
    ) -> dict[str, Any]:
        """レポートにトレンド快照を埋め込む."""
        enriched = dict(report)
        metadata = dict(report.get("metadata", {}))
        metadata["trend_snapshot_count"] = len(trends)
        enriched["metadata"] = metadata
        enriched["trend_snapshot"] = list(trends)
        if "created_at" not in enriched:
            enriched["created_at"] = report.get("generated_at", datetime.now().isoformat())
        return enriched

    async def _upsert_report_history(self, report: dict[str, Any]) -> None:
        """レポート履歴をDBへ保存."""
        report_id = str(report.get("id", ""))
        if not report_id:
            return

        generated_at_raw = str(report.get("generated_at") or report.get("created_at") or datetime.now().isoformat())
        generated_at = self._parse_iso_datetime(generated_at_raw)

        await init_db()
        async with self._session_factory() as session:
            existing = await session.get(ReportHistoryModel, report_id)
            if existing:
                existing.generated_at = generated_at
                existing.payload = report
            else:
                session.add(
                    ReportHistoryModel(
                        id=report_id,
                        generated_at=generated_at,
                        payload=report,
                    )
                )
            await session.commit()

    async def _load_report_history(self, limit: int) -> list[dict[str, Any]]:
        """DBからレポート履歴を取得."""
        await init_db()
        async with self._session_factory() as session:
            stmt = select(ReportHistoryModel).order_by(ReportHistoryModel.generated_at.desc()).limit(limit)
            rows = await session.execute(stmt)
            return [
                dict(model.payload)
                for model in rows.scalars().all()
                if isinstance(model.payload, dict)
            ]

    def _parse_iso_datetime(self, value: str) -> datetime:
        """ISO文字列を安全にdatetimeへ変換."""
        normalized = value.replace("Z", "+00:00")
        try:
            return datetime.fromisoformat(normalized)
        except ValueError:
            return datetime.now()

    async def list_trends(self, limit: int | None = None) -> list[dict[str, Any]]:
        """トレンド一覧を取得."""
        async with self._lock:
            trends = list(self._trends)
        return trends[:limit] if limit else trends

    async def get_trend(self, trend_id: str) -> dict[str, Any] | None:
        """トレンドを取得."""
        async with self._lock:
            for trend in self._trends:
                if trend.get("id") == trend_id:
                    return trend
        return None

    async def list_articles(self, limit: int | None = None) -> list[dict[str, Any]]:
        """記事一覧を取得."""
        async with self._lock:
            articles = list(self._articles)
        return articles[:limit] if limit else articles

    async def list_reports(self, limit: int | None = None) -> list[dict[str, Any]]:
        """レポート一覧を取得."""
        async with self._lock:
            reports = list(self._reports)
        return reports[:limit] if limit else reports

    async def get_report(self, report_id: str) -> dict[str, Any] | None:
        """レポートを取得."""
        async with self._lock:
            for report in self._reports:
                if report.get("id") == report_id:
                    return report
        return None

    async def list_notifications(self) -> list[dict[str, Any]]:
        """通知一覧を取得."""
        async with self._lock:
            return list(self._notifications)

    async def list_signals(self, limit: int | None = None) -> list[dict[str, Any]]:
        """信号一覧を取得."""
        async with self._lock:
            signals = list(self._signals)
        return signals[:limit] if limit else signals

    async def list_evidences(self, limit: int | None = None) -> list[dict[str, Any]]:
        """証拠一覧を取得."""
        async with self._lock:
            evidences = list(self._evidences)
        return evidences[:limit] if limit else evidences

    async def get_meta(self) -> dict[str, Any]:
        """メタ情報を取得."""
        async with self._lock:
            return {
                "updated_at": self._updated_at,
                "articles": len(self._articles),
                "trends": len(self._trends),
                "reports": len(self._reports),
                "notifications": len(self._notifications),
                "signals": len(self._signals),
                "evidences": len(self._evidences),
            }
