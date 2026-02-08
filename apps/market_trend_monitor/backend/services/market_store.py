# -*- coding: utf-8 -*-
"""Market Trend Monitor のインメモリストア.

API から参照される最新スナップショットを保持します。
"""

import asyncio
from datetime import datetime
from typing import Any


class MarketStore:
    """インメモリストア."""

    def __init__(self) -> None:
        """初期化."""
        self._lock = asyncio.Lock()
        self._articles: list[dict[str, Any]] = []
        self._trends: list[dict[str, Any]] = []
        self._reports: list[dict[str, Any]] = []
        self._notifications: list[dict[str, Any]] = []
        self._signals: list[dict[str, Any]] = []
        self._evidences: list[dict[str, Any]] = []
        self._updated_at: str | None = None

    async def update_from_flow(self, flow_results: dict[str, Any]) -> None:
        """フロー結果からストアを更新."""
        async with self._lock:
            collector = flow_results.get("collector", {})
            analyzer = flow_results.get("analyzer", {})
            reporter = flow_results.get("reporter", {})
            notifier = flow_results.get("notifier", {})
            evidence = flow_results.get("evidence_ledger", {})
            signal = flow_results.get("signal_scorer", {})

            self._articles = list(collector.get("articles", []))
            self._trends = list(analyzer.get("trends", []))

            report = reporter.get("report")
            self._reports = [report] if isinstance(report, dict) else []

            self._notifications = list(notifier.get("notifications", []))
            self._evidences = list(evidence.get("evidences", []))
            self._signals = list(signal.get("signals", []))
            self._updated_at = datetime.now().isoformat()

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
