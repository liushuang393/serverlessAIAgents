"""トレンド履歴サービス.

トレンドのライフサイクル追跡、速度・加速度計算を提供します。
Phase 11: トレンド履歴のDB永続化。
Phase 12: 速度・加速度分析。
"""

from __future__ import annotations

import logging
import uuid
from datetime import datetime
from typing import TYPE_CHECKING, Any

from apps.market_trend_monitor.backend.db.models import TrendHistoryModel
from apps.market_trend_monitor.backend.db.session import async_session, init_db
from sqlalchemy import select

if TYPE_CHECKING:
    from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker


class TrendHistoryService:
    """トレンド履歴サービス."""

    def __init__(
        self,
        session_factory: async_sessionmaker[AsyncSession] | None = None,
    ) -> None:
        self._logger = logging.getLogger(self.__class__.__name__)
        self._session_factory = session_factory or async_session

    async def capture_trends(
        self,
        trends: list[dict[str, Any]],
        run_id: str | None = None,
    ) -> int:
        """トレンドスナップショットをDBに保存.

        Args:
            trends: トレンドデータリスト
            run_id: 実行ID

        Returns:
            保存件数
        """
        await init_db()
        run_id = run_id or str(uuid.uuid4())
        now = datetime.now()
        count = 0

        async with self._session_factory() as session:
            for trend in trends:
                model = TrendHistoryModel(
                    id=str(uuid.uuid4()),
                    topic=trend.get("topic", ""),
                    score=float(trend.get("score", 0.0)),
                    growth_rate=float(trend.get("growth_rate", 0.0)),
                    sentiment=trend.get("sentiment", "neutral"),
                    articles_count=int(trend.get("articles_count", 0)),
                    run_id=run_id,
                    captured_at=now,
                    metadata_json=trend.get("metadata", {}),
                )
                session.add(model)
                count += 1
            await session.commit()

        self._logger.info("トレンド履歴を保存: %d件, run_id=%s", count, run_id)
        return count

    async def get_topic_history(
        self,
        topic: str,
        limit: int = 50,
    ) -> list[dict[str, Any]]:
        """トピックの履歴を取得.

        Args:
            topic: トピック名
            limit: 最大件数

        Returns:
            履歴データリスト（時系列順）
        """
        await init_db()
        async with self._session_factory() as session:
            stmt = (
                select(TrendHistoryModel)
                .where(TrendHistoryModel.topic == topic)
                .order_by(TrendHistoryModel.captured_at.desc())
                .limit(limit)
            )
            rows = await session.execute(stmt)
            results = [self._model_to_dict(r[0]) for r in rows.fetchall()]
            return list(reversed(results))

    async def get_velocity(self, topic: str, window: int = 5) -> float:
        """トレンドの速度（1階微分）を計算.

        直近window件のスコア平均 - その前window件のスコア平均。

        Args:
            topic: トピック名
            window: ウィンドウサイズ

        Returns:
            速度値
        """
        history = await self.get_topic_history(topic, limit=window * 2)
        if len(history) < window + 1:
            return 0.0

        recent = history[-window:]
        older = history[-(window * 2):-window]

        if not older:
            return 0.0

        recent_avg = sum(h["score"] for h in recent) / len(recent)
        older_avg = sum(h["score"] for h in older) / len(older)
        return recent_avg - older_avg

    async def get_acceleration(self, topic: str, window: int = 5) -> float:
        """トレンドの加速度（2階微分）を計算.

        速度の変化率。

        Args:
            topic: トピック名
            window: ウィンドウサイズ

        Returns:
            加速度値
        """
        history = await self.get_topic_history(topic, limit=window * 3)
        if len(history) < window * 2 + 1:
            return 0.0

        # 3区間に分割して速度を2つ計算
        seg1 = history[:window]
        seg2 = history[window:window * 2]
        seg3 = history[window * 2:]

        if not seg1 or not seg2 or not seg3:
            return 0.0

        avg1 = sum(h["score"] for h in seg1) / len(seg1)
        avg2 = sum(h["score"] for h in seg2) / len(seg2)
        avg3 = sum(h["score"] for h in seg3) / len(seg3)

        velocity_old = avg2 - avg1
        velocity_new = avg3 - avg2
        return velocity_new - velocity_old

    async def list_topics(self) -> list[str]:
        """追跡中のトピック一覧を取得."""
        await init_db()
        async with self._session_factory() as session:
            stmt = select(TrendHistoryModel.topic).distinct()
            rows = await session.execute(stmt)
            return [row[0] for row in rows.fetchall()]

    @staticmethod
    def _model_to_dict(model: TrendHistoryModel) -> dict[str, Any]:
        return {
            "id": model.id,
            "topic": model.topic,
            "score": model.score,
            "growth_rate": model.growth_rate,
            "sentiment": model.sentiment,
            "articles_count": model.articles_count,
            "run_id": model.run_id,
            "captured_at": model.captured_at.isoformat(),
            "metadata": model.metadata_json,
        }
