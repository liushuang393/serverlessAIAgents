# -*- coding: utf-8 -*-
"""情報源台帳サービス.

情報源の信頼度・有効性・利用条件を管理します。
"""

from __future__ import annotations

import logging
import uuid
from datetime import datetime
from typing import Any

from sqlalchemy import func, select
from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker

from apps.market_trend_monitor.backend.db.models import SourceRegistryModel
from apps.market_trend_monitor.backend.db.session import async_session, init_db
from apps.market_trend_monitor.backend.models import SourceRegistryEntry, SourceType


class SourceRegistryService:
    """情報源台帳サービス."""

    def __init__(self, session_factory: async_sessionmaker[AsyncSession] | None = None) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._session_factory = session_factory or async_session

    async def ensure_defaults(self) -> None:
        """デフォルト情報源を登録（空の場合のみ）."""
        await init_db()
        async with self._session_factory() as session:
            result = await session.execute(select(func.count(SourceRegistryModel.id)))
            count = result.scalar_one()
            if count > 0:
                return

            defaults = [
                ("NewsAPI", SourceType.NEWS, "https://newsapi.org"),
                ("GitHub", SourceType.GITHUB, "https://api.github.com"),
                ("arXiv", SourceType.ARXIV, "https://export.arxiv.org"),
                ("RSS Feeds", SourceType.RSS, "https://example.com/rss"),
            ]

            for name, source_type, base_url in defaults:
                entry = SourceRegistryModel(
                    id=str(uuid.uuid4()),
                    name=name,
                    source_type=source_type.value,
                    base_url=base_url,
                    reliability_score=self._default_reliability(source_type),
                    enabled=True,
                    last_checked_at=datetime.now(),
                    metadata_json={},
                )
                session.add(entry)

            await session.commit()
            self._logger.info("デフォルト情報源を登録しました")

    async def list_sources(self, enabled_only: bool = False) -> list[SourceRegistryEntry]:
        """情報源一覧を取得."""
        await init_db()
        async with self._session_factory() as session:
            stmt = select(SourceRegistryModel)
            if enabled_only:
                stmt = stmt.where(SourceRegistryModel.enabled.is_(True))
            stmt = stmt.order_by(SourceRegistryModel.name.asc())
            rows = await session.execute(stmt)
            return [self._model_to_entry(row[0]) for row in rows.fetchall()]

    async def get_source(self, source_id: str) -> SourceRegistryEntry | None:
        """情報源を取得."""
        await init_db()
        async with self._session_factory() as session:
            result = await session.execute(
                select(SourceRegistryModel).where(SourceRegistryModel.id == source_id)
            )
            row = result.first()
            return self._model_to_entry(row[0]) if row else None

    async def register_source(
        self,
        name: str,
        source_type: SourceType,
        base_url: str,
        reliability_score: float = 0.5,
        enabled: bool = True,
        terms_url: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> SourceRegistryEntry:
        """情報源を登録."""
        await init_db()
        async with self._session_factory() as session:
            entry = SourceRegistryModel(
                id=str(uuid.uuid4()),
                name=name,
                source_type=source_type.value,
                base_url=base_url,
                reliability_score=reliability_score,
                enabled=enabled,
                terms_url=terms_url,
                last_checked_at=datetime.now(),
                metadata_json=metadata or {},
            )
            session.add(entry)
            await session.commit()
            await session.refresh(entry)

            self._logger.info("情報源を登録しました: %s", entry.id)
            return self._model_to_entry(entry)

    async def update_source(
        self,
        source_id: str,
        *,
        enabled: bool | None = None,
        reliability_score: float | None = None,
        terms_url: str | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> SourceRegistryEntry | None:
        """情報源を更新."""
        await init_db()
        async with self._session_factory() as session:
            result = await session.execute(
                select(SourceRegistryModel).where(SourceRegistryModel.id == source_id)
            )
            row = result.first()
            if not row:
                return None

            entry = row[0]
            if enabled is not None:
                entry.enabled = enabled
            if reliability_score is not None:
                entry.reliability_score = reliability_score
            if terms_url is not None:
                entry.terms_url = terms_url
            if metadata is not None:
                entry.metadata_json = metadata

            entry.last_checked_at = datetime.now()
            await session.commit()
            await session.refresh(entry)
            self._logger.info("情報源を更新しました: %s", entry.id)
            return self._model_to_entry(entry)

    def _default_reliability(self, source_type: SourceType) -> float:
        """デフォルト信頼度を取得."""
        mapping = {
            SourceType.NEWS: 0.6,
            SourceType.GITHUB: 0.8,
            SourceType.ARXIV: 0.9,
            SourceType.RSS: 0.5,
        }
        return mapping.get(source_type, 0.5)

    def _model_to_entry(self, model: SourceRegistryModel) -> SourceRegistryEntry:
        return SourceRegistryEntry(
            id=model.id,
            name=model.name,
            source_type=SourceType(model.source_type),
            base_url=model.base_url,
            reliability_score=model.reliability_score,
            enabled=model.enabled,
            terms_url=model.terms_url,
            last_checked_at=model.last_checked_at,
            metadata=model.metadata_json,
        )
