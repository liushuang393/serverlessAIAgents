"""Evidence Ledger サービス.

証拠台帳の管理を行うビジネスロジック層。
- 証拠の登録・検索・更新
- 主張の管理とレベル自動昇格
- 証拠チェーンの構築
"""

from __future__ import annotations

import hashlib
import logging
import uuid
from datetime import datetime
from typing import TYPE_CHECKING

from apps.market_trend_monitor.backend.db.models import ClaimModel, EvidenceModel
from apps.market_trend_monitor.backend.db.session import async_session, init_db
from apps.market_trend_monitor.backend.models import (
    Article,
    Claim,
    ClaimLevel,
    Evidence,
    SourceType,
)
from sqlalchemy import select


if TYPE_CHECKING:
    from sqlalchemy.ext.asyncio import AsyncSession, async_sessionmaker


class EvidenceService:
    """証拠管理サービス.

    全ての結論に対する証拠の追跡可能性を確保します。
    """

    def __init__(self, session_factory: async_sessionmaker[AsyncSession] | None = None) -> None:
        """初期化."""
        self._logger = logging.getLogger(self.__class__.__name__)
        self._session_factory = session_factory or async_session
        # 情報源別信頼度設定
        self._source_reliability: dict[SourceType, float] = {
            SourceType.NEWS: 0.6,
            SourceType.GITHUB: 0.8,
            SourceType.ARXIV: 0.9,
            SourceType.RSS: 0.5,
        }

    async def register_evidence_from_article(self, article: Article) -> Evidence:
        """記事から証拠を登録.

        Args:
            article: 収集した記事

        Returns:
            登録された証拠
        """
        content_hash = self._compute_content_hash(article.url, article.content)

        await init_db()
        async with self._session_factory() as session:
            existing = await self._find_by_hash(session, content_hash)
            if existing:
                self._logger.debug("重複証拠を検出: %s", existing.id)
                return self._model_to_evidence(existing)

            evidence = EvidenceModel(
                id=str(uuid.uuid4()),
                source_id=article.id,
                source_type=article.source.value,
                url=article.url,
                title=article.title,
                content_hash=content_hash,
                extracted_data={
                    "keywords": article.keywords,
                    "content": article.content,
                    "metadata": article.metadata,
                },
                collected_at=article.collected_at,
                reliability_score=self._calculate_reliability(article.source),
                metadata_json=article.metadata,
            )
            session.add(evidence)
            await session.commit()
            await session.refresh(evidence)

            self._logger.info("証拠を登録: %s", evidence.id)
            return self._model_to_evidence(evidence)

    async def register_evidences_batch(self, articles: list[Article]) -> list[Evidence]:
        """複数記事から証拠を一括登録."""
        await init_db()
        results: list[Evidence] = []
        for article in articles:
            results.append(await self.register_evidence_from_article(article))
        return results

    async def create_claim(self, statement: str, evidence_ids: list[str]) -> Claim:
        """主張を作成."""
        await init_db()
        async with self._session_factory() as session:
            valid_evidence_ids = await self._filter_existing_evidence_ids(session, evidence_ids)
            confidence = await self._calculate_claim_confidence(session, valid_evidence_ids)

            claim = Claim(
                id=str(uuid.uuid4()),
                statement=statement,
                evidence_ids=valid_evidence_ids,
                confidence=confidence,
            )
            claim.update_level()

            model = ClaimModel(
                id=claim.id,
                statement=claim.statement,
                level=claim.level.value,
                confidence=claim.confidence,
                evidence_ids=claim.evidence_ids,
                counter_evidence_ids=claim.counter_evidence_ids,
                created_at=claim.created_at,
                updated_at=claim.updated_at,
                metadata_json=claim.metadata,
            )
            session.add(model)
            await session.commit()
            await session.refresh(model)

            self._logger.info("主張を作成: %s, level=%s", claim.id, claim.level.value)
            return claim

    async def add_evidence_to_claim(self, claim_id: str, evidence_id: str) -> Claim | None:
        """主張に証拠を追加."""
        await init_db()
        async with self._session_factory() as session:
            claim_model = await self._get_claim_model(session, claim_id)
            if not claim_model:
                return None

            if not await self._has_evidence(session, evidence_id):
                return self._model_to_claim(claim_model)

            if evidence_id not in claim_model.evidence_ids:
                claim_model.evidence_ids.append(evidence_id)
                claim_model.confidence = await self._calculate_claim_confidence(
                    session, claim_model.evidence_ids
                )
                level = self._calculate_claim_level(
                    evidence_count=len(claim_model.evidence_ids),
                    confidence=claim_model.confidence,
                )
                claim_model.level = level.value
                claim_model.updated_at = datetime.now()
                await session.commit()
                await session.refresh(claim_model)

            return self._model_to_claim(claim_model)

    async def get_evidence_chain(self, claim_id: str) -> list[Evidence]:
        """主張の証拠チェーンを取得."""
        await init_db()
        async with self._session_factory() as session:
            claim_model = await self._get_claim_model(session, claim_id)
            if not claim_model:
                return []

            evidences = await self._get_evidences_by_ids(session, claim_model.evidence_ids)
            return sorted(evidences, key=lambda e: e.reliability_score, reverse=True)

    async def get_evidence(self, evidence_id: str) -> Evidence | None:
        """証拠を取得."""
        await init_db()
        async with self._session_factory() as session:
            model = await self._get_evidence_model(session, evidence_id)
            return self._model_to_evidence(model) if model else None

    async def get_claim(self, claim_id: str) -> Claim | None:
        """主張を取得."""
        await init_db()
        async with self._session_factory() as session:
            model = await self._get_claim_model(session, claim_id)
            return self._model_to_claim(model) if model else None

    async def list_evidences(
        self,
        source_type: SourceType | None = None,
        min_reliability: float = 0.0,
    ) -> list[Evidence]:
        """証拠一覧を取得."""
        await init_db()
        async with self._session_factory() as session:
            stmt = select(EvidenceModel)
            if source_type:
                stmt = stmt.where(EvidenceModel.source_type == source_type.value)
            if min_reliability > 0:
                stmt = stmt.where(EvidenceModel.reliability_score >= min_reliability)
            stmt = stmt.order_by(EvidenceModel.collected_at.desc())
            rows = await session.execute(stmt)
            return [self._model_to_evidence(r[0]) for r in rows.fetchall()]

    async def list_claims(
        self,
        level: ClaimLevel | None = None,
        min_confidence: float = 0.0,
    ) -> list[Claim]:
        """主張一覧を取得."""
        await init_db()
        async with self._session_factory() as session:
            stmt = select(ClaimModel)
            if level:
                stmt = stmt.where(ClaimModel.level == level.value)
            if min_confidence > 0:
                stmt = stmt.where(ClaimModel.confidence >= min_confidence)
            stmt = stmt.order_by(ClaimModel.updated_at.desc())
            rows = await session.execute(stmt)
            return [self._model_to_claim(r[0]) for r in rows.fetchall()]

    async def list_evidences_in_window(
        self,
        since: datetime,
        until: datetime | None = None,
    ) -> list[Evidence]:
        """期間内の証拠を取得."""
        await init_db()
        async with self._session_factory() as session:
            stmt = select(EvidenceModel).where(EvidenceModel.collected_at >= since)
            if until:
                stmt = stmt.where(EvidenceModel.collected_at < until)
            rows = await session.execute(stmt)
            return [self._model_to_evidence(r[0]) for r in rows.fetchall()]

    async def get_keyword_weighted_stats(
        self,
        since: datetime,
        until: datetime | None = None,
    ) -> dict[str, float]:
        """キーワードの加重出現数を取得."""
        evidences = await self.list_evidences_in_window(since, until)
        stats: dict[str, float] = {}
        for evidence in evidences:
            keywords = list(evidence.extracted_data.get("keywords", []))
            for keyword in keywords:
                stats[keyword] = stats.get(keyword, 0.0) + evidence.reliability_score
        return stats

    def _compute_content_hash(self, url: str, content: str) -> str:
        """コンテンツハッシュを計算."""
        data = f"{url}:{content}".encode()
        return hashlib.sha256(data).hexdigest()[:16]

    async def _find_by_hash(
        self, session: AsyncSession, content_hash: str
    ) -> EvidenceModel | None:
        """ハッシュで証拠を検索."""
        result = await session.execute(
            select(EvidenceModel).where(EvidenceModel.content_hash == content_hash)
        )
        row = result.first()
        return row[0] if row else None

    def _calculate_reliability(self, source_type: SourceType) -> float:
        """情報源タイプに基づく信頼度を計算."""
        return self._source_reliability.get(source_type, 0.5)

    async def _calculate_claim_confidence(
        self, session: AsyncSession, evidence_ids: list[str]
    ) -> float:
        """証拠に基づく主張信頼度を計算."""
        if not evidence_ids:
            return 0.0

        result = await session.execute(
            select(EvidenceModel.reliability_score).where(EvidenceModel.id.in_(evidence_ids))
        )
        scores = [row[0] for row in result.fetchall()]
        if not scores:
            return 0.0

        avg_reliability = sum(scores) / len(scores)
        evidence_bonus = min(len(scores) / 5, 1.0) * 0.15
        return min(avg_reliability + evidence_bonus, 1.0)

    async def _filter_existing_evidence_ids(
        self, session: AsyncSession, evidence_ids: list[str]
    ) -> list[str]:
        if not evidence_ids:
            return []
        result = await session.execute(
            select(EvidenceModel.id).where(EvidenceModel.id.in_(evidence_ids))
        )
        return [row[0] for row in result.fetchall()]

    async def _get_evidences_by_ids(
        self, session: AsyncSession, evidence_ids: list[str]
    ) -> list[Evidence]:
        if not evidence_ids:
            return []
        result = await session.execute(
            select(EvidenceModel).where(EvidenceModel.id.in_(evidence_ids))
        )
        return [self._model_to_evidence(row[0]) for row in result.fetchall()]

    async def _get_evidence_model(
        self, session: AsyncSession, evidence_id: str
    ) -> EvidenceModel | None:
        result = await session.execute(
            select(EvidenceModel).where(EvidenceModel.id == evidence_id)
        )
        row = result.first()
        return row[0] if row else None

    async def _get_claim_model(
        self, session: AsyncSession, claim_id: str
    ) -> ClaimModel | None:
        result = await session.execute(select(ClaimModel).where(ClaimModel.id == claim_id))
        row = result.first()
        return row[0] if row else None

    async def _has_evidence(self, session: AsyncSession, evidence_id: str) -> bool:
        result = await session.execute(
            select(EvidenceModel.id).where(EvidenceModel.id == evidence_id)
        )
        return result.first() is not None

    def _model_to_evidence(self, model: EvidenceModel) -> Evidence:
        return Evidence(
            id=model.id,
            source_id=model.source_id,
            source_type=SourceType(model.source_type),
            url=model.url,
            title=model.title,
            content_hash=model.content_hash,
            extracted_data=model.extracted_data,
            collected_at=model.collected_at,
            reliability_score=model.reliability_score,
            metadata=model.metadata_json,
        )

    def _model_to_claim(self, model: ClaimModel) -> Claim:
        return Claim(
            id=model.id,
            statement=model.statement,
            level=ClaimLevel(model.level),
            confidence=model.confidence,
            evidence_ids=list(model.evidence_ids or []),
            counter_evidence_ids=list(model.counter_evidence_ids or []),
            created_at=model.created_at,
            updated_at=model.updated_at,
            metadata=model.metadata_json,
        )

    def _calculate_claim_level(self, evidence_count: int, confidence: float) -> ClaimLevel:
        if evidence_count >= 5 and confidence > 0.85:
            return ClaimLevel.CONCLUSION
        if evidence_count >= 3 and confidence >= 0.7:
            return ClaimLevel.FINDING
        if evidence_count >= 2 and confidence >= 0.5:
            return ClaimLevel.HYPOTHESIS
        return ClaimLevel.LEAD
