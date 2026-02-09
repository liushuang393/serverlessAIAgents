"""決策履歴リポジトリ.

目的:
    決策記録の CRUD 操作 + Redis キャッシュ

使用例:
    repo = DecisionRepository()
    record = await repo.save(decision_response)
    history = await repo.find_by_id(record_id)
"""

import json
import logging
from datetime import datetime
from typing import Any
from uuid import UUID

from apps.decision_governance_engine.repositories.database import (
    get_db_session,
    get_redis,
)
from apps.decision_governance_engine.repositories.models import (
    Claim,
    DecisionRecord,
    EvidenceItem,
)
from sqlalchemy import desc, select


logger = logging.getLogger(__name__)

# キャッシュ TTL（秒）
CACHE_TTL_SECONDS = 3600  # 1時間


class DecisionRepository:
    """決策履歴リポジトリ.

    決策記録の永続化・照会・キャッシュを担当。
    """

    def __init__(self) -> None:
        """初期化."""
        self._cache_prefix = "dge:decision:"

    async def save(
        self,
        request_id: UUID,
        question: str,
        decision_role: str,
        mode: str = "STANDARD",
        confidence: float | None = None,
        results: dict[str, Any] | None = None,
        evidence: list[dict[str, Any]] | None = None,
        claims: list[dict[str, Any]] | None = None,
        processing_time_ms: int | None = None,
        requester_role: str | None = None,
        organization_size: str | None = None,
    ) -> DecisionRecord:
        """決策記録を保存.

        Args:
            request_id: リクエスト一意ID
            question: 入力された質問
            decision_role: GO/NO_GO/DELAY/PILOT
            mode: FAST/STANDARD/AUDIT
            confidence: 確信度
            results: 各セクション結果 dict
            evidence: 証拠リスト
            claims: クレームリスト
            processing_time_ms: 処理時間
            requester_role: リクエスタ役割
            organization_size: 組織規模

        Returns:
            保存された DecisionRecord
        """
        results = results or {}

        async with get_db_session() as session:
            record = DecisionRecord(
                request_id=request_id,
                question=question,
                mode=mode,
                decision_role=decision_role,
                confidence=confidence,
                cognitive_gate_result=results.get("cognitive_gate"),
                gatekeeper_result=results.get("gatekeeper"),
                dao_result=results.get("dao"),
                fa_result=results.get("fa"),
                shu_result=results.get("shu"),
                qi_result=results.get("qi"),
                review_result=results.get("review"),
                processing_time_ms=processing_time_ms,
                requester_role=requester_role,
                organization_size=organization_size,
            )
            session.add(record)
            await session.flush()

            # 証拠を保存
            if evidence:
                for ev in evidence:
                    ev_item = EvidenceItem(
                        decision_record_id=record.id,
                        url=ev.get("url"),
                        title=ev.get("title"),
                        publisher=ev.get("publisher"),
                        snippet=ev.get("snippet"),
                        summary=ev.get("summary"),
                        reliability=ev.get("reliability", "MEDIUM"),
                        tags=ev.get("tags"),
                    )
                    session.add(ev_item)

            # クレームを保存
            if claims:
                for cl in claims:
                    claim = Claim(
                        decision_record_id=record.id,
                        claim_type=cl.get("claim_type", "INFERENCE"),
                        statement=cl.get("statement", ""),
                        source_section=cl.get("source_section"),
                        confidence=cl.get("confidence"),
                    )
                    session.add(claim)

            logger.info(f"Decision record saved: {record.id}")

            # キャッシュ無効化
            await self._invalidate_cache(str(request_id))

            return record

    async def find_by_request_id(self, request_id: UUID) -> DecisionRecord | None:
        """リクエストIDで決策記録を検索."""
        async with get_db_session() as session:
            result = await session.execute(
                select(DecisionRecord).where(DecisionRecord.request_id == request_id)
            )
            return result.scalar_one_or_none()

    async def find_recent(
        self,
        limit: int = 20,
        decision_role: str | None = None,
        mode: str | None = None,
    ) -> list[DecisionRecord]:
        """最近の決策記録を取得.

        Args:
            limit: 取得件数上限
            decision_role: フィルタ（GO/NO_GO/DELAY/PILOT）
            mode: フィルタ（FAST/STANDARD/AUDIT）

        Returns:
            DecisionRecord リスト（新しい順）
        """
        async with get_db_session() as session:
            query = select(DecisionRecord).where(DecisionRecord.deleted_at.is_(None))

            if decision_role:
                query = query.where(DecisionRecord.decision_role == decision_role)
            if mode:
                query = query.where(DecisionRecord.mode == mode)

            query = query.order_by(desc(DecisionRecord.created_at)).limit(limit)
            result = await session.execute(query)
            return list(result.scalars().all())

    async def find_by_date_range(
        self,
        start_date: datetime,
        end_date: datetime,
        limit: int = 100,
    ) -> list[DecisionRecord]:
        """日付範囲で決策記録を検索."""
        async with get_db_session() as session:
            result = await session.execute(
                select(DecisionRecord)
                .where(DecisionRecord.created_at >= start_date)
                .where(DecisionRecord.created_at <= end_date)
                .where(DecisionRecord.deleted_at.is_(None))
                .order_by(desc(DecisionRecord.created_at))
                .limit(limit)
            )
            return list(result.scalars().all())

    async def soft_delete(self, request_id: UUID) -> bool:
        """決策記録をソフト削除."""
        async with get_db_session() as session:
            result = await session.execute(
                select(DecisionRecord).where(DecisionRecord.request_id == request_id)
            )
            record = result.scalar_one_or_none()
            if record:
                record.deleted_at = datetime.utcnow()
                await self._invalidate_cache(str(request_id))
                return True
            return False

    async def _get_from_cache(self, key: str) -> dict[str, Any] | None:
        """Redis キャッシュから取得."""
        redis = await get_redis()
        if not redis:
            return None
        try:
            data = await redis.get(f"{self._cache_prefix}{key}")
            if data:
                return json.loads(data)
        except Exception as e:
            logger.warning(f"Cache get failed: {e}")
        return None

    async def _set_cache(self, key: str, data: Any) -> None:
        """Redis キャッシュに保存."""
        redis = await get_redis()
        if not redis:
            return
        try:
            if hasattr(data, "__dict__"):
                # SQLAlchemy モデルの場合は dict 化
                cache_data = {
                    "id": str(data.id),
                    "request_id": str(data.request_id),
                    "question": data.question,
                    "decision_role": data.decision_role,
                    "confidence": float(data.confidence) if data.confidence else None,
                    "mode": data.mode,
                    "created_at": data.created_at.isoformat() if data.created_at else None,
                }
            else:
                cache_data = data
            await redis.setex(
                f"{self._cache_prefix}{key}",
                CACHE_TTL_SECONDS,
                json.dumps(cache_data, ensure_ascii=False),
            )
        except Exception as e:
            logger.warning(f"Cache set failed: {e}")

    async def _invalidate_cache(self, key: str) -> None:
        """Redis キャッシュを無効化."""
        redis = await get_redis()
        if not redis:
            return
        try:
            await redis.delete(f"{self._cache_prefix}{key}")
        except Exception as e:
            logger.warning(f"Cache invalidate failed: {e}")

