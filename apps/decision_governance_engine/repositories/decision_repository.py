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
from enum import Enum
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
        report_case_id: str | None = None,
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
            report_case_id: 提案書ID（PROP-*）
            results: 各セクション結果 dict
            evidence: 証拠リスト
            claims: クレームリスト
            processing_time_ms: 処理時間
            requester_role: リクエスタ役割
            organization_size: 組織規模

        Returns:
            保存された DecisionRecord
        """
        results = self._to_jsonable(results or {})
        evidence = self._to_jsonable(evidence or [])
        claims = self._to_jsonable(claims or [])

        async with get_db_session() as session:
            record = DecisionRecord(
                request_id=request_id,
                question=question,
                mode=mode,
                decision_role=decision_role,
                confidence=confidence,
                report_case_id=report_case_id,
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

    # ステージ名 → DecisionRecord カラム名のマッピング
    _STAGE_COLUMN_MAP: dict[str, str] = {
        "cognitive_gate": "cognitive_gate_result",
        "gatekeeper": "gatekeeper_result",
        "clarification": "cognitive_gate_result",  # clarification は cognitive_gate に含める
        "dao": "dao_result",
        "fa": "fa_result",
        "shu": "shu_result",
        "qi": "qi_result",
        "review": "review_result",
    }

    async def upsert_stage(
        self,
        request_id: UUID,
        question: str,
        stage_name: str,
        stage_result: dict[str, Any],
        decision_role: str = "PILOT",
        mode: str = "STANDARD",
    ) -> DecisionRecord:
        """ステージ単位で決策記録をINSERT/UPDATE（upsert）.

        各ステージ完了後に呼ばれ、該当カラムのみ更新する。
        request_id が未登録なら新規INSERT、既存なら部分UPDATE。

        Args:
            request_id: リクエスト一意ID
            question: 入力された質問
            stage_name: ステージ名（dao, fa, shu, qi 等）
            stage_result: ステージの実行結果
            decision_role: 暫定の決策結果
            mode: 実行モード

        Returns:
            更新された DecisionRecord
        """
        column_name = self._STAGE_COLUMN_MAP.get(stage_name)
        if not column_name:
            logger.warning(f"Unknown stage for upsert: {stage_name}")
            return None  # type: ignore[return-value]

        normalized_result = self._to_jsonable(stage_result)

        async with get_db_session() as session:
            # 既存レコードを検索
            result = await session.execute(
                select(DecisionRecord).where(DecisionRecord.request_id == request_id)
            )
            record = result.scalar_one_or_none()

            if record is None:
                # 新規レコード作成（最初のステージ）
                kwargs: dict[str, Any] = {
                    "request_id": request_id,
                    "question": question,
                    "mode": mode,
                    "decision_role": decision_role,
                    column_name: normalized_result,
                }
                record = DecisionRecord(**kwargs)
                session.add(record)
            else:
                # 既存レコードの該当カラムを更新
                setattr(record, column_name, normalized_result)

            await session.flush()
            logger.info(f"Stage '{stage_name}' upserted for request {request_id}")

            await self._invalidate_cache(str(request_id))
            return record

    async def get_completed_stages(self, request_id: UUID) -> set[str]:
        """完了済みステージ名の集合を返す（再分析スキップ用）.

        Args:
            request_id: リクエスト一意ID

        Returns:
            完了済みステージ名のセット（例: {"dao", "fa"}）
        """
        async with get_db_session() as session:
            result = await session.execute(
                select(DecisionRecord).where(DecisionRecord.request_id == request_id)
            )
            record = result.scalar_one_or_none()
            if record is None:
                return set()

            completed: set[str] = set()
            # 逆マッピング：カラム名 → ステージ名
            for stage_name, col_name in self._STAGE_COLUMN_MAP.items():
                if stage_name == "clarification":
                    continue  # clarification は cognitive_gate と共用
                val = getattr(record, col_name, None)
                if val is not None:
                    completed.add(stage_name)
            return completed

    async def finalize(
        self,
        request_id: UUID,
        decision_role: str = "PILOT",
        confidence: float | None = None,
        report_case_id: str | None = None,
        processing_time_ms: int | None = None,
    ) -> bool:
        """パイプライン完了後にメタ情報を確定（decision_role, confidence 等）.

        ステージ単位保存で作成済みレコードの最終更新を行う。
        レコードが存在しない場合は False を返す。

        Args:
            request_id: リクエスト一意ID
            decision_role: 最終決策結果（GO/NO_GO/DELAY/PILOT）
            confidence: 最終確信度
            report_case_id: 提案書ID
            processing_time_ms: 全体処理時間

        Returns:
            更新成功なら True
        """
        async with get_db_session() as session:
            result = await session.execute(
                select(DecisionRecord).where(DecisionRecord.request_id == request_id)
            )
            record = result.scalar_one_or_none()
            if record is None:
                logger.warning(f"finalize: record not found for {request_id}")
                return False

            record.decision_role = decision_role
            if confidence is not None:
                record.confidence = confidence
            if report_case_id:
                record.report_case_id = report_case_id
            if processing_time_ms is not None:
                record.processing_time_ms = processing_time_ms

            await session.flush()
            logger.info(f"Decision record finalized: {request_id}")
            await self._invalidate_cache(str(request_id))
            return True

    async def find_by_request_id(self, request_id: UUID) -> DecisionRecord | None:
        """リクエストIDで決策記録を検索."""
        async with get_db_session() as session:
            result = await session.execute(
                select(DecisionRecord).where(DecisionRecord.request_id == request_id)
            )
            return result.scalar_one_or_none()

    async def find_by_report_case_id(
        self,
        report_case_id: str,
    ) -> DecisionRecord | None:
        """提案書ID（PROP-*）で決策記録を検索."""
        async with get_db_session() as session:
            result = await session.execute(
                select(DecisionRecord).where(
                    DecisionRecord.report_case_id == report_case_id
                )
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

    async def append_human_review_record(
        self,
        request_id: UUID,
        review_record: dict[str, Any],
        updated_review: dict[str, Any] | None = None,
    ) -> bool:
        """人間確認レコードを review_result に追記."""
        async with get_db_session() as session:
            result = await session.execute(
                select(DecisionRecord).where(DecisionRecord.request_id == request_id)
            )
            record = result.scalar_one_or_none()
            if record is None:
                return False

            current_review = record.review_result or {}
            if not isinstance(current_review, dict):
                current_review = {}
            history = current_review.get("human_review_records", [])
            if not isinstance(history, list):
                history = []

            history.append(self._to_jsonable(review_record))
            current_review["human_review_records"] = history[-50:]

            if updated_review:
                normalized_review = self._to_jsonable(updated_review)
                for key in ["overall_verdict", "confidence_score", "findings", "final_warnings"]:
                    if key in normalized_review:
                        current_review[key] = normalized_review[key]

            record.review_result = current_review
            record.human_review_records = current_review["human_review_records"]
            return True

    def _to_jsonable(self, value: Any) -> Any:
        """JSONB 保存可能な形式へ正規化."""
        if value is None:
            return None
        if isinstance(value, (str, int, float, bool)):
            return value
        if isinstance(value, Enum):
            return value.value
        if isinstance(value, datetime):
            return value.isoformat()
        if isinstance(value, UUID):
            return str(value)
        if hasattr(value, "model_dump"):
            try:
                return self._to_jsonable(value.model_dump())
            except Exception:
                return self._to_jsonable(dict(value))  # type: ignore[arg-type]
        if isinstance(value, dict):
            return {
                str(k): self._to_jsonable(v)
                for k, v in value.items()
            }
        if isinstance(value, list):
            return [self._to_jsonable(v) for v in value]
        if isinstance(value, tuple):
            return [self._to_jsonable(v) for v in value]
        return str(value)

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
