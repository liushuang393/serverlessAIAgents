"""Evidence Ledger サービス.

証拠台帳の管理を行うビジネスロジック層。
- 証拠の登録・検索・更新
- 主張の管理とレベル自動昇格
- 証拠チェーンの構築
"""

from __future__ import annotations

import hashlib
import logging
import math
import uuid
from datetime import datetime
from typing import TYPE_CHECKING, Any

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
    Phase 9: BayesianConfidenceServiceによる信頼度のベイズ更新。
    Phase 12: SourceReliabilityTrackerによる動的信頼度。
    """

    def __init__(
        self,
        session_factory: async_sessionmaker[AsyncSession] | None = None,
        *,
        bayesian_confidence_service: Any | None = None,
        source_reliability_tracker: Any | None = None,
    ) -> None:
        """初期化.

        Args:
            session_factory: セッションファクトリ
            bayesian_confidence_service: ベイズ信頼度サービス（Phase 9）
            source_reliability_tracker: 情報源信頼度追跡（Phase 12）
        """
        self._logger = logging.getLogger(self.__class__.__name__)
        self._session_factory = session_factory or async_session
        # Phase 9: ベイズ信頼度サービス
        self._bayesian_confidence = bayesian_confidence_service
        # Phase 12: 動的信頼度追跡
        self._reliability_tracker = source_reliability_tracker
        # 情報源別信頼度設定（fallback）
        self._source_reliability: dict[SourceType, float] = {
            SourceType.NEWS: 0.6,
            SourceType.GITHUB: 0.8,
            SourceType.ARXIV: 0.9,
            SourceType.RSS: 0.5,
        }
        # Grounding Guard: 結論公開可否を判定するしきい値
        self._min_evidence_for_publish = 3
        self._min_avg_reliability = 0.65
        self._min_source_diversity = 2
        self._min_citation_ready_ratio = 0.7
        self._min_fresh_ratio = 0.4
        self._min_high_reliability = 0.75
        self._min_citation_reliability = 0.6
        self._freshness_window_days = 21
        self._min_content_chars_for_quote = 120

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

    async def add_evidence_to_claim(
        self,
        claim_id: str,
        evidence_id: str,
        is_supporting: bool = True,
    ) -> Claim | None:
        """主張に証拠を追加.

        Args:
            claim_id: 主張ID
            evidence_id: 証拠ID
            is_supporting: 支持証拠かどうか（Phase 9）
        """
        await init_db()
        async with self._session_factory() as session:
            claim_model = await self._get_claim_model(session, claim_id)
            if not claim_model:
                return None

            if not await self._has_evidence(session, evidence_id):
                return self._model_to_claim(claim_model)

            if evidence_id not in claim_model.evidence_ids:
                claim_model.evidence_ids = [*claim_model.evidence_ids, evidence_id]
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

            # Phase 9: ベイズ信頼度更新
            if self._bayesian_confidence:
                try:
                    evidence_model = await self._get_evidence_model(session, evidence_id)
                    weight = evidence_model.reliability_score if evidence_model else 0.5
                    await self._bayesian_confidence.update_confidence(
                        claim_id=claim_id,
                        is_supporting=is_supporting,
                        weight=weight,
                    )
                except Exception as e:
                    self._logger.warning("ベイズ信頼度更新失敗: %s", e)

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

    async def get_grounding_guard(self) -> dict[str, Any]:
        """証拠品質を集約し、結論公開可否を判定.

        NotebookLM の引用トレース可能性と、MiroThinker の軌跡可監査性を
        組み合わせた運用を想定した診断メトリクスを返します。
        """
        evidences = await self.list_evidences()
        claims = await self.list_claims()
        now = datetime.now()

        evidence_diagnostics = [
            self._build_evidence_diagnostic(evidence, now) for evidence in evidences
        ]
        evidence_lookup = {item["evidence_id"]: item for item in evidence_diagnostics}
        claim_diagnostics = [
            self._build_claim_diagnostic(claim, evidence_lookup) for claim in claims
        ]

        total_evidence = len(evidence_diagnostics)
        source_diversity = len({item["source_type"] for item in evidence_diagnostics})
        citation_ready_count = sum(1 for item in evidence_diagnostics if item["citation_ready"])
        high_reliability_count = sum(
            1
            for item in evidence_diagnostics
            if item["reliability_score"] >= self._min_high_reliability
        )
        fresh_count = sum(1 for item in evidence_diagnostics if item["is_fresh"])
        supported_claim_count = sum(
            1 for item in claim_diagnostics if item["status"] == "supported"
        )

        avg_reliability = (
            sum(item["reliability_score"] for item in evidence_diagnostics) / total_evidence
            if total_evidence
            else 0.0
        )
        citation_ready_ratio = self._safe_ratio(citation_ready_count, total_evidence)
        fresh_ratio = self._safe_ratio(fresh_count, total_evidence)
        supported_claim_ratio = self._safe_ratio(supported_claim_count, len(claim_diagnostics))

        blockers: list[dict[str, Any]] = []
        if total_evidence < self._min_evidence_for_publish:
            blockers.append(
                {
                    "code": "LOW_EVIDENCE_COUNT",
                    "message": "証拠件数が不足しています。",
                    "current": total_evidence,
                    "required": self._min_evidence_for_publish,
                }
            )
        if avg_reliability < self._min_avg_reliability:
            blockers.append(
                {
                    "code": "LOW_AVG_RELIABILITY",
                    "message": "平均信頼度が公開基準に達していません。",
                    "current": round(avg_reliability, 3),
                    "required": self._min_avg_reliability,
                }
            )
        if source_diversity < self._min_source_diversity:
            blockers.append(
                {
                    "code": "LOW_SOURCE_DIVERSITY",
                    "message": "情報源の多様性が不足しています。",
                    "current": source_diversity,
                    "required": self._min_source_diversity,
                }
            )
        if citation_ready_ratio < self._min_citation_ready_ratio:
            blockers.append(
                {
                    "code": "LOW_CITATION_READY_RATIO",
                    "message": "引用可能な証拠比率が不足しています。",
                    "current": round(citation_ready_ratio, 3),
                    "required": self._min_citation_ready_ratio,
                }
            )
        if fresh_ratio < self._min_fresh_ratio:
            blockers.append(
                {
                    "code": "LOW_FRESH_RATIO",
                    "message": "証拠の鮮度が低下しています。",
                    "current": round(fresh_ratio, 3),
                    "required": self._min_fresh_ratio,
                }
            )

        action_map = {
            "LOW_EVIDENCE_COUNT": "同一テーマで追加収集を実行し、最低3件以上に増やしてください。",
            "LOW_AVG_RELIABILITY": "低信頼ソース比率を下げ、一次情報ソースを優先してください。",
            "LOW_SOURCE_DIVERSITY": "異なる source_type から最低2系統の裏取りを追加してください。",
            "LOW_CITATION_READY_RATIO": "本文付きURL証拠を追加し、引用可能な状態へ整備してください。",
            "LOW_FRESH_RATIO": "直近3週間以内の証拠を追加収集してください。",
        }
        actions = [action_map[item["code"]] for item in blockers]

        status = "ready" if not blockers else "needs_more_evidence"
        if status == "ready":
            actions.append("出力時は主張ごとに evidence_id / URL を必ず明示してください。")

        return {
            "status": status,
            "summary": {
                "total_evidence": total_evidence,
                "claims_count": len(claim_diagnostics),
                "avg_reliability": round(avg_reliability, 3),
                "source_diversity": source_diversity,
                "citation_ready_count": citation_ready_count,
                "citation_ready_ratio": round(citation_ready_ratio, 3),
                "high_reliability_count": high_reliability_count,
                "fresh_count": fresh_count,
                "fresh_ratio": round(fresh_ratio, 3),
                "supported_claim_count": supported_claim_count,
                "supported_claim_ratio": round(supported_claim_ratio, 3),
            },
            "thresholds": {
                "min_evidence_for_publish": self._min_evidence_for_publish,
                "min_avg_reliability": self._min_avg_reliability,
                "min_source_diversity": self._min_source_diversity,
                "min_citation_ready_ratio": self._min_citation_ready_ratio,
                "min_fresh_ratio": self._min_fresh_ratio,
                "freshness_window_days": self._freshness_window_days,
                "min_content_chars_for_quote": self._min_content_chars_for_quote,
            },
            "blockers": blockers,
            "actions": actions,
            "evidence_diagnostics": evidence_diagnostics,
            "claim_diagnostics": claim_diagnostics,
        }

    def _compute_content_hash(self, url: str, content: str) -> str:
        """コンテンツハッシュを計算."""
        data = f"{url}:{content}".encode()
        return hashlib.sha256(data).hexdigest()[:16]

    def _build_evidence_diagnostic(self, evidence: Evidence, now: datetime) -> dict[str, Any]:
        """証拠単位のトレーサビリティ診断を作成."""
        content = str(evidence.extracted_data.get("content", "")).strip()
        content_length = len(content)
        has_content = content_length >= self._min_content_chars_for_quote
        has_keywords = bool(evidence.extracted_data.get("keywords"))
        has_url = bool(evidence.url)
        freshness_days = max((now - evidence.collected_at).days, 0)
        is_fresh = freshness_days <= self._freshness_window_days

        citation_ready = (
            has_url and has_content and evidence.reliability_score >= self._min_citation_reliability
        )

        traceability_score = (
            evidence.reliability_score * 0.45
            + (1.0 if has_url else 0.0) * 0.25
            + (1.0 if has_content else 0.0) * 0.2
            + (1.0 if has_keywords else 0.0) * 0.1
        )

        return {
            "evidence_id": evidence.id,
            "source_type": evidence.source_type.value,
            "reliability_score": round(evidence.reliability_score, 3),
            "content_length": content_length,
            "freshness_days": freshness_days,
            "is_fresh": is_fresh,
            "citation_ready": citation_ready,
            "traceability_score": round(self._clamp(traceability_score), 3),
            "quote_preview": self._build_quote_preview(content),
        }

    def _build_claim_diagnostic(
        self,
        claim: Claim,
        evidence_lookup: dict[str, dict[str, Any]],
    ) -> dict[str, Any]:
        """主張単位の被支持度診断を作成."""
        linked = [evidence_lookup[eid] for eid in claim.evidence_ids if eid in evidence_lookup]
        evidence_count = len(claim.evidence_ids)
        linked_count = len(linked)
        citation_ready_count = sum(1 for item in linked if item["citation_ready"])
        avg_traceability = (
            sum(float(item["traceability_score"]) for item in linked) / linked_count
            if linked_count
            else 0.0
        )
        coverage_score = self._safe_ratio(citation_ready_count, evidence_count)

        if evidence_count == 0:
            status = "unsupported"
        elif coverage_score >= 0.8 and claim.confidence >= 0.7:
            status = "supported"
        elif coverage_score >= 0.5:
            status = "partial"
        else:
            status = "weak"

        return {
            "claim_id": claim.id,
            "statement": claim.statement,
            "level": claim.level.value,
            "confidence": round(claim.confidence, 3),
            "evidence_count": evidence_count,
            "linked_evidence_count": linked_count,
            "citation_ready_count": citation_ready_count,
            "coverage_score": round(coverage_score, 3),
            "avg_traceability": round(avg_traceability, 3),
            "status": status,
        }

    def _build_quote_preview(self, content: str) -> str:
        """本文から引用プレビューを抽出."""
        if not content:
            return ""
        normalized = " ".join(content.split())
        if len(normalized) <= 180:
            return normalized
        return f"{normalized[:180]}..."

    def _safe_ratio(self, numerator: int, denominator: int) -> float:
        """ゼロ除算を回避した比率計算."""
        if denominator <= 0:
            return 0.0
        return self._clamp(numerator / denominator)

    def _clamp(self, value: float) -> float:
        """0.0-1.0 に正規化."""
        return max(0.0, min(1.0, value))

    async def _find_by_hash(self, session: AsyncSession, content_hash: str) -> EvidenceModel | None:
        """ハッシュで証拠を検索."""
        result = await session.execute(
            select(EvidenceModel).where(EvidenceModel.content_hash == content_hash)
        )
        row = result.first()
        return row[0] if row else None

    def _calculate_reliability(self, source_type: SourceType) -> float:
        """情報源タイプに基づく信頼度を計算.

        Phase 12: 動的追跡サービスが利用可能な場合はそちらを優先。
        """
        if self._reliability_tracker:
            return self._reliability_tracker.get_reliability(source_type.value)
        return self._source_reliability.get(source_type, 0.5)

    def _apply_time_decay(self, reliability: float, collected_at: datetime) -> float:
        """Phase 12: 証拠の時間減衰を適用.

        指数減衰関数: decay = exp(-age_days / 180)
        半減期 ≈ 125日。下限30%。
        """
        age_days = (datetime.now() - collected_at).days
        decay = math.exp(-age_days / 180.0)
        return reliability * (0.3 + 0.7 * decay)

    async def _calculate_claim_confidence(
        self, session: AsyncSession, evidence_ids: list[str]
    ) -> float:
        """証拠に基づく主張信頼度を計算（時間減衰適用）."""
        if not evidence_ids:
            return 0.0

        result = await session.execute(
            select(
                EvidenceModel.reliability_score,
                EvidenceModel.collected_at,
            ).where(EvidenceModel.id.in_(evidence_ids))
        )
        rows = result.fetchall()
        if not rows:
            return 0.0

        # Phase 12: 時間減衰を適用した信頼度
        decayed_scores = [self._apply_time_decay(row[0], row[1]) for row in rows]
        avg_reliability = sum(decayed_scores) / len(decayed_scores)
        evidence_bonus = min(len(decayed_scores) / 5, 1.0) * 0.15
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
        result = await session.execute(select(EvidenceModel).where(EvidenceModel.id == evidence_id))
        row = result.first()
        return row[0] if row else None

    async def _get_claim_model(self, session: AsyncSession, claim_id: str) -> ClaimModel | None:
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
