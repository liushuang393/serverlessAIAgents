"""In-process strategy registry for Evolution V2."""

from __future__ import annotations

import hashlib
import uuid
from collections import defaultdict
from datetime import UTC, datetime

from agentflow.evolution.scoring import SuccessFirstScoringEngine
from agentflow.evolution.types import (
    RetrievalMode,
    ScopeLevel,
    ScoreUpdateRequest,
    StrategyCapsule,
    StrategyRecord,
    StrategyScoreSnapshot,
    StrategySearchRequest,
    StrategySearchResponse,
    StrategySearchResultItem,
    StrategyStatus,
)


class StrategyRegistry:
    """In-memory registry with scope-aware lexical retrieval."""

    def __init__(
        self,
        *,
        scoring_engine: SuccessFirstScoringEngine | None = None,
    ) -> None:
        self._records: dict[str, StrategyRecord] = {}
        self._keyword_index: dict[str, set[str]] = defaultdict(set)
        self._scoring = scoring_engine or SuccessFirstScoringEngine()

    async def register(
        self,
        capsule: StrategyCapsule,
        *,
        status: StrategyStatus = StrategyStatus.CANDIDATE,
        parent_strategy_id: str | None = None,
    ) -> StrategyRecord:
        now = datetime.now(UTC)
        strategy_id = f"strat-{uuid.uuid4().hex[:12]}"
        intent_hash = hashlib.md5(
            capsule.intent.strip().lower().encode("utf-8"),
            usedforsecurity=False,
        ).hexdigest()

        version = self._next_version(capsule.scope, intent_hash)
        record = StrategyRecord(
            id=strategy_id,
            tenant_id=capsule.scope.tenant_id,
            app_id=capsule.scope.app_id,
            product_line=capsule.scope.product_line,
            scope_level=capsule.scope.scope_level,
            intent_hash=intent_hash,
            capsule=capsule,
            status=status,
            version=version,
            parent_strategy_id=parent_strategy_id,
            valid_from=capsule.validity.valid_from,
            valid_to=capsule.validity.valid_to,
            last_verified_at=now if status == StrategyStatus.VERIFIED else None,
            created_at=now,
            updated_at=now,
            score=StrategyScoreSnapshot(final_score=0.0),
        )

        self._records[strategy_id] = record
        for keyword in capsule.keywords:
            self._keyword_index[keyword.lower()].add(strategy_id)
        return record

    async def search(self, request: StrategySearchRequest) -> StrategySearchResponse:
        if request.decision_mode == RetrievalMode.SKIP:
            return StrategySearchResponse(
                decision="skip",
                reasons=["decision_mode=skip"],
            )

        candidates = self._select_scope_candidates(request)
        tokens = self._tokens(f"{request.intent} {request.task_signature or ''}")

        scored: list[tuple[float, StrategyRecord, list[str]]] = []
        now = datetime.now(UTC)
        for record in candidates:
            lexical = self._lexical_score(tokens, record)
            base = record.score.final_score
            if base <= 0:
                base = 0.5

            scope_boost = self._scope_boost(record.scope_level)
            staleness_penalty = self._staleness_penalty(record, now)
            score = max(0.0, min(1.0, 0.55 * base + 0.45 * lexical + scope_boost - staleness_penalty))

            reasons = [
                f"lexical={lexical:.2f}",
                f"base={base:.2f}",
                f"scope={record.scope_level.value}",
            ]
            if staleness_penalty > 0:
                reasons.append(f"staleness_penalty={staleness_penalty:.2f}")

            threshold = 0.45 if request.decision_mode == RetrievalMode.LIGHT else 0.35
            if score >= threshold:
                scored.append((score, record, reasons))

        scored.sort(key=lambda item: item[0], reverse=True)
        top = scored[: request.top_k]
        items = [
            StrategySearchResultItem(
                strategy_id=record.id,
                scope_level=record.scope_level,
                status=record.status,
                score=score,
                reasons=reasons,
                capsule=record.capsule,
                last_verified_at=record.last_verified_at,
            )
            for score, record, reasons in top
        ]

        if not items:
            return StrategySearchResponse(
                decision="fallback_execute",
                reasons=["no_match_above_threshold"],
                score_breakdown={"candidates": len(candidates)},
            )

        return StrategySearchResponse(
            decision="strategy_match",
            reasons=["matched_candidates"],
            strategies=items,
            score_breakdown={
                "candidates": len(candidates),
                "returned": len(items),
                "top_score": items[0].score,
            },
        )

    async def get(self, strategy_id: str) -> StrategyRecord | None:
        return self._records.get(strategy_id)

    async def update_status(self, strategy_id: str, status: StrategyStatus) -> None:
        record = self._records.get(strategy_id)
        if record is None:
            return
        record.status = status
        record.updated_at = datetime.now(UTC)
        if status == StrategyStatus.VERIFIED:
            record.last_verified_at = datetime.now(UTC)

    async def score_update(self, request: ScoreUpdateRequest) -> StrategyRecord | None:
        record = self._records.get(request.strategy_id)
        if record is None:
            return None

        snapshot = record.score
        current_count = snapshot.sample_count
        new_count = current_count + 1

        success_value = 1.0 if request.success else 0.0
        snapshot.success_rate_30d = (
            (snapshot.success_rate_30d * current_count + success_value) / new_count if new_count > 0 else success_value
        )
        snapshot.success_rate_7d = (
            0.7 * snapshot.success_rate_7d + 0.3 * success_value if current_count > 0 else success_value
        )

        if request.success:
            snapshot.reuse_count_30d += 1

        reuse = min(1.0, snapshot.reuse_count_30d / 50.0)
        age_days = self._verified_age_days(record)
        snapshot.freshness_decay = self._scoring.freshness_decay(age_days)
        snapshot.avg_latency_ms = (
            (snapshot.avg_latency_ms * current_count + request.latency_ms) / new_count
            if new_count > 0
            else request.latency_ms
        )
        snapshot.condition_match = request.condition_match
        snapshot.cost_efficiency = request.cost_efficiency

        suspect = self._scoring.should_mark_suspect(
            failure_streak=request.failure_streak,
            success_7d=snapshot.success_rate_7d,
            success_30d=snapshot.success_rate_30d,
            environment_mismatch=request.environment_mismatch,
            last_verified_age_days=age_days,
            max_age_days=record.capsule.validity.max_age_days,
        )
        snapshot.suspicion_level = 1.0 if suspect else max(0.0, snapshot.suspicion_level * 0.8)

        snapshot.final_score = self._scoring.compute_final_score(
            success_rate_30d=snapshot.success_rate_30d,
            success_rate_7d=snapshot.success_rate_7d,
            reuse=reuse,
            freshness=snapshot.freshness_decay,
            condition_match=snapshot.condition_match,
            cost_efficiency=snapshot.cost_efficiency,
            suspicion_penalty=0.25 * snapshot.suspicion_level,
        )
        snapshot.sample_count = new_count
        snapshot.updated_at = datetime.now(UTC)

        if suspect:
            record.status = StrategyStatus.SUSPECT

        record.updated_at = datetime.now(UTC)
        return record

    def _next_version(self, scope: object, intent_hash: str) -> int:
        tenant_id = getattr(scope, "tenant_id", None)
        app_id = getattr(scope, "app_id", None)
        product_line = getattr(scope, "product_line", None)
        scope_level = getattr(scope, "scope_level", ScopeLevel.TENANT_APP)

        version = 1
        for record in self._records.values():
            if (
                record.intent_hash == intent_hash
                and record.scope_level == scope_level
                and record.tenant_id == tenant_id
                and record.app_id == app_id
                and record.product_line == product_line
            ):
                version = max(version, record.version + 1)
        return version

    def _select_scope_candidates(self, request: StrategySearchRequest) -> list[StrategyRecord]:
        app_scope = [
            record
            for record in self._records.values()
            if record.scope_level == ScopeLevel.TENANT_APP
            and record.status != StrategyStatus.DEPRECATED
            and record.tenant_id == request.tenant_id
            and record.app_id == request.app_id
        ]

        pl_scope = [
            record
            for record in self._records.values()
            if record.scope_level == ScopeLevel.TENANT_PRODUCT_LINE
            and record.status != StrategyStatus.DEPRECATED
            and record.tenant_id == request.tenant_id
            and record.product_line == request.product_line
        ]

        global_scope = [
            record
            for record in self._records.values()
            if record.scope_level == ScopeLevel.GLOBAL_VERIFIED and record.status == StrategyStatus.VERIFIED
        ]

        return app_scope + pl_scope + global_scope

    def _tokens(self, value: str) -> set[str]:
        return {token.strip().lower() for token in value.split() if token.strip()}

    def _lexical_score(self, query_tokens: set[str], record: StrategyRecord) -> float:
        if not query_tokens:
            return 0.0
        if not record.capsule.keywords:
            return 0.0

        keyword_tokens = {token.lower() for token in record.capsule.keywords}
        overlap = len(query_tokens & keyword_tokens)
        return overlap / max(1, len(query_tokens))

    def _scope_boost(self, scope_level: ScopeLevel) -> float:
        if scope_level == ScopeLevel.TENANT_APP:
            return 0.12
        if scope_level == ScopeLevel.TENANT_PRODUCT_LINE:
            return 0.06
        return 0.0

    def _verified_age_days(self, record: StrategyRecord) -> int:
        baseline = record.last_verified_at or record.created_at
        return max(0, int((datetime.now(UTC) - baseline).total_seconds() / 86400))

    def _staleness_penalty(self, record: StrategyRecord, now: datetime) -> float:
        if record.valid_to and now > record.valid_to:
            return 0.40

        max_age_days = record.capsule.validity.max_age_days
        age_days = self._verified_age_days(record)
        if age_days > max_age_days:
            return 0.25

        if record.status == StrategyStatus.SUSPECT:
            return 0.20

        return 0.0
