"""SQLAlchemy models for Evolution V2 storage."""

from __future__ import annotations

from datetime import UTC, datetime
from typing import Any

from sqlalchemy import (
    JSON,
    BigInteger,
    Boolean,
    DateTime,
    Float,
    ForeignKey,
    Index,
    Integer,
    String,
    Text,
    UniqueConstraint,
)
from sqlalchemy.dialects.postgresql import JSONB
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column


_JSON_TYPE = JSON().with_variant(JSONB, "postgresql")


class EvolutionBase(DeclarativeBase):
    """Base metadata for evolution tables."""


class EvolutionStrategy(EvolutionBase):
    """Strategy master table."""

    __tablename__ = "evolution_strategy"
    __table_args__ = (
        UniqueConstraint(
            "tenant_id",
            "app_id",
            "product_line",
            "scope_level",
            "intent_hash",
            "version",
            name="uq_evolution_strategy_intent_version_scope",
        ),
        Index(
            "ix_evolution_strategy_scope",
            "scope_level",
            "tenant_id",
            "app_id",
            "product_line",
            "status",
        ),
    )

    id: Mapped[str] = mapped_column(String(64), primary_key=True)
    tenant_id: Mapped[str | None] = mapped_column(String(120), nullable=True)
    app_id: Mapped[str | None] = mapped_column(String(120), nullable=True)
    product_line: Mapped[str | None] = mapped_column(String(64), nullable=True)
    scope_level: Mapped[str] = mapped_column(String(40), nullable=False)
    intent_hash: Mapped[str] = mapped_column(String(64), nullable=False)
    capsule_json: Mapped[dict[str, Any]] = mapped_column(_JSON_TYPE, nullable=False)
    status: Mapped[str] = mapped_column(String(32), nullable=False, default="candidate")
    version: Mapped[int] = mapped_column(Integer, nullable=False, default=1)
    parent_strategy_id: Mapped[str | None] = mapped_column(
        String(64),
        ForeignKey("evolution_strategy.id", ondelete="SET NULL"),
        nullable=True,
    )
    valid_from: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)
    valid_to: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)
    last_verified_at: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        default=lambda: datetime.now(UTC),
    )
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        default=lambda: datetime.now(UTC),
    )


class EvolutionStrategyKeyword(EvolutionBase):
    """Keyword inverted index table."""

    __tablename__ = "evolution_strategy_keyword"
    __table_args__ = (Index("ix_evolution_strategy_keyword_keyword", "keyword"),)

    id: Mapped[int] = mapped_column(BigInteger, primary_key=True, autoincrement=True)
    strategy_id: Mapped[str] = mapped_column(
        String(64),
        ForeignKey("evolution_strategy.id", ondelete="CASCADE"),
        nullable=False,
    )
    keyword: Mapped[str] = mapped_column(String(120), nullable=False)
    weight: Mapped[float] = mapped_column(Float, nullable=False, default=1.0)


class EvolutionStrategyScore(EvolutionBase):
    """Score snapshot table."""

    __tablename__ = "evolution_strategy_score"

    strategy_id: Mapped[str] = mapped_column(
        String(64),
        ForeignKey("evolution_strategy.id", ondelete="CASCADE"),
        primary_key=True,
    )
    success_rate_7d: Mapped[float] = mapped_column(Float, nullable=False, default=0.0)
    success_rate_30d: Mapped[float] = mapped_column(Float, nullable=False, default=0.0)
    reuse_count_30d: Mapped[int] = mapped_column(Integer, nullable=False, default=0)
    avg_latency_ms: Mapped[float] = mapped_column(Float, nullable=False, default=0.0)
    freshness_decay: Mapped[float] = mapped_column(Float, nullable=False, default=1.0)
    suspicion_level: Mapped[float] = mapped_column(Float, nullable=False, default=0.0)
    final_score: Mapped[float] = mapped_column(Float, nullable=False, default=0.0)
    sample_count: Mapped[int] = mapped_column(Integer, nullable=False, default=0)
    condition_match: Mapped[float] = mapped_column(Float, nullable=False, default=1.0)
    cost_efficiency: Mapped[float] = mapped_column(Float, nullable=False, default=0.5)
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        default=lambda: datetime.now(UTC),
    )


class EvolutionExecutionEvent(EvolutionBase):
    """Raw execution event table."""

    __tablename__ = "evolution_execution_event"
    __table_args__ = (Index("ix_evolution_execution_event_run_id_created_at", "run_id", "created_at"),)

    id: Mapped[int] = mapped_column(BigInteger, primary_key=True, autoincrement=True)
    run_id: Mapped[str] = mapped_column(String(80), nullable=False)
    step_id: Mapped[str | None] = mapped_column(String(80), nullable=True)
    event_type: Mapped[str] = mapped_column(String(64), nullable=False)
    payload_json: Mapped[dict[str, Any]] = mapped_column(_JSON_TYPE, nullable=False, default=dict)
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        default=lambda: datetime.now(UTC),
    )


class EvolutionOutcome(EvolutionBase):
    """Outcome facts table."""

    __tablename__ = "evolution_outcome"
    __table_args__ = (Index("ix_evolution_outcome_strategy_id_created_at", "strategy_id", "created_at"),)

    id: Mapped[int] = mapped_column(BigInteger, primary_key=True, autoincrement=True)
    run_id: Mapped[str] = mapped_column(String(80), nullable=False)
    strategy_id: Mapped[str | None] = mapped_column(
        String(64),
        ForeignKey("evolution_strategy.id", ondelete="SET NULL"),
        nullable=True,
    )
    task_signature_hash: Mapped[str | None] = mapped_column(String(64), nullable=True)
    difficulty: Mapped[float] = mapped_column(Float, nullable=False, default=0.5)
    self_confidence: Mapped[float] = mapped_column(Float, nullable=False, default=0.5)
    used_retrieval: Mapped[bool] = mapped_column(Boolean, nullable=False, default=False)
    success: Mapped[bool] = mapped_column(Boolean, nullable=False, default=True)
    failure_reason: Mapped[str | None] = mapped_column(Text, nullable=True)
    latency_ms: Mapped[float] = mapped_column(Float, nullable=False, default=0.0)
    token_in: Mapped[int] = mapped_column(Integer, nullable=False, default=0)
    token_out: Mapped[int] = mapped_column(Integer, nullable=False, default=0)
    cost_usd: Mapped[float] = mapped_column(Float, nullable=False, default=0.0)
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        default=lambda: datetime.now(UTC),
    )


class EvolutionValidationResult(EvolutionBase):
    """Validation result table."""

    __tablename__ = "evolution_validation_result"

    id: Mapped[int] = mapped_column(BigInteger, primary_key=True, autoincrement=True)
    strategy_id: Mapped[str] = mapped_column(
        String(64),
        ForeignKey("evolution_strategy.id", ondelete="CASCADE"),
        nullable=False,
    )
    job_id: Mapped[str] = mapped_column(String(80), nullable=False, index=True)
    status: Mapped[str] = mapped_column(String(32), nullable=False, index=True)
    checks_json: Mapped[dict[str, Any]] = mapped_column(_JSON_TYPE, nullable=False, default=dict)
    score_delta: Mapped[float] = mapped_column(Float, nullable=False, default=0.0)
    created_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        default=lambda: datetime.now(UTC),
    )


__all__ = [
    "EvolutionBase",
    "EvolutionExecutionEvent",
    "EvolutionOutcome",
    "EvolutionStrategy",
    "EvolutionStrategyKeyword",
    "EvolutionStrategyScore",
    "EvolutionValidationResult",
]
