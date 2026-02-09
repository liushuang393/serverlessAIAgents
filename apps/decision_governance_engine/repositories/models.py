"""決策履歴 SQLAlchemy モデル定義.

目的:
    決策記録・証拠・クレームの ORM モデル

依存:
    sqlalchemy[asyncio], asyncpg
"""

from datetime import datetime
from typing import Any
from uuid import UUID, uuid4

from sqlalchemy import (
    Column,
    DateTime,
    ForeignKey,
    Integer,
    Numeric,
    String,
    Table,
    Text,
)
from sqlalchemy.dialects.postgresql import JSONB
from sqlalchemy.dialects.postgresql import UUID as PG_UUID
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column, relationship


class Base(DeclarativeBase):
    """SQLAlchemy 宣言的ベースクラス."""


# クレーム-証拠 関連テーブル
claim_evidence_refs = Table(
    "claim_evidence_refs",
    Base.metadata,
    Column("claim_id", PG_UUID(as_uuid=True), ForeignKey("claims.id", ondelete="CASCADE"), primary_key=True),
    Column("evidence_id", PG_UUID(as_uuid=True), ForeignKey("evidence_items.id", ondelete="CASCADE"), primary_key=True),
)


class DecisionRecord(Base):
    """決策記録モデル.

    主要フィールド:
        - request_id: リクエスト一意ID
        - question: 入力された質問
        - decision_role: GO/NO_GO/DELAY/PILOT
        - confidence: 確信度 (0.0-1.0)
    """
    __tablename__ = "decision_records"

    id: Mapped[UUID] = mapped_column(PG_UUID(as_uuid=True), primary_key=True, default=uuid4)
    request_id: Mapped[UUID] = mapped_column(PG_UUID(as_uuid=True), unique=True, nullable=False)

    # 入力
    question: Mapped[str] = mapped_column(Text, nullable=False)
    mode: Mapped[str] = mapped_column(String(20), default="STANDARD")

    # 結果
    decision_role: Mapped[str] = mapped_column(String(20), nullable=False)
    confidence: Mapped[float | None] = mapped_column(Numeric(5, 4))

    # 各セクション結果
    cognitive_gate_result: Mapped[dict[str, Any] | None] = mapped_column(JSONB)
    gatekeeper_result: Mapped[dict[str, Any] | None] = mapped_column(JSONB)
    dao_result: Mapped[dict[str, Any] | None] = mapped_column(JSONB)
    fa_result: Mapped[dict[str, Any] | None] = mapped_column(JSONB)
    shu_result: Mapped[dict[str, Any] | None] = mapped_column(JSONB)
    qi_result: Mapped[dict[str, Any] | None] = mapped_column(JSONB)
    review_result: Mapped[dict[str, Any] | None] = mapped_column(JSONB)

    # サマリ
    summary_bullets: Mapped[list[str] | None] = mapped_column(JSONB)
    warnings: Mapped[list[str] | None] = mapped_column(JSONB)

    # メタ情報
    requester_role: Mapped[str | None] = mapped_column(String(50))
    organization_size: Mapped[str | None] = mapped_column(String(20))

    # 監査情報
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=datetime.utcnow)
    updated_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=datetime.utcnow, onupdate=datetime.utcnow)
    processing_time_ms: Mapped[int | None] = mapped_column(Integer)
    llm_tokens_used: Mapped[int | None] = mapped_column(Integer)
    deleted_at: Mapped[datetime | None] = mapped_column(DateTime(timezone=True))

    # リレーション
    evidence_items: Mapped[list["EvidenceItem"]] = relationship(back_populates="decision_record", cascade="all, delete-orphan")
    claims: Mapped[list["Claim"]] = relationship(back_populates="decision_record", cascade="all, delete-orphan")


class EvidenceItem(Base):
    """証拠アイテムモデル.

    外部情報源からの証拠データを格納。
    """
    __tablename__ = "evidence_items"

    id: Mapped[UUID] = mapped_column(PG_UUID(as_uuid=True), primary_key=True, default=uuid4)
    decision_record_id: Mapped[UUID] = mapped_column(PG_UUID(as_uuid=True), ForeignKey("decision_records.id", ondelete="CASCADE"), nullable=False)

    url: Mapped[str | None] = mapped_column(Text)
    title: Mapped[str | None] = mapped_column(Text)
    publisher: Mapped[str | None] = mapped_column(Text)
    snippet: Mapped[str | None] = mapped_column(Text)
    summary: Mapped[str | None] = mapped_column(Text)
    reliability: Mapped[str] = mapped_column(String(10), default="MEDIUM")
    tags: Mapped[list[str] | None] = mapped_column(JSONB)
    retrieved_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=datetime.utcnow)
    published_at: Mapped[datetime | None] = mapped_column(DateTime(timezone=True))
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=datetime.utcnow)

    # リレーション
    decision_record: Mapped["DecisionRecord"] = relationship(back_populates="evidence_items")
    claims: Mapped[list["Claim"]] = relationship(secondary=claim_evidence_refs, back_populates="evidence_refs")


class Claim(Base):
    """クレーム（主張）モデル.

    決策過程で生成された主張・断言を格納。
    """
    __tablename__ = "claims"

    id: Mapped[UUID] = mapped_column(PG_UUID(as_uuid=True), primary_key=True, default=uuid4)
    decision_record_id: Mapped[UUID] = mapped_column(PG_UUID(as_uuid=True), ForeignKey("decision_records.id", ondelete="CASCADE"), nullable=False)

    claim_type: Mapped[str] = mapped_column(String(20), nullable=False)  # FACT/INFERENCE/ASSUMPTION/RECOMMENDATION
    statement: Mapped[str] = mapped_column(Text, nullable=False)
    source_section: Mapped[str | None] = mapped_column(String(20))
    confidence: Mapped[float | None] = mapped_column(Numeric(5, 4))
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), default=datetime.utcnow)

    # リレーション
    decision_record: Mapped["DecisionRecord"] = relationship(back_populates="claims")
    evidence_refs: Mapped[list["EvidenceItem"]] = relationship(secondary=claim_evidence_refs, back_populates="claims")

