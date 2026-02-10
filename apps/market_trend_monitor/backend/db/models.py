"""DB モデル定義."""

from __future__ import annotations

from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.db.base import Base
from sqlalchemy import JSON, Boolean, DateTime, Float, String, Text
from sqlalchemy.orm import Mapped, mapped_column


class EvidenceModel(Base):
    """証拠テーブル."""

    __tablename__ = "evidences"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    source_id: Mapped[str] = mapped_column(String(64), index=True)
    source_type: Mapped[str] = mapped_column(String(32), index=True)
    url: Mapped[str] = mapped_column(Text)
    title: Mapped[str] = mapped_column(Text)
    content_hash: Mapped[str] = mapped_column(String(32), unique=True, index=True)
    extracted_data: Mapped[dict[str, Any]] = mapped_column(JSON, default=dict)
    collected_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now, index=True)
    reliability_score: Mapped[float] = mapped_column(Float, default=0.5)
    metadata_json: Mapped[dict[str, Any]] = mapped_column("metadata", JSON, default=dict)


class ClaimModel(Base):
    """主張テーブル."""

    __tablename__ = "claims"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    statement: Mapped[str] = mapped_column(Text)
    level: Mapped[str] = mapped_column(String(32), index=True)
    confidence: Mapped[float] = mapped_column(Float, default=0.0)
    evidence_ids: Mapped[list[str]] = mapped_column(JSON, default=list)
    counter_evidence_ids: Mapped[list[str]] = mapped_column(JSON, default=list)
    created_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now)
    updated_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now)
    metadata_json: Mapped[dict[str, Any]] = mapped_column("metadata", JSON, default=dict)


class SourceRegistryModel(Base):
    """情報源台帳テーブル."""

    __tablename__ = "source_registry"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    name: Mapped[str] = mapped_column(String(128), index=True)
    source_type: Mapped[str] = mapped_column(String(32), index=True)
    base_url: Mapped[str] = mapped_column(Text)
    reliability_score: Mapped[float] = mapped_column(Float, default=0.5)
    enabled: Mapped[bool] = mapped_column(Boolean, default=True)
    terms_url: Mapped[str | None] = mapped_column(Text, nullable=True)
    last_checked_at: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    metadata_json: Mapped[dict[str, Any]] = mapped_column("metadata", JSON, default=dict)


class ReportHistoryModel(Base):
    """レポート履歴テーブル."""

    __tablename__ = "report_history"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    generated_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now, index=True)
    payload: Mapped[dict[str, Any]] = mapped_column(JSON, default=dict)
