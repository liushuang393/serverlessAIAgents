"""DB モデル定義.

Phase 11: Signal/Prediction/RedTeam/Bayesian/Adaptive/TrendHistory/ClaimEvidence テーブル追加。
"""

from __future__ import annotations

from datetime import datetime
from typing import Any

from apps.market_trend_monitor.backend.db.base import Base
from sqlalchemy import JSON, Boolean, DateTime, Float, Integer, String, Text
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
    embedding_vector: Mapped[list[float] | None] = mapped_column(JSON, nullable=True)
    vector_indexed: Mapped[bool] = mapped_column(Boolean, default=False)


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


class AppSettingModel(Base):
    """アプリ設定テーブル."""

    __tablename__ = "app_settings"

    key: Mapped[str] = mapped_column(String(128), primary_key=True)
    value: Mapped[dict[str, Any]] = mapped_column(JSON, default=dict)
    updated_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now, index=True)


# ============================================================
# Phase 11: 新規テーブル
# ============================================================


class SignalModel(Base):
    """信号評価テーブル."""

    __tablename__ = "signals"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    trend_id: Mapped[str] = mapped_column(String(36), index=True)
    reliability: Mapped[float] = mapped_column(Float, default=0.0)
    leading: Mapped[float] = mapped_column(Float, default=0.0)
    relevance: Mapped[float] = mapped_column(Float, default=0.0)
    actionability: Mapped[float] = mapped_column(Float, default=0.0)
    convergence: Mapped[float] = mapped_column(Float, default=0.0)
    total: Mapped[float] = mapped_column(Float, default=0.0)
    grade: Mapped[str] = mapped_column(String(2), index=True)
    evaluated_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now, index=True)
    metadata_json: Mapped[dict[str, Any]] = mapped_column("metadata", JSON, default=dict)


class PredictionModel(Base):
    """予測テーブル."""

    __tablename__ = "predictions"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    statement: Mapped[str] = mapped_column(Text)
    target_date: Mapped[str] = mapped_column(String(16), index=True)
    confidence: Mapped[float] = mapped_column(Float, default=0.0)
    claim_id: Mapped[str | None] = mapped_column(String(36), nullable=True, index=True)
    status: Mapped[str] = mapped_column(String(32), default="pending", index=True)
    created_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now, index=True)
    metadata_json: Mapped[dict[str, Any]] = mapped_column("metadata", JSON, default=dict)


class PredictionReviewModel(Base):
    """予測復盤テーブル."""

    __tablename__ = "prediction_reviews"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    prediction_id: Mapped[str] = mapped_column(String(36), index=True)
    actual_outcome: Mapped[str] = mapped_column(Text)
    outcome: Mapped[str] = mapped_column(String(32), index=True)
    accuracy_score: Mapped[float] = mapped_column(Float, default=0.0)
    reviewed_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now)
    notes: Mapped[str] = mapped_column(Text, default="")
    metadata_json: Mapped[dict[str, Any]] = mapped_column("metadata", JSON, default=dict)


class ChallengeModel(Base):
    """Red Teamチャレンジテーブル."""

    __tablename__ = "challenges"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    claim_id: Mapped[str] = mapped_column(String(36), index=True)
    challenge_type: Mapped[str] = mapped_column(String(64), index=True)
    argument: Mapped[str] = mapped_column(Text)
    counter_evidence_ids: Mapped[list[str]] = mapped_column(JSON, default=list)
    is_valid: Mapped[bool | None] = mapped_column(Boolean, nullable=True)
    impact_score: Mapped[float] = mapped_column(Float, default=0.0)
    created_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now)
    metadata_json: Mapped[dict[str, Any]] = mapped_column("metadata", JSON, default=dict)


class BayesianStateModel(Base):
    """ベイズ状態テーブル."""

    __tablename__ = "bayesian_states"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    claim_id: Mapped[str] = mapped_column(String(36), unique=True, index=True)
    alpha: Mapped[float] = mapped_column(Float, default=2.0)
    beta: Mapped[float] = mapped_column(Float, default=2.0)
    update_count: Mapped[int] = mapped_column(Integer, default=0)
    updated_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now)


class ScoringWeightsModel(Base):
    """スコアリング重みテーブル."""

    __tablename__ = "scoring_weights"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    reliability: Mapped[float] = mapped_column(Float, default=0.2)
    leading: Mapped[float] = mapped_column(Float, default=0.2)
    relevance: Mapped[float] = mapped_column(Float, default=0.2)
    actionability: Mapped[float] = mapped_column(Float, default=0.2)
    convergence: Mapped[float] = mapped_column(Float, default=0.2)
    update_count: Mapped[int] = mapped_column(Integer, default=0)
    updated_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now, index=True)


class TrendHistoryModel(Base):
    """トレンド履歴テーブル."""

    __tablename__ = "trend_history"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    topic: Mapped[str] = mapped_column(String(256), index=True)
    score: Mapped[float] = mapped_column(Float, default=0.0)
    growth_rate: Mapped[float] = mapped_column(Float, default=0.0)
    sentiment: Mapped[str] = mapped_column(String(16), default="neutral")
    articles_count: Mapped[int] = mapped_column(Integer, default=0)
    run_id: Mapped[str] = mapped_column(String(36), index=True)
    captured_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now, index=True)
    metadata_json: Mapped[dict[str, Any]] = mapped_column("metadata", JSON, default=dict)


class ClaimEvidenceModel(Base):
    """Claim-Evidence中間テーブル."""

    __tablename__ = "claim_evidence"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    claim_id: Mapped[str] = mapped_column(String(36), index=True)
    evidence_id: Mapped[str] = mapped_column(String(36), index=True)
    is_supporting: Mapped[bool] = mapped_column(Boolean, default=True)
    added_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now)


class CollectJobModel(Base):
    """データ収集ジョブテーブル.

    バックグラウンド収集ジョブの状態を永続化し、
    画面遷移後もジョブ状態をポーリングで確認できるようにする。
    """

    __tablename__ = "collect_jobs"

    id: Mapped[str] = mapped_column(String(36), primary_key=True)
    # running / completed / failed
    status: Mapped[str] = mapped_column(String(16), default="running", index=True)
    progress: Mapped[int] = mapped_column(Integer, default=0)
    current_step: Mapped[str] = mapped_column(String(64), default="")
    started_at: Mapped[datetime] = mapped_column(DateTime, default=datetime.now, index=True)
    completed_at: Mapped[datetime | None] = mapped_column(DateTime, nullable=True)
    articles_count: Mapped[int | None] = mapped_column(Integer, nullable=True)
    trends_count: Mapped[int | None] = mapped_column(Integer, nullable=True)
    error: Mapped[str | None] = mapped_column(Text, nullable=True)
