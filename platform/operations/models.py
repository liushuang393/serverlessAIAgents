"""Layer 5 Platform DB モデル."""

from __future__ import annotations

from datetime import UTC, datetime

from sqlalchemy import JSON, Boolean, DateTime, Integer, String, Text
from sqlalchemy.orm import DeclarativeBase, Mapped, mapped_column


def utcnow() -> datetime:
    """UTC 現在時刻を返す."""
    return datetime.now(tz=UTC)


class Base(DeclarativeBase):
    """Platform DB 宣言的ベース."""


class LLMProviderSecret(Base):
    """Provider ごとの暗号化 API Key."""

    __tablename__ = "llm_provider_secrets"

    provider_name: Mapped[str] = mapped_column(String(64), primary_key=True)
    api_key_env: Mapped[str | None] = mapped_column(String(128), nullable=True)
    encrypted_secret: Mapped[str] = mapped_column(Text, nullable=False)
    secret_mask: Mapped[str] = mapped_column(String(64), nullable=False)
    source_label: Mapped[str] = mapped_column(String(32), nullable=False, default="platform_encrypted")
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), nullable=False, default=utcnow)
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        default=utcnow,
        onupdate=utcnow,
    )


class LLMEngineDeployment(Base):
    """Local backend 配備状態."""

    __tablename__ = "llm_engine_deployments"

    engine_name: Mapped[str] = mapped_column(String(64), primary_key=True)
    engine_type: Mapped[str] = mapped_column(String(32), nullable=False)
    deployment_mode: Mapped[str] = mapped_column(String(32), nullable=False, default="docker")
    docker_image: Mapped[str | None] = mapped_column(String(255), nullable=True)
    served_model_name: Mapped[str | None] = mapped_column(String(255), nullable=True)
    container_name: Mapped[str | None] = mapped_column(String(255), nullable=True)
    host_port: Mapped[int | None] = mapped_column(Integer, nullable=True)
    public_base_url: Mapped[str | None] = mapped_column(String(512), nullable=True)
    compose_path: Mapped[str | None] = mapped_column(String(512), nullable=True)
    compose_yaml: Mapped[str | None] = mapped_column(Text, nullable=True)
    gpu_enabled: Mapped[bool] = mapped_column(Boolean, nullable=False, default=False)
    gpu_devices_json: Mapped[list[str]] = mapped_column(JSON, nullable=False, default=list)
    gpu_count: Mapped[int | None] = mapped_column(Integer, nullable=True)
    extra_env_json: Mapped[dict[str, str]] = mapped_column(JSON, nullable=False, default=dict)
    status: Mapped[str] = mapped_column(String(32), nullable=False, default="unknown")
    last_error: Mapped[str | None] = mapped_column(Text, nullable=True)
    deployed_at: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)
    stopped_at: Mapped[datetime | None] = mapped_column(DateTime(timezone=True), nullable=True)
    created_at: Mapped[datetime] = mapped_column(DateTime(timezone=True), nullable=False, default=utcnow)
    updated_at: Mapped[datetime] = mapped_column(
        DateTime(timezone=True),
        nullable=False,
        default=utcnow,
        onupdate=utcnow,
    )
