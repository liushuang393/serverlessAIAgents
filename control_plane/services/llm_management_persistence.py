"""Platform 主導 LLM 管理の永続化・暗号化ヘルパー."""

from __future__ import annotations

import base64
import hashlib
import os
import sqlite3
from datetime import UTC, datetime
from pathlib import Path

from cryptography.fernet import Fernet, InvalidToken
from pydantic import BaseModel, Field
from sqlalchemy import select

from control_plane.db import LLMEngineDeployment, LLMProviderSecret, get_platform_db_session


_RUNTIME_CACHE_RELATIVE_PATH = Path("control_plane") / "data" / "control_plane_runtime_cache.db"


class PlatformSecretStatus(BaseModel):
    """Provider secret の可視化状態."""

    configured: bool = False
    masked: str | None = None
    source: str = "unavailable"
    available: bool = False
    last_error: str | None = None


class PlatformEngineDeploymentRecord(BaseModel):
    """Engine deployment レコード."""

    engine_name: str
    engine_type: str
    deployment_mode: str = "docker"
    docker_image: str | None = None
    served_model_name: str | None = None
    container_name: str | None = None
    host_port: int | None = None
    public_base_url: str | None = None
    compose_path: str | None = None
    compose_yaml: str | None = None
    gpu_enabled: bool = False
    gpu_devices: list[str] = Field(default_factory=list)
    gpu_count: int | None = None
    extra_env: dict[str, str] = Field(default_factory=dict)
    status: str = "unknown"
    last_error: str | None = None
    deployed_at: str | None = None
    stopped_at: str | None = None


def _runtime_cache_path() -> Path:
    override = os.getenv("PLATFORM_RUNTIME_CACHE_DB", "").strip()
    if override:
        return Path(override)
    return Path.cwd() / _RUNTIME_CACHE_RELATIVE_PATH


def _mask_secret(secret: str) -> str:
    stripped = secret.strip()
    if len(stripped) <= 6:
        return "*" * len(stripped)
    return f"{stripped[:4]}...{stripped[-2:]}"


def _master_key_raw() -> str | None:
    value = os.getenv("PLATFORM_SECRET_MASTER_KEY", "").strip()
    return value or None


def has_platform_master_key() -> bool:
    """master key の有無."""
    return _master_key_raw() is not None


def _build_fernet() -> Fernet:
    raw = _master_key_raw()
    if raw is None:
        msg = "PLATFORM_SECRET_MASTER_KEY が未設定のため暗号化保存を利用できません。"
        raise RuntimeError(msg)

    try:
        decoded = base64.urlsafe_b64decode(raw.encode("utf-8"))
    except Exception:
        decoded = b""

    if len(decoded) == 32 and len(raw) == 44:
        key = raw.encode("utf-8")
    else:
        digest = hashlib.sha256(raw.encode("utf-8")).digest()
        key = base64.urlsafe_b64encode(digest)
    return Fernet(key)


def encrypt_platform_secret(secret: str) -> tuple[str, str]:
    """secret を暗号化してマスクとともに返す."""
    cipher = _build_fernet()
    token = cipher.encrypt(secret.encode("utf-8")).decode("utf-8")
    return token, _mask_secret(secret)


def decrypt_platform_secret(token: str) -> str | None:
    """暗号化 token を復号する."""
    try:
        cipher = _build_fernet()
        return cipher.decrypt(token.encode("utf-8")).decode("utf-8")
    except (InvalidToken, RuntimeError):
        return None


def _ensure_runtime_cache_schema() -> sqlite3.Connection:
    path = _runtime_cache_path()
    path.parent.mkdir(parents=True, exist_ok=True)
    conn = sqlite3.connect(path)
    conn.execute(
        """
        CREATE TABLE IF NOT EXISTS llm_provider_secrets (
            provider_name TEXT PRIMARY KEY,
            api_key_env TEXT,
            encrypted_secret TEXT NOT NULL,
            secret_mask TEXT NOT NULL,
            updated_at TEXT NOT NULL
        )
        """
    )
    conn.commit()
    return conn


def cache_platform_secret(
    *,
    provider_name: str,
    api_key_env: str | None,
    encrypted_secret: str,
    secret_mask: str,
) -> None:
    """ランタイム用 SQLite キャッシュに secret を保存する."""
    conn = _ensure_runtime_cache_schema()
    try:
        conn.execute(
            """
            INSERT INTO llm_provider_secrets (
                provider_name,
                api_key_env,
                encrypted_secret,
                secret_mask,
                updated_at
            ) VALUES (?, ?, ?, ?, ?)
            ON CONFLICT(provider_name) DO UPDATE SET
                api_key_env=excluded.api_key_env,
                encrypted_secret=excluded.encrypted_secret,
                secret_mask=excluded.secret_mask,
                updated_at=excluded.updated_at
            """,
            (
                provider_name.strip().lower(),
                api_key_env.strip() if api_key_env else None,
                encrypted_secret,
                secret_mask,
                datetime.now(tz=UTC).isoformat(),
            ),
        )
        conn.commit()
    finally:
        conn.close()


def delete_cached_platform_secret(provider_name: str) -> None:
    """ランタイムキャッシュから secret を削除する."""
    conn = _ensure_runtime_cache_schema()
    try:
        conn.execute(
            "DELETE FROM llm_provider_secrets WHERE provider_name = ?",
            (provider_name.strip().lower(),),
        )
        conn.commit()
    finally:
        conn.close()


def resolve_platform_cached_secret(provider_name: str) -> tuple[str | None, PlatformSecretStatus]:
    """ランタイムキャッシュから provider secret を解決する."""
    conn = _ensure_runtime_cache_schema()
    try:
        row = conn.execute(
            """
            SELECT encrypted_secret, secret_mask
            FROM llm_provider_secrets
            WHERE provider_name = ?
            """,
            (provider_name.strip().lower(),),
        ).fetchone()
    finally:
        conn.close()

    if row is None:
        return None, PlatformSecretStatus()

    encrypted_secret = str(row[0])
    secret_mask = str(row[1])
    secret = decrypt_platform_secret(encrypted_secret)
    if secret is None:
        return None, PlatformSecretStatus(
            configured=True,
            masked=secret_mask,
            source="platform_encrypted",
            available=False,
            last_error="platform_master_key_missing_or_invalid",
        )
    return secret, PlatformSecretStatus(
        configured=True,
        masked=secret_mask,
        source="platform_encrypted",
        available=True,
    )


async def sync_runtime_secret_cache_from_db() -> None:
    """DB の secret をランタイムキャッシュへ同期する."""
    async with get_platform_db_session() as session:
        result = await session.execute(select(LLMProviderSecret))
        secrets = result.scalars().all()

    conn = _ensure_runtime_cache_schema()
    try:
        conn.execute("DELETE FROM llm_provider_secrets")
        for item in secrets:
            conn.execute(
                """
                INSERT INTO llm_provider_secrets (
                    provider_name,
                    api_key_env,
                    encrypted_secret,
                    secret_mask,
                    updated_at
                ) VALUES (?, ?, ?, ?, ?)
                """,
                (
                    item.provider_name,
                    item.api_key_env,
                    item.encrypted_secret,
                    item.secret_mask,
                    item.updated_at.isoformat(),
                ),
            )
        conn.commit()
    finally:
        conn.close()


class LLMManagementPersistence:
    """Platform LLM 永続化ストア."""

    async def upsert_provider_secret(
        self,
        *,
        provider_name: str,
        api_key_env: str | None,
        secret_value: str,
    ) -> PlatformSecretStatus:
        """暗号化 secret を保存する."""
        encrypted_secret, secret_mask = encrypt_platform_secret(secret_value)
        normalized_provider = provider_name.strip().lower()
        normalized_env = api_key_env.strip() if api_key_env else None

        async with get_platform_db_session() as session:
            existing = await session.get(LLMProviderSecret, normalized_provider)
            if existing is None:
                session.add(
                    LLMProviderSecret(
                        provider_name=normalized_provider,
                        api_key_env=normalized_env,
                        encrypted_secret=encrypted_secret,
                        secret_mask=secret_mask,
                        source_label="platform_encrypted",
                    )
                )
            else:
                existing.api_key_env = normalized_env
                existing.encrypted_secret = encrypted_secret
                existing.secret_mask = secret_mask
                existing.source_label = "platform_encrypted"

        cache_platform_secret(
            provider_name=normalized_provider,
            api_key_env=normalized_env,
            encrypted_secret=encrypted_secret,
            secret_mask=secret_mask,
        )
        return PlatformSecretStatus(
            configured=True,
            masked=secret_mask,
            source="platform_encrypted",
            available=True,
        )

    async def delete_provider_secret(self, provider_name: str) -> None:
        """保存済み secret を削除する."""
        normalized_provider = provider_name.strip().lower()
        async with get_platform_db_session() as session:
            existing = await session.get(LLMProviderSecret, normalized_provider)
            if existing is not None:
                await session.delete(existing)
        delete_cached_platform_secret(normalized_provider)

    async def get_provider_secret_statuses(self) -> dict[str, PlatformSecretStatus]:
        """DB 上の provider secret 状態一覧."""
        async with get_platform_db_session() as session:
            result = await session.execute(select(LLMProviderSecret))
            rows = result.scalars().all()

        statuses: dict[str, PlatformSecretStatus] = {}
        for row in rows:
            statuses[row.provider_name] = PlatformSecretStatus(
                configured=True,
                masked=row.secret_mask,
                source=row.source_label,
                available=True,
            )
        return statuses

    async def upsert_engine_deployment(
        self,
        deployment: PlatformEngineDeploymentRecord,
    ) -> PlatformEngineDeploymentRecord:
        """engine deployment を保存する."""
        async with get_platform_db_session() as session:
            existing = await session.get(LLMEngineDeployment, deployment.engine_name)
            if existing is None:
                session.add(
                    LLMEngineDeployment(
                        engine_name=deployment.engine_name,
                        engine_type=deployment.engine_type,
                        deployment_mode=deployment.deployment_mode,
                        docker_image=deployment.docker_image,
                        served_model_name=deployment.served_model_name,
                        container_name=deployment.container_name,
                        host_port=deployment.host_port,
                        public_base_url=deployment.public_base_url,
                        compose_path=deployment.compose_path,
                        compose_yaml=deployment.compose_yaml,
                        gpu_enabled=deployment.gpu_enabled,
                        gpu_devices_json=list(deployment.gpu_devices),
                        gpu_count=deployment.gpu_count,
                        extra_env_json=dict(deployment.extra_env),
                        status=deployment.status,
                        last_error=deployment.last_error,
                        deployed_at=_parse_dt(deployment.deployed_at),
                        stopped_at=_parse_dt(deployment.stopped_at),
                    )
                )
            else:
                existing.engine_type = deployment.engine_type
                existing.deployment_mode = deployment.deployment_mode
                existing.docker_image = deployment.docker_image
                existing.served_model_name = deployment.served_model_name
                existing.container_name = deployment.container_name
                existing.host_port = deployment.host_port
                existing.public_base_url = deployment.public_base_url
                existing.compose_path = deployment.compose_path
                existing.compose_yaml = deployment.compose_yaml
                existing.gpu_enabled = deployment.gpu_enabled
                existing.gpu_devices_json = list(deployment.gpu_devices)
                existing.gpu_count = deployment.gpu_count
                existing.extra_env_json = dict(deployment.extra_env)
                existing.status = deployment.status
                existing.last_error = deployment.last_error
                existing.deployed_at = _parse_dt(deployment.deployed_at)
                existing.stopped_at = _parse_dt(deployment.stopped_at)
        return deployment

    async def list_engine_deployments(self) -> dict[str, PlatformEngineDeploymentRecord]:
        """保存済み engine deployment 一覧."""
        async with get_platform_db_session() as session:
            result = await session.execute(select(LLMEngineDeployment))
            rows = result.scalars().all()
        return {
            row.engine_name: PlatformEngineDeploymentRecord(
                engine_name=row.engine_name,
                engine_type=row.engine_type,
                deployment_mode=row.deployment_mode,
                docker_image=row.docker_image,
                served_model_name=row.served_model_name,
                container_name=row.container_name,
                host_port=row.host_port,
                public_base_url=row.public_base_url,
                compose_path=row.compose_path,
                compose_yaml=row.compose_yaml,
                gpu_enabled=row.gpu_enabled,
                gpu_devices=list(row.gpu_devices_json or []),
                gpu_count=row.gpu_count,
                extra_env=dict(row.extra_env_json or {}),
                status=row.status,
                last_error=row.last_error,
                deployed_at=_dump_dt(row.deployed_at),
                stopped_at=_dump_dt(row.stopped_at),
            )
            for row in rows
        }


def _parse_dt(value: str | None) -> datetime | None:
    if value is None:
        return None
    return datetime.fromisoformat(value)


def _dump_dt(value: datetime | None) -> str | None:
    if value is None:
        return None
    return value.isoformat()


__all__ = [
    "LLMManagementPersistence",
    "PlatformEngineDeploymentRecord",
    "PlatformSecretStatus",
    "cache_platform_secret",
    "decrypt_platform_secret",
    "delete_cached_platform_secret",
    "encrypt_platform_secret",
    "has_platform_master_key",
    "resolve_platform_cached_secret",
    "sync_runtime_secret_cache_from_db",
]
