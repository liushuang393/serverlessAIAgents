"""FAQ システム向け知識ベース設定管理."""

from __future__ import annotations

import asyncio
import os
from datetime import UTC, datetime
from enum import Enum
from typing import Any

from apps.faq_system.backend.db.models import KnowledgeBaseSetting
from apps.faq_system.backend.db.session import (
    create_all_tables,
    ensure_database_ready,
    get_database_url,
    get_db_session,
)
from pydantic import BaseModel, Field
from sqlalchemy.exc import OperationalError


class KnowledgeBaseType(str, Enum):
    """知識ベース種別."""

    INTERNAL = "internal"
    EXTERNAL = "external"
    CONFIDENTIAL = "confidential"


class KnowledgeBaseSettings(BaseModel):
    """知識ベース設定."""

    internal_collection: str = Field(
        default="internal_kb",
        description="社内KBコレクション名",
    )
    external_collection: str = Field(
        default="external_kb",
        description="対客KBコレクション名",
    )
    confidential_collection: str = Field(
        default="confidential_kb",
        description="機密KBコレクション名",
    )
    default_kb: KnowledgeBaseType = Field(
        default=KnowledgeBaseType.INTERNAL,
        description="既定KB種別",
    )

    def resolve_collection(self, kb_type: KnowledgeBaseType) -> str:
        """KB種別からコレクション名を解決."""
        if kb_type == KnowledgeBaseType.INTERNAL:
            return self.internal_collection
        if kb_type == KnowledgeBaseType.EXTERNAL:
            return self.external_collection
        return self.confidential_collection


class KnowledgeBaseUpdateRequest(BaseModel):
    """知識ベース設定更新リクエスト."""

    internal_collection: str | None = Field(None, description="社内KBコレクション名")
    external_collection: str | None = Field(None, description="対客KBコレクション名")
    confidential_collection: str | None = Field(None, description="機密KBコレクション名")
    default_kb: KnowledgeBaseType | None = Field(None, description="既定KB種別")


class KnowledgeBaseRegistry:
    """知識ベース設定レジストリ."""

    def __init__(self) -> None:
        self._settings = self._build_default_settings()
        self._lock = asyncio.Lock()
        self._initialized = False

    @property
    def settings(self) -> KnowledgeBaseSettings:
        """現在設定を取得."""
        return self._settings

    async def ensure_initialized(self) -> None:
        """DB から設定をロード（未存在なら作成）."""
        if self._initialized:
            return

        async with self._lock:
            if self._initialized:
                return

            await ensure_database_ready()
            db_settings = await self._load_settings_record()
            if db_settings is None:
                defaults = self._build_default_settings()
                async with get_db_session() as session:
                    session.add(
                        KnowledgeBaseSetting(
                            id="default",
                            internal_collection=defaults.internal_collection,
                            external_collection=defaults.external_collection,
                            confidential_collection=defaults.confidential_collection,
                            default_kb=defaults.default_kb.value,
                            created_at=datetime.now(tz=UTC),
                            updated_at=datetime.now(tz=UTC),
                        )
                    )
                self._settings = defaults
            else:
                self._settings = self._to_model(db_settings)

            self._initialized = True

    async def update(self, request: KnowledgeBaseUpdateRequest) -> KnowledgeBaseSettings:
        """設定を更新."""
        await self.ensure_initialized()

        current = self._settings.model_dump()
        for key, value in request.model_dump(exclude_none=True).items():
            current[key] = value

        updated_settings = KnowledgeBaseSettings(**current)

        async with get_db_session() as session:
            db_settings = await session.get(KnowledgeBaseSetting, "default")
            if db_settings is None:
                db_settings = KnowledgeBaseSetting(
                    id="default",
                    internal_collection=updated_settings.internal_collection,
                    external_collection=updated_settings.external_collection,
                    confidential_collection=updated_settings.confidential_collection,
                    default_kb=updated_settings.default_kb.value,
                    created_at=datetime.now(tz=UTC),
                    updated_at=datetime.now(tz=UTC),
                )
                session.add(db_settings)
            else:
                db_settings.internal_collection = updated_settings.internal_collection
                db_settings.external_collection = updated_settings.external_collection
                db_settings.confidential_collection = updated_settings.confidential_collection
                db_settings.default_kb = updated_settings.default_kb.value
                db_settings.updated_at = datetime.now(tz=UTC)

        self._settings = updated_settings
        return self._settings

    def resolve_collection(
        self,
        *,
        kb_type: KnowledgeBaseType | None = None,
        explicit_collection: str | None = None,
    ) -> str:
        """コレクション名を決定."""
        if explicit_collection:
            return explicit_collection
        resolved_type = kb_type or self._settings.default_kb
        return self._settings.resolve_collection(resolved_type)

    def to_dict(self) -> dict[str, Any]:
        """辞書形式で返却."""
        return self._settings.model_dump()

    @staticmethod
    def _to_model(db_settings: KnowledgeBaseSetting) -> KnowledgeBaseSettings:
        return KnowledgeBaseSettings(
            internal_collection=db_settings.internal_collection,
            external_collection=db_settings.external_collection,
            confidential_collection=db_settings.confidential_collection,
            default_kb=KnowledgeBaseType(db_settings.default_kb),
        )

    @staticmethod
    def _build_default_settings() -> KnowledgeBaseSettings:
        return KnowledgeBaseSettings(
            internal_collection=os.getenv("FAQ_KB_INTERNAL_COLLECTION", "internal_kb"),
            external_collection=os.getenv("FAQ_KB_EXTERNAL_COLLECTION", "external_kb"),
            confidential_collection=os.getenv("FAQ_KB_CONFIDENTIAL_COLLECTION", "confidential_kb"),
            default_kb=KnowledgeBaseType(
                os.getenv("FAQ_KB_DEFAULT_TYPE", KnowledgeBaseType.INTERNAL.value)
            ),
        )

    async def _load_settings_record(self) -> KnowledgeBaseSetting | None:
        """設定レコードをロード（欠表時は条件付きで自動修復）."""
        try:
            async with get_db_session() as session:
                return await session.get(KnowledgeBaseSetting, "default")
        except OperationalError as exc:
            if not self._is_missing_settings_table_error(exc):
                raise

            if self._allow_auto_table_repair():
                await create_all_tables()
                async with get_db_session() as session:
                    return await session.get(KnowledgeBaseSetting, "default")

            raise RuntimeError(
                "Table knowledge_base_settings is missing. "
                "Run: alembic -c apps/faq_system/alembic.ini upgrade head"
            ) from exc

    @staticmethod
    def _is_missing_settings_table_error(exc: OperationalError) -> bool:
        detail = str(exc).lower()
        return "no such table" in detail and "knowledge_base_settings" in detail

    @staticmethod
    def _allow_auto_table_repair() -> bool:
        override = os.getenv("FAQ_DB_AUTO_CREATE_ON_MISSING_TABLE", "")
        if override:
            return override.lower() in {"1", "true", "yes", "on"}
        return get_database_url().startswith("sqlite")


kb_registry = KnowledgeBaseRegistry()
