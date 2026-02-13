"""FAQ チャット履歴永続化サービス."""

from __future__ import annotations

import secrets
from datetime import UTC, datetime
from typing import Any

from sqlalchemy import Select, select

from apps.faq_system.backend.auth.models import UserInfo
from apps.faq_system.backend.db.models import ChatMessage
from apps.faq_system.backend.db.session import ensure_database_ready, get_db_session


class ChatHistoryService:
    """チャット履歴を DB に保存・取得する."""

    async def save_message(
        self,
        *,
        session_id: str,
        role: str,
        content: str,
        transport: str,
        user: UserInfo | None,
        metadata: dict[str, Any] | None = None,
    ) -> None:
        """メッセージを保存."""
        if not content.strip():
            return

        await ensure_database_ready()
        async with get_db_session() as session:
            session.add(
                ChatMessage(
                    id=f"msg-{secrets.token_hex(12)}",
                    session_id=session_id,
                    user_id=user.user_id if user else None,
                    username=user.username if user else None,
                    role=role,
                    transport=transport,
                    content=content,
                    metadata_json=metadata or {},
                    created_at=datetime.now(tz=UTC),
                )
            )

    async def list_messages(
        self,
        *,
        session_id: str,
        limit: int = 100,
        user: UserInfo | None = None,
    ) -> list[dict[str, Any]]:
        """セッション履歴を取得."""
        if limit <= 0:
            return []

        await ensure_database_ready()
        stmt: Select[tuple[ChatMessage]] = select(ChatMessage).where(
            ChatMessage.session_id == session_id
        )
        if user is not None:
            stmt = stmt.where(ChatMessage.user_id == user.user_id)

        stmt = stmt.order_by(ChatMessage.created_at.desc()).limit(min(limit, 500))

        async with get_db_session() as session:
            rows = (await session.execute(stmt)).scalars().all()

        messages = list(reversed(rows))
        return [
            {
                "id": row.id,
                "session_id": row.session_id,
                "user_id": row.user_id,
                "username": row.username,
                "role": row.role,
                "transport": row.transport,
                "content": row.content,
                "metadata": row.metadata_json,
                "created_at": row.created_at.isoformat(),
            }
            for row in messages
        ]
