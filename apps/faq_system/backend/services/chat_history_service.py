"""FAQ チャット履歴永続化サービス."""

from __future__ import annotations

import re
import secrets
from datetime import UTC, datetime
from typing import TYPE_CHECKING, Any

from apps.faq_system.backend.db.models import ChatMessage
from apps.faq_system.backend.db.session import ensure_database_ready, get_db_session
from sqlalchemy import Select, delete, func, select
from sqlalchemy.engine import CursorResult


if TYPE_CHECKING:
    from apps.faq_system.backend.auth.models import UserInfo


class ChatHistoryService:
    """チャット履歴を DB に保存・取得する."""

    _SESSION_TITLE_MAX_CHARS = 17

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
        stmt: Select[tuple[ChatMessage]] = select(ChatMessage).where(ChatMessage.session_id == session_id)
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

    # -----------------------------------------------------------------
    # セッション管理
    # -----------------------------------------------------------------

    async def list_sessions(
        self,
        user: UserInfo,
        *,
        limit: int = 50,
        offset: int = 0,
    ) -> list[dict[str, Any]]:
        """ユーザーのチャットセッション一覧を取得.

        Returns:
            各セッションの要約リスト (session_id, message_count,
            last_message_at, preview)。最終メッセージ日時の降順。
        """
        await ensure_database_ready()

        # session_id 毎の集計
        agg_stmt = (
            select(
                ChatMessage.session_id,
                func.count(ChatMessage.id).label("message_count"),
                func.max(ChatMessage.created_at).label("last_message_at"),
            )
            .where(ChatMessage.user_id == user.user_id)
            .group_by(ChatMessage.session_id)
            .order_by(func.max(ChatMessage.created_at).desc())
            .limit(min(limit, 200))
            .offset(offset)
        )

        # 先頭メッセージ一括取得（N 回を 1 回に削減）
        session_ids_subq = agg_stmt.with_only_columns(ChatMessage.session_id).subquery()

        first_msg_subq = (
            select(
                ChatMessage.session_id,
                func.min(ChatMessage.created_at).label("first_at"),
            )
            .where(
                ChatMessage.session_id.in_(select(session_ids_subq)),
                ChatMessage.user_id == user.user_id,
                ChatMessage.role == "user",
            )
            .group_by(ChatMessage.session_id)
            .subquery()
        )

        preview_stmt = (
            select(ChatMessage.session_id, ChatMessage.content)
            .join(
                first_msg_subq,
                (ChatMessage.session_id == first_msg_subq.c.session_id)
                & (ChatMessage.created_at == first_msg_subq.c.first_at),
            )
        )

        async with get_db_session() as session:
            rows = (await session.execute(agg_stmt)).all()
            previews: dict[str, str] = dict((await session.execute(preview_stmt)).all())

        results: list[dict[str, Any]] = []
        for row in rows:
            preview_text = previews.get(row.session_id)
            results.append(
                {
                    "session_id": row.session_id,
                    "title": self._auto_title_from_text(preview_text) if preview_text else row.session_id,
                    "message_count": row.message_count,
                    "last_message_at": row.last_message_at.isoformat() if row.last_message_at else None,
                    "preview": (preview_text or "")[:80],
                }
            )

        return results

    async def delete_session(
        self,
        session_id: str,
        user: UserInfo,
    ) -> bool:
        """セッションの全メッセージを削除.

        Returns:
            削除が行われた場合 True。
        """
        await ensure_database_ready()
        stmt = delete(ChatMessage).where(
            ChatMessage.session_id == session_id,
            ChatMessage.user_id == user.user_id,
        )
        async with get_db_session() as session:
            result = await session.execute(stmt)
            if isinstance(result, CursorResult):
                return (result.rowcount or 0) > 0
            return False

    @staticmethod
    def _auto_title_from_text(text: str) -> str:
        """テキストからセッションタイトルを自動生成.

        最初のユーザーメッセージを 17 文字以内に正規化して使用する。
        """
        if not text:
            return "新しいチャット"
        normalized = re.sub(r"\s+", " ", text.strip())
        max_chars = ChatHistoryService._SESSION_TITLE_MAX_CHARS
        if len(normalized) <= max_chars:
            return normalized
        return f"{normalized[:max_chars]}…"
