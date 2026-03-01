# apps/faq_system/tests/test_chat_history_service.py
"""chat_history_service の単体テスト."""

from __future__ import annotations

from unittest.mock import AsyncMock, MagicMock, patch

import pytest
from apps.faq_system.backend.services.chat_history_service import ChatHistoryService
from sqlalchemy.engine import CursorResult


def _make_user(user_id: str = "user-1") -> MagicMock:
    user = MagicMock()
    user.user_id = user_id
    return user


def _make_mock_session_ctx() -> tuple[AsyncMock, MagicMock]:
    """get_db_session の async context manager モックを返す."""
    mock_session = AsyncMock()
    mock_context = MagicMock()
    mock_context.__aenter__ = AsyncMock(return_value=mock_session)
    mock_context.__aexit__ = AsyncMock(return_value=False)
    return mock_session, mock_context


class TestListSessions:
    @pytest.mark.asyncio
    async def test_list_sessions_uses_two_queries(self) -> None:
        """N+1 修正後は DB execute が正確に 2 回だけ呼ばれる."""
        mock_session, mock_context = _make_mock_session_ctx()

        # agg_stmt の結果: 1 セッション分
        agg_row = MagicMock()
        agg_row.session_id = "sess-abc"
        agg_row.message_count = 3
        agg_row.last_message_at = MagicMock()
        agg_row.last_message_at.isoformat.return_value = "2024-01-01T00:00:00"

        agg_result = MagicMock()
        agg_result.all.return_value = [agg_row]

        # preview_stmt の結果: session_id -> content のマッピング
        preview_result = MagicMock()
        preview_result.all.return_value = [("sess-abc", "こんにちは、質問です")]

        mock_session.execute = AsyncMock(side_effect=[agg_result, preview_result])

        with (
            patch(
                "apps.faq_system.backend.services.chat_history_service.get_db_session",
                return_value=mock_context,
            ),
            patch(
                "apps.faq_system.backend.services.chat_history_service.ensure_database_ready",
                new_callable=AsyncMock,
            ),
        ):
            svc = ChatHistoryService()
            result = await svc.list_sessions(_make_user())

        # execute が正確に 2 回呼ばれる（N+1 ではなく 1+1）
        assert mock_session.execute.call_count == 2
        assert len(result) == 1
        assert result[0]["session_id"] == "sess-abc"
        assert result[0]["message_count"] == 3

    @pytest.mark.asyncio
    async def test_list_sessions_empty(self) -> None:
        """セッションが 0 件の場合は空リストを返す."""
        mock_session, mock_context = _make_mock_session_ctx()

        agg_result = MagicMock()
        agg_result.all.return_value = []

        preview_result = MagicMock()
        preview_result.all.return_value = []

        mock_session.execute = AsyncMock(side_effect=[agg_result, preview_result])

        with (
            patch(
                "apps.faq_system.backend.services.chat_history_service.get_db_session",
                return_value=mock_context,
            ),
            patch(
                "apps.faq_system.backend.services.chat_history_service.ensure_database_ready",
                new_callable=AsyncMock,
            ),
        ):
            svc = ChatHistoryService()
            result = await svc.list_sessions(_make_user())

        assert result == []


class TestDeleteSession:
    @pytest.mark.asyncio
    async def test_delete_session_returns_true_when_rows_deleted(self) -> None:
        """削除行数 1 の場合は True を返す."""
        mock_session, mock_context = _make_mock_session_ctx()

        cursor_result = MagicMock(spec=CursorResult)
        cursor_result.rowcount = 1
        mock_session.execute = AsyncMock(return_value=cursor_result)

        with (
            patch(
                "apps.faq_system.backend.services.chat_history_service.get_db_session",
                return_value=mock_context,
            ),
            patch(
                "apps.faq_system.backend.services.chat_history_service.ensure_database_ready",
                new_callable=AsyncMock,
            ),
        ):
            svc = ChatHistoryService()
            returned = await svc.delete_session("sess-xyz", _make_user())

        assert returned is True

    @pytest.mark.asyncio
    async def test_delete_session_returns_false_when_no_rows(self) -> None:
        """削除行数 0 の場合は False を返す."""
        mock_session, mock_context = _make_mock_session_ctx()

        cursor_result = MagicMock(spec=CursorResult)
        cursor_result.rowcount = 0
        mock_session.execute = AsyncMock(return_value=cursor_result)

        with (
            patch(
                "apps.faq_system.backend.services.chat_history_service.get_db_session",
                return_value=mock_context,
            ),
            patch(
                "apps.faq_system.backend.services.chat_history_service.ensure_database_ready",
                new_callable=AsyncMock,
            ),
        ):
            svc = ChatHistoryService()
            returned = await svc.delete_session("sess-xyz", _make_user())

        assert returned is False
