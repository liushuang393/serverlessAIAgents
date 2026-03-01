# FAQ System Improvements Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use superpowers:executing-plans to implement this plan task-by-task.

**Goal:** FAQ System のバグ修正・性能改善・設計再整合・UX/セキュリティ改善を一括実施する。

**Architecture:** 設計書 `docs/plans/2026-02-28-faq-system-improvement-design.md` に準拠。新規 `query_classifier.py` を中心に分散した分類ロジックを統一し、各バグを根本から修正する。

**Tech Stack:** Python 3.13+, FastAPI, SQLAlchemy (async), Pydantic v2, React + TypeScript + Zustand, pytest, mypy, Ruff

---

### Task 1: QueryClassifier 統一モジュール作成

**Files:**
- Create: `apps/faq_system/backend/services/query_classifier.py`
- Create: `apps/faq_system/tests/test_query_classifier.py`

**Step 1: テストを書く**

```python
# apps/faq_system/tests/test_query_classifier.py
import pytest
from apps.faq_system.backend.services.query_classifier import QueryClassifier, QueryType, classify_query


class TestQueryClassifier:
    def setup_method(self) -> None:
        self.clf = QueryClassifier()

    # --- FAQ（スコア 0） ---
    def test_faq_japanese(self) -> None:
        assert self.clf.classify("年次有給休暇は何日もらえますか") == QueryType.FAQ

    def test_faq_english(self) -> None:
        assert self.clf.classify("What is the return policy?") == QueryType.FAQ

    # --- HYBRID（スコア 1） ---
    def test_hybrid_japanese_single_keyword(self) -> None:
        assert self.clf.classify("売上に関するポリシーを教えて") == QueryType.HYBRID

    def test_hybrid_chinese_single_keyword(self) -> None:
        assert self.clf.classify("销售政策是什么") == QueryType.HYBRID

    # --- SQL（スコア ≥ 2） ---
    def test_sql_japanese_multiple_keywords(self) -> None:
        assert self.clf.classify("月別の売上合計を教えて") == QueryType.SQL

    def test_sql_chinese_multiple_keywords(self) -> None:
        assert self.clf.classify("统计各部门的销售数量") == QueryType.SQL

    def test_sql_english_multiple_keywords(self) -> None:
        assert self.clf.classify("show monthly revenue count by region") == QueryType.SQL

    # --- module-level shortcut ---
    def test_classify_query_function(self) -> None:
        assert classify_query("月別の売上合計") == QueryType.SQL
```

**Step 2: テストが失敗することを確認**

```bash
conda activate agentflow
cd /home/liush/workspase/serverlessAIAgents
pytest apps/faq_system/tests/test_query_classifier.py -v
```

期待結果: `ImportError` または `ModuleNotFoundError`

**Step 3: 実装**

```python
# apps/faq_system/backend/services/query_classifier.py
"""クエリタイプ分類器 — 日本語・中国語・英語対応."""
from __future__ import annotations

from enum import Enum


class QueryType(str, Enum):
    """クエリタイプ."""

    FAQ = "faq"
    SQL = "sql"
    HYBRID = "hybrid"


class QueryClassifier:
    """質問文からクエリタイプを判定する統一分類器.

    スコアリング:
        ≥ 2 → SQL
        == 1 → HYBRID
        == 0 → FAQ
    """

    _SQL_KEYWORDS: frozenset[str] = frozenset([
        # 日本語
        "売上", "収入", "数量", "統計", "レポート", "top", "ランキング",
        "トレンド", "比較", "金額", "注文", "顧客数", "件数", "合計",
        "平均", "月別", "年別", "日別",
        # 中国語（既存互換）
        "销售", "收入", "数量", "统计", "报表", "排名", "趋势",
        "对比", "同比", "环比", "金额", "订单", "客户数",
        # 英語
        "revenue", "sales", "count", "report", "ranking", "trend",
        "comparison", "total", "average", "monthly", "yearly",
    ])

    def classify(self, question: str) -> QueryType:
        """質問文を分析してクエリタイプを返す.

        Args:
            question: 判定する質問文

        Returns:
            QueryType.SQL / HYBRID / FAQ
        """
        lower = question.lower()
        score = sum(1 for k in self._SQL_KEYWORDS if k in lower)
        if score >= 2:
            return QueryType.SQL
        if score == 1:
            return QueryType.HYBRID
        return QueryType.FAQ


# モジュールレベル シングルトン
_classifier = QueryClassifier()


def classify_query(question: str) -> QueryType:
    """モジュールレベルのショートカット関数."""
    return _classifier.classify(question)


__all__ = ["QueryClassifier", "QueryType", "classify_query"]
```

**Step 4: テストが通ることを確認**

```bash
pytest apps/faq_system/tests/test_query_classifier.py -v
```

期待結果: `8 passed`

**Step 5: コミット**

```bash
git add apps/faq_system/backend/services/query_classifier.py \
        apps/faq_system/tests/test_query_classifier.py
git commit -m "feat(faq): add unified QueryClassifier supporting ja/zh/en"
```

---

### Task 2: `InternalKBAgent` バグ修正（LLM実装・変数未使用）

**Files:**
- Modify: `apps/faq_system/backend/agents/internal_kb_agent.py`
- Create: `apps/faq_system/tests/test_internal_kb_agent.py`

**Step 1: テストを書く**

```python
# apps/faq_system/tests/test_internal_kb_agent.py
import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from apps.faq_system.backend.agents.internal_kb_agent import InternalKBAgent


@pytest.fixture
def mock_llm_client() -> AsyncMock:
    client = AsyncMock()
    client.chat.return_value = {"content": "LLMからの回答です。"}
    return client


@pytest.fixture
def agent(mock_llm_client: AsyncMock) -> InternalKBAgent:
    a = InternalKBAgent()
    a._llm_client = mock_llm_client
    a._initialized = True
    return a


class TestInternalKBAgentBugFixes:
    @pytest.mark.asyncio
    async def test_generate_answer_calls_llm(
        self, agent: InternalKBAgent, mock_llm_client: AsyncMock
    ) -> None:
        """_generate_answer は LLM を呼び出すこと."""
        search_results = [
            {
                "document_id": "doc1",
                "content": "有給休暇は年20日です",
                "score": 0.85,
                "citation": {"title": "就業規則", "source": "HR", "version": "v1.0", "update_date": "2025-01"},
            }
        ]
        response = await agent._generate_answer("有給は何日？", search_results)
        mock_llm_client.chat.assert_called_once()
        assert response.answer == "LLMからの回答です。"
        assert response.confidence > 0

    @pytest.mark.asyncio
    async def test_generate_conservative_answer_calls_llm(
        self, agent: InternalKBAgent, mock_llm_client: AsyncMock
    ) -> None:
        """_generate_conservative_answer は LLM を呼び出すこと."""
        search_results = [
            {
                "document_id": "doc1",
                "content": "第10条 年次有給休暇は...",
                "score": 0.9,
                "citation": {"title": "就業規則", "source": "HR", "version": "v1.0", "update_date": "2025-01"},
            }
        ]
        response = await agent._generate_conservative_answer("規則を教えて", search_results)
        mock_llm_client.chat.assert_called_once()
        assert response.answer == "LLMからの回答です。"

    @pytest.mark.asyncio
    async def test_run_stream_uses_question(
        self, agent: InternalKBAgent
    ) -> None:
        """run_stream が question を利用してメッセージを生成すること."""
        agent.run = AsyncMock(return_value={"answer": "ok", "question": "テスト質問"})  # type: ignore[method-assign]
        events = []
        async for event in agent.run_stream({"question": "テスト質問"}):
            events.append(event)

        progress_messages = [e["message"] for e in events if e.get("type") == "progress"]
        # いずれかの進捗メッセージが question を含む
        assert any("テスト質問" in msg for msg in progress_messages)
```

**Step 2: テストが失敗することを確認**

```bash
pytest apps/faq_system/tests/test_internal_kb_agent.py -v
```

期待結果: `FAILED` (LLM が呼ばれない・変数未使用)

**Step 3: `_generate_answer` を修正**

`apps/faq_system/backend/agents/internal_kb_agent.py` の `_generate_answer` メソッド（434行目付近）を以下に置き換える:

```python
    async def _generate_answer(
        self,
        question: str,
        search_results: list[dict[str, Any]],
    ) -> InternalKBResponse:
        """通常モード回答生成."""
        if not search_results:
            return InternalKBResponse(
                answer="申し訳ありません。関連する情報が見つかりませんでした。",
                confidence=0.0,
            )

        context_parts = []
        citations = []

        for i, result in enumerate(search_results[:5]):
            context_parts.append(f"[{i + 1}] {result['content']}")
            citations.append(
                Citation(
                    index=i + 1,
                    document_id=result.get("document_id", ""),
                    title=result.get("citation", {}).get("title", ""),
                    source=result.get("citation", {}).get("source", ""),
                    version=result.get("citation", {}).get("version", ""),
                    update_date=result.get("citation", {}).get("update_date", ""),
                    snippet=result.get("content", "")[:200],
                    relevance_score=result.get("score", 0.0),
                )
            )

        context = "\n\n".join(context_parts)
        prompt_messages = [
            {"role": "system", "content": self.SYSTEM_PROMPT},
            {
                "role": "user",
                "content": (
                    f"以下のコンテキストを参照して質問に回答してください。\n\n"
                    f"コンテキスト:\n{context}\n\n質問: {question}"
                ),
            },
        ]
        llm_response = await self._llm_client.chat(prompt_messages)
        answer = llm_response.get("content", "回答を生成できませんでした。")

        return InternalKBResponse(
            answer=answer,
            confidence=min(search_results[0].get("score", 0.5), 0.9),
            citations=citations,
        )
```

**Step 4: `_generate_conservative_answer` を修正**

同ファイルの `_generate_conservative_answer` メソッド（450行目付近）のハードコード部分を置き換え:

```python
        prompt_messages = [
            {"role": "system", "content": self.CONSERVATIVE_SYSTEM_PROMPT},
            {
                "role": "user",
                "content": (
                    f"以下の規則・制度の原文を参照して、質問に正確に回答してください。\n\n"
                    f"原文:\n{content}\n\n質問: {question}"
                ),
            },
        ]
        llm_response = await self._llm_client.chat(prompt_messages)
        answer = llm_response.get("content", "回答を生成できませんでした。")

        confidence = top_result.get("score", 0.5)

        if confidence < 0.7:
            answer += "\n\n⚠️ この回答は参考情報です。正確な内容は担当部門にご確認ください。"
```

**Step 5: `run_stream` の変数未使用を修正**

272行目の `input_data.get("question", "")` を:

```python
        question = input_data.get("question", "")
```

に変更し、進捗メッセージを更新:

```python
        yield {
            "type": "progress",
            "progress": 40,
            "message": f"「{question[:20]}」を知識ベースで検索中...",
        }
```

**Step 6: テストが通ることを確認**

```bash
pytest apps/faq_system/tests/test_internal_kb_agent.py -v
```

期待結果: `3 passed`

**Step 7: コミット**

```bash
git add apps/faq_system/backend/agents/internal_kb_agent.py \
        apps/faq_system/tests/test_internal_kb_agent.py
git commit -m "fix(faq): implement LLM calls in InternalKBAgent and fix unused variable in run_stream"
```

---

### Task 3: `chat_history_service.py` — type:ignore 修正 + N+1 クエリ解消

**Files:**
- Modify: `apps/faq_system/backend/services/chat_history_service.py`
- Create: `apps/faq_system/tests/test_chat_history_service.py`

**Step 1: テストを書く**

```python
# apps/faq_system/tests/test_chat_history_service.py
import pytest
from unittest.mock import AsyncMock, MagicMock, patch
from apps.faq_system.backend.services.chat_history_service import ChatHistoryService
from apps.faq_system.backend.auth.models import UserInfo


def _make_user(user_id: str = "u1") -> UserInfo:
    return UserInfo(user_id=user_id, username="taro", role="employee", department="IT")


class TestDeleteSession:
    @pytest.mark.asyncio
    async def test_delete_returns_true_when_rows_deleted(self) -> None:
        """削除が行われた場合 True を返すこと（type:ignore 不要な実装）."""
        svc = ChatHistoryService()
        mock_result = MagicMock()
        mock_result.rowcount = 3

        with patch(
            "apps.faq_system.backend.services.chat_history_service.get_db_session"
        ) as mock_ctx, patch(
            "apps.faq_system.backend.services.chat_history_service.ensure_database_ready",
            new_callable=AsyncMock,
        ):
            mock_session = AsyncMock()
            mock_session.execute.return_value = mock_result
            mock_ctx.return_value.__aenter__.return_value = mock_session
            mock_ctx.return_value.__aexit__.return_value = False

            result = await svc.delete_session("session-abc", _make_user())
            assert result is True

    @pytest.mark.asyncio
    async def test_delete_returns_false_when_no_rows(self) -> None:
        """行が削除されなかった場合 False を返すこと."""
        svc = ChatHistoryService()
        mock_result = MagicMock()
        mock_result.rowcount = 0

        with patch(
            "apps.faq_system.backend.services.chat_history_service.get_db_session"
        ) as mock_ctx, patch(
            "apps.faq_system.backend.services.chat_history_service.ensure_database_ready",
            new_callable=AsyncMock,
        ):
            mock_session = AsyncMock()
            mock_session.execute.return_value = mock_result
            mock_ctx.return_value.__aenter__.return_value = mock_session
            mock_ctx.return_value.__aexit__.return_value = False

            result = await svc.delete_session("session-xyz", _make_user())
            assert result is False


class TestListSessionsNPlusOne:
    @pytest.mark.asyncio
    async def test_list_sessions_single_db_context(self) -> None:
        """list_sessions が get_db_session を 2 回以内しか呼ばないこと（N+1解消）."""
        svc = ChatHistoryService()

        call_count = 0
        original_ctx = MagicMock()

        def counting_ctx(*args: object, **kwargs: object) -> MagicMock:
            nonlocal call_count
            call_count += 1
            return original_ctx

        mock_agg_rows = [
            MagicMock(session_id="s1", message_count=5, last_message_at=None),
            MagicMock(session_id="s2", message_count=3, last_message_at=None),
            MagicMock(session_id="s3", message_count=1, last_message_at=None),
        ]
        mock_preview_rows = [("s1", "初回メッセージ"), ("s2", "別の質問"), ("s3", "三つ目")]

        mock_session = AsyncMock()
        mock_session.execute.side_effect = [
            MagicMock(all=lambda: mock_agg_rows),        # 集計クエリ
            MagicMock(all=lambda: mock_preview_rows),    # プレビュー一括取得
        ]
        original_ctx.__aenter__ = AsyncMock(return_value=mock_session)
        original_ctx.__aexit__ = AsyncMock(return_value=False)

        with patch(
            "apps.faq_system.backend.services.chat_history_service.get_db_session",
            side_effect=counting_ctx,
        ), patch(
            "apps.faq_system.backend.services.chat_history_service.ensure_database_ready",
            new_callable=AsyncMock,
        ):
            results = await svc.list_sessions(_make_user(), limit=10)

        assert call_count <= 2, f"get_db_session was called {call_count} times (N+1 not fixed)"
        assert len(results) == 3
```

**Step 2: テストが失敗することを確認**

```bash
pytest apps/faq_system/tests/test_chat_history_service.py -v
```

期待結果: `test_list_sessions_single_db_context` が FAILED（N+1問題のため call_count > 2）

**Step 3: `delete_session` の `type: ignore` を修正**

`chat_history_service.py:172` を以下に置き換え:

```python
    async def delete_session(
        self,
        session_id: str,
        user: UserInfo,
    ) -> bool:
        """セッションの全メッセージを削除."""
        await ensure_database_ready()
        stmt = delete(ChatMessage).where(
            ChatMessage.session_id == session_id,
            ChatMessage.user_id == user.user_id,
        )
        async with get_db_session() as session:
            result = await session.execute(stmt)
            rowcount: int = getattr(result, "rowcount", 0) or 0
            return rowcount > 0
```

**Step 4: `list_sessions` の N+1 を修正**

`list_sessions` メソッド全体を置き換え:

```python
    async def list_sessions(
        self,
        user: UserInfo,
        *,
        limit: int = 50,
        offset: int = 0,
    ) -> list[dict[str, Any]]:
        """ユーザーのチャットセッション一覧を取得（N+1解消版）."""
        await ensure_database_ready()

        # 1. セッション集計
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

        async with get_db_session() as session:
            rows = (await session.execute(agg_stmt)).all()

        if not rows:
            return []

        session_ids = [row.session_id for row in rows]

        # 2. 各セッションの最初のユーザーメッセージを一括取得（N+1解消）
        first_at_subq = (
            select(
                ChatMessage.session_id,
                func.min(ChatMessage.created_at).label("first_at"),
            )
            .where(
                ChatMessage.session_id.in_(session_ids),
                ChatMessage.user_id == user.user_id,
                ChatMessage.role == "user",
            )
            .group_by(ChatMessage.session_id)
            .subquery()
        )

        preview_stmt = (
            select(ChatMessage.session_id, ChatMessage.content)
            .join(
                first_at_subq,
                (ChatMessage.session_id == first_at_subq.c.session_id)
                & (ChatMessage.created_at == first_at_subq.c.first_at),
            )
            .where(ChatMessage.user_id == user.user_id)
        )

        async with get_db_session() as session:
            preview_rows = (await session.execute(preview_stmt)).all()

        # session_id → content のマップ
        preview_map: dict[str, str] = {r.session_id: r.content for r in preview_rows}

        return [
            {
                "session_id": row.session_id,
                "title": self._auto_title_from_text(preview_map.get(row.session_id, ""))
                if preview_map.get(row.session_id)
                else row.session_id,
                "message_count": row.message_count,
                "last_message_at": row.last_message_at.isoformat()
                if row.last_message_at
                else None,
                "preview": (preview_map.get(row.session_id, "") or "")[:80],
            }
            for row in rows
        ]
```

**Step 5: テストが通ることを確認**

```bash
pytest apps/faq_system/tests/test_chat_history_service.py -v
```

期待結果: `3 passed`

**Step 6: コミット**

```bash
git add apps/faq_system/backend/services/chat_history_service.py \
        apps/faq_system/tests/test_chat_history_service.py
git commit -m "fix(faq): resolve N+1 query in list_sessions and remove type:ignore in delete_session"
```

---

### Task 4: 分類器の委譲 — `faq_service.py` と `enhanced_faq_agent.py`

**Files:**
- Modify: `apps/faq_system/backend/services/faq_service.py`
- Modify: `apps/faq_system/backend/agents/enhanced_faq_agent.py`

**Step 1: `faq_service.py` を修正**

ファイル上部のインポートに追加:
```python
from apps.faq_system.backend.services.query_classifier import (
    QueryType as _UnifiedQueryType,
    classify_query,
)
```

`_classify_query` メソッドを以下に置き換え:

```python
    def _classify_query(self, question: str) -> QueryType:
        """クエリタイプを分類（統一分類器に委譲）."""
        result = classify_query(question)
        # faq_service の QueryType へ変換
        mapping = {
            "sql": QueryType.SQL,
            "hybrid": QueryType.HYBRID,
            "faq": QueryType.FAQ,
        }
        return mapping.get(result.value, QueryType.FAQ)
```

**Step 2: `enhanced_faq_agent.py` を修正**

インポートに追加:
```python
from apps.faq_system.backend.services.query_classifier import classify_query
```

`_classify_query` メソッドを以下に置き換え:

```python
    def _classify_query(self, question: str) -> str:
        """クエリタイプを判定（統一分類器に委譲）."""
        return classify_query(question).value
```

**Step 3: 既存テストが通ることを確認**

```bash
pytest apps/faq_system/tests/test_query_classifier.py -v
```

期待結果: `8 passed`（回帰なし）

**Step 4: コミット**

```bash
git add apps/faq_system/backend/services/faq_service.py \
        apps/faq_system/backend/agents/enhanced_faq_agent.py
git commit -m "refactor(faq): delegate _classify_query to unified QueryClassifier"
```

---

### Task 5: `EnhancedFAQAgent` — 提案生成を `SuggestionService` に委譲

**Files:**
- Modify: `apps/faq_system/backend/agents/enhanced_faq_agent.py`
- Create: `apps/faq_system/tests/test_enhanced_faq_agent_suggestions.py`

**Step 1: テストを書く**

```python
# apps/faq_system/tests/test_enhanced_faq_agent_suggestions.py
import pytest
from unittest.mock import AsyncMock, MagicMock
from apps.faq_system.backend.agents.enhanced_faq_agent import EnhancedFAQAgent


@pytest.fixture
def agent() -> EnhancedFAQAgent:
    a = EnhancedFAQAgent()
    a._initialized = True
    return a


class TestGenerateSuggestions:
    @pytest.mark.asyncio
    async def test_uses_suggestion_service_when_available(
        self, agent: EnhancedFAQAgent
    ) -> None:
        """SuggestionService が利用可能な場合はその結果を返すこと."""
        mock_svc = AsyncMock()
        mock_result = MagicMock()
        mock_result.success = True
        mock_result.data = {
            "suggestions": [
                {"text": "前月との比較は？", "type": "followup"},
                {"text": "地域別の内訳は？", "type": "followup"},
            ]
        }
        mock_svc.execute.return_value = mock_result
        agent._suggestion_service = mock_svc

        result = await agent._generate_suggestions("月別の売上を教えて", "sql")

        mock_svc.execute.assert_called_once()
        assert len(result) == 2
        assert result[0]["text"] == "前月との比較は？"

    @pytest.mark.asyncio
    async def test_falls_back_when_service_raises(
        self, agent: EnhancedFAQAgent
    ) -> None:
        """SuggestionService が例外を投げた場合はフォールバック提案を返すこと."""
        mock_svc = AsyncMock()
        mock_svc.execute.side_effect = RuntimeError("connection error")
        agent._suggestion_service = mock_svc

        result = await agent._generate_suggestions("有給休暇の申請方法は？", "faq")

        assert len(result) > 0
        assert all("text" in s for s in result)

    @pytest.mark.asyncio
    async def test_falls_back_when_service_is_none(
        self, agent: EnhancedFAQAgent
    ) -> None:
        """SuggestionService が None の場合もフォールバック提案を返すこと."""
        agent._suggestion_service = None  # type: ignore[assignment]

        result = await agent._generate_suggestions("何か質問", "faq")

        assert len(result) > 0
```

**Step 2: テストが失敗することを確認**

```bash
pytest apps/faq_system/tests/test_enhanced_faq_agent_suggestions.py -v
```

期待結果: `FAILED` (`_suggestion_service` 属性が存在しない)

**Step 3: `enhanced_faq_agent.py` を修正**

`_ensure_initialized` メソッドに以下を追加:

```python
        from agentflow.services import SuggestionConfig, SuggestionService

        self._suggestion_service: SuggestionService | None = SuggestionService(
            SuggestionConfig(max_suggestions=5, language="ja")
        )
```

`_generate_suggestions` メソッドを置き換え:

```python
    async def _generate_suggestions(
        self, question: str, query_type: str
    ) -> list[dict[str, Any]]:
        """SuggestionService 経由でフォローアップ提案を生成."""
        if self._suggestion_service is not None:
            try:
                result = await self._suggestion_service.execute(
                    action="suggest",
                    question=question,
                    query_type=query_type,
                )
                if result.success:
                    suggestions = result.data.get("suggestions", [])
                    if suggestions:
                        return suggestions
            except Exception:
                self._logger.warning("提案生成失敗、フォールバックを使用")

        # フォールバック
        if query_type == "sql":
            return [
                {"text": "前月との比較を見せて", "type": "followup"},
                {"text": "カテゴリ別の内訳は？", "type": "followup"},
                {"text": "トップ10を表示", "type": "followup"},
            ]
        return [
            {"text": "もう少し詳しく教えて", "type": "followup"},
            {"text": "関連する情報は？", "type": "followup"},
            {"text": "例を見せて", "type": "followup"},
        ]
```

**Step 4: テストが通ることを確認**

```bash
pytest apps/faq_system/tests/test_enhanced_faq_agent_suggestions.py -v
```

期待結果: `3 passed`

**Step 5: コミット**

```bash
git add apps/faq_system/backend/agents/enhanced_faq_agent.py \
        apps/faq_system/tests/test_enhanced_faq_agent_suggestions.py
git commit -m "feat(faq): delegate suggestion generation to SuggestionService with fallback"
```

---

### Task 6: `app_config.json` — 未登録 Agent を追加

**Files:**
- Modify: `apps/faq_system/app_config.json`

**Step 1: `app_config.json` の `agents` 配列に追記**

`"agents": [` の `SalesAgent` エントリの後ろに以下を追加:

```json
    {
      "name": "EnhancedFAQAgent",
      "module": "apps.faq_system.backend.agents.enhanced_faq_agent",
      "capabilities": ["faq", "rag", "sql", "rich_response", "citation"],
      "business_base": "knowledge",
      "pattern": "specialist"
    },
    {
      "name": "InternalKBAgent",
      "module": "apps.faq_system.backend.agents.internal_kb_agent",
      "capabilities": ["faq", "rag", "rbac", "conservative_mode", "ticket"],
      "business_base": "knowledge",
      "pattern": "specialist"
    },
    {
      "name": "ExternalKBAgent",
      "module": "apps.faq_system.backend.agents.external_kb_agent",
      "capabilities": ["faq", "rag", "external_kb"],
      "business_base": "knowledge",
      "pattern": "specialist"
    },
    {
      "name": "MaintenanceAgent",
      "module": "apps.faq_system.backend.agents.maintenance_agent",
      "capabilities": ["maintenance", "impact_analysis", "documentation"],
      "business_base": "operations",
      "pattern": "specialist"
    },
    {
      "name": "AnalyticsAgent",
      "module": "apps.faq_system.backend.agents.analytics_agent",
      "capabilities": ["sql", "chart", "analysis", "trend"],
      "business_base": "reasoning",
      "pattern": "analyzer"
    }
```

**Step 2: JSON が正しいことを確認**

```bash
python -c "import json; json.load(open('apps/faq_system/app_config.json')); print('JSON OK')"
```

期待結果: `JSON OK`

**Step 3: コミット**

```bash
git add apps/faq_system/app_config.json
git commit -m "feat(faq): register EnhancedFAQAgent/InternalKBAgent/ExternalKBAgent/MaintenanceAgent/AnalyticsAgent in app_config.json"
```

---

### Task 7: Sidebar 削除確認 — i18n 対応インライン確認に置き換え

**Files:**
- Modify: `apps/faq_system/frontend/src/components/layout/Sidebar.tsx`
- Modify: `apps/faq_system/frontend/src/i18n/index.ts`

**Step 1: `i18n/index.ts` に翻訳キーを追加**

既存の翻訳オブジェクトに以下を追記（`ja`/`en`/`zh` 各言語）:

```typescript
// 追加するキー
'sidebar.confirm_delete': { ja: '削除確認', en: 'Confirm Delete', zh: '确认删除' },
'common.cancel':          { ja: 'キャンセル', en: 'Cancel', zh: '取消' },
```

**Step 2: `Sidebar.tsx` を修正**

`useState` の import に `useState` が含まれることを確認（既存）。

`const [loggingOut, setLoggingOut] = useState(false);` の直下に追加:

```tsx
const [pendingDeleteId, setPendingDeleteId] = useState<string | null>(null);
```

削除ボタン部分（既存の `<button ... onClick={() => { if (confirm(...)) deleteSession(...) }}>` ）を以下に置き換え:

```tsx
{pendingDeleteId === session.session_id ? (
    <div className="absolute right-1 top-1/2 -translate-y-1/2 flex gap-1 z-10">
        <button
            type="button"
            className="px-2 py-1 text-[10px] rounded-lg bg-red-500/20 text-red-400 border border-red-500/30 hover:bg-red-500/30 transition-colors"
            onClick={() => {
                void deleteSession(session.session_id);
                setPendingDeleteId(null);
            }}
        >
            {t('sidebar.confirm_delete')}
        </button>
        <button
            type="button"
            className="px-2 py-1 text-[10px] rounded-lg bg-white/5 text-[var(--text-muted)] border border-white/10 hover:bg-white/10 transition-colors"
            onClick={() => setPendingDeleteId(null)}
        >
            {t('common.cancel')}
        </button>
    </div>
) : (
    <button
        type="button"
        className={`opacity-0 group-hover:opacity-100 p-1.5 rounded-lg hover:bg-red-500/20 hover:text-red-400 absolute right-2 transition-all bg-transparent border-none cursor-pointer ${
            currentSessionId === session.session_id ? 'text-red-400/50' : 'text-[var(--text-muted)]'
        }`}
        onClick={() => setPendingDeleteId(session.session_id)}
    >
        <Trash2 size={14} />
    </button>
)}
```

**Step 3: 型チェックとビルド確認**

```bash
cd apps/faq_system/frontend
npm run type-check
npm run build
```

期待結果: エラーなし

**Step 4: コミット**

```bash
cd /home/liush/workspase/serverlessAIAgents
git add apps/faq_system/frontend/src/components/layout/Sidebar.tsx \
        apps/faq_system/frontend/src/i18n/index.ts
git commit -m "feat(faq-ui): replace confirm() with inline i18n confirmation for session delete"
```

---

### Task 8: CORS を環境変数で制御

**Files:**
- Modify: `apps/faq_system/main.py`
- Modify: `apps/faq_system/.env.example`

**Step 1: `main.py` の CORS 設定を修正**

`app.add_middleware(CORSMiddleware, ...)` のブロックを以下に置き換え:

```python
# CORS 許可オリジン（環境変数 FAQ_CORS_ORIGINS で上書き可能）
# 本番環境では必ず明示指定すること
_raw_origins = os.getenv("FAQ_CORS_ORIGINS", "*").strip()
_cors_origins: list[str] = (
    ["*"]
    if _raw_origins == "*"
    else [o.strip() for o in _raw_origins.split(",") if o.strip()]
)

app.add_middleware(
    CORSMiddleware,
    allow_origins=_cors_origins,
    allow_credentials=True,
    allow_methods=["*"],
    allow_headers=["*"],
)
```

**Step 2: `.env.example` に追記**

```bash
# CORS 許可オリジン（カンマ区切り。本番では必ず明示指定）
# 例: FAQ_CORS_ORIGINS=https://faq.example.com,https://admin.example.com
FAQ_CORS_ORIGINS=http://localhost:3004
```

**Step 3: 動作確認（手動）**

```bash
conda activate agentflow
python -c "
import os
os.environ['FAQ_CORS_ORIGINS'] = 'https://example.com,https://app.example.com'
# main.py の CORS 変数計算をシミュレート
_raw = os.getenv('FAQ_CORS_ORIGINS', '*').strip()
origins = ['*'] if _raw == '*' else [o.strip() for o in _raw.split(',') if o.strip()]
print('origins:', origins)
assert origins == ['https://example.com', 'https://app.example.com']
print('OK')
"
```

期待結果: `origins: ['https://example.com', 'https://app.example.com']` / `OK`

**Step 4: コミット**

```bash
git add apps/faq_system/main.py apps/faq_system/.env.example
git commit -m "fix(faq): make CORS origins configurable via FAQ_CORS_ORIGINS env var"
```

---

### Task 9: 全チェック実行・最終確認

**Step 1: Python チェック**

```bash
conda activate agentflow
cd /home/liush/workspase/serverlessAIAgents
ruff format apps/faq_system/
ruff check apps/faq_system/
mypy apps/faq_system/
pytest apps/faq_system/tests/ -v --cov=apps/faq_system --cov-fail-under=80
```

**Step 2: フロントエンドチェック**

```bash
cd apps/faq_system/frontend
npm run lint
npm run type-check
npm run build
```

**Step 3: 失敗がある場合の対処**

- Ruff エラー → `ruff check --fix` で自動修正後、手動修正
- mypy エラー → `code-rules/global/mypy-avoid-patterns.md` を参照して修正
- pytest カバレッジ不足 → 不足テストを追記
- ESLint / tsc エラー → エラーメッセージに従い修正

**Step 4: 最終コミット（全チェック通過後）**

```bash
cd /home/liush/workspase/serverlessAIAgents
git add -A
git commit -m "chore(faq): all checks pass after improvement round"
```

---

## 完了条件チェックリスト

- [ ] Task 1〜9 の全テストが PASS
- [ ] `ruff format` / `ruff check` エラーゼロ
- [ ] `mypy apps/faq_system/` エラーゼロ（または既存エラーを悪化させていない）
- [ ] `pytest --cov-fail-under=80` 通過
- [ ] `npm run type-check` / `npm run build` エラーゼロ
- [ ] `app_config.json` が有効な JSON であること
- [ ] `FAQ_CORS_ORIGINS` が `.env.example` にドキュメント化されていること
