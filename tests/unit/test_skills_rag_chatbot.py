"""RAG Skill と ChatBot Skill のユニットテスト."""

import pytest

from agentflow.skills.chatbot import (
    ChatBotConfig,
    ChatBotSkill,
    ChatMessage,
    ChatSession,
)
from agentflow.skills.rag import RAGConfig, RAGResult, RAGSkill


class TestRAGSkill:
    """RAG Skill テスト."""

    @pytest.fixture
    def rag_skill(self) -> RAGSkill:
        """RAG Skill インスタンスを作成."""
        return RAGSkill()

    @pytest.mark.asyncio
    async def test_start_stop(self, rag_skill: RAGSkill) -> None:
        """開始・停止テスト."""
        await rag_skill.start()
        await rag_skill.stop()

    @pytest.mark.asyncio
    async def test_add_document(self, rag_skill: RAGSkill) -> None:
        """ドキュメント追加テスト."""
        await rag_skill.start()
        try:
            doc_id = await rag_skill.add_document(
                content="AgentFlow は軽量 AI Agent フレームワークです。",
                topic="framework",
            )
            assert doc_id is not None
            assert isinstance(doc_id, str)
        finally:
            await rag_skill.stop()

    @pytest.mark.asyncio
    async def test_query(self, rag_skill: RAGSkill) -> None:
        """質問応答テスト."""
        await rag_skill.start()
        try:
            # ドキュメント追加
            await rag_skill.add_document(
                content="AgentFlow は軽量 AI Agent フレームワークです。",
                topic="framework",
            )
            await rag_skill.add_document(
                content="PocketFlow をベースに設計されています。",
                topic="framework",
            )

            # 質問
            result = await rag_skill.query("AgentFlow とは何ですか？")

            assert isinstance(result, RAGResult)
            assert result.answer is not None
            assert isinstance(result.sources, list)
        finally:
            await rag_skill.stop()

    def test_rag_config_defaults(self) -> None:
        """RAG 設定デフォルト値テスト."""
        config = RAGConfig()
        assert config.top_k == 5
        assert config.min_similarity == 0.3

    @pytest.mark.asyncio
    async def test_get_status(self, rag_skill: RAGSkill) -> None:
        """ステータス取得テスト."""
        await rag_skill.start()
        try:
            status = rag_skill.get_status()
            assert "rag_config" in status
            assert "memory" in status
        finally:
            await rag_skill.stop()


class TestChatBotSkill:
    """ChatBot Skill テスト."""

    @pytest.fixture
    def chatbot_skill(self) -> ChatBotSkill:
        """ChatBot Skill インスタンスを作成."""
        return ChatBotSkill()

    def test_create_session(self, chatbot_skill: ChatBotSkill) -> None:
        """セッション作成テスト."""
        session = chatbot_skill.create_session()
        assert isinstance(session, ChatSession)
        assert session.id is not None
        # システムプロンプトが追加されていること
        assert len(session.messages) == 1
        assert session.messages[0].role == "system"

    def test_get_session(self, chatbot_skill: ChatBotSkill) -> None:
        """セッション取得テスト."""
        session = chatbot_skill.create_session()
        retrieved = chatbot_skill.get_session(session.id)
        assert retrieved is not None
        assert retrieved.id == session.id

    @pytest.mark.asyncio
    async def test_chat(self, chatbot_skill: ChatBotSkill) -> None:
        """チャットテスト."""
        session = chatbot_skill.create_session()
        response = await chatbot_skill.chat(session.id, "こんにちは")

        assert response is not None
        assert isinstance(response, str)
        # 履歴が更新されていること
        assert len(session.messages) == 3  # system + user + assistant

    @pytest.mark.asyncio
    async def test_chat_session_not_found(self, chatbot_skill: ChatBotSkill) -> None:
        """存在しないセッションでのチャットテスト."""
        with pytest.raises(KeyError):
            await chatbot_skill.chat("nonexistent", "こんにちは")

    def test_clear_session(self, chatbot_skill: ChatBotSkill) -> None:
        """セッション削除テスト."""
        session = chatbot_skill.create_session()
        assert chatbot_skill.clear_session(session.id) is True
        assert chatbot_skill.get_session(session.id) is None

    def test_list_sessions(self, chatbot_skill: ChatBotSkill) -> None:
        """セッション一覧テスト."""
        chatbot_skill.create_session()
        chatbot_skill.create_session()

        sessions = chatbot_skill.list_sessions()
        assert len(sessions) == 2

    def test_chatbot_config_defaults(self) -> None:
        """ChatBot 設定デフォルト値テスト."""
        config = ChatBotConfig()
        assert config.max_history == 20
        assert config.enable_rag is False
        assert config.enable_agent is False


class TestChatSession:
    """ChatSession テスト."""

    def test_add_message(self) -> None:
        """メッセージ追加テスト."""
        session = ChatSession()
        msg = session.add_message("user", "テストメッセージ")

        assert isinstance(msg, ChatMessage)
        assert msg.role == "user"
        assert msg.content == "テストメッセージ"
        assert len(session.messages) == 1

    def test_get_history_with_limit(self) -> None:
        """履歴取得（制限あり）テスト."""
        session = ChatSession()
        for i in range(5):
            session.add_message("user", f"メッセージ{i}")

        history = session.get_history(limit=3)
        assert len(history) == 3
        assert history[0].content == "メッセージ2"  # 最後の3件

    def test_to_llm_messages(self) -> None:
        """LLM メッセージ変換テスト."""
        session = ChatSession()
        session.add_message("system", "システム")
        session.add_message("user", "ユーザー")

        llm_messages = session.to_llm_messages()
        assert len(llm_messages) == 2
        # to_llm_messages() は dict リストを返す
        assert llm_messages[0]["role"] == "system"
        assert llm_messages[1]["role"] == "user"
