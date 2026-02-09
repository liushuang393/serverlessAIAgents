"""ChatBot Skill - 対話管理と Agent 連携能力.

このモジュールは、マルチターン対話と Agent 呼び出しを統合した
ChatBot 機能を提供します：
- 会話履歴管理
- コンテキスト維持
- Agent（Coordinator）との連携
- RAG 統合（オプション）

設計原則：
- 簡単：最小設定で対話開始
- 柔軟：Agent/RAG をオプション統合
- 拡張：カスタムフック対応
- 松耦合：LLM プロバイダーを意識しない
"""

from __future__ import annotations

import logging
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import TYPE_CHECKING, Any

from agentflow.providers import get_llm


if TYPE_CHECKING:
    from collections.abc import Callable

    from agentflow.patterns.coordinator import CoordinatorBase
    from agentflow.providers.llm_provider import LLMProvider
    from agentflow.skills.rag import RAGSkill


@dataclass
class ChatMessage:
    """チャットメッセージ.

    Attributes:
        id: メッセージ ID
        role: 役割（user/assistant/system）
        content: メッセージ内容
        timestamp: タイムスタンプ
        metadata: 追加メタデータ
    """

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    role: str = "user"
    content: str = ""
    timestamp: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class ChatSession:
    """チャットセッション.

    Attributes:
        id: セッション ID
        messages: メッセージ履歴
        created_at: 作成日時
        metadata: セッションメタデータ
    """

    id: str = field(default_factory=lambda: str(uuid.uuid4()))
    messages: list[ChatMessage] = field(default_factory=list)
    created_at: datetime = field(default_factory=datetime.now)
    metadata: dict[str, Any] = field(default_factory=dict)

    def add_message(self, role: str, content: str, **kwargs: Any) -> ChatMessage:
        """メッセージを追加.

        Args:
            role: メッセージ役割
            content: メッセージ内容
            **kwargs: 追加メタデータ

        Returns:
            追加されたメッセージ
        """
        msg = ChatMessage(role=role, content=content, metadata=kwargs)
        self.messages.append(msg)
        return msg

    def get_history(self, limit: int | None = None) -> list[ChatMessage]:
        """履歴を取得.

        Args:
            limit: 最大取得数（None で全件）

        Returns:
            メッセージリスト
        """
        if limit:
            return self.messages[-limit:]
        return list(self.messages)

    def to_llm_messages(self, limit: int | None = None) -> list[dict[str, str]]:
        """LLM 用メッセージに変換（松耦合設計）.

        Args:
            limit: 最大取得数

        Returns:
            メッセージリスト（dict 形式）
        """
        history = self.get_history(limit)
        return [{"role": m.role, "content": m.content} for m in history]


@dataclass
class ChatBotConfig:
    """ChatBot 設定.

    Attributes:
        system_prompt: システムプロンプト
        max_history: 最大履歴保持数
        enable_rag: RAG 統合を有効化
        enable_agent: Agent 呼び出しを有効化
        agent_trigger: Agent 呼び出しトリガー（キーワード）
    """

    system_prompt: str = "あなたは親切で知識豊富なアシスタントです。"
    max_history: int = 20
    enable_rag: bool = False
    enable_agent: bool = False
    agent_trigger: str = "/agent"


class ChatBotSkill:
    """ChatBot Skill - 対話管理と Agent 連携.

    マルチターン対話を管理し、必要に応じて Agent や RAG を呼び出します。

    Example:
        >>> # 基本使用
        >>> bot = ChatBotSkill()
        >>> session = bot.create_session()
        >>> response = await bot.chat(session.id, "こんにちは")
        >>>
        >>> # Agent 連携
        >>> bot = ChatBotSkill(coordinator=my_coordinator)
        >>> response = await bot.chat(session.id, "/agent 市場調査して")
        >>>
        >>> # RAG 連携
        >>> bot = ChatBotSkill(rag_skill=my_rag)
        >>> response = await bot.chat(session.id, "製品について教えて")
    """

    def __init__(
        self,
        config: ChatBotConfig | None = None,
        coordinator: CoordinatorBase | None = None,
        rag_skill: RAGSkill | None = None,
        pre_hook: Callable[[str, ChatSession], str] | None = None,
        post_hook: Callable[[str, ChatSession], str] | None = None,
        *,
        temperature: float | None = None,
    ) -> None:
        """初期化.

        Note:
            LLM プロバイダー/モデルは環境変数から自動検出されます。
            具体的なプロバイダーを指定する必要はありません（松耦合設計）。

        Args:
            config: ChatBot 設定
            coordinator: Agent 協調器（オプション）
            rag_skill: RAG スキル（オプション）
            pre_hook: 入力前処理フック
            post_hook: 出力後処理フック
            temperature: LLM 温度パラメータ（省略時はデフォルト）
        """
        self._config = config or ChatBotConfig()
        self._logger = logging.getLogger(__name__)

        # LLM プロバイダー（環境変数から自動検出・松耦合）
        self._llm: LLMProvider = get_llm(temperature=temperature)

        # オプション機能
        self._coordinator = coordinator
        self._rag = rag_skill
        self._pre_hook = pre_hook
        self._post_hook = post_hook

        # セッション管理
        self._sessions: dict[str, ChatSession] = {}

    def create_session(self, metadata: dict[str, Any] | None = None) -> ChatSession:
        """新規セッションを作成.

        Args:
            metadata: セッションメタデータ

        Returns:
            新規セッション
        """
        session = ChatSession(metadata=metadata or {})
        # システムプロンプトを追加
        session.add_message("system", self._config.system_prompt)
        self._sessions[session.id] = session
        self._logger.info(f"Created session: {session.id}")
        return session

    def get_session(self, session_id: str) -> ChatSession | None:
        """セッションを取得.

        Args:
            session_id: セッション ID

        Returns:
            セッション、存在しない場合 None
        """
        return self._sessions.get(session_id)

    async def chat(
        self,
        session_id: str,
        user_input: str,
        **kwargs: Any,
    ) -> str:
        """チャット応答を生成.

        Args:
            session_id: セッション ID
            user_input: ユーザー入力
            **kwargs: 追加パラメータ

        Returns:
            アシスタント応答

        Raises:
            KeyError: セッションが存在しない場合
        """
        session = self._sessions.get(session_id)
        if not session:
            msg = f"Session not found: {session_id}"
            raise KeyError(msg)

        # 前処理フック
        if self._pre_hook:
            user_input = self._pre_hook(user_input, session)

        # ユーザーメッセージを追加
        session.add_message("user", user_input)

        # Agent トリガーチェック
        if self._config.enable_agent and self._coordinator:
            if user_input.startswith(self._config.agent_trigger):
                task = user_input[len(self._config.agent_trigger):].strip()
                result = await self._coordinator.execute(task, **kwargs)
                response = f"[Agent結果]\n{result.get('result', result)}"
                session.add_message("assistant", response)
                return response

        # RAG 有効時
        if self._config.enable_rag and self._rag:
            rag_result = await self._rag.query(user_input)
            # RAG コンテキストをシステムに追加
            context_msg = f"[参考情報]\n{rag_result.context_used}"
            messages = session.to_llm_messages(self._config.max_history)
            messages.insert(1, {"role": "system", "content": context_msg})
        else:
            messages = session.to_llm_messages(self._config.max_history)

        # LLM 応答生成（松耦合：プロバイダー不明）
        response = await self._llm.chat(messages)
        assistant_response = response["content"]

        # 後処理フック
        if self._post_hook:
            assistant_response = self._post_hook(assistant_response, session)

        # 応答を履歴に追加
        session.add_message("assistant", assistant_response)

        return assistant_response

    def clear_session(self, session_id: str) -> bool:
        """セッションをクリア.

        Args:
            session_id: セッション ID

        Returns:
            削除成功した場合 True
        """
        if session_id in self._sessions:
            del self._sessions[session_id]
            return True
        return False

    def list_sessions(self) -> list[dict[str, Any]]:
        """全セッション一覧を取得.

        Returns:
            セッション情報リスト
        """
        return [
            {
                "id": s.id,
                "message_count": len(s.messages),
                "created_at": s.created_at.isoformat(),
            }
            for s in self._sessions.values()
        ]

