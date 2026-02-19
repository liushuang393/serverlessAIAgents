"""Turn-Based Compressor - ターン数ベースの会話圧縮.

会話の長さ（ターン数）に基づいて自動的に圧縮を実行する。
Token閾値だけでなく、ターン数でも圧縮をトリガー。

設計原則:
- ターン数トリガー: 10ターンごとに圧縮
- KeyNotes保護: 重要情報は圧縮されない
- 段階的圧縮: 古いメッセージから優先的に圧縮
- 要約生成: 圧縮時に要約を生成

使用例:
    >>> compressor = TurnBasedCompressor(key_notes_store=notes_store)
    >>> compressor.add_message(user_msg)
    >>> compressor.add_message(assistant_msg)
    >>> # 10ターン後に自動圧縮
    >>> if compressor.should_compress():
    ...     result = await compressor.compress()
"""

from __future__ import annotations

import logging
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any, Protocol

from agentflow.context.key_notes import KeyNotesStore, NoteImportance


_logger = logging.getLogger(__name__)


class MessageRole(str, Enum):
    """メッセージロール."""

    SYSTEM = "system"
    USER = "user"
    ASSISTANT = "assistant"
    TOOL = "tool"


@dataclass
class Message:
    """メッセージ.

    Attributes:
        role: ロール
        content: 内容
        timestamp: タイムスタンプ
        turn_number: ターン番号
        metadata: メタデータ
    """

    role: MessageRole
    content: str
    timestamp: datetime = field(default_factory=lambda: datetime.now(UTC))
    turn_number: int = 0
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class CompressionResult:
    """圧縮結果.

    Attributes:
        compressed_messages: 圧縮後のメッセージ
        summary: 生成された要約
        original_count: 元のメッセージ数
        compressed_count: 圧縮後のメッセージ数
        extracted_notes_count: 抽出されたNote数
        token_reduction: Token削減率
    """

    compressed_messages: list[Message]
    summary: str
    original_count: int
    compressed_count: int
    extracted_notes_count: int = 0
    token_reduction: float = 0.0


@dataclass
class TurnConfig:
    """ターン圧縮設定.

    Attributes:
        turn_threshold: 圧縮トリガーターン数
        token_threshold: 圧縮トリガーToken数
        keep_recent_turns: 保持する最近ターン数
        max_summary_tokens: 要約の最大Token数
        extract_notes: KeyNotes抽出有効化
    """

    turn_threshold: int = 10
    token_threshold: int = 4000
    keep_recent_turns: int = 3
    max_summary_tokens: int = 300
    extract_notes: bool = True


class LLMSummarizer(Protocol):
    """LLM要約インターフェース（DI用）."""

    async def summarize(
        self,
        messages: list[dict[str, str]],
        max_tokens: int,
    ) -> str:
        """メッセージを要約."""
        ...


class TurnBasedCompressor:
    """ターン数ベースの会話圧縮器.

    会話のターン数に基づいて自動的に圧縮を実行し、
    重要情報をKeyNotesに抽出する。

    Example:
        >>> compressor = TurnBasedCompressor(
        ...     config=TurnConfig(turn_threshold=10),
        ...     key_notes_store=KeyNotesStore(),
        ... )
        >>>
        >>> # メッセージ追加
        >>> for msg in messages:
        ...     compressor.add_message(msg)
        >>>
        >>> # 圧縮チェック
        >>> if compressor.should_compress():
        ...     result = await compressor.compress()
        ...     print(f"圧縮: {result.original_count} -> {result.compressed_count}")
    """

    def __init__(
        self,
        config: TurnConfig | None = None,
        key_notes_store: KeyNotesStore | None = None,
        llm_summarizer: LLMSummarizer | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 圧縮設定
            key_notes_store: KeyNotesストア
            llm_summarizer: LLM要約器
        """
        self._config = config or TurnConfig()
        self._key_notes = key_notes_store or KeyNotesStore()
        self._summarizer = llm_summarizer
        self._messages: list[Message] = []
        self._turn_count = 0
        self._last_compression_turn = 0
        self._summaries: list[str] = []  # 過去の要約を保持
        self._logger = logging.getLogger(__name__)

    def add_message(
        self,
        role: str | MessageRole,
        content: str,
        metadata: dict[str, Any] | None = None,
    ) -> Message:
        """メッセージを追加.

        Args:
            role: メッセージロール
            content: メッセージ内容
            metadata: メタデータ

        Returns:
            追加されたメッセージ
        """
        if isinstance(role, str):
            role = MessageRole(role)

        # ターンカウント（user/assistantペアで1ターン）
        if role == MessageRole.USER:
            self._turn_count += 1

        message = Message(
            role=role,
            content=content,
            turn_number=self._turn_count,
            metadata=metadata or {},
        )

        self._messages.append(message)
        self._logger.debug(
            "メッセージ追加: turn=%d, role=%s, len=%d", self._turn_count, role.value, len(content)
        )

        return message

    def should_compress(self) -> bool:
        """圧縮が必要かチェック.

        Returns:
            圧縮が必要か
        """
        # ターン数チェック
        turns_since_compression = self._turn_count - self._last_compression_turn
        if turns_since_compression >= self._config.turn_threshold:
            return True

        # Token数チェック（概算）
        total_tokens = sum(len(m.content) // 4 for m in self._messages)
        return total_tokens >= self._config.token_threshold

    async def compress(self) -> CompressionResult:
        """会話を圧縮.

        Returns:
            圧縮結果
        """
        original_count = len(self._messages)

        if original_count == 0:
            return CompressionResult(
                compressed_messages=[],
                summary="",
                original_count=0,
                compressed_count=0,
            )

        # 最近のメッセージを保持
        keep_count = self._config.keep_recent_turns * 2  # user + assistant
        recent_messages = self._messages[-keep_count:] if keep_count > 0 else []
        to_compress = self._messages[:-keep_count] if keep_count > 0 else self._messages

        # KeyNotes抽出
        extracted_count = 0
        if self._config.extract_notes and to_compress:
            extracted_count = await self._extract_key_notes(to_compress)

        # 要約生成
        summary = ""
        if to_compress:
            summary = await self._generate_summary(to_compress)
            self._summaries.append(summary)

        # 圧縮後のメッセージを構築
        compressed_messages: list[Message] = []

        # システムメッセージは保持
        system_messages = [m for m in self._messages if m.role == MessageRole.SYSTEM]
        compressed_messages.extend(system_messages)

        # 要約をシステムメッセージとして追加
        if summary:
            compressed_messages.append(
                Message(
                    role=MessageRole.SYSTEM,
                    content=f"[会話要約]\n{summary}",
                    metadata={"is_summary": True},
                )
            )

        # 最近のメッセージを追加
        compressed_messages.extend([m for m in recent_messages if m.role != MessageRole.SYSTEM])

        # Token削減率計算
        original_tokens = sum(len(m.content) // 4 for m in self._messages)
        compressed_tokens = sum(len(m.content) // 4 for m in compressed_messages)
        reduction = 1.0 - (compressed_tokens / original_tokens) if original_tokens > 0 else 0.0

        # 状態更新
        self._messages = compressed_messages
        self._last_compression_turn = self._turn_count

        result = CompressionResult(
            compressed_messages=compressed_messages,
            summary=summary,
            original_count=original_count,
            compressed_count=len(compressed_messages),
            extracted_notes_count=extracted_count,
            token_reduction=reduction,
        )

        self._logger.info(
            "会話圧縮完了: %d -> %d (削減率: %.1f%%, Notes: %d)",
            original_count,
            len(compressed_messages),
            reduction * 100,
            extracted_count,
        )

        return result

    async def _extract_key_notes(self, messages: list[Message]) -> int:
        """メッセージからKeyNotesを抽出.

        Args:
            messages: 対象メッセージ

        Returns:
            抽出されたNote数
        """
        count = 0
        for msg in messages:
            if msg.role in (MessageRole.USER, MessageRole.ASSISTANT):
                notes = await self._key_notes.extract_and_store(
                    content=msg.content,
                    source=f"turn_{msg.turn_number}",
                )
                count += len(notes)
        return count

    async def _generate_summary(self, messages: list[Message]) -> str:
        """メッセージを要約.

        Args:
            messages: 対象メッセージ

        Returns:
            要約テキスト
        """
        if self._summarizer:
            # LLM要約
            msg_dicts = [
                {"role": m.role.value, "content": m.content}
                for m in messages
                if m.role != MessageRole.SYSTEM
            ]
            try:
                return await self._summarizer.summarize(
                    msg_dicts,
                    self._config.max_summary_tokens,
                )
            except Exception as e:
                self._logger.warning("LLM要約に失敗: %s", e)

        # フォールバック: 簡易要約
        return self._simple_summarize(messages)

    def _simple_summarize(self, messages: list[Message]) -> str:
        """簡易要約（LLMなし）.

        Args:
            messages: 対象メッセージ

        Returns:
            簡易要約
        """
        user_messages = [m for m in messages if m.role == MessageRole.USER]
        assistant_messages = [m for m in messages if m.role == MessageRole.ASSISTANT]

        parts = []

        if user_messages:
            # ユーザーの主要トピックを抽出
            topics = []
            for msg in user_messages[:3]:  # 最初の3つ
                # 最初の50文字
                topic = msg.content[:50].replace("\n", " ")
                if len(msg.content) > 50:
                    topic += "..."
                topics.append(topic)
            parts.append(f"ユーザーの質問/要求: {'; '.join(topics)}")

        if assistant_messages:
            parts.append(f"応答数: {len(assistant_messages)}")

        # KeyNotesからの情報
        notes = self._key_notes.get_notes_by_importance(NoteImportance.HIGH)
        if notes:
            note_summaries = [n.content[:30] for n in notes[:3]]
            parts.append(f"重要情報: {'; '.join(note_summaries)}")

        return "\n".join(parts) if parts else "会話履歴（詳細省略）"

    def get_messages(self) -> list[Message]:
        """現在のメッセージを取得.

        Returns:
            メッセージリスト
        """
        return self._messages.copy()

    def get_messages_as_dicts(self) -> list[dict[str, str]]:
        """メッセージを辞書形式で取得.

        Returns:
            辞書形式のメッセージリスト
        """
        return [{"role": m.role.value, "content": m.content} for m in self._messages]

    def get_turn_count(self) -> int:
        """現在のターン数を取得.

        Returns:
            ターン数
        """
        return self._turn_count

    def get_context_with_notes(self) -> str:
        """KeyNotesを含むコンテキストを取得.

        Returns:
            コンテキスト文字列
        """
        parts = []

        # KeyNotes
        notes_context = self._key_notes.to_context_string()
        if notes_context:
            parts.append(notes_context)

        # 過去の要約
        if self._summaries:
            parts.append("# 会話履歴要約")
            for i, summary in enumerate(self._summaries[-3:], 1):  # 最新3つ
                parts.append(f"## セッション{i}\n{summary}")

        return "\n\n".join(parts)

    def reset(self) -> None:
        """状態をリセット."""
        self._messages.clear()
        self._turn_count = 0
        self._last_compression_turn = 0
        self._summaries.clear()

    def get_key_notes_store(self) -> KeyNotesStore:
        """KeyNotesストアを取得.

        Returns:
            KeyNotesストア
        """
        return self._key_notes

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得.

        Returns:
            統計情報
        """
        return {
            "turn_count": self._turn_count,
            "message_count": len(self._messages),
            "last_compression_turn": self._last_compression_turn,
            "summary_count": len(self._summaries),
            "key_notes_count": len(self._key_notes.get_all_notes()),
            "estimated_tokens": sum(len(m.content) // 4 for m in self._messages),
        }
