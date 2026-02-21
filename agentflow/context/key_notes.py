"""Key Notes Store - 重要情報の永続化.

会話中の重要情報を抽出し、圧縮されても失われないように
永続化する。長期会話で重要なコンテキストを維持。

設計原則:
- 自動抽出: LLM/ルールベースで重要情報を自動抽出
- 重複排除: 類似情報の重複を防止
- 優先度管理: 重要度に基づく優先度付け
- 永続化: 圧縮時も保持される

使用例:
    >>> store = KeyNotesStore()
    >>> await store.extract_and_store(
    ...     content="ユーザーの名前は田中さんです",
    ...     source="user_info",
    ... )
    >>> notes = store.get_all_notes()
"""

from __future__ import annotations

import hashlib
import logging
import re
from dataclasses import dataclass, field
from datetime import UTC, datetime
from enum import Enum
from typing import Any, Protocol


_logger = logging.getLogger(__name__)


class NoteImportance(str, Enum):
    """重要度レベル."""

    CRITICAL = "critical"  # 絶対保持（ユーザー名、設定等）
    HIGH = "high"  # 高優先度（明示的な要求、決定事項）
    MEDIUM = "medium"  # 中優先度（関連情報）
    LOW = "low"  # 低優先度（補足情報）


class NoteCategory(str, Enum):
    """Noteカテゴリ."""

    USER_INFO = "user_info"  # ユーザー情報
    PREFERENCE = "preference"  # 設定・好み
    DECISION = "decision"  # 決定事項
    FACT = "fact"  # 事実情報
    CONTEXT = "context"  # コンテキスト情報
    INSTRUCTION = "instruction"  # 指示・要求
    SUMMARY = "summary"  # 要約


@dataclass
class KeyNote:
    """重要Note.

    Attributes:
        id: 一意識別子
        content: Noteの内容
        importance: 重要度
        category: カテゴリ
        source: 抽出元
        created_at: 作成日時
        last_accessed: 最終アクセス日時
        access_count: アクセス回数
        metadata: 追加メタデータ
    """

    id: str
    content: str
    importance: NoteImportance
    category: NoteCategory
    source: str = ""
    created_at: datetime = field(default_factory=lambda: datetime.now(UTC))
    last_accessed: datetime = field(default_factory=lambda: datetime.now(UTC))
    access_count: int = 0
    metadata: dict[str, Any] = field(default_factory=dict)

    def touch(self) -> None:
        """アクセスを記録."""
        self.last_accessed = datetime.now(UTC)
        self.access_count += 1


@dataclass
class StoreConfig:
    """ストア設定.

    Attributes:
        max_notes: 最大Note数
        dedup_threshold: 重複判定閾値
        auto_extract: 自動抽出有効化
        persist_path: 永続化パス（Noneでメモリのみ）
    """

    max_notes: int = 100
    dedup_threshold: float = 0.8
    auto_extract: bool = True
    persist_path: str | None = None


class LLMExtractor(Protocol):
    """LLM抽出インターフェース（DI用）."""

    async def extract_key_info(self, content: str) -> list[dict[str, Any]]:
        """コンテンツから重要情報を抽出."""
        ...


class KeyNotesStore:
    """重要Notes永続化ストア.

    会話中の重要情報を抽出・保存し、圧縮されても
    失われないように管理する。

    Example:
        >>> store = KeyNotesStore()
        >>>
        >>> # 手動追加
        >>> note = store.add_note(
        ...     content="予算は100万円以内",
        ...     importance=NoteImportance.HIGH,
        ...     category=NoteCategory.DECISION,
        ... )
        >>>
        >>> # 自動抽出
        >>> await store.extract_and_store(
        ...     content="私は田中太郎、東京在住です",
        ...     source="user_message",
        ... )
        >>>
        >>> # 取得
        >>> notes = store.get_notes_by_importance(NoteImportance.HIGH)
    """

    # 重要情報を示唆するパターン（多言語）
    _IMPORTANCE_PATTERNS = {
        NoteImportance.CRITICAL: [
            # ユーザー識別情報
            r"(私|僕|わたし)の(名前|氏名)は",
            r"(我|我的)(名字|姓名)是",
            r"my name is",
            r"i am called",
        ],
        NoteImportance.HIGH: [
            # 決定・要求
            r"(必ず|絶対に|絶対|必須|マスト)",
            r"(一定要|必须|务必)",
            r"(must|always|never|required)",
            # 明示的設定
            r"(設定|設定して|セット)",
            r"(设置|配置)",
            r"(set|configure|setting)",
        ],
        NoteImportance.MEDIUM: [
            # 好み
            r"(好み|好き|嫌い|苦手)",
            r"(喜欢|不喜欢|偏好)",
            r"(prefer|like|dislike|favorite)",
        ],
    }

    # カテゴリ判定パターン
    _CATEGORY_PATTERNS = {
        NoteCategory.USER_INFO: [
            r"(名前|氏名|年齢|住所|メール|電話)",
            r"(名字|姓名|年龄|地址|邮箱|电话)",
            r"(name|age|address|email|phone)",
        ],
        NoteCategory.PREFERENCE: [
            r"(好み|好き|嫌い|お気に入り)",
            r"(喜欢|偏好|收藏)",
            r"(prefer|like|favorite)",
        ],
        NoteCategory.DECISION: [
            r"(決定|決めた|確定|合意)",
            r"(决定|确定|同意)",
            r"(decided|agreed|confirmed)",
        ],
        NoteCategory.INSTRUCTION: [
            r"(してください|してね|お願い)",
            r"(请|麻烦|要求)",
            r"(please|should|must)",
        ],
    }

    def __init__(
        self,
        config: StoreConfig | None = None,
        llm_extractor: LLMExtractor | None = None,
    ) -> None:
        """初期化.

        Args:
            config: ストア設定
            llm_extractor: LLM抽出器（オプション）
        """
        self._config = config or StoreConfig()
        self._llm_extractor = llm_extractor
        self._notes: dict[str, KeyNote] = {}
        self._logger = logging.getLogger(__name__)

    def add_note(
        self,
        content: str,
        importance: NoteImportance = NoteImportance.MEDIUM,
        category: NoteCategory = NoteCategory.CONTEXT,
        source: str = "",
        metadata: dict[str, Any] | None = None,
    ) -> KeyNote:
        """Noteを追加.

        Args:
            content: Note内容
            importance: 重要度
            category: カテゴリ
            source: 抽出元
            metadata: 追加メタデータ

        Returns:
            追加されたNote
        """
        # 重複チェック
        if self._is_duplicate(content):
            self._logger.debug("重複Note検出、スキップ: %s", content[:50])
            # 既存のNoteを返す
            for note in self._notes.values():
                if (
                    self._calculate_similarity(content, note.content)
                    >= self._config.dedup_threshold
                ):
                    note.touch()
                    return note

        # ID生成
        note_id = self._generate_id(content)

        # Note作成
        note = KeyNote(
            id=note_id,
            content=content,
            importance=importance,
            category=category,
            source=source,
            metadata=metadata or {},
        )

        # 容量チェック
        if len(self._notes) >= self._config.max_notes:
            self._evict_lowest_priority()

        self._notes[note_id] = note
        self._logger.debug("Note追加: %s (重要度: %s)", content[:50], importance.value)

        return note

    async def extract_and_store(
        self,
        content: str,
        source: str = "",
    ) -> list[KeyNote]:
        """コンテンツから重要情報を抽出して保存.

        Args:
            content: 抽出対象コンテンツ
            source: 抽出元

        Returns:
            抽出・保存されたNoteリスト
        """
        extracted_notes: list[KeyNote] = []

        # ルールベース抽出
        rule_based = self._extract_by_rules(content)
        for info in rule_based:
            note = self.add_note(
                content=info["content"],
                importance=info["importance"],
                category=info["category"],
                source=source,
            )
            extracted_notes.append(note)

        # LLM抽出（設定されている場合）
        if self._llm_extractor and self._config.auto_extract:
            try:
                llm_results = await self._llm_extractor.extract_key_info(content)
                for result in llm_results:
                    importance = NoteImportance(result.get("importance", "medium"))
                    category = NoteCategory(result.get("category", "context"))
                    note = self.add_note(
                        content=result["content"],
                        importance=importance,
                        category=category,
                        source=source,
                        metadata=result.get("metadata", {}),
                    )
                    extracted_notes.append(note)
            except Exception as e:
                self._logger.warning("LLM抽出に失敗: %s", e)

        return extracted_notes

    def get_note(self, note_id: str) -> KeyNote | None:
        """IDでNoteを取得.

        Args:
            note_id: Note ID

        Returns:
            Note、またはNone
        """
        note = self._notes.get(note_id)
        if note:
            note.touch()
        return note

    def get_all_notes(self) -> list[KeyNote]:
        """全Notesを取得.

        Returns:
            全Noteリスト（重要度順）
        """
        notes = list(self._notes.values())
        # 重要度→アクセス回数でソート
        importance_order = {
            NoteImportance.CRITICAL: 0,
            NoteImportance.HIGH: 1,
            NoteImportance.MEDIUM: 2,
            NoteImportance.LOW: 3,
        }
        notes.sort(key=lambda n: (importance_order[n.importance], -n.access_count))
        return notes

    def get_notes_by_importance(
        self,
        min_importance: NoteImportance = NoteImportance.LOW,
    ) -> list[KeyNote]:
        """指定重要度以上のNotesを取得.

        Args:
            min_importance: 最小重要度

        Returns:
            条件に合致するNoteリスト
        """
        importance_order = {
            NoteImportance.CRITICAL: 0,
            NoteImportance.HIGH: 1,
            NoteImportance.MEDIUM: 2,
            NoteImportance.LOW: 3,
        }
        min_order = importance_order[min_importance]

        return [note for note in self.get_all_notes() if importance_order[note.importance] <= min_order]

    def get_notes_by_category(self, category: NoteCategory) -> list[KeyNote]:
        """カテゴリでNotesを取得.

        Args:
            category: カテゴリ

        Returns:
            条件に合致するNoteリスト
        """
        return [note for note in self._notes.values() if note.category == category]

    def to_context_string(
        self,
        max_tokens: int = 500,
        min_importance: NoteImportance = NoteImportance.MEDIUM,
    ) -> str:
        """コンテキスト文字列に変換.

        Args:
            max_tokens: 最大Token数（概算）
            min_importance: 最小重要度

        Returns:
            コンテキスト文字列
        """
        notes = self.get_notes_by_importance(min_importance)
        parts: list[str] = []
        current_length = 0

        for note in notes:
            note_str = f"- [{note.category.value}] {note.content}"
            note_length = len(note_str) // 4  # 概算Token数

            if current_length + note_length > max_tokens:
                break

            parts.append(note_str)
            current_length += note_length

        if not parts:
            return ""

        return "# 重要情報\n" + "\n".join(parts)

    def remove_note(self, note_id: str) -> bool:
        """Noteを削除.

        Args:
            note_id: Note ID

        Returns:
            削除成功したか
        """
        if note_id in self._notes:
            del self._notes[note_id]
            return True
        return False

    def clear(self) -> None:
        """全Notesをクリア."""
        self._notes.clear()

    def _extract_by_rules(self, content: str) -> list[dict[str, Any]]:
        """ルールベースで重要情報を抽出.

        Args:
            content: 対象コンテンツ

        Returns:
            抽出された情報リスト
        """
        results: list[dict[str, Any]] = []

        # 重要度判定
        for importance, patterns in self._IMPORTANCE_PATTERNS.items():
            for pattern in patterns:
                matches = re.finditer(pattern, content, re.IGNORECASE)
                for match in matches:
                    # マッチ周辺のテキストを抽出
                    start = max(0, match.start() - 20)
                    end = min(len(content), match.end() + 50)
                    extracted = content[start:end].strip()

                    # カテゴリ判定
                    category = self._detect_category(extracted)

                    results.append(
                        {
                            "content": extracted,
                            "importance": importance,
                            "category": category,
                        }
                    )

        return results

    def _detect_category(self, content: str) -> NoteCategory:
        """カテゴリを判定.

        Args:
            content: 対象コンテンツ

        Returns:
            判定されたカテゴリ
        """
        for category, patterns in self._CATEGORY_PATTERNS.items():
            for pattern in patterns:
                if re.search(pattern, content, re.IGNORECASE):
                    return category

        return NoteCategory.CONTEXT

    def _is_duplicate(self, content: str) -> bool:
        """重複チェック.

        Args:
            content: チェック対象

        Returns:
            重複しているか
        """
        for note in self._notes.values():
            if self._calculate_similarity(content, note.content) >= self._config.dedup_threshold:
                return True
        return False

    def _calculate_similarity(self, text1: str, text2: str) -> float:
        """テキスト類似度を計算（Jaccard類似度）.

        Args:
            text1: テキスト1
            text2: テキスト2

        Returns:
            類似度（0-1）
        """
        words1 = set(re.findall(r"\w+", text1.lower()))
        words2 = set(re.findall(r"\w+", text2.lower()))

        if not words1 or not words2:
            return 0.0

        intersection = words1 & words2
        union = words1 | words2

        return len(intersection) / len(union)

    def _generate_id(self, content: str) -> str:
        """Note IDを生成.

        Args:
            content: Note内容

        Returns:
            一意ID
        """
        timestamp = datetime.now(UTC).isoformat()
        hash_input = f"{content}:{timestamp}"
        return hashlib.sha256(hash_input.encode()).hexdigest()[:12]

    def _evict_lowest_priority(self) -> None:
        """最低優先度のNoteを削除."""
        if not self._notes:
            return

        # CRITICALは削除しない
        evictable = [note for note in self._notes.values() if note.importance != NoteImportance.CRITICAL]

        if not evictable:
            self._logger.warning("削除可能なNoteがありません")
            return

        # 重要度低→アクセス少→古い順でソート
        importance_order = {
            NoteImportance.CRITICAL: 0,
            NoteImportance.HIGH: 1,
            NoteImportance.MEDIUM: 2,
            NoteImportance.LOW: 3,
        }
        evictable.sort(
            key=lambda n: (
                -importance_order[n.importance],
                n.access_count,
                n.created_at,
            )
        )

        # 最低優先度を削除
        to_remove = evictable[0]
        del self._notes[to_remove.id]
        self._logger.debug("Note削除（容量超過）: %s", to_remove.content[:50])
