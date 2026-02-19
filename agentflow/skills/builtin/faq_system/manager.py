"""FAQマネージャー.

知識ベースの管理と検索を提供。
"""

from __future__ import annotations

import logging
import re
import uuid
from dataclasses import dataclass, field
from datetime import datetime
from typing import Any


logger = logging.getLogger(__name__)


@dataclass
class FAQEntry:
    """FAQ エントリー."""

    id: str = field(default_factory=lambda: str(uuid.uuid4())[:8])
    question: str = ""
    answer: str = ""
    category: str = ""
    tags: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)
    view_count: int = 0
    helpful_count: int = 0

    def to_dict(self) -> dict[str, Any]:
        """辞書に変換."""
        return {
            "id": self.id,
            "question": self.question,
            "answer": self.answer,
            "category": self.category,
            "tags": self.tags,
            "metadata": self.metadata,
            "created_at": self.created_at.isoformat(),
            "updated_at": self.updated_at.isoformat(),
            "view_count": self.view_count,
            "helpful_count": self.helpful_count,
        }


@dataclass
class SearchResult:
    """検索結果."""

    entry: FAQEntry
    score: float
    matched_terms: list[str] = field(default_factory=list)


class FAQManager:
    """FAQマネージャー.

    FAQ知識ベースの管理、検索、回答生成を行う。

    Features:
    - FAQ追加・編集・削除
    - キーワード検索
    - カテゴリ・タグフィルタ
    - LLMによる回答生成（オプション）

    Example:
        >>> faq = FAQManager()
        >>> await faq.add_question("How to reset password?", "Go to settings...")
        >>> results = await faq.search("password")
    """

    def __init__(self, llm_client: Any = None) -> None:
        """初期化.

        Args:
            llm_client: LLMクライアント（回答生成用）
        """
        self._llm = llm_client
        self._entries: dict[str, FAQEntry] = {}
        self._logger = logging.getLogger(__name__)

    async def add_question(
        self,
        question: str,
        answer: str,
        category: str = "",
        tags: list[str] | None = None,
        metadata: dict[str, Any] | None = None,
    ) -> FAQEntry:
        """FAQを追加.

        Args:
            question: 質問
            answer: 回答
            category: カテゴリ
            tags: タグリスト
            metadata: メタデータ

        Returns:
            作成されたFAQエントリー
        """
        entry = FAQEntry(
            question=question,
            answer=answer,
            category=category,
            tags=tags or [],
            metadata=metadata or {},
        )
        self._entries[entry.id] = entry
        self._logger.info(f"FAQ追加: {entry.id}")
        return entry

    async def update_question(
        self,
        entry_id: str,
        question: str | None = None,
        answer: str | None = None,
        category: str | None = None,
        tags: list[str] | None = None,
    ) -> FAQEntry | None:
        """FAQを更新.

        Args:
            entry_id: エントリーID
            question: 新しい質問（Noneで変更なし）
            answer: 新しい回答（Noneで変更なし）
            category: 新しいカテゴリ（Noneで変更なし）
            tags: 新しいタグ（Noneで変更なし）

        Returns:
            更新されたエントリー（存在しない場合はNone）
        """
        entry = self._entries.get(entry_id)
        if not entry:
            return None

        if question is not None:
            entry.question = question
        if answer is not None:
            entry.answer = answer
        if category is not None:
            entry.category = category
        if tags is not None:
            entry.tags = tags

        entry.updated_at = datetime.now()
        return entry

    async def delete_question(self, entry_id: str) -> bool:
        """FAQを削除.

        Args:
            entry_id: エントリーID

        Returns:
            削除成功の場合True
        """
        if entry_id in self._entries:
            del self._entries[entry_id]
            return True
        return False

    async def get_question(self, entry_id: str) -> FAQEntry | None:
        """FAQを取得.

        Args:
            entry_id: エントリーID

        Returns:
            エントリー（存在しない場合はNone）
        """
        entry = self._entries.get(entry_id)
        if entry:
            entry.view_count += 1
        return entry

    async def search(
        self,
        query: str,
        category: str | None = None,
        tags: list[str] | None = None,
        limit: int = 10,
    ) -> list[SearchResult]:
        """FAQを検索.

        Args:
            query: 検索クエリ
            category: カテゴリフィルタ
            tags: タグフィルタ
            limit: 最大結果数

        Returns:
            検索結果リスト（スコア降順）
        """
        results: list[SearchResult] = []
        query_terms = self._tokenize(query.lower())

        for entry in self._entries.values():
            # フィルタ
            if category and entry.category != category:
                continue
            if tags and not any(t in entry.tags for t in tags):
                continue

            # スコア計算
            score, matched = self._calculate_score(entry, query_terms)
            if score > 0:
                results.append(SearchResult(entry=entry, score=score, matched_terms=matched))

        # スコア降順でソート
        results.sort(key=lambda r: -r.score)
        return results[:limit]

    def _tokenize(self, text: str) -> list[str]:
        """テキストをトークン化."""
        # 簡易的な単語分割
        return re.findall(r"\w+", text.lower())

    def _calculate_score(
        self,
        entry: FAQEntry,
        query_terms: list[str],
    ) -> tuple[float, list[str]]:
        """検索スコアを計算."""
        entry_text = f"{entry.question} {entry.answer} {' '.join(entry.tags)}".lower()
        entry_terms = self._tokenize(entry_text)

        matched: list[str] = []
        score = 0.0

        for term in query_terms:
            if term in entry_terms:
                matched.append(term)
                # 質問に含まれる場合は高スコア
                if term in entry.question.lower():
                    score += 2.0
                else:
                    score += 1.0

        # 完全一致ボーナス
        if entry.question.lower() == " ".join(query_terms):
            score += 10.0

        return score, matched

    async def generate_answer(
        self,
        query: str,
        use_knowledge_base: bool = True,
    ) -> str:
        """回答を生成.

        LLMを使用して、知識ベースを参照しながら回答を生成。

        Args:
            query: 質問
            use_knowledge_base: 知識ベースを参照するか

        Returns:
            生成された回答
        """
        context = ""

        if use_knowledge_base:
            # 関連FAQを検索
            results = await self.search(query, limit=3)
            if results:
                context = "\n".join(
                    [f"Q: {r.entry.question}\nA: {r.entry.answer}" for r in results]
                )

        if not self._llm:
            # LLMなしの場合は最も関連性の高いFAQの回答を返す
            if results:
                return results[0].entry.answer
            return "申し訳ございません。該当する情報が見つかりませんでした。"

        # LLMで回答生成
        prompt = f"""以下の情報を参考に、質問に回答してください。

参考情報:
{context}

質問: {query}

回答:"""

        try:
            response = await self._llm.chat(
                [
                    {"role": "system", "content": "あなたはFAQサポートアシスタントです。"},
                    {"role": "user", "content": prompt},
                ]
            )
            return response.content if hasattr(response, "content") else str(response)
        except Exception as e:
            self._logger.exception(f"回答生成エラー: {e}")
            if results:
                return results[0].entry.answer
            return "申し訳ございません。回答を生成できませんでした。"

    async def mark_helpful(self, entry_id: str, helpful: bool = True) -> bool:
        """役立ったマークを付ける.

        Args:
            entry_id: エントリーID
            helpful: 役立ったかどうか

        Returns:
            成功の場合True
        """
        entry = self._entries.get(entry_id)
        if not entry:
            return False

        if helpful:
            entry.helpful_count += 1
        return True

    def list_categories(self) -> list[str]:
        """カテゴリ一覧を取得."""
        return list({e.category for e in self._entries.values() if e.category})

    def list_tags(self) -> list[str]:
        """タグ一覧を取得."""
        tags: set[str] = set()
        for entry in self._entries.values():
            tags.update(entry.tags)
        return list(tags)

    def get_stats(self) -> dict[str, Any]:
        """統計情報を取得."""
        return {
            "total_entries": len(self._entries),
            "categories": self.list_categories(),
            "tags": self.list_tags(),
            "total_views": sum(e.view_count for e in self._entries.values()),
            "total_helpful": sum(e.helpful_count for e in self._entries.values()),
        }
