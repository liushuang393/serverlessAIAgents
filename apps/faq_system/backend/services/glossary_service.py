"""術語辞書サービス（Glossary Service）.

術語の管理と検索精度向上を支援。

機能:
- 同義語辞書管理
- 片仮名/略称対応
- 部門別ローカル用語
- 検索クエリ拡張

使用例:
    >>> from apps.faq_system.backend.services import GlossaryService
    >>>
    >>> service = GlossaryService()
    >>> expanded = service.expand_query("有休申請")
    >>> print(expanded)  # ["有休申請", "年次有給休暇申請", "休暇申請", ...]
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from datetime import datetime
from enum import Enum
from typing import Any


logger = logging.getLogger(__name__)


class TermType(str, Enum):
    """用語タイプ."""

    OFFICIAL = "official"  # 正式名称
    ABBREVIATION = "abbreviation"  # 略称
    KATAKANA = "katakana"  # カタカナ
    LOCAL = "local"  # 部門ローカル
    ENGLISH = "english"  # 英語
    SYNONYM = "synonym"  # 同義語


@dataclass
class Term:
    """用語定義.

    Attributes:
        term_id: 用語ID
        canonical: 正式名称
        term_type: 用語タイプ
        value: 用語
        category: カテゴリ
        department: 部門（ローカル用語の場合）
        description: 説明
    """

    term_id: str
    canonical: str  # 正式名称
    term_type: TermType = TermType.OFFICIAL
    value: str = ""  # 略称やカタカナ等
    category: str = ""
    department: str = ""
    description: str = ""
    created_at: datetime = field(default_factory=datetime.now)
    updated_at: datetime = field(default_factory=datetime.now)


@dataclass
class TermEntry:
    """術語エントリー.

    Attributes:
        canonical: 正式名称
        synonyms: 同義語リスト
        abbreviations: 略称リスト
        katakana: カタカナ表記リスト
        english: 英語表記リスト
        local_terms: 部門ローカル用語
        category: カテゴリ
        description: 説明
    """

    canonical: str
    synonyms: list[str] = field(default_factory=list)
    abbreviations: list[str] = field(default_factory=list)
    katakana: list[str] = field(default_factory=list)
    english: list[str] = field(default_factory=list)
    local_terms: dict[str, list[str]] = field(default_factory=dict)  # {部門: [用語]}
    category: str = ""
    description: str = ""


@dataclass
class GlossaryConfig:
    """術語辞書設定."""

    # クエリ拡張
    enable_synonym_expansion: bool = True
    enable_abbreviation_expansion: bool = True
    enable_katakana_expansion: bool = True
    max_expansions: int = 10

    # 正規化
    normalize_whitespace: bool = True
    normalize_case: bool = True

    # 部門スコープ
    use_department_scope: bool = True


class GlossaryService:
    """術語辞書サービス.

    術語の管理と検索クエリ拡張。

    Example:
        >>> service = GlossaryService()
        >>>
        >>> # クエリ拡張
        >>> expanded = service.expand_query("有休申請")
        >>> # ["有休申請", "年次有給休暇申請", "休暇申請", ...]
        >>>
        >>> # 正規化
        >>> normalized = service.normalize("ＰＣ")
        >>> # "PC"
    """

    # デフォルト術語辞書
    DEFAULT_ENTRIES = [
        TermEntry(
            canonical="年次有給休暇",
            synonyms=["年休", "有給休暇", "有給"],
            abbreviations=["有休", "年休"],
            katakana=["ネンキュウ", "ユウキュウ"],
            english=["annual leave", "paid leave"],
            category="人事",
            description="労働基準法に基づく年次有給休暇",
        ),
        TermEntry(
            canonical="就業規則",
            synonyms=["社則", "勤務規則"],
            abbreviations=["就規"],
            category="人事",
            description="会社の就業に関する規則",
        ),
        TermEntry(
            canonical="経費精算",
            synonyms=["経費申請", "立替精算"],
            abbreviations=["経精", "経費"],
            katakana=["ケイヒセイサン"],
            english=["expense reimbursement"],
            category="経理",
        ),
        TermEntry(
            canonical="情報セキュリティ",
            synonyms=["セキュリティ", "情報安全"],
            abbreviations=["情セキ", "IS"],
            katakana=["セキュリティ", "インフォメーションセキュリティ"],
            english=["information security", "InfoSec"],
            category="IT",
        ),
        TermEntry(
            canonical="パーソナルコンピュータ",
            synonyms=["パソコン", "コンピュータ"],
            abbreviations=["PC"],
            katakana=["パソコン", "ピーシー"],
            english=["personal computer", "PC"],
            category="IT",
        ),
        TermEntry(
            canonical="システム開発",
            synonyms=["開発", "ソフトウェア開発"],
            abbreviations=["シス開", "開発"],
            katakana=["システムカイハツ"],
            english=["system development"],
            category="IT",
        ),
        TermEntry(
            canonical="データベース",
            synonyms=["DB"],
            abbreviations=["DB", "ディービー"],
            katakana=["データベース", "ディービー"],
            english=["database", "DB"],
            category="IT",
        ),
        TermEntry(
            canonical="売上高",
            synonyms=["売上", "売上金額", "セールス"],
            abbreviations=["売上"],
            english=["revenue", "sales"],
            category="経営",
            description="税抜売上金額",
        ),
        TermEntry(
            canonical="総取引額",
            synonyms=["取引額", "流通総額"],
            abbreviations=["GMV"],
            english=["Gross Merchandise Value", "GMV"],
            category="経営",
            description="税込総取引額（キャンセル含む）",
        ),
    ]

    def __init__(
        self,
        config: GlossaryConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 設定
        """
        self._config = config or GlossaryConfig()
        self._entries: dict[str, TermEntry] = {}
        self._reverse_index: dict[str, str] = {}  # 用語 -> 正式名称
        self._logger = logging.getLogger(__name__)

        # デフォルト術語をロード
        for entry in self.DEFAULT_ENTRIES:
            self.add_entry(entry)

    def add_entry(self, entry: TermEntry) -> None:
        """術語エントリーを追加.

        Args:
            entry: 術語エントリー
        """
        self._entries[entry.canonical] = entry

        # 逆引きインデックスを構築
        all_terms = (
            [entry.canonical, *entry.synonyms, *entry.abbreviations, *entry.katakana, *entry.english]
        )

        for term in all_terms:
            if term:
                self._reverse_index[term.lower()] = entry.canonical

        # 部門ローカル用語
        for _dept, terms in entry.local_terms.items():
            for term in terms:
                if term:
                    self._reverse_index[term.lower()] = entry.canonical

    def get_entry(self, term: str) -> TermEntry | None:
        """術語エントリーを取得.

        Args:
            term: 用語

        Returns:
            術語エントリー、または None
        """
        term_lower = term.lower()

        # 正式名称で検索
        if term_lower in [e.canonical.lower() for e in self._entries.values()]:
            return self._entries.get(term)

        # 逆引きインデックスで検索
        canonical = self._reverse_index.get(term_lower)
        if canonical:
            return self._entries.get(canonical)

        return None

    def expand_query(
        self,
        query: str,
        department: str | None = None,
    ) -> list[str]:
        """クエリを拡張.

        Args:
            query: 検索クエリ
            department: 部門（スコープ用）

        Returns:
            拡張されたクエリリスト
        """
        expanded = [query]

        # 単語を抽出
        words = self._tokenize(query)

        for word in words:
            entry = self.get_entry(word)
            if not entry:
                continue

            # 同義語拡張
            if self._config.enable_synonym_expansion:
                for synonym in entry.synonyms:
                    expanded_query = query.replace(word, synonym)
                    if expanded_query not in expanded:
                        expanded.append(expanded_query)

            # 略称拡張
            if self._config.enable_abbreviation_expansion:
                for abbrev in entry.abbreviations:
                    expanded_query = query.replace(word, abbrev)
                    if expanded_query not in expanded:
                        expanded.append(expanded_query)

            # カタカナ拡張
            if self._config.enable_katakana_expansion:
                for katakana in entry.katakana:
                    expanded_query = query.replace(word, katakana)
                    if expanded_query not in expanded:
                        expanded.append(expanded_query)

            # 部門ローカル用語
            if department and department in entry.local_terms:
                for local in entry.local_terms[department]:
                    expanded_query = query.replace(word, local)
                    if expanded_query not in expanded:
                        expanded.append(expanded_query)

        # 正式名称を追加
        for word in words:
            canonical = self._reverse_index.get(word.lower())
            if canonical:
                expanded_query = query.replace(word, canonical)
                if expanded_query not in expanded:
                    expanded.append(expanded_query)

        return expanded[:self._config.max_expansions]

    def normalize(self, text: str) -> str:
        """テキストを正規化.

        Args:
            text: テキスト

        Returns:
            正規化されたテキスト
        """
        result = text

        # 全角英数字を半角に
        result = self._to_halfwidth(result)

        # 空白の正規化
        if self._config.normalize_whitespace:
            result = " ".join(result.split())

        # 大文字小文字の正規化
        if self._config.normalize_case:
            # 略称は大文字を維持
            pass  # 実装によって調整

        return result

    def get_canonical(self, term: str) -> str | None:
        """正式名称を取得.

        Args:
            term: 用語

        Returns:
            正式名称、または None
        """
        return self._reverse_index.get(term.lower())

    def suggest_corrections(self, term: str) -> list[str]:
        """修正候補を提案.

        Args:
            term: 用語

        Returns:
            修正候補リスト
        """
        suggestions = []
        term_lower = term.lower()

        # 部分一致で検索
        for registered_term, canonical in self._reverse_index.items():
            if (
                term_lower in registered_term
                or registered_term in term_lower
            ) and canonical not in suggestions:
                suggestions.append(canonical)

        return suggestions[:5]

    def list_entries(
        self, category: str | None = None
    ) -> list[TermEntry]:
        """術語エントリーを一覧.

        Args:
            category: カテゴリ（フィルタ）

        Returns:
            術語エントリーリスト
        """
        entries = list(self._entries.values())

        if category:
            entries = [e for e in entries if e.category == category]

        return entries

    def get_categories(self) -> list[str]:
        """カテゴリ一覧を取得.

        Returns:
            カテゴリリスト
        """
        return list({e.category for e in self._entries.values() if e.category})

    def export_dictionary(self) -> list[dict[str, Any]]:
        """辞書をエクスポート.

        Returns:
            辞書データ
        """
        return [
            {
                "canonical": entry.canonical,
                "synonyms": entry.synonyms,
                "abbreviations": entry.abbreviations,
                "katakana": entry.katakana,
                "english": entry.english,
                "category": entry.category,
                "description": entry.description,
            }
            for entry in self._entries.values()
        ]

    def _tokenize(self, text: str) -> list[str]:
        """テキストをトークン化.

        Args:
            text: テキスト

        Returns:
            トークンリスト
        """
        # 簡易トークン化（実際は形態素解析を使用）
        # スペースと記号で分割
        tokens = re.split(r"[\s\u3000,、。．.]+", text)
        return [t for t in tokens if t]

    def _to_halfwidth(self, text: str) -> str:
        """全角英数字を半角に変換.

        Args:
            text: テキスト

        Returns:
            変換後テキスト
        """
        # 全角英数字 -> 半角
        result = ""
        for char in text:
            code = ord(char)
            # 全角英数字 (０-９, Ａ-Ｚ, ａ-ｚ)
            if 0xFF10 <= code <= 0xFF19:  # ０-９
                result += chr(code - 0xFF10 + ord("0"))
            elif 0xFF21 <= code <= 0xFF3A:  # Ａ-Ｚ
                result += chr(code - 0xFF21 + ord("A"))
            elif 0xFF41 <= code <= 0xFF5A:  # ａ-ｚ
                result += chr(code - 0xFF41 + ord("a"))
            elif code == 0x3000:  # 全角スペース
                result += " "
            else:
                result += char

        return result


__all__ = [
    "GlossaryConfig",
    "GlossaryService",
    "Term",
    "TermEntry",
    "TermType",
]
