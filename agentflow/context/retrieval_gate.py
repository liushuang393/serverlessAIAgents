# -*- coding: utf-8 -*-
"""Retrieval Gate - RAG検索必要性判定.

全てのクエリでRAG検索を実行するのではなく、
必要な場合のみ検索を行うための判定ロジック。

設計原則:
- 軽量判定: LLM呼び出しなしで高速判定
- 多言語対応: 日本語・中国語・英語のキーワード
- コンテキスト考慮: 既存情報との重複チェック
- カスタマイズ可能: ルールの追加・変更が容易

使用例:
    >>> gate = RetrievalGate()
    >>> decision = await gate.should_retrieve(
    ...     query="この文書の内容を教えて",
    ...     context={"topic": "技術文書"},
    ... )
    >>> if decision.should_retrieve:
    ...     results = await rag.search(query)
"""

from __future__ import annotations

import logging
import re
from dataclasses import dataclass, field
from enum import Enum
from typing import Any

_logger = logging.getLogger(__name__)


class RetrievalReason(str, Enum):
    """検索判定理由."""

    # 検索すべき理由
    EXPLICIT_REQUEST = "explicit_request"       # 明示的な検索要求
    FACTUAL_QUESTION = "factual_question"       # 事実に関する質問
    DOMAIN_RELATED = "domain_related"           # ドメイン関連
    REFERENCE_NEEDED = "reference_needed"       # 参照が必要
    UNKNOWN_TOPIC = "unknown_topic"             # 未知のトピック

    # 検索不要の理由
    CASUAL_CHAT = "casual_chat"                 # 雑談
    CONTEXT_SUFFICIENT = "context_sufficient"   # 既存コンテキストで十分
    META_QUESTION = "meta_question"             # メタ質問（システムについて）
    SIMPLE_TASK = "simple_task"                 # 単純タスク（翻訳、計算等）
    RECENT_RETRIEVAL = "recent_retrieval"       # 直近で検索済み


@dataclass
class RetrievalDecision:
    """検索判定結果.

    Attributes:
        should_retrieve: 検索すべきか
        confidence: 判定信頼度（0-1）
        reason: 判定理由
        suggested_query: 推奨検索クエリ（リライト後）
        metadata: 追加情報
    """

    should_retrieve: bool
    confidence: float
    reason: RetrievalReason
    suggested_query: str | None = None
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class GateConfig:
    """判定設定.

    Attributes:
        confidence_threshold: 判定閾値
        enable_query_rewrite: クエリリライト有効化
        recent_retrieval_window: 直近検索ウィンドウ（秒）
        domain_keywords: ドメインキーワード
    """

    confidence_threshold: float = 0.5
    enable_query_rewrite: bool = True
    recent_retrieval_window: int = 60
    domain_keywords: list[str] = field(default_factory=list)


class RetrievalGate:
    """RAG検索必要性判定ゲート.

    全クエリで検索を実行するのは非効率。
    このゲートは軽量なルールベースで検索の必要性を判定する。

    Example:
        >>> gate = RetrievalGate(
        ...     config=GateConfig(domain_keywords=["API", "設計", "仕様"]),
        ... )
        >>> decision = await gate.should_retrieve("このAPIの仕様を教えて")
        >>> print(decision)  # RetrievalDecision(should_retrieve=True, ...)
    """

    # 検索を示唆するキーワード（多言語）
    _RETRIEVAL_KEYWORDS = {
        # 日本語
        "文書", "ドキュメント", "資料", "参照", "調べ", "検索", "探し",
        "仕様", "マニュアル", "ガイド", "手順", "根拠", "出典", "ソース",
        "教えて", "説明", "詳細", "内容", "情報", "確認",
        # 中国語
        "文档", "资料", "参考", "查找", "搜索", "规范", "手册",
        "指南", "依据", "来源", "根据", "告诉", "说明", "详细", "内容",
        # 英語
        "document", "documentation", "reference", "search", "find",
        "specification", "manual", "guide", "procedure", "source",
        "based on", "according to", "explain", "tell me", "information",
    }

    # 雑談を示唆するキーワード
    _CASUAL_KEYWORDS = {
        # 日本語
        "こんにちは", "ありがとう", "さようなら", "おはよう", "お疲れ",
        "どう思う", "感想", "意見", "気持ち", "元気",
        # 中国語
        "你好", "谢谢", "再见", "早上好", "辛苦",
        "怎么看", "感想", "意见", "心情",
        # 英語
        "hello", "hi", "thanks", "thank you", "bye", "goodbye",
        "how are you", "what do you think", "opinion", "feel",
    }

    # メタ質問キーワード
    _META_KEYWORDS = {
        # 日本語
        "あなたは", "君は", "お前は", "何ができる", "機能", "使い方",
        # 中国語
        "你是", "你能", "功能", "怎么用",
        # 英語
        "who are you", "what can you", "how to use", "your function",
    }

    # 単純タスクキーワード
    _SIMPLE_TASK_KEYWORDS = {
        # 日本語
        "翻訳", "計算", "変換", "フォーマット", "整形",
        # 中国語
        "翻译", "计算", "转换", "格式化",
        # 英語
        "translate", "calculate", "convert", "format",
    }

    def __init__(
        self,
        config: GateConfig | None = None,
    ) -> None:
        """初期化.

        Args:
            config: 判定設定
        """
        self._config = config or GateConfig()
        self._recent_queries: list[tuple[float, str]] = []  # (timestamp, query)
        self._logger = logging.getLogger(__name__)

    async def should_retrieve(
        self,
        query: str,
        context: dict[str, Any] | None = None,
    ) -> RetrievalDecision:
        """検索の必要性を判定.

        Args:
            query: ユーザークエリ
            context: 現在のコンテキスト（既存情報含む）

        Returns:
            検索判定結果
        """
        context = context or {}
        query_lower = query.lower()

        # 1. 明示的な検索要求チェック
        explicit_score = self._check_explicit_request(query_lower)
        if explicit_score > 0.5:  # 閾値を下げて検索漏れを防ぐ
            return RetrievalDecision(
                should_retrieve=True,
                confidence=explicit_score,
                reason=RetrievalReason.EXPLICIT_REQUEST,
                suggested_query=self._rewrite_query(query) if self._config.enable_query_rewrite else query,
            )

        # 2. 雑談チェック
        casual_score = self._check_casual_chat(query_lower)
        if casual_score > 0.6:
            return RetrievalDecision(
                should_retrieve=False,
                confidence=casual_score,
                reason=RetrievalReason.CASUAL_CHAT,
            )

        # 3. メタ質問チェック
        meta_score = self._check_meta_question(query_lower)
        if meta_score > 0.6:
            return RetrievalDecision(
                should_retrieve=False,
                confidence=meta_score,
                reason=RetrievalReason.META_QUESTION,
            )

        # 4. 単純タスクチェック
        simple_score = self._check_simple_task(query_lower)
        if simple_score > 0.6:
            return RetrievalDecision(
                should_retrieve=False,
                confidence=simple_score,
                reason=RetrievalReason.SIMPLE_TASK,
            )

        # 5. ドメイン関連チェック
        domain_score = self._check_domain_related(query_lower)
        if domain_score > 0.5:
            return RetrievalDecision(
                should_retrieve=True,
                confidence=domain_score,
                reason=RetrievalReason.DOMAIN_RELATED,
                suggested_query=self._rewrite_query(query) if self._config.enable_query_rewrite else query,
            )

        # 6. 事実質問チェック
        factual_score = self._check_factual_question(query_lower)
        if factual_score > 0.5:
            return RetrievalDecision(
                should_retrieve=True,
                confidence=factual_score,
                reason=RetrievalReason.FACTUAL_QUESTION,
                suggested_query=self._rewrite_query(query) if self._config.enable_query_rewrite else query,
            )

        # 7. コンテキスト充足チェック
        context_score = self._check_context_sufficient(query_lower, context)
        if context_score > 0.6:
            return RetrievalDecision(
                should_retrieve=False,
                confidence=context_score,
                reason=RetrievalReason.CONTEXT_SUFFICIENT,
            )

        # デフォルト: 不明な場合は検索しない
        return RetrievalDecision(
            should_retrieve=False,
            confidence=0.5,
            reason=RetrievalReason.CONTEXT_SUFFICIENT,
            metadata={"note": "判定不明、デフォルトで検索スキップ"},
        )

    def _check_explicit_request(self, query: str) -> float:
        """明示的な検索要求をチェック.

        Args:
            query: クエリ（小文字化済み）

        Returns:
            スコア（0-1）
        """
        matches = sum(1 for kw in self._RETRIEVAL_KEYWORDS if kw.lower() in query)
        # 1マッチで0.4、2マッチで0.65、3マッチ以上で0.8+
        if matches == 0:
            return 0.0
        if matches == 1:
            return 0.55
        if matches == 2:
            return 0.75
        return min(1.0, 0.8 + (matches - 3) * 0.1)

    def _check_casual_chat(self, query: str) -> float:
        """雑談をチェック.

        Args:
            query: クエリ（小文字化済み）

        Returns:
            スコア（0-1）
        """
        matches = sum(1 for kw in self._CASUAL_KEYWORDS if kw.lower() in query)

        # 短いクエリは雑談の可能性が高い
        length_factor = 0.2 if len(query) < 20 else 0.0

        return min(1.0, matches * 0.4 + length_factor)

    def _check_meta_question(self, query: str) -> float:
        """メタ質問をチェック.

        Args:
            query: クエリ（小文字化済み）

        Returns:
            スコア（0-1）
        """
        matches = sum(1 for kw in self._META_KEYWORDS if kw.lower() in query)
        return min(1.0, matches * 0.5)

    def _check_simple_task(self, query: str) -> float:
        """単純タスクをチェック.

        Args:
            query: クエリ（小文字化済み）

        Returns:
            スコア（0-1）
        """
        matches = sum(1 for kw in self._SIMPLE_TASK_KEYWORDS if kw.lower() in query)
        return min(1.0, matches * 0.5)

    def _check_domain_related(self, query: str) -> float:
        """ドメイン関連をチェック.

        Args:
            query: クエリ（小文字化済み）

        Returns:
            スコア（0-1）
        """
        if not self._config.domain_keywords:
            return 0.0

        matches = sum(
            1 for kw in self._config.domain_keywords if kw.lower() in query
        )
        return min(1.0, matches * 0.4)

    def _check_factual_question(self, query: str) -> float:
        """事実質問をチェック.

        Args:
            query: クエリ（小文字化済み）

        Returns:
            スコア（0-1）
        """
        # 疑問詞パターン（多言語）
        question_patterns = [
            # 日本語
            r"(何|なに|いつ|どこ|だれ|誰|どう|なぜ|どの|どれ|いくつ|いくら)",
            r"(ですか|ますか|でしょうか|だろうか)",
            # 中国語
            r"(什么|什麽|为什么|怎么|哪|哪里|哪个|谁|几|多少)",
            r"(吗|呢|？)",
            # 英語
            r"\b(what|when|where|who|why|how|which)\b",
            r"\?$",
        ]

        score = 0.0
        for pattern in question_patterns:
            if re.search(pattern, query, re.IGNORECASE):
                score += 0.2

        return min(1.0, score)

    def _check_context_sufficient(
        self,
        query: str,
        context: dict[str, Any],
    ) -> float:
        """コンテキストが十分かチェック.

        Args:
            query: クエリ（小文字化済み）
            context: 現在のコンテキスト

        Returns:
            スコア（0-1）
        """
        # コンテキストに関連情報が既にある場合
        existing_info = context.get("existing_info", "")
        if existing_info:
            # クエリのキーワードがコンテキストに含まれているか
            query_words = set(re.findall(r"\w+", query))
            context_words = set(re.findall(r"\w+", str(existing_info).lower()))

            overlap = len(query_words & context_words)
            if overlap > 2:
                return 0.7

        # RAG結果が既にコンテキストにある場合
        if context.get("rag_results"):
            return 0.8

        return 0.0

    def _rewrite_query(self, query: str) -> str:
        """クエリをリライト.

        検索に適した形式に変換する。

        Args:
            query: 元のクエリ

        Returns:
            リライト後のクエリ
        """
        # 不要な敬語・フィラーを除去
        removals = [
            r"(してください|お願いします|教えてください|教えて)",
            r"(请|请问|麻烦)",
            r"(please|could you|can you|would you)",
        ]

        result = query
        for pattern in removals:
            result = re.sub(pattern, "", result, flags=re.IGNORECASE)

        # 空白の正規化
        result = re.sub(r"\s+", " ", result).strip()

        return result if result else query

    def add_domain_keywords(self, keywords: list[str]) -> None:
        """ドメインキーワードを追加.

        Args:
            keywords: 追加するキーワード
        """
        self._config.domain_keywords.extend(keywords)

    def set_confidence_threshold(self, threshold: float) -> None:
        """判定閾値を設定.

        Args:
            threshold: 新しい閾値
        """
        self._config.confidence_threshold = threshold
