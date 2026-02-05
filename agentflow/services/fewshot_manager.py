# -*- coding: utf-8 -*-
"""Few-shot Example Manager - NL2SQL 動的示例選択.

学術研究に基づく Few-shot 示例管理:
- BM25 類似度マッチング（ベクトルDB不要）
- クエリパターン分類
- 動的示例選択

使用例:
    >>> from agentflow.services.fewshot_manager import FewshotManager
    >>>
    >>> manager = FewshotManager()
    >>> manager.add_example(
    ...     query="今月の売上TOP10",
    ...     sql="SELECT ... ORDER BY amount DESC LIMIT 10",
    ...     pattern="ranking",
    ... )
    >>> examples = manager.get_similar_examples("先月の売上TOP5", k=3)
"""

from __future__ import annotations

import logging
import math
import re
from collections import Counter
from dataclasses import dataclass, field
from typing import Any

logger = logging.getLogger(__name__)


@dataclass
class FewshotExample:
    """Few-shot 示例."""

    query: str
    sql: str
    pattern: str = "general"
    description: str = ""
    difficulty: str = "medium"
    tables: list[str] = field(default_factory=list)
    keywords: list[str] = field(default_factory=list)
    metadata: dict[str, Any] = field(default_factory=dict)


@dataclass
class FewshotManagerConfig:
    """Few-shot Manager 設定."""

    default_k: int = 3
    max_examples: int = 1000
    min_similarity: float = 0.1
    use_pattern_filter: bool = True
    bm25_k1: float = 1.5
    bm25_b: float = 0.75


class BM25:
    """BM25 類似度計算.

    向量DBを使わずにテキスト類似度を計算。
    """

    def __init__(self, k1: float = 1.5, b: float = 0.75) -> None:
        """初期化.

        Args:
            k1: 用語頻度の飽和パラメータ
            b: 文書長正規化パラメータ
        """
        self.k1 = k1
        self.b = b
        self.corpus: list[list[str]] = []
        self.doc_freqs: Counter[str] = Counter()
        self.avg_doc_len: float = 0.0
        self.n_docs: int = 0

    def add_document(self, tokens: list[str]) -> int:
        """文書を追加.

        Args:
            tokens: トークンリスト

        Returns:
            文書インデックス
        """
        self.corpus.append(tokens)
        self.doc_freqs.update(set(tokens))
        self.n_docs += 1
        total_len = sum(len(doc) for doc in self.corpus)
        self.avg_doc_len = total_len / self.n_docs
        return len(self.corpus) - 1

    def score(self, query_tokens: list[str], doc_idx: int) -> float:
        """BM25スコアを計算.

        Args:
            query_tokens: クエリトークン
            doc_idx: 文書インデックス

        Returns:
            BM25スコア
        """
        if doc_idx >= len(self.corpus):
            return 0.0

        doc = self.corpus[doc_idx]
        doc_len = len(doc)
        doc_counter = Counter(doc)

        score = 0.0
        for term in query_tokens:
            if term not in doc_counter:
                continue

            tf = doc_counter[term]
            df = self.doc_freqs.get(term, 0)

            idf = math.log((self.n_docs - df + 0.5) / (df + 0.5) + 1.0)
            numerator = tf * (self.k1 + 1)
            denominator = tf + self.k1 * (1 - self.b + self.b * doc_len / self.avg_doc_len)

            score += idf * (numerator / denominator)

        return score

    def search(self, query_tokens: list[str], k: int = 5) -> list[tuple[int, float]]:
        """類似文書を検索.

        Args:
            query_tokens: クエリトークン
            k: 返す文書数

        Returns:
            [(doc_idx, score), ...]
        """
        scores = [
            (i, self.score(query_tokens, i))
            for i in range(len(self.corpus))
        ]
        scores.sort(key=lambda x: x[1], reverse=True)
        return scores[:k]


class FewshotManager:
    """Few-shot Example Manager.

    NL2SQL の精度向上のための動的示例選択。
    学術研究（DAIL-SQL等）に基づく設計。
    """

    # デフォルト示例
    DEFAULT_EXAMPLES = [
        FewshotExample(
            query="今月の売上TOP10",
            sql="""SELECT customer_name, SUM(amount) as total_sales
FROM sales
WHERE DATE_TRUNC('month', order_date) = DATE_TRUNC('month', CURRENT_DATE)
GROUP BY customer_name
ORDER BY total_sales DESC
LIMIT 10""",
            pattern="ranking",
            difficulty="easy",
            tables=["sales"],
            keywords=["TOP", "売上", "ランキング"],
        ),
        FewshotExample(
            query="地域別の注文数を教えて",
            sql="""SELECT region, COUNT(*) as order_count
FROM orders o
JOIN customers c ON o.customer_id = c.id
GROUP BY region
ORDER BY order_count DESC""",
            pattern="aggregation",
            difficulty="medium",
            tables=["orders", "customers"],
            keywords=["地域", "注文", "集計"],
        ),
        FewshotExample(
            query="先月と今月の売上比較",
            sql="""SELECT
  DATE_TRUNC('month', order_date) as month,
  SUM(amount) as total_sales
FROM sales
WHERE order_date >= DATE_TRUNC('month', CURRENT_DATE - INTERVAL '1 month')
GROUP BY DATE_TRUNC('month', order_date)
ORDER BY month""",
            pattern="comparison",
            difficulty="medium",
            tables=["sales"],
            keywords=["比較", "先月", "今月"],
        ),
        FewshotExample(
            query="売上が100万円以上の顧客一覧",
            sql="""SELECT c.name, SUM(s.amount) as total_sales
FROM customers c
JOIN sales s ON c.id = s.customer_id
GROUP BY c.id, c.name
HAVING SUM(s.amount) >= 1000000
ORDER BY total_sales DESC""",
            pattern="filter",
            difficulty="medium",
            tables=["customers", "sales"],
            keywords=["以上", "顧客", "フィルタ"],
        ),
        FewshotExample(
            query="カテゴリ別の平均単価",
            sql="""SELECT p.category, AVG(s.amount / s.quantity) as avg_unit_price
FROM sales s
JOIN products p ON s.product_id = p.id
GROUP BY p.category
ORDER BY avg_unit_price DESC""",
            pattern="aggregation",
            difficulty="medium",
            tables=["sales", "products"],
            keywords=["平均", "カテゴリ", "単価"],
        ),
    ]

    # クエリパターン分類
    PATTERN_KEYWORDS = {
        "ranking": ["TOP", "ランキング", "上位", "下位", "ワースト"],
        "aggregation": ["合計", "平均", "最大", "最小", "件数", "カウント"],
        "comparison": ["比較", "前月比", "前年比", "推移", "変化"],
        "filter": ["以上", "以下", "超える", "未満", "含む", "除く"],
        "join": ["結合", "関連", "紐づ", "連携"],
        "time_series": ["日別", "月別", "推移", "トレンド"],
    }

    def __init__(self, config: FewshotManagerConfig | None = None) -> None:
        """初期化.

        Args:
            config: 設定
        """
        self._config = config or FewshotManagerConfig()
        self._examples: list[FewshotExample] = []
        self._bm25 = BM25(k1=self._config.bm25_k1, b=self._config.bm25_b)
        self._logger = logging.getLogger(__name__)

        # デフォルト示例を登録
        for example in self.DEFAULT_EXAMPLES:
            self.add_example(example)

    def add_example(self, example: FewshotExample) -> int:
        """示例を追加.

        Args:
            example: Few-shot示例

        Returns:
            示例インデックス
        """
        if len(self._examples) >= self._config.max_examples:
            self._logger.warning("Max examples reached, skipping")
            return -1

        tokens = self._tokenize(example.query)
        self._bm25.add_document(tokens)
        self._examples.append(example)

        return len(self._examples) - 1

    def get_similar_examples(
        self,
        query: str,
        k: int | None = None,
        pattern_filter: str | None = None,
    ) -> list[FewshotExample]:
        """類似示例を取得.

        Args:
            query: クエリ
            k: 返す示例数
            pattern_filter: パターンフィルタ

        Returns:
            類似示例リスト
        """
        k = k or self._config.default_k
        query_tokens = self._tokenize(query)

        # BM25検索
        results = self._bm25.search(query_tokens, k=k * 2)

        # パターンフィルタ
        detected_pattern = self._detect_pattern(query) if self._config.use_pattern_filter else None

        selected = []
        for idx, score in results:
            if score < self._config.min_similarity:
                continue

            example = self._examples[idx]

            # パターンフィルタ適用
            if pattern_filter and example.pattern != pattern_filter:
                continue
            if detected_pattern and example.pattern != detected_pattern:
                # パターンマッチしなくてもスコアが高ければ含める
                if score < self._config.min_similarity * 2:
                    continue

            selected.append(example)
            if len(selected) >= k:
                break

        return selected

    def format_examples_prompt(self, examples: list[FewshotExample]) -> str:
        """示例をプロンプト形式にフォーマット.

        Args:
            examples: 示例リスト

        Returns:
            フォーマット済みプロンプト
        """
        lines = ["## Few-shot Examples"]
        for i, ex in enumerate(examples, 1):
            lines.append(f"\n### Example {i}")
            lines.append(f"Question: {ex.query}")
            lines.append(f"SQL:\n```sql\n{ex.sql}\n```")
        return "\n".join(lines)

    def _tokenize(self, text: str) -> list[str]:
        """テキストをトークン化.

        Args:
            text: テキスト

        Returns:
            トークンリスト
        """
        # 簡易トークン化（日本語対応）
        text = text.lower()
        # 英数字とカタカナを抽出
        tokens = re.findall(r'[a-z0-9]+|[ぁ-んァ-ヶ一-龥]+', text)
        return tokens

    def _detect_pattern(self, query: str) -> str | None:
        """クエリパターンを検出.

        Args:
            query: クエリ

        Returns:
            パターン名、または None
        """
        query_lower = query.lower()

        best_pattern = None
        best_count = 0

        for pattern, keywords in self.PATTERN_KEYWORDS.items():
            count = sum(1 for kw in keywords if kw.lower() in query_lower)
            if count > best_count:
                best_count = count
                best_pattern = pattern

        return best_pattern if best_count > 0 else None

    def list_examples(self, pattern: str | None = None) -> list[FewshotExample]:
        """示例一覧を取得.

        Args:
            pattern: パターンフィルタ

        Returns:
            示例リスト
        """
        if pattern:
            return [ex for ex in self._examples if ex.pattern == pattern]
        return self._examples.copy()

    def clear(self) -> None:
        """全示例をクリア."""
        self._examples.clear()
        self._bm25 = BM25(k1=self._config.bm25_k1, b=self._config.bm25_b)


__all__ = [
    "FewshotManager",
    "FewshotManagerConfig",
    "FewshotExample",
    "BM25",
]

