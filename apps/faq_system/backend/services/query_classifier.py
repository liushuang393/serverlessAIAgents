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

    _SQL_KEYWORDS: frozenset[str] = frozenset(
        [
            # 日本語
            "売上",
            "収入",
            "数量",
            "統計",
            "レポート",
            "top",
            "ランキング",
            "トレンド",
            "比較",
            "金額",
            "注文",
            "顧客数",
            "件数",
            "合計",
            "平均",
            "月別",
            "年別",
            "日別",
            # 中国語（既存互換）
            "销售",
            "收入",
            "数量",
            "统计",
            "报表",
            "排名",
            "趋势",
            "对比",
            "同比",
            "环比",
            "金额",
            "订单",
            "客户数",
            # 英語
            "revenue",
            "sales",
            "count",
            "report",
            "ranking",
            "trend",
            "comparison",
            "total",
            "average",
            "monthly",
            "yearly",
        ]
    )

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
