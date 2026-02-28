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
