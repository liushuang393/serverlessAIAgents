# apps/faq_system/tests/test_classifier_delegation.py
"""_classify_query が統一 QueryClassifier に委譲することを確認するテスト."""

from apps.faq_system.backend.agents.enhanced_faq_agent import EnhancedFAQAgent
from apps.faq_system.backend.services.faq_service import FAQService, QueryType


class TestFAQServiceClassifyDelegation:
    def setup_method(self) -> None:
        self.service = FAQService()

    def test_faq_service_classify_delegates_sql_keywords(self) -> None:
        result = self.service._classify_query("売上 収入 統計")
        assert result == QueryType.SQL

    def test_faq_service_classify_delegates_faq(self) -> None:
        result = self.service._classify_query("返品ポリシーは？")
        assert result == QueryType.FAQ


class TestEnhancedAgentClassifyDelegation:
    def setup_method(self) -> None:
        self.agent = EnhancedFAQAgent()

    def test_enhanced_agent_classify_delegates_sql_keywords(self) -> None:
        result = self.agent._classify_query("売上 収入 統計")
        assert result == "sql"

    def test_enhanced_agent_classify_delegates_faq(self) -> None:
        result = self.agent._classify_query("返品ポリシーは？")
        assert result == "faq"
