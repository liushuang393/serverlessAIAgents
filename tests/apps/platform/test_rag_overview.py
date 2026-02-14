# -*- coding: utf-8 -*-
"""RAGOverviewService のユニットテスト.

テスト対象: apps/platform/services/rag_overview.py
"""

from __future__ import annotations

from apps.platform.services.rag_overview import RAGOverviewService


class TestGetOverview:
    """get_overview() メソッドのテスト."""

    def test_returns_expected_keys(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """概要辞書に必要なキーが含まれる."""
        overview = rag_overview.get_overview()
        assert "description" in overview
        assert "chunk_strategies" in overview
        assert "rerankers" in overview
        assert "apps_using_rag" in overview
        assert "stats" in overview

    def test_description_is_string(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """description が文字列."""
        overview = rag_overview.get_overview()
        assert isinstance(overview["description"], str)
        assert len(overview["description"]) > 0

    def test_stats_embedded(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """stats が概要に埋め込まれている."""
        overview = rag_overview.get_overview()
        stats = overview["stats"]
        assert stats["total_strategies"] == 5
        assert stats["total_rerankers"] == 4
        assert stats["total_apps_using_rag"] >= 1

    def test_overview_has_patterns(self, rag_overview: RAGOverviewService) -> None:
        """概要にパターン一覧が含まれる."""
        overview = rag_overview.get_overview()
        assert "patterns" in overview
        assert len(overview["patterns"]) >= 3


class TestListStrategies:
    """list_strategies() メソッドのテスト."""

    def test_returns_five_strategies(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """5 つのチャンキング戦略を返す."""
        strategies = rag_overview.list_strategies()
        assert len(strategies) == 5

    def test_strategy_structure(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """各戦略に name, label, description が含まれる."""
        strategies = rag_overview.list_strategies()
        for s in strategies:
            assert "name" in s
            assert "label" in s
            assert "description" in s

    def test_known_strategy_names(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """既知の戦略名が含まれる."""
        names = {s["name"] for s in rag_overview.list_strategies()}
        assert "recursive" in names
        assert "semantic" in names
        assert "sentence" in names
        assert "token" in names
        assert "markdown" in names


class TestListRerankers:
    """list_rerankers() メソッドのテスト."""

    def test_returns_four_rerankers(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """4 つのリランカーを返す."""
        rerankers = rag_overview.list_rerankers()
        assert len(rerankers) == 4

    def test_reranker_structure(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """各リランカーに name, label, description が含まれる."""
        for r in rag_overview.list_rerankers():
            assert "name" in r
            assert "label" in r
            assert "description" in r

    def test_known_reranker_names(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """既知のリランカー名が含まれる."""
        names = {r["name"] for r in rag_overview.list_rerankers()}
        assert "cohere" in names
        assert "cross_encoder" in names
        assert "bm25" in names
        assert "none" in names


class TestPatternsAndConfigs:
    """パターン・App設定管理のテスト."""

    def test_list_patterns(self, rag_overview: RAGOverviewService) -> None:
        """RAG パターンが取得できる."""
        patterns = rag_overview.list_patterns()
        assert len(patterns) >= 3
        assert any(item["name"] == "balanced_knowledge" for item in patterns)

    def test_list_app_configs(self, rag_overview: RAGOverviewService) -> None:
        """全 App 設定を取得できる."""
        apps = rag_overview.list_app_configs()
        assert len(apps) >= 2
        assert all("rag" in item for item in apps)

    def test_update_app_config(self, rag_overview: RAGOverviewService) -> None:
        """RAG 設定更新が反映される."""
        updated = rag_overview.update_app_config(
            "rag_app",
            {
                "enabled": True,
                "pattern": "faq_precision",
                "retrieval_method": "hybrid",
                "top_k": 7,
            },
        )
        assert updated["rag"]["enabled"] is True
        assert updated["rag"]["pattern"] == "faq_precision"
        assert updated["rag"]["top_k"] == 7

    def test_update_invalid_pattern(self, rag_overview: RAGOverviewService) -> None:
        """未知パターンは ValueError."""
        import pytest

        with pytest.raises(ValueError, match="Unknown RAG pattern"):
            rag_overview.update_app_config("rag_app", {"pattern": "unknown"})


class TestAppsUsingRag:
    """apps_using_rag() メソッドのテスト."""

    def test_finds_rag_app(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """RAG 対応 App を検出する."""
        apps = rag_overview.apps_using_rag()
        assert len(apps) >= 1
        app_names = {a["app_name"] for a in apps}
        assert "rag_app" in app_names

    def test_app_structure(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """各 App に必要なフィールドが含まれる."""
        apps = rag_overview.apps_using_rag()
        for app in apps:
            assert "app_name" in app
            assert "display_name" in app
            assert "icon" in app
            assert "rag_details" in app
            assert isinstance(app["rag_details"], list)

    def test_rag_details_content(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """rag_details に取得方式情報が含まれる."""
        apps = rag_overview.apps_using_rag()
        rag_app = next(a for a in apps if a["app_name"] == "rag_app")
        details = rag_app["rag_details"]
        assert any("retrieval:" in d for d in details)

    def test_non_rag_app_excluded(
        self, rag_overview: RAGOverviewService,
    ) -> None:
        """RAG 非対応 App は除外される."""
        apps = rag_overview.apps_using_rag()
        app_names = {a["app_name"] for a in apps}
        assert "minimal_app" not in app_names
        assert "test_app" not in app_names
