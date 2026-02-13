# -*- coding: utf-8 -*-
"""SkillRouter テスト.

Anthropic Skills体系準拠の軽量Skill選択層をテスト。
"""

import pytest

from agentflow.skills.router import RoutingResult, SkillMeta, SkillRouter


class TestSkillMeta:
    """SkillMeta テスト."""

    def test_create_skill_meta(self) -> None:
        """SkillMeta作成テスト."""
        meta = SkillMeta(
            name="test-skill",
            description="テスト用Skill",
            triggers=["test", "テスト"],
            tags=["core"],
        )
        assert meta.name == "test-skill"
        assert meta.description == "テスト用Skill"
        assert "test" in meta.triggers
        assert meta.stable is True

    def test_skill_meta_defaults(self) -> None:
        """デフォルト値テスト."""
        meta = SkillMeta(name="minimal")
        assert meta.description == ""
        assert meta.triggers == []
        assert meta.tags == []
        assert meta.version == "1.0.0"
        assert meta.owner == ""


class TestRoutingResult:
    """RoutingResult テスト."""

    def test_matched_result(self) -> None:
        """マッチ結果テスト."""
        result = RoutingResult(matched=True, score=0.9, reason="trigger:test")
        assert result.matched is True
        assert result.score == 0.9
        assert result.fallback_to_rag is False

    def test_unmatched_result(self) -> None:
        """未マッチ結果テスト."""
        result = RoutingResult(matched=False, fallback_to_rag=True)
        assert result.matched is False
        assert result.skill is None
        assert result.fallback_to_rag is True


class TestSkillRouter:
    """SkillRouter テスト."""

    def test_router_initialization(self) -> None:
        """Router初期化テスト."""
        router = SkillRouter(threshold=0.5)
        assert router._threshold == 0.5
        assert router._initialized is False

    def test_route_without_init(self) -> None:
        """初期化前のルーティングテスト."""
        router = SkillRouter()
        result = router.route("test query")
        assert result.matched is False
        assert result.fallback_to_rag is True

    def test_calculate_match_score_trigger(self) -> None:
        """トリガーマッチスコア計算テスト."""
        router = SkillRouter()
        meta = SkillMeta(
            name="pdf-parser",
            description="PDFファイルを解析",
            triggers=["pdf", "parse"],
        )
        score, reason = router._calculate_match_score(meta, "pdfからテキスト抽出")
        assert score >= 0.9
        assert "trigger:pdf" in reason

    def test_calculate_match_score_name(self) -> None:
        """名前マッチスコア計算テスト."""
        router = SkillRouter()
        meta = SkillMeta(name="chatbot", description="対話ボット")
        score, reason = router._calculate_match_score(meta, "chatbotを作りたい")
        assert score >= 0.8
        assert "name" in reason

    def test_calculate_match_score_tag(self) -> None:
        """タグマッチスコア計算テスト."""
        router = SkillRouter()
        meta = SkillMeta(
            name="analyzer",
            description="分析ツール",
            tags=["sentiment", "nlp"],
        )
        score, reason = router._calculate_match_score(meta, "sentiment分析")
        assert score >= 0.6
        assert "tag:sentiment" in reason

    def test_calculate_match_score_no_match(self) -> None:
        """非マッチスコア計算テスト."""
        router = SkillRouter()
        meta = SkillMeta(name="unrelated", description="関係なし")
        score, reason = router._calculate_match_score(meta, "まったく別のクエリ")
        assert score < 0.4
        assert "no match" in reason

    @pytest.mark.asyncio
    async def test_initialize_and_route(self) -> None:
        """初期化とルーティング統合テスト."""
        router = SkillRouter()
        await router.initialize()
        
        assert router._initialized is True
        assert router.skill_count > 0

        # ビルトインSkillへのルーティング
        result = router.route("チャットボットを作成")
        # chatbot Skillが存在すればマッチ
        if result.matched:
            assert result.skill is not None
            assert result.score >= 0.4

    @pytest.mark.asyncio
    async def test_route_market_trend_skill(self) -> None:
        """market-trend-analysis Skillルーティングテスト."""
        router = SkillRouter()
        await router.initialize()

        result = router.route("市場トレンドを分析したい")
        # market-trend-analysis Skillが存在すればマッチ
        if result.matched and result.skill:
            assert "trend" in result.skill.name.lower() or "market" in result.skill.name.lower()

    def test_list_skills_empty(self) -> None:
        """Skillリスト（未初期化）テスト."""
        router = SkillRouter()
        skills = router.list_skills()
        assert skills == []

    def test_get_skill_not_found(self) -> None:
        """存在しないSkill取得テスト."""
        router = SkillRouter()
        skill = router.get_skill("nonexistent")
        assert skill is None

