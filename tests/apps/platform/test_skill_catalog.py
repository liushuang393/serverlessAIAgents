"""SkillCatalogService のユニットテスト.

テスト対象: apps/platform/services/skill_catalog.py
"""

from __future__ import annotations

from typing import TYPE_CHECKING

import pytest
from apps.platform.services.skill_catalog import (
    SKILL_CATEGORIES,
    SkillCatalogService,
    SkillInfo,
    infer_category,
)


if TYPE_CHECKING:
    from pathlib import Path


class TestSkillInfo:
    """SkillInfo データクラスのテスト."""

    def test_to_dict(self) -> None:
        """to_dict() が全フィールドを含む辞書を返す."""
        skill = SkillInfo(
            name="test_skill",
            label="Test Skill",
            description="テストスキル",
            version="1.0.0",
            author="Test",
            tags=["tag1", "tag2"],
            tags_legacy=["Tag-1", "Tag 2"],
            triggers=["hello"],
            requirements=["openai"],
            examples=["example1"],
            path="skills/test/SKILL.md",
        )
        d = skill.to_dict()
        assert d["name"] == "test_skill"
        assert d["label"] == "Test Skill"
        assert d["description"] == "テストスキル"
        assert d["version"] == "1.0.0"
        assert len(d["tags"]) == 2
        assert d["tags_legacy"] == ["Tag-1", "Tag 2"]
        assert d["triggers"] == ["hello"]
        assert d["requirements"] == ["openai"]
        assert d["path"] == "skills/test/SKILL.md"

    def test_defaults(self) -> None:
        """デフォルト値が正しく設定される."""
        skill = SkillInfo(name="minimal")
        assert skill.label == "minimal"
        assert skill.description == ""
        assert skill.version == "1.0.0"
        assert skill.tags == []
        assert skill.triggers == []


class TestScan:
    """scan() メソッドのテスト."""

    @pytest.mark.asyncio
    async def test_scan_finds_valid_skills(self, skills_dir: Path) -> None:
        """有効な SKILL.md を検出する."""
        catalog = SkillCatalogService(skills_dir=skills_dir)
        count = await catalog.scan()
        # chatbot + rag = 2（invalid_skill は frontmatter なしで除外）
        assert count == 2

    @pytest.mark.asyncio
    async def test_scan_nonexistent_dir(self, tmp_path: Path) -> None:
        """存在しないディレクトリでは 0 を返す."""
        catalog = SkillCatalogService(skills_dir=tmp_path / "nonexistent")
        count = await catalog.scan()
        assert count == 0

    @pytest.mark.asyncio
    async def test_scan_empty_dir(self, tmp_path: Path) -> None:
        """空ディレクトリでは 0 を返す."""
        catalog = SkillCatalogService(skills_dir=tmp_path)
        count = await catalog.scan()
        assert count == 0

    @pytest.mark.asyncio
    async def test_scan_clears_previous(self, skills_dir: Path) -> None:
        """再スキャン時に前回の結果をクリアする."""
        catalog = SkillCatalogService(skills_dir=skills_dir)
        await catalog.scan()
        assert len(catalog.list_skills()) == 2
        await catalog.scan()
        assert len(catalog.list_skills()) == 2

    @pytest.mark.asyncio
    async def test_scan_skips_no_frontmatter(self, skills_dir: Path) -> None:
        """frontmatter なしの SKILL.md をスキップする."""
        catalog = SkillCatalogService(skills_dir=skills_dir)
        await catalog.scan()
        assert catalog.get_skill("invalid_skill") is None


class TestListSkills:
    """list_skills() メソッドのテスト."""

    def test_returns_sorted_by_name(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """名前順でソートされる."""
        skills = skill_catalog.list_skills()
        names = [s.name for s in skills]
        assert names == sorted(names)

    def test_skill_has_metadata(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """スキルにメタデータが含まれる."""
        skills = skill_catalog.list_skills()
        chatbot = next(s for s in skills if s.name == "chatbot")
        assert chatbot.label == "chatbot"
        assert chatbot.description == "汎用チャットボットスキル"
        assert chatbot.version == "1.0.0"
        assert chatbot.author == "AgentFlow Team"
        assert any("chat" in tag for tag in chatbot.tags)
        assert "chat" in chatbot.tags_legacy
        assert "こんにちは" in chatbot.triggers
        assert "openai" in chatbot.requirements


class TestGetSkill:
    """get_skill() メソッドのテスト."""

    def test_found(self, skill_catalog: SkillCatalogService) -> None:
        """存在するスキルを取得."""
        skill = skill_catalog.get_skill("chatbot")
        assert skill is not None
        assert skill.name == "chatbot"

    def test_not_found(self, skill_catalog: SkillCatalogService) -> None:
        """存在しないスキルは None."""
        assert skill_catalog.get_skill("nonexistent") is None


class TestSearchByTag:
    """search_by_tag() メソッドのテスト."""

    def test_exact_match(self, skill_catalog: SkillCatalogService) -> None:
        """タグ完全一致で検索."""
        results = skill_catalog.search_by_tag("chat")
        assert len(results) == 1
        assert results[0].name == "chatbot"

    def test_partial_match(self, skill_catalog: SkillCatalogService) -> None:
        """タグ部分一致で検索."""
        results = skill_catalog.search_by_tag("retriev")
        assert len(results) == 1
        assert results[0].name == "rag"

    def test_no_match(self, skill_catalog: SkillCatalogService) -> None:
        """マッチしない場合は空リスト."""
        assert skill_catalog.search_by_tag("nonexistent") == []


class TestAllTags:
    """all_tags() メソッドのテスト."""

    def test_returns_tag_count(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """tag, count フィールドを含む."""
        tags = skill_catalog.all_tags()
        assert len(tags) > 0
        first = tags[0]
        assert "tag" in first
        assert "count" in first

    def test_sorted_by_count_desc(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """出現回数の降順でソートされる."""
        tags = skill_catalog.all_tags()
        counts = [t["count"] for t in tags]
        assert counts == sorted(counts, reverse=True)

    def test_unique_tags(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """各タグが一意."""
        tags = skill_catalog.all_tags()
        tag_names = [t["tag"] for t in tags]
        assert len(tag_names) == len(set(tag_names))


class TestSkillStats:
    """stats() メソッドのテスト."""

    def test_returns_expected_keys(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """統計辞書に必要なキーが含まれる."""
        s = skill_catalog.stats()
        assert "total_skills" in s
        assert "total_tags" in s
        assert "total_triggers" in s

    def test_correct_counts(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """正しいカウントを返す."""
        s = skill_catalog.stats()
        assert s["total_skills"] == 2
        assert s["total_tags"] >= 4
        # chatbot: こんにちは, 教えて / rag: 検索して, 調べて = 4 unique
        assert s["total_triggers"] == 4


class TestEnsureList:
    """_ensure_list() 静的メソッドのテスト."""

    def test_none_returns_empty(self) -> None:
        """None は空リスト."""
        assert SkillCatalogService._ensure_list(None) == []

    def test_list_returns_str_list(self) -> None:
        """リストは文字列リストに変換."""
        assert SkillCatalogService._ensure_list([1, "two", 3]) == ["1", "two", "3"]

    def test_scalar_returns_single_list(self) -> None:
        """スカラー値は単一要素リスト."""
        assert SkillCatalogService._ensure_list("hello") == ["hello"]
        assert SkillCatalogService._ensure_list(42) == ["42"]


# ------------------------------------------------------------------
# カテゴリ関連テスト
# ------------------------------------------------------------------


class TestInferCategory:
    """infer_category() 関数のテスト."""

    def test_tag_based_inference(self) -> None:
        """タグからカテゴリを推論する."""
        assert infer_category(["conversation", "nlp"], "chatbot") == "ai_assistant"

    def test_code_development_tags(self) -> None:
        """コード開発系タグの推論."""
        assert infer_category(["docker", "deployment"], "deployer") == "code_development"

    def test_web_search_tags(self) -> None:
        """Web検索系タグの推論."""
        assert infer_category(["trend-analysis", "news-analysis"], "news") == "web_search"

    def test_media_creative_tags(self) -> None:
        """メディア・クリエイティブ系タグの推論."""
        assert infer_category(["image-generation", "comfyui"], "image-gen") == "media_creative"

    def test_enterprise_office_tags(self) -> None:
        """企業オフィス系タグの推論."""
        assert infer_category(["calendar", "schedule"], "scheduler") == "enterprise_office"

    def test_name_fallback(self) -> None:
        """タグにマッチしない場合、名前からフォールバック推論する."""
        assert infer_category(["unknown-tag"], "voice-assistant") == "ai_assistant"

    def test_default_common(self) -> None:
        """タグ・名前どちらにもマッチしない場合は common."""
        assert infer_category(["unknown"], "unknown-skill") == "common"

    def test_empty_tags(self) -> None:
        """空タグリストでは名前ベースまたは common にフォールバック."""
        assert infer_category([], "unknown") == "common"

    def test_majority_vote(self) -> None:
        """複数カテゴリにマッチする場合、多数決で決定."""
        # conversation=ai_assistant, dialogue=ai_assistant, docker=code_development
        # → ai_assistant が 2 票で勝つ
        result = infer_category(["conversation", "dialogue", "docker"], "test")
        assert result == "ai_assistant"


class TestSkillCategories:
    """SKILL_CATEGORIES 定数のテスト."""

    def test_has_8_categories(self) -> None:
        """8 カテゴリが定義されている."""
        assert len(SKILL_CATEGORIES) == 8

    def test_required_category_ids(self) -> None:
        """必須カテゴリ ID が全て存在する."""
        expected = {
            "common", "code_development", "web_search",
            "enterprise_office", "ad_marketing", "enterprise_workflow",
            "ai_assistant", "media_creative",
        }
        assert set(SKILL_CATEGORIES.keys()) == expected

    def test_each_category_has_order_and_icon(self) -> None:
        """各カテゴリに order と icon が含まれる."""
        for cat_id, meta in SKILL_CATEGORIES.items():
            assert "order" in meta, f"{cat_id} に order がない"
            assert "icon" in meta, f"{cat_id} に icon がない"
            assert isinstance(meta["order"], int)
            assert isinstance(meta["icon"], str)

    def test_orders_are_unique(self) -> None:
        """order 値が一意."""
        orders = [m["order"] for m in SKILL_CATEGORIES.values()]
        assert len(orders) == len(set(orders))


class TestGetCategories:
    """get_categories() メソッドのテスト."""

    def test_returns_all_categories(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """全 8 カテゴリを返す."""
        categories = skill_catalog.get_categories()
        assert len(categories) == 8

    def test_sorted_by_order(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """order 順でソートされている."""
        categories = skill_catalog.get_categories()
        orders = [c["order"] for c in categories]
        assert orders == sorted(orders)

    def test_category_structure(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """各カテゴリに必要なフィールドがある."""
        categories = skill_catalog.get_categories()
        for cat in categories:
            assert "id" in cat
            assert "icon" in cat
            assert "order" in cat
            assert "skill_count" in cat
            assert isinstance(cat["skill_count"], int)

    def test_skill_count_sums_to_total(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """全カテゴリの skill_count 合計がスキル総数と一致."""
        categories = skill_catalog.get_categories()
        total = sum(c["skill_count"] for c in categories)
        assert total == len(skill_catalog.list_skills())


class TestGetSkillsGroupedByCategory:
    """get_skills_grouped_by_category() メソッドのテスト."""

    def test_returns_non_empty_groups(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """スキルが存在するカテゴリのみ返す."""
        groups = skill_catalog.get_skills_grouped_by_category()
        assert len(groups) > 0
        for group in groups:
            assert len(group["skills"]) > 0

    def test_group_structure(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """各グループに id, icon, order, skills がある."""
        groups = skill_catalog.get_skills_grouped_by_category()
        for group in groups:
            assert "id" in group
            assert "icon" in group
            assert "order" in group
            assert "skills" in group
            assert isinstance(group["skills"], list)

    def test_sorted_by_order(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """order 順でソートされている."""
        groups = skill_catalog.get_skills_grouped_by_category()
        orders = [g["order"] for g in groups]
        assert orders == sorted(orders)

    def test_skills_are_dicts(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """各スキルが辞書形式（to_dict 結果）."""
        groups = skill_catalog.get_skills_grouped_by_category()
        for group in groups:
            for skill in group["skills"]:
                assert isinstance(skill, dict)
                assert "name" in skill
                assert "category" in skill

    def test_total_skills_match(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """全グループのスキル数合計がスキル総数と一致."""
        groups = skill_catalog.get_skills_grouped_by_category()
        total = sum(len(g["skills"]) for g in groups)
        assert total == len(skill_catalog.list_skills())


class TestGetSkillsByCategory:
    """get_skills_by_category() メソッドのテスト."""

    def test_returns_matching_skills(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """指定カテゴリのスキルのみ返す."""
        # chatbot は conversation タグ → ai_assistant カテゴリ
        skills = skill_catalog.get_skills_by_category("ai_assistant")
        for skill in skills:
            assert skill.category == "ai_assistant"

    def test_empty_category_returns_empty(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """スキルが存在しないカテゴリは空リスト."""
        skills = skill_catalog.get_skills_by_category("ad_marketing")
        assert skills == []

    def test_sorted_by_name(
        self,
        skill_catalog: SkillCatalogService,
    ) -> None:
        """名前順でソートされている."""
        # common カテゴリ（rag は retrieval タグ → common）
        skills = skill_catalog.get_skills_by_category("common")
        names = [s.name for s in skills]
        assert names == sorted(names)


class TestSkillInfoCategory:
    """SkillInfo の category フィールドのテスト."""

    def test_to_dict_includes_category(self) -> None:
        """to_dict() に category フィールドが含まれる."""
        skill = SkillInfo(name="test", category="code_development")
        d = skill.to_dict()
        assert "category" in d
        assert d["category"] == "code_development"

    def test_default_category_empty(self) -> None:
        """デフォルトの category は空文字列."""
        skill = SkillInfo(name="test")
        assert skill.category == ""
