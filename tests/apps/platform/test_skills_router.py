"""Skills Router エンドポイントのユニットテスト.

テスト対象: apps/platform/routers/skills.py
phase3_test_client を使用してエンドポイントを検証する。
"""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from fastapi.testclient import TestClient


class TestListSkills:
    """GET /api/studios/framework/skills テスト."""

    def test_returns_skill_list(self, phase3_test_client: TestClient) -> None:
        """スキル一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills")
        assert resp.status_code == 200
        data = resp.json()
        assert "skills" in data
        assert "total" in data
        assert data["total"] == 2

    def test_skill_item_structure(self, phase3_test_client: TestClient) -> None:
        """各スキルアイテムに必要なフィールドがある."""
        resp = phase3_test_client.get("/api/studios/framework/skills")
        skills = resp.json()["skills"]
        assert len(skills) > 0
        skill = skills[0]
        required_keys = {
            "name",
            "label",
            "description",
            "version",
            "author",
            "tags",
            "tags_legacy",
            "triggers",
            "requirements",
        }
        assert required_keys.issubset(skill.keys())


class TestGetSkillStats:
    """GET /api/studios/framework/skills/stats テスト."""

    def test_returns_stats(self, phase3_test_client: TestClient) -> None:
        """スキル統計を返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/stats")
        assert resp.status_code == 200
        data = resp.json()
        assert "total_skills" in data
        assert "total_tags" in data
        assert "total_triggers" in data
        assert data["total_skills"] == 2


class TestListTags:
    """GET /api/studios/framework/skills/tags テスト."""

    def test_returns_tags(self, phase3_test_client: TestClient) -> None:
        """タグ一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/tags")
        assert resp.status_code == 200
        data = resp.json()
        assert "tags" in data
        assert "total" in data
        assert data["total"] >= 4

    def test_tag_structure(self, phase3_test_client: TestClient) -> None:
        """各タグに tag, count フィールドがある."""
        resp = phase3_test_client.get("/api/studios/framework/skills/tags")
        tags = resp.json()["tags"]
        assert len(tags) > 0
        tag = tags[0]
        assert "tag" in tag
        assert "count" in tag


class TestSearchSkills:
    """GET /api/studios/framework/skills/search テスト."""

    def test_search_found(self, phase3_test_client: TestClient) -> None:
        """タグ検索でマッチするスキルを返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/search", params={"tag": "chat"})
        assert resp.status_code == 200
        data = resp.json()
        assert data["total"] >= 1
        assert data["query"] == "chat"
        names = {s["name"] for s in data["skills"]}
        assert "chatbot" in names

    def test_search_no_match(self, phase3_test_client: TestClient) -> None:
        """マッチしない検索は空リストを返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/search", params={"tag": "nonexistent_xyz"})
        assert resp.status_code == 200
        assert resp.json()["total"] == 0

    def test_search_missing_param_returns_422(self, phase3_test_client: TestClient) -> None:
        """必須パラメータなしは 422 を返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/search")
        assert resp.status_code == 422


class TestGetSkillDetail:
    """GET /api/studios/framework/skills/{skill_name} テスト."""

    def test_existing_skill(self, phase3_test_client: TestClient) -> None:
        """存在するスキルの詳細を返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/chatbot")
        assert resp.status_code == 200
        data = resp.json()
        assert data["name"] == "chatbot"
        assert data["label"] == "chatbot"
        assert data["description"] == "汎用チャットボットスキル"
        assert any("chat" in tag for tag in data["tags"])
        assert "openai" in data["requirements"]

    def test_nonexistent_skill_returns_404(self, phase3_test_client: TestClient) -> None:
        """存在しないスキルは 404 を返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/nonexistent")
        assert resp.status_code == 404
        detail = resp.json()["detail"]
        assert detail["error_code"] == "SKILL_NOT_FOUND"


# ------------------------------------------------------------------
# カテゴリ関連 API エンドポイント E2E テスト
# ------------------------------------------------------------------


class TestGetCategories:
    """GET /api/studios/framework/skills/categories テスト."""

    def test_returns_categories(self, phase3_test_client: TestClient) -> None:
        """カテゴリ一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/categories")
        assert resp.status_code == 200
        data = resp.json()
        assert "categories" in data
        assert "total" in data
        assert data["total"] == 8

    def test_category_structure(self, phase3_test_client: TestClient) -> None:
        """各カテゴリに必要なフィールドがある."""
        resp = phase3_test_client.get("/api/studios/framework/skills/categories")
        categories = resp.json()["categories"]
        for cat in categories:
            assert "id" in cat
            assert "icon" in cat
            assert "order" in cat
            assert "skill_count" in cat

    def test_sorted_by_order(self, phase3_test_client: TestClient) -> None:
        """order 順でソートされている."""
        resp = phase3_test_client.get("/api/studios/framework/skills/categories")
        categories = resp.json()["categories"]
        orders = [c["order"] for c in categories]
        assert orders == sorted(orders)


class TestGetSkillsGrouped:
    """GET /api/studios/framework/skills/grouped テスト."""

    def test_returns_grouped(self, phase3_test_client: TestClient) -> None:
        """グループ化されたスキル一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/grouped")
        assert resp.status_code == 200
        data = resp.json()
        assert "groups" in data
        assert len(data["groups"]) > 0

    def test_group_structure(self, phase3_test_client: TestClient) -> None:
        """各グループに id, icon, order, skills がある."""
        resp = phase3_test_client.get("/api/studios/framework/skills/grouped")
        groups = resp.json()["groups"]
        for group in groups:
            assert "id" in group
            assert "icon" in group
            assert "order" in group
            assert "skills" in group
            assert isinstance(group["skills"], list)
            assert len(group["skills"]) > 0

    def test_sorted_by_order(self, phase3_test_client: TestClient) -> None:
        """order 順でソートされている."""
        resp = phase3_test_client.get("/api/studios/framework/skills/grouped")
        groups = resp.json()["groups"]
        orders = [g["order"] for g in groups]
        assert orders == sorted(orders)

    def test_skills_have_category_field(self, phase3_test_client: TestClient) -> None:
        """各スキルに category フィールドがある."""
        resp = phase3_test_client.get("/api/studios/framework/skills/grouped")
        groups = resp.json()["groups"]
        for group in groups:
            for skill in group["skills"]:
                assert "category" in skill
                assert skill["category"] == group["id"]

    def test_total_skills_match(self, phase3_test_client: TestClient) -> None:
        """グループ内スキル数合計が /skills の total と一致."""
        grouped_resp = phase3_test_client.get("/api/studios/framework/skills/grouped")
        list_resp = phase3_test_client.get("/api/studios/framework/skills")
        grouped_total = sum(
            len(g["skills"]) for g in grouped_resp.json()["groups"]
        )
        assert grouped_total == list_resp.json()["total"]


class TestGetSkillsByCategory:
    """GET /api/studios/framework/skills/category/{category_id} テスト."""

    def test_valid_category(self, phase3_test_client: TestClient) -> None:
        """有効なカテゴリ ID でスキル一覧を返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/category/ai_assistant")
        assert resp.status_code == 200
        data = resp.json()
        assert "skills" in data
        assert "category" in data
        assert data["category"] == "ai_assistant"
        assert "total" in data

    def test_empty_category(self, phase3_test_client: TestClient) -> None:
        """スキルが存在しないカテゴリは空リストを返す."""
        resp = phase3_test_client.get("/api/studios/framework/skills/category/ad_marketing")
        assert resp.status_code == 200
        data = resp.json()
        assert data["total"] == 0
        assert data["skills"] == []

    def test_unknown_category_returns_empty(self, phase3_test_client: TestClient) -> None:
        """未知のカテゴリ ID は空リストを返す（404 ではない）."""
        resp = phase3_test_client.get("/api/studios/framework/skills/category/nonexistent")
        assert resp.status_code == 200
        assert resp.json()["total"] == 0
        assert resp.json()["skills"] == []
