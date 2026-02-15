# -*- coding: utf-8 -*-
"""Skills Router エンドポイントのユニットテスト.

テスト対象: apps/platform/routers/skills.py
phase3_test_client を使用してエンドポイントを検証する。
"""

from __future__ import annotations

from fastapi.testclient import TestClient


class TestListSkills:
    """GET /api/skills テスト."""

    def test_returns_skill_list(self, phase3_test_client: TestClient) -> None:
        """スキル一覧を返す."""
        resp = phase3_test_client.get("/api/skills")
        assert resp.status_code == 200
        data = resp.json()
        assert "skills" in data
        assert "total" in data
        assert data["total"] == 2

    def test_skill_item_structure(self, phase3_test_client: TestClient) -> None:
        """各スキルアイテムに必要なフィールドがある."""
        resp = phase3_test_client.get("/api/skills")
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
    """GET /api/skills/stats テスト."""

    def test_returns_stats(self, phase3_test_client: TestClient) -> None:
        """スキル統計を返す."""
        resp = phase3_test_client.get("/api/skills/stats")
        assert resp.status_code == 200
        data = resp.json()
        assert "total_skills" in data
        assert "total_tags" in data
        assert "total_triggers" in data
        assert data["total_skills"] == 2


class TestListTags:
    """GET /api/skills/tags テスト."""

    def test_returns_tags(self, phase3_test_client: TestClient) -> None:
        """タグ一覧を返す."""
        resp = phase3_test_client.get("/api/skills/tags")
        assert resp.status_code == 200
        data = resp.json()
        assert "tags" in data
        assert "total" in data
        assert data["total"] >= 4

    def test_tag_structure(self, phase3_test_client: TestClient) -> None:
        """各タグに tag, count フィールドがある."""
        resp = phase3_test_client.get("/api/skills/tags")
        tags = resp.json()["tags"]
        assert len(tags) > 0
        tag = tags[0]
        assert "tag" in tag
        assert "count" in tag


class TestSearchSkills:
    """GET /api/skills/search テスト."""

    def test_search_found(self, phase3_test_client: TestClient) -> None:
        """タグ検索でマッチするスキルを返す."""
        resp = phase3_test_client.get("/api/skills/search", params={"tag": "chat"})
        assert resp.status_code == 200
        data = resp.json()
        assert data["total"] >= 1
        assert data["query"] == "chat"
        names = {s["name"] for s in data["skills"]}
        assert "chatbot" in names

    def test_search_no_match(self, phase3_test_client: TestClient) -> None:
        """マッチしない検索は空リストを返す."""
        resp = phase3_test_client.get("/api/skills/search", params={"tag": "nonexistent_xyz"})
        assert resp.status_code == 200
        assert resp.json()["total"] == 0

    def test_search_missing_param_returns_422(self, phase3_test_client: TestClient) -> None:
        """必須パラメータなしは 422 を返す."""
        resp = phase3_test_client.get("/api/skills/search")
        assert resp.status_code == 422


class TestGetSkillDetail:
    """GET /api/skills/{skill_name} テスト."""

    def test_existing_skill(self, phase3_test_client: TestClient) -> None:
        """存在するスキルの詳細を返す."""
        resp = phase3_test_client.get("/api/skills/chatbot")
        assert resp.status_code == 200
        data = resp.json()
        assert data["name"] == "chatbot"
        assert data["label"] == "chatbot"
        assert data["description"] == "汎用チャットボットスキル"
        assert any("chat" in tag for tag in data["tags"])
        assert "openai" in data["requirements"]

    def test_nonexistent_skill_returns_404(self, phase3_test_client: TestClient) -> None:
        """存在しないスキルは 404 を返す."""
        resp = phase3_test_client.get("/api/skills/nonexistent")
        assert resp.status_code == 404
        detail = resp.json()["detail"]
        assert detail["error_code"] == "SKILL_NOT_FOUND"
