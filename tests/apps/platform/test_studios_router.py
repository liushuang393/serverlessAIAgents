"""Studios Router エンドポイントのユニットテスト."""

from __future__ import annotations

from typing import TYPE_CHECKING


if TYPE_CHECKING:
    from fastapi.testclient import TestClient


class TestStudiosRouter:
    """`/api/studios` テスト."""

    def test_list_studios(self, test_client: TestClient) -> None:
        """3 つの Studio を返す."""
        resp = test_client.get("/api/studios")
        assert resp.status_code == 200
        data = resp.json()
        assert data["total"] == 3
        ids = {item["id"] for item in data["studios"]}
        assert ids == {"migration", "faq", "assistant"}

    def test_list_templates(self, test_client: TestClient) -> None:
        """テンプレート一覧を返す."""
        resp = test_client.get("/api/studios/migration/templates")
        assert resp.status_code == 200
        data = resp.json()
        assert data["studio"] == "migration"
        assert data["total"] >= 1

    def test_create_run_and_get_artifacts(self, test_client: TestClient) -> None:
        """実行登録後に成果物を取得できる."""
        create = test_client.post(
            "/api/studios/assistant/runs",
            json={
                "template_id": "assistant:controlled-ops",
                "risk_level": "medium",
                "security_mode": "approval_required",
            },
        )
        assert create.status_code == 200
        payload = create.json()
        assert payload["status"] == "completed"
        run_id = payload["run_id"]

        artifacts = test_client.get(f"/api/studios/assistant/runs/{run_id}/artifacts")
        assert artifacts.status_code == 200
        data = artifacts.json()
        assert data["run_id"] == run_id
        assert len(data["artifacts"]) == 3

    def test_unknown_studio_returns_404(self, test_client: TestClient) -> None:
        """未知の studio は 404."""
        resp = test_client.get("/api/studios/unknown/templates")
        assert resp.status_code == 404
        assert resp.json()["detail"]["error_code"] == "STUDIO_NOT_FOUND"
