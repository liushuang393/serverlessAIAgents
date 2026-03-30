"""BuilderDraftStore のユニットテスト."""

from __future__ import annotations

from control_plane.services.builder_draft_store import BuilderDraftStore


def test_builder_draft_store_crud(tmp_path) -> None:
    """draft を作成・更新・削除できる."""
    store = BuilderDraftStore(tmp_path / "builder_drafts.json")

    created = store.create_draft(
        {
            "name": "OpsBuilder",
            "template_id": "ops_automation",
            "goal": "Run ops workflow",
            "spec_kind": "system",
            "spec": {"name": "OpsSystem"},
            "generated_files": {"backend/app.py": "print('ok')"},
            "status": "draft",
        }
    )

    assert created["id"]
    assert created["status"] == "draft"
    assert len(store.list_drafts()) == 1

    updated = store.update_draft(
        created["id"],
        {
            "name": "OpsBuilderV2",
            "template_id": "ops_automation",
            "goal": "Run ops workflow",
            "spec_kind": "system",
            "spec": {"name": "OpsSystemV2"},
            "generated_files": {"backend/app.py": "print('ok')"},
            "status": "generated",
        },
    )

    assert updated["name"] == "OpsBuilderV2"
    assert updated["status"] == "generated"
    assert store.get_draft(created["id"])["spec"]["name"] == "OpsSystemV2"

    store.delete_draft(created["id"])
    assert store.list_drafts() == []
